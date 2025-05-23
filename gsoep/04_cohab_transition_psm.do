********************************************************************************
********************************************************************************
* Project: Marriage as Gendering
* Owner: Kimberly McErlean
* Started: March 2025
* File: cohab_transition_analysis
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files actually conducts the analysis

use "$created_data/gsoep_matched_couples.dta", clear

********************************************************************************
* Final sample restrictions and things
********************************************************************************
// deduplicate
tab marst_defacto
tab marst_defacto if sex_pl==1

/* need to end up with this amount of respondents after the below

  marst_defacto |      Freq.     Percent        Cum.
----------------+-----------------------------------
        Married |    215,977       89.24       89.24
      Partnered |     26,039       10.76      100.00
----------------+-----------------------------------
          Total |    242,016      100.00


*/

// keep only one respondent per household (bc all data recorded for all)
sort hid pid syear
browse hid pid partner_id_pl syear sex_pl sex_pl_sp

bysort syear hid : egen per_id = rank(pid)
browse hid pid partner_id_pl syear per_id sex_pl sex_pl_sp

tab per_id, m // 1s should approximately total above
keep if inlist(per_id,1,3,5,7)

tab marst_defacto
/*
  marst_defacto |      Freq.     Percent        Cum.
----------------+-----------------------------------
        Married |    215,841       89.18       89.18
      Partnered |     26,183       10.82      100.00
----------------+-----------------------------------
          Total |    242,024      100.00
*/

// now only keep relationships that started after a certain date
tab syear marst_defacto, m
tab rel_start_all marst_defacto, m

unique pid partner_id_pl, by(marst_defacto) 
unique pid partner_id_pl if rel_start_all >=1985, by(marst_defacto) // I def don't want 1984
unique pid partner_id_pl if rel_start_all >=1990, by(marst_defacto)

keep if rel_start_all >= 1985 // consistent with psid except ukhls only started 1991 so think I will need to revisit this...

// variables to get treated and control
bysort pid partner_id_pl: egen years_observed=count(syear)

bysort pid partner_id_pl: egen years_married=count(syear) if marst_defacto==1
bysort pid partner_id_pl (years_married): replace years_married = years_married[1]

bysort pid partner_id_pl: egen years_cohab=count(syear) if marst_defacto==2
bysort pid partner_id_pl (years_cohab): replace years_cohab = years_cohab[1]

sort pid partner_id_pl syear
browse pid partner_id_pl syear years_observed years_married years_cohab ever_transition marst_defacto marr_trans rel_start_all year_transitioned

gen always_married=0
replace always_married=1 if years_observed==years_married & years_married!=.

gen always_cohab=0
replace always_cohab=1 if years_observed==years_cohab & years_cohab!=.

	// none of these should overlap
	tab always_cohab always_married, m
	tab always_cohab ever_transition, m
	tab always_married ever_transition, m
	
browse pid partner_id_pl syear years_observed years_married years_cohab ever_transition marst_defacto marr_trans rel_start_all year_transitioned if ever_transition==1 & (always_cohab==1 | always_married==1)
	
// only keep certain ages
tab age employed_binary, row
keep if (age>=18 & age<=60) &  (age_sp>=18 & age_sp<=60) // sort of drops off a cliff after 60?

// create couple-level education
tab isced97_pg isced97_pg_sp, m

gen couple_educ_gp=0
replace couple_educ_gp = 1 if isced97_pg==6 | isced97_pg_sp==6
replace couple_educ_gp=. if isced97_pg==. & isced97_pg_sp==.

tab couple_educ_gp, m

// home_owners
recode housing_status_hl (.n=.)(.s=.)(1/2=0)(3=1)(4/5=0), gen(home_owner)
tab home_owner, m
tab housing_status_hl home_owner, m

// children
gen kids_in_hh=.
replace kids_in_hh=0 if num_children_hl==0 & num_children_hl_sp==0
replace kids_in_hh=1 if (num_children_hl>0 & num_children_hl<100) | (num_children_hl_sp>0 & num_children_hl_sp<100)

tab kids_in_hh, m
tab num_children_hl kids_in_hh, m

// ugh I forgot birth history before (added to step 1, but not merging on there for now - will do later when i inevitably revisit all of this)
merge m:1 pid using "$temp/biobirth_cleaned.dta"
drop if _merge==2
drop _merge

tab any_births, m
tab first_birth_year any_births, m

// then add other birth variables I need
gen birth_after_trans=0
replace birth_after_trans = 1 if first_birth_year >= year_transitioned & first_birth_year!=9999 & any_births!=0 & any_births!=.

browse pid partner_id_pl syear rel_start_all marst_defacto marr_trans birth_after_trans year_transitioned any_births first_birth_year num_children_hl num_children_u16_hl kids_in_hh

// age
gen age_woman = age if sex_pl==2
replace age_woman = age_sp if sex_pl==1

gen age_man = age if sex_pl==1
replace age_man = age_sp if sex_pl==2

********************************************************************************
* before dropping, get descriptive comparison of cohabitors to married couples
********************************************************************************

tabstat female_earn_pct_net female_hours_pct_t female_housework_pct_avg female_childcare_pct_avg, by(marst_defacto) 
ttest female_earn_pct_net, by(marst_defacto) 
ttest female_hours_pct_t, by(marst_defacto) 
ttest female_housework_pct_avg, by(marst_defacto) 
ttest female_childcare_pct_avg, by(marst_defacto) 

unique pid partner_id_pl, by(marst_defacto) 
unique pid partner_id_pl if marst_defacto==1
unique pid partner_id_pl if marst_defacto==2

// % ever transition
tab marst_defacto ever_transition, row

// some small descriptives
tabstat age_woman age_man couple_earnings_net couple_educ_gp home_owner kids_in_hh, by(marst_defacto)

********************************************************************************
**# okay make analytical sample and recode duration relative to marital transition
********************************************************************************
keep if ever_transition==1 | always_cohab==1 // so keeping a "control" group - basically drops those always married

gen treated=.
replace treated=1 if ever_transition==1 & always_cohab!=1
replace treated=0 if always_cohab==1

tab year_transitioned ever_transition, m

gen relationship_duration = syear - rel_start_all


gen duration_cohab=.
replace duration_cohab = syear - year_transitioned if treated==1
replace duration_cohab = relationship_duration if treated==0

tab duration_cohab, m

browse pid partner_id_pl syear treated marst_defacto year_transitioned relationship_duration duration_cohab

// duration transitioned

browse pid partner_id_pl syear marst_defacto marr_trans relationship_duration duration_cohab year_transitioned

gen dur_transitioned=relationship_duration if marr_trans==1
bysort pid partner_id_pl (dur_transitioned): replace dur_transitioned = dur_transitioned[1]

sort pid partner_id_pl syear
browse pid partner_id_pl syear marst_defacto marr_trans dur_transitioned year_transitioned relationship_duration duration_cohab 

********************************************************************************
* Add descriptive statistics and descriptive exploration
********************************************************************************
unique pid partner_id_pl, by(treated) 
unique pid partner_id_pl if treated==0
unique pid partner_id_pl if treated==1

// some small descriptives
tabstat age_woman age_man couple_earnings_net couple_educ_gp home_owner kids_in_hh, by(treated)

********************************************************************************
********************************************************************************
********************************************************************************
** PROPENSITY SCORE MATCHING EXPLORATION
********************************************************************************
********************************************************************************
********************************************************************************
* First, do some sample things
// want to observe in relatively early year of relationship
bysort pid partner_id_pl: egen min_dur = min(relationship_duration)
tab min_dur, m // 98.6% observer year 1 or 2

keep if min_dur <=5 // not really an issue here but will drop nonetheless

// for treated, need to observe at least one year in each state
tab years_observed treated, m
tab years_married treated, m
tab years_cohab treated, m

browse  pid partner_id_pl syear marst_defacto treated relationship_duration year_transitioned years_observed years_married years_cohab
keep if treated==0 | (treated==1 & years_married>=1 & years_married!=. & years_cohab>=1 & years_cohab!=.)

// for control, do we want to restrict to min of 2 years? or is one sufficient? 
// (I am loosely following Kapelle and she says just one valid wealth wave is needed)

********************************************************************************
**# Preliminary PSM (this is very crude)
********************************************************************************
// missing variable check (will only create if no missing)
inspect isced97_pg isced97_pg_sp emplst_pg emplst_pg_sp psample_pl psample_pl_sp where_germany_pl age age_sp couple_earnings_net housework_daily_avg housework_daily_avg_sp housing_status_hl num_children_hl rel_start_all first_birth_year 
// will replace num_children_hl with first_birth_year and sumkids bc about 8% missing

logit treated i.isced97_pg i.isced97_pg_sp i.emplst_pg i.emplst_pg_sp i.psample_pl i.psample_pl_sp i.where_germany_pl age age_sp couple_earnings_net housework_daily_avg housework_daily_avg_sp i.housing_status_hl rel_start_all first_birth_year sumkids if relationship_duration==min_dur // PSM based on characteristics at start of cohab OR first observed. 
predict psm if relationship_duration==min_dur

bysort pid partner_id_pl: egen pscore = max(psm)
inspect pscore // about 7% missing
sort pid partner_id_pl syear
browse pid partner_id_pl relationship_duration psm pscore min_dur isced97_pg isced97_pg_sp where_germany_pl age age_sp couple_earnings_net housing_status_hl first_birth_year sumkids rel_start_all

// check for overlap
tabstat psm, by(treated)
sum psm if treated==0, det
sum psm if treated==1, det
tabstat pscore, by(treated)

set scheme cleanplots
twoway (histogram psm if treated==1, width(.02) color(pink%30)) (histogram psm if treated==0, width(.02) color(gs8%30)),  legend(order(1 "Treated" 2 "Control") rows(1) position(6)) xtitle("Propensity Score")
twoway (histogram pscore if treated==1, width(.02) color(pink%30)) (histogram pscore if treated==0, width(.02) color(gs8%30)),  legend(order(1 "Treated" 2 "Control") rows(1) position(6)) xtitle("Propensity Score")

********************************************************************************
**# Option 1: Use PSMATCH2 to match AT BASELINE
********************************************************************************
// nearest neighbor matching
psmatch2 treated, pscore(psm) neighbor(5) 
sort _id
browse pid treated psm pscore rel_start_all min_dur marr_trans ever_transition year_transitioned relationship_duration _pscore _treated _support _weight _id _n1 _n2 _n3 _n4 _n5 _nn _pdif _Unique
browse pid partner_id_pl treated psm pscore rel_start_all min_dur marr_trans ever_transition year_transitioned relationship_duration _pscore _treated _support _weight _id _n1 _n2 _n3 _n4 _n5 _nn _pdif _Unique if _treated!=.

tab _weight treated
tab treated _nn if relationship_duration == min_dur, m

sort pid partner_id_pl relationship_duration
browse pid partner_id_pl syear marst_defacto treated psm pscore rel_start_all relationship_duration min_dur year_transitioned duration_cohab _pscore _treated _support _weight _id _n1 _n2 _n3 _n4 _n5 _nn _pdif _Unique ///
if inlist(pid, 5802, 2645804, 5151003, 889202, 1135002, 2998601)

// let's create a lookup file of ids for the controls
preserve

keep _id pid partner_id_pl rel_start_all min_dur years_observed
rename _id _n1
gen _n2 = _n1
gen _n3 = _n1
gen _n4 = _n1
gen _n5 = _n1
rename pid matched_pid
rename partner_id_pl matched_partner_id_pl
rename rel_start_all matched_rel_start
rename min_dur matched_min_dur
rename years_observed matched_years_obs

save "$temp/psm_id_lookup_nn5.dta", replace

restore

// now merge on the control info
*Try to make this a loop through the 5 (_n1 to _n5)
forvalues n=1/5{
	merge m:1 _n`n' using "$temp/psm_id_lookup_nn5.dta"
	drop if _merge==2
	drop _merge
	
	rename (matched_pid matched_partner_id_pl matched_rel_start matched_min_dur matched_years_obs) ///
	(matched_pid`n' matched_partner_id_pl`n' matched_rel_start`n' matched_min_dur`n' matched_years_obs`n')

	bysort pid partner_id_pl (matched_pid`n'): 				replace matched_pid`n' = matched_pid`n'[1]
	bysort pid partner_id_pl (matched_partner_id_pl`n'): 	replace matched_partner_id_pl`n' = matched_partner_id_pl`n'[1]
	bysort pid partner_id_pl (matched_rel_start`n'): 		replace matched_rel_start`n' = matched_rel_start`n'[1]
	bysort pid partner_id_pl (matched_min_dur`n'): 			replace matched_min_dur`n' = matched_min_dur`n'[1]
	bysort pid partner_id_pl (matched_years_obs`n'): 		replace matched_years_obs`n' = matched_years_obs`n'[1]
}

sort pid partner_id_pl syear
browse pid partner_id_pl syear treated rel_start_all relationship_duration  min_dur years_observed _id _n* matched_pid* matched_partner_id_pl* matched_rel_start* matched_min_dur* matched_years_obs*

// going to attempt to rescale all durations so start at min dur and just increment 1 - to make control and treated match
sort pid partner_id_pl relationship_duration

browse pid partner_id_pl relationship_duration treated

gen relationship_counter=.
replace relationship_counter = 0 if relationship_duration == min_dur
replace relationship_counter = relationship_counter[_n-1] + 1 if relationship_counter==. & pid == pid[_n-1] & partner_id_pl == partner_id_pl[_n-1]
tab relationship_counter, m

gen transition_counter=.
replace transition_counter = relationship_counter if relationship_duration == dur_transitioned
bysort pid partner_id_pl (transition_counter): replace transition_counter = transition_counter[1]

sort pid partner_id_pl relationship_duration
browse pid partner_id_pl syear rel_start_all relationship_duration relationship_counter transition_counter year_transitioned dur_transitioned

save "$temp/GSOEP_psm_dataset_nn5.dta", replace // save here so I can start to create a lookup file

**************************************
* Now create treated only version
**************************************
keep if treated==1

//do they all have matches?
inspect matched_pid* // okay no lolz
inspect pscore // okay, it's people that don't have propensity score, probably bc of missing data

drop if matched_pid1==.

browse  pid partner_id_pl syear rel_start_all relationship_duration  min_dur years_observed _id _n* _nn matched_pid* matched_partner_id_pl* matched_rel_start* matched_min_dur* matched_years_obs*

save "$temp/GSOEP_psm_treated.dta", replace  // will use this as base, then add control info, then resave

**************************************
* Now clean up full file for matching
**************************************
use "$temp/GSOEP_psm_dataset_nn5.dta", clear

keep pid partner_id_pl relationship_counter relationship_duration female_earn_pct_net female_hours_pct_t female_housework_pct_avg

forvalues n=1/5{
	gen long matched_pid`n' = pid
	gen long matched_partner_id_pl`n' = partner_id_pl
	gen matched_true_dur`n' = relationship_duration
	gen matched_earn_pct`n' = female_earn_pct_net
	gen matched_hours_pct`n' = female_hours_pct_t
	gen matched_hw_pct`n' = female_housework_pct_avg
}

// browse pid partner_id_pl relationship_counter matched_pid1 matched_partner_id_pl1

drop pid partner_id_pl relationship_duration female_earn_pct_net female_hours_pct_t female_housework_pct_avg

save "$temp/GSOEP_psm_tomatch.dta", replace

**************************************
* Merge partner info
**************************************
use "$temp/GSOEP_psm_treated.dta", clear

forvalues n=1/5{
	merge m:1 matched_pid`n' matched_partner_id_pl`n' relationship_counter using "$temp/GSOEP_psm_tomatch.dta", ///
	keepusing(matched_true_dur`n' matched_earn_pct`n' matched_hours_pct`n' matched_hw_pct`n') // some people have same id, so it's not 1:1 in master
	drop if _merge==2

	tab _merge // okay, not a lot of matched. so some fo this is because cohab is shorter. but need to figure out how many have NO matching records at all...
	egen ever_match`n' = max(_merge)
	tab ever_match`n', m // okay, so they all have at least one matching record. if none, max would be 1, so this would be bad
	egen all_match`n' = min(_merge) // do any match ALL records?
	tab all_match`n', m // lol NO

	gen has_control`n' = 0 if _merge==1
	replace has_control`n' = 1 if _merge==3
	drop _merge
}


sort pid partner_id_pl relationship_counter

browse pid partner_id_pl syear rel_start_all relationship_counter relationship_duration has_control* matched_pid* matched_partner_id_pl* matched_rel_start* matched_years_obs* matched_true_dur* female_earn_pct_net female_hours_pct_t female_housework_pct_avg matched_earn_pct* matched_hours_pct* matched_hw_pct*

save "$created_data/GSOEP_psm_matched.dta", replace

**************************************
* Preliminary analysis
**************************************
tab relationship_counter has_control1, m // row
drop if relationship_counter >=10 // very few records and matches. might even need to remove more than that

browse pid partner_id_pl syear rel_start_all relationship_counter relationship_duration has_control* matched_pid* matched_partner_id_pl* matched_rel_start* matched_years_obs* matched_true_dur* female_earn_pct_net female_hours_pct_t female_housework_pct_avg matched_earn_pct* matched_hours_pct* matched_hw_pct*

gen duration_centered = relationship_counter - transition_counter

preserve

collapse (median) female_earn_pct_net female_hours_pct_t female_housework_pct_avg matched_earn_pct* matched_hours_pct* matched_hw_pct*, by(duration_centered)

twoway (line female_earn_pct_net duration_centered if duration_centered>=-5) (line matched_earn_pct* duration_centered if duration_centered>=-5), legend(order(1 "Treated" 2 "Control1" 3 "Control2" 4 "Control3" 5 "Control4" 6 "Control5")) xtitle(`"Duration from Marital Transition"') // rows(1) position(6) - old legend characteristics

twoway (line female_hours_pct_t duration_centered if duration_centered>=-5) (line matched_hours_pct* duration_centered if duration_centered>=-5), legend(order(1 "Treated" 2 "Control1" 3 "Control2" 4 "Control3" 5 "Control4" 6 "Control5")) xtitle(`"Duration from Marital Transition"')

twoway (line female_housework_pct_avg duration_centered if duration_centered>=-5) (line matched_hw_pct* duration_centered if duration_centered>=-5), legend(order(1 "Treated" 2 "Control1" 3 "Control2" 4 "Control3" 5 "Control4" 6 "Control5")) xtitle(`"Duration from Marital Transition"')

restore

**************************************
**# * Actual analysis with matched data
**************************************
* Need to get these stacked instead of next to each other, so can create a column for treatment and interact, like I do below...do I have to cut, rename, and append?
* Yeah, definitely will need to add other characteristics so can control? Or is that all taken care of in propensity score anyway?

browse pid partner_id_pl syear rel_start_all relationship_counter transition_counter duration_centered matched_pid* matched_partner_id_pl* matched_rel_start* matched_years_obs* matched_true_dur* female_earn_pct_net female_hours_pct_t female_housework_pct_avg matched_earn_pct* matched_hours_pct* matched_hw_pct*

// get treated that I will then append control on to

preserve

keep pid partner_id_pl syear rel_start_all treated relationship_counter transition_counter duration_centered female_earn_pct_net female_hours_pct_t female_housework_pct_avg birth_after_trans any_births kids_in_hh

// create variables that I'll add to the control ones
gen control_n=.
gen treated_pid=.
gen treated_partner=.

save "$temp/GSOEP_psm_base.dta", replace

restore

// get control

forvalues n=1/5{
	
	preserve

	keep pid partner_id_pl matched_pid`n' matched_partner_id_pl`n' syear matched_rel_start`n' relationship_counter transition_counter duration_centered matched_earn_pct`n' matched_hours_pct`n' matched_hw_pct`n' birth_after_trans any_births kids_in_hh

	gen treated = 0
	gen control_n = `n'
	rename (pid partner_id_pl) (treated_pid treated_partner)
	rename (matched_pid`n' matched_partner_id_pl`n' matched_rel_start`n' matched_earn_pct`n' matched_hours_pct`n' matched_hw_pct`n') ///
	(pid partner_id_pl rel_start_all female_earn_pct_net female_hours_pct_t female_housework_pct_avg)

	save "$temp/GSOEP_psm_control_nn`n'.dta", replace

	restore

}

// now append
use "$temp/GSOEP_psm_base.dta", clear
forvalues n=1/5{
	append using "$temp/GSOEP_psm_control_nn`n'.dta"
}

save "$created_data/GSOEP_psm_matched_long.dta", replace

// 
**# START HERE FOR ANALYSIS

use "$created_data/GSOEP_psm_matched_long.dta", clear

keep if duration_centered>=-3 & duration_centered <=5
gen duration_pos = duration_centered + 4 // can't be negative

******************************
* OLS
******************************

// unadjusted regression
regress female_earn_pct_net treated##i.duration_pos
margins duration_pos#treated
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5")xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

regress female_hours_pct_t treated##i.duration_pos
margins duration_pos#treated
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Paid Work Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

regress female_housework_pct_avg treated##i.duration_pos
margins duration_pos#treated
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Housework Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

******************************
* Growth Curves
******************************

// is this the same or different as growth curve models? This is following Kapelle 2022. I think this is better. They are directionally similar; this better controls for time dependence within individuals.
egen couple_id = group(pid partner_id_pl)

mixed female_earn_pct_net treated##i.duration_pos || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1)) // so is this how I graph the curve
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink)) xline(4, lpattern(dash) lwidth(thin) lcolor(pink))  


mixed female_hours_pct_t treated##i.duration_pos || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1)) // so is this how I graph the curve
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Paid Work Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink)) xline(4, lpattern(dash) lwidth(thin) lcolor(pink))  


mixed female_housework_pct_avg treated##i.duration_pos || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1)) // so is this how I graph the curve
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Housework Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink)) xline(4, lpattern(dash) lwidth(thin) lcolor(pink))  

// Attempting to add parenthood

mixed female_earn_pct_net treated##i.duration_pos##i.birth_after_trans || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1) birth_after_trans=(0 1)) // so is this how I graph the curve
marginsplot

mixed female_earn_pct_net treated##i.duration_pos##i.kids_in_hh || couple_id: duration_pos // alt measurement
margins, at(duration_pos=(1(1)9) treated=(0 1) kids_in_hh=(0 1))
marginsplot


mixed female_earn_pct_net treated##i.duration_pos if birth_after_trans==0 || couple_id: duration_pos 
margins, at(duration_pos=(1(1)9) treated=(0 1))
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

mixed female_earn_pct_net treated##i.duration_pos if birth_after_trans==1 || couple_id: duration_pos 
margins, at(duration_pos=(1(1)9) treated=(0 1))
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

// do I actually want same treated group for all?

gen birth_adj = birth_after_trans
replace birth_adj = 0 if treated==0 

mixed female_earn_pct_net treated##i.duration_pos##i.birth_adj || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1) birth_adj=(0 1))
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 3 "CF Married" 4 "Child Married")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot3opts(lcolor(pink) mcolor(pink)) ci3opts(lcolor(pink)) plot4opts(lcolor(pink%30) mcolor(pink%30)) ci4opts(lcolor(pink%30))

mixed female_hours_pct_t treated##i.duration_pos##i.birth_adj || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1) birth_adj=(0 1))
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Paid Work Hrs") legend(rows(1) position(bottom) order(1 "Cohab" 3 "CF Married" 4 "Child Married")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot3opts(lcolor(pink) mcolor(pink)) ci3opts(lcolor(pink)) plot4opts(lcolor(pink%30) mcolor(pink%30)) ci4opts(lcolor(pink%30)) xline(4, lpattern(dash) lwidth(thin) lcolor(pink))  

mixed female_housework_pct_avg treated##i.duration_pos##i.birth_adj || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1) birth_adj=(0 1))
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Housework Hrs") legend(rows(1) position(bottom) order(1 "Cohab" 3 "CF Married" 4 "Child Married")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot3opts(lcolor(pink) mcolor(pink)) ci3opts(lcolor(pink)) plot4opts(lcolor(pink%30) mcolor(pink%30)) ci4opts(lcolor(pink%30)) xline(4, lpattern(dash) lwidth(thin) lcolor(pink))  


/* This was initial really crude attempt
// used in gender workshop
********************************************************************************
**# Option 2: keep groups of same length and recenter control, then weight?!
* Using IPW based on PSM above?
********************************************************************************
gen keep_flag=0
replace keep_flag = 1 if treated==1 & dur >=-4 & dur<=5 & years_observed>=5
replace keep_flag = 1 if treated==0 & dur>=0 & dur<=9 & years_observed>=5

unique pid partner_id_pl if keep_flag==1, by(treated)

tab duration_cohab treated if keep_flag==1

gen recenter_dur = duration_cohab if treated==1
replace recenter_dur = duration_cohab - 4 if treated==0

tab recenter_dur treated if keep_flag==1, m

keep if keep_flag==1
gen recenter_dur_pos = recenter_dur+5

gen ipw=.
replace ipw=1/pscore if treated==1
replace ipw=1/(1-pscore) if treated==0

browse pid partner_id_pl treated recenter_dur pscore ipw psm female_earn_pct_net female_hours_pct_t female_housework_pct_avg female_childcare_pct_avg if keep_flag==1

tabstat pscore ipw, by(treated)

set scheme cleanplots

// unadjusted
regress female_earn_pct_net treated##i.recenter_dur_pos 
margins recenter_dur_pos#treated
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

regress female_hours_pct_t treated##i.recenter_dur_pos 
margins recenter_dur_pos#treated
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Paid Work Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

regress female_housework_pct_avg treated##i.recenter_dur_pos 
margins recenter_dur_pos#treated
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Housework Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

regress female_childcare_pct_avg treated##i.recenter_dur_pos 
margins recenter_dur_pos#treated
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Childcare Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))


// adjusted
// local controls "i.isced97_pg i.isced97_pg_sp i.where_germany_pl age age_sp couple_earnings_net i.housing_status_hl num_children_hl rel_start_all"
local control "i.isced97_pg i.isced97_pg_sp i.psample_pl i.psample_pl_sp i.where_germany_pl age age_sp couple_earnings_net i.housing_status_hl num_children_hl rel_start_all"

regress female_earn_pct_net treated##i.recenter_dur_pos `controls' [pweight=ipw]
margins recenter_dur_pos#treated // does this work?? like is it this simple?
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

regress female_hours_pct_t treated##i.recenter_dur_pos  `controls' [pweight=ipw]
margins recenter_dur_pos#treated // does this work??
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Paid Work Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

regress female_housework_pct_avg treated##i.recenter_dur_pos `controls'  [pweight=ipw]
margins recenter_dur_pos#treated // does this work??
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Housework Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")  plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

regress female_childcare_pct_avg treated##i.recenter_dur_pos `controls'  [pweight=ipw]
margins recenter_dur_pos#treated // does this work??
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Childcare Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")  plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))
*/