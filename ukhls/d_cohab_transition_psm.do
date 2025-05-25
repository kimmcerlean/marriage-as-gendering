********************************************************************************
********************************************************************************
* Project: Marriage as Gendering
* Owner: Kimberly McErlean
* Started: September 2024
* File: cohab_transition_analysis
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file conducts descriptive and statistical analysis

********************************************************************************
* Final sample restrictions and things
********************************************************************************
use "$created_data/ukhls_matched_couples.dta", clear

// deduplicate
tab marital_status_defacto sex, m

/* want to look like this-ish distribution after
marital_statu |          Sex
    s_defacto |      male     female |     Total
--------------+----------------------+----------
      Married |   186,034    189,169 |   375,203 
   Cohabiting |    32,268     34,352 |    66,620 
--------------+----------------------+----------
        Total |   218,302    223,521 |   441,823 
*/

sort hidp year pidp
browse hidp pidp partner_id year sex sex_sp

bysort year hidp : egen per_id = rank(pidp)
sort hidp year pidp
browse hidp pidp partner_id year per_id sex sex_sp

tab per_id, m // 1s should approximately total above
keep if inlist(per_id,1,3,5,7)

tab marital_status_defacto, m
/*
marital_statu |
    s_defacto |      Freq.     Percent        Cum.
--------------+-----------------------------------
      Married |    190,886       84.18       84.18
   Cohabiting |     35,870       15.82      100.00
--------------+-----------------------------------
*/

// now only keep relationships that started after a certain date
tab year marital_status_defacto, m row
tab rel_start_all marital_status_defacto, m row

unique pidp partner_id, by(marital_status_defacto) 
unique pidp partner_id if rel_start_all >=1991, by(marital_status_defacto) 
unique pidp partner_id if rel_start_all >=1992, by(marital_status_defacto) //want to make sure not a lot of relationships arbitrarily recorded against 1991. this feels reasonable

keep if rel_start_all >= 1991 // if I want to observe first year, this is the earliest I can start here. 
// other surveys (psid, gsoep), I did 1985 so might need to revisit 

// variables to get treated and control
tab year_transitioned ever_transition, m
sort pidp partner_id year
browse pidp partner_id year int_year marital_status_defacto marr_trans ever_transition year_transitioned

bysort pidp partner_id: egen years_observed=count(year)

bysort pidp partner_id: egen years_married=count(year) if marital_status_defacto==1
bysort pidp partner_id (years_married): replace years_married = years_married[1]

bysort pidp partner_id: egen years_cohab=count(year) if marital_status_defacto==2
bysort pidp partner_id (years_cohab): replace years_cohab = years_cohab[1]

sort pidp partner_id year
browse pidp partner_id year marital_status_defacto years_observed years_married years_cohab ever_transition marr_trans rel_start_all year_transitioned

gen always_married=0
replace always_married=1 if years_observed==years_married & years_married!=.

gen always_cohab=0
replace always_cohab=1 if years_observed==years_cohab & years_cohab!=.

	// none of these should overlap
	tab always_cohab always_married, m
	tab always_cohab ever_transition, m
	tab always_married ever_transition, m

// only keep certain ages
tab age_all employed, row
keep if (age_all>=18 & age_all<60) &  (age_all_sp>=18 & age_all_sp<60) // sort of drops off a cliff after 60?

// create couple-level education
tab college_degree college_degree_sp

gen couple_educ_gp=0
replace couple_educ_gp = 1 if college_degree==1 | college_degree_sp==1
replace couple_educ_gp=. if college_degree==. & college_degree_sp==.

// home_owners
recode tenure_dv (-9/-7=.)(1/2=1)(3/8=0), gen(home_owner)
tab home_owner, m
tab tenure_dv home_owner, m

// age
gen age_woman = age_all if sex==2
replace age_woman = age_all_sp if sex==1

gen age_man = age_all if sex==1
replace age_man = age_all_sp if sex==2

********************************************************************************
* before dropping, get descriptive comparison of cohabitors to married couples
********************************************************************************
tabstat female_earn_pct_t female_hours_pct_t female_housework_pct_t, by(marital_status_defacto) 
ttest female_earn_pct_t, by(marital_status_defacto) 
ttest female_hours_pct_t, by(marital_status_defacto) 
ttest female_housework_pct_t, by(marital_status_defacto) 

unique pidp partner_id, by(marital_status_defacto) 
unique pidp partner_id if marital_status_defacto==1
unique pidp partner_id if marital_status_defacto==2

// % ever transition
tab marital_status_defacto ever_transition, row

// some small descriptives
tabstat age_woman age_man couple_earnings_t couple_educ_gp home_owner kids_in_hh, by(marital_status_defacto)

********************************************************************************
**# okay make analytical sample and recode duration relative to marital transition
********************************************************************************
keep if ever_transition==1 | always_cohab==1 // so keeping a "control" group - basically drops those always married

gen treated=.
replace treated=1 if ever_transition==1 & always_cohab!=1
replace treated=0 if always_cohab==1

tab year_transitioned ever_transition, m

gen relationship_duration = year - rel_start_all
drop if relationship_duration < 0

gen duration_cohab=.
replace duration_cohab = year - year_transitioned if treated==1
replace duration_cohab = relationship_duration if treated==0

tab duration_cohab, m

browse pidp partner_id int_year year treated marital_status_defacto marr_trans rel_start_all year_transitioned relationship_duration duration_cohab

// duration transitioned
browse pidp partner_id int_year  year marr_trans relationship_duration duration_cohab year_transitioned

gen dur_transitioned=relationship_duration if marr_trans==1
bysort pidp partner_id (year_transitioned): replace year_transitioned = year_transitioned[1]
bysort pidp partner_id (dur_transitioned): replace dur_transitioned = dur_transitioned[1]

sort pidp partner_id year
browse pidp partner_id int_year year marr_trans dur_transitioned year_transitioned relationship_duration duration_cohab 

//  combined indicator of paid and unpaid, using HOURS - okay currently missing for all years that housework hours are
/*
gen hours_housework=.
replace hours_housework=1 if paid_dol_ot==1 & unpaid_dol==1 // dual both (egal)
replace hours_housework=2 if paid_dol_ot==1 & unpaid_dol==2 // dual earner, female HM (second shift)
replace hours_housework=3 if paid_dol_ot==2 & unpaid_dol==1 // male BW, dual HW (mm not sure)
replace hours_housework=4 if paid_dol_ot==2 & unpaid_dol==2 // male BW, female HM (conventional)
replace hours_housework=5 if paid_dol_ot==3 & unpaid_dol==1 // female BW, dual HW (gender-atypical)
replace hours_housework=6 if paid_dol_ot==3 & unpaid_dol==2 // female BW, female HM (undoing gender)
replace hours_housework=7 if unpaid_dol==3  // all where male does more housework (gender-atypical)
replace hours_housework=8 if paid_dol_ot==4  // no earners

label define hours_housework 1 "Egal" 2 "Second Shift" 3 "Male BW, dual HW" 4 "Conventional" 5 "Gender-atypical" 6 "Undoing gender" 7 "Male HW dominant" 8 "No Earners"
label values hours_housework hours_housework 
*/

gen earn_housework=.
replace earn_housework=1 if hh_earn_type_t==1 & housework_bkt_t==1 // dual both (egal)
replace earn_housework=2 if hh_earn_type_t==1 & housework_bkt_t==2 // dual earner, female HM (second shift)
replace earn_housework=3 if hh_earn_type_t==2 & housework_bkt_t==2 // male BW, female HM (traditional)
replace earn_housework=4 if hh_earn_type_t==3 & housework_bkt_t==3 // female BW, male HM (counter-traditional)
replace earn_housework=5 if earn_housework==. & hh_earn_type_t!=. & housework_bkt_t!=. // all others

label define earn_housework 1 "Egal" 2 "Second Shift" 3 "Traditional" 4 "Counter Traditional" 5 "All others"
label values earn_housework earn_housework 

// also add indicator of parenthood status to have / pull through in later steps (this created one and the children ones)
browse pidp partner_id int_year rel_start_all rel_end_all year_transitioned year_first_birth age_youngest_child xw_anychild_dv nkids_dv nchild_dv

gen birth_after_trans=0
replace birth_after_trans = 1 if year_first_birth >= year_transitioned & year_first_birth!=9999 & xw_anychild_dv!=2 & xw_anychild_dv!=.

browse pidp partner_id int_year rel_start_all rel_end_all birth_after_trans year_transitioned year_first_birth age_youngest_child xw_anychild_dv nkids_dv nchild_dv

gen own_child = 0
replace own_child = 1 if nchild_dv > 0 & nchild_dv!=.

********************************************************************************
**# Some descriptive statistics
********************************************************************************

unique pidp partner_id 
unique pidp partner_id year_transitioned
unique pidp partner_id  if relationship_duration==0

sum duration_cohab if duration_cohab < 0 // average cohab duration
sum duration_cohab if duration_cohab > 0 & duration_cohab !=. // average marital duration

tab couple_educ_gp treated, m row
tab couple_educ_gp if relationship_duration==0

tab xw_memorig treated, m row
tab xw_ethn_dv treated, m row

tab kids_in_hh
tab kids_in_hh if relationship_duration==0

unique pidp partner_id, by(treated) 
unique pidp partner_id if treated==0
unique pidp partner_id if treated==1

// some small descriptives
tabstat age_woman age_man couple_earnings_t couple_educ_gp home_owner kids_in_hh, by(treated)

// tab had_birth
// unique pidp partner_id  if had_birth==1 // use this for % experiencing a birth
// unique pidp partner_id  if had_birth==1 & duration_cohab==0
// tab had_birth if duration_cohab==0

sum female_earn_pct_t
tab hh_earn_type_t
sum female_hours_pct_t
tab hh_hours_type_t
sum female_housework_pct_t
tab housework_bkt_t
tab earn_housework

sum female_earn_pct_t if relationship_duration==0
tab hh_earn_type_t if relationship_duration==0
sum female_hours_pct_t if relationship_duration==0
tab hh_hours_type_t if relationship_duration==0
sum female_housework_pct_t if relationship_duration==0
tab housework_bkt_t if relationship_duration==0
tab earn_housework if relationship_duration==0

********************************************************************************
********************************************************************************
********************************************************************************
** PROPENSITY SCORE MATCHING EXPLORATION
********************************************************************************
********************************************************************************
********************************************************************************
* First, do some sample things
// want to observe in relatively early year of relationship
bysort pidp partner_id: egen min_dur = min(relationship_duration)
tab min_dur, m // 70% observed year 1 or 2

keep if min_dur <=5

// for treated, need to observe at least one year in each state
tab years_observed treated, m
tab years_married treated, m
tab years_cohab treated, m

browse  pidp partner_id year marital_status_defacto treated relationship_duration year_transitioned years_observed years_married years_cohab
keep if treated==0 | (treated==1 & years_married>=1 & years_married!=. & years_cohab>=1 & years_cohab!=.)

// for control, do we want to restrict to min of 2 years? or is one sufficient? 
// (I am loosely following Kapelle and she says just one valid wealth wave is needed)

********************************************************************************
**# Preliminary PSM (this is very crude)
********************************************************************************
// okay, might need to think about this for UK because housework is only every other year, but survey is annual, so housework has A LOT of missing.  PSID is different because nothing is asked annually
// might need to impute for UKHLS? but for now, not including housework in PSM bc will severly restrict sample
inspect hiqual_dv hiqual_dv_sp xw_memorig xw_memorig_sp xw_ethn_dv xw_ethn_dv_sp age_all age_all_sp jbstat jbstat_sp couple_earnings_t housework_man housework_woman home_owner nchild_dv rel_start_all if relationship_duration==min_dur

logit treated i.hiqual_dv i.hiqual_dv_sp i.xw_memorig i.xw_memorig_sp i.xw_ethn_dv i.xw_ethn_dv_sp age_all age_all_sp i.jbstat i.jbstat_sp couple_earnings_t i.home_owner nchild_dv rel_start_all if relationship_duration==min_dur // PSM based on characteristics at start of cohab OR first observed // housework_man housework_woman
predict psm if relationship_duration==min_dur

bysort pidp partner_id: egen pscore = max(psm)
sort pidp partner_id int_year
inspect pscore // about 7% missing

// check for overlap
tabstat psm, by(treated)
sum psm if treated==0, det
sum psm if treated==1, det
tabstat pscore, by(treated)

set scheme cleanplots
twoway (histogram psm if treated==1, width(.02) color(pink%30)) (histogram psm if treated==0, width(.02) color(gs8%30)),  legend(order(1 "Treated" 2 "Control") rows(1) position(6)) xtitle("Propensity Score")

********************************************************************************
**# Option 1: Use PSMATCH2 to match AT BASELINE
********************************************************************************
// nearest neighbor matching
psmatch2 treated, pscore(psm) neighbor(5) 
sort _id
browse pidp treated psm pscore rel_start_all min_dur marr_trans ever_transition year_transitioned relationship_duration _pscore _treated _support _weight _id _n1 _n2 _n3 _n4 _n5 _nn _pdif _Unique
browse pidp partner_id treated psm pscore rel_start_all min_dur marr_trans ever_transition year_transitioned relationship_duration _pscore _treated _support _weight _id _n1 _n2 _n3 _n4 _n5 _nn _pdif _Unique if _treated!=.

tab _weight treated
tab treated _nn if relationship_duration == min_dur, m

sort pidp partner_id relationship_duration
browse pidp partner_id year marital_status_defacto treated psm pscore rel_start_all relationship_duration min_dur year_transitioned duration_cohab _pscore _treated _support _weight _id _n1 _n2 _n3 _n4 _n5 _nn _pdif _Unique ///
if inlist(pidp, 95282, 142882, 411097405, 6173051, 24038082, 275073812)

// let's create a lookup file of ids for the controls
preserve

keep _id pidp partner_id rel_start_all min_dur years_observed
rename _id _n1
gen _n2 = _n1
gen _n3 = _n1
gen _n4 = _n1
gen _n5 = _n1
rename pidp matched_pidp
rename partner_id matched_partner_id
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
	
	rename (matched_pidp matched_partner_id matched_rel_start matched_min_dur matched_years_obs) ///
	(matched_pidp`n' matched_partner_id`n' matched_rel_start`n' matched_min_dur`n' matched_years_obs`n')

	bysort pidp partner_id (matched_pidp`n'): 			replace matched_pidp`n' = matched_pidp`n'[1]
	bysort pidp partner_id (matched_partner_id`n'): 	replace matched_partner_id`n' = matched_partner_id`n'[1]
	bysort pidp partner_id (matched_rel_start`n'): 	replace matched_rel_start`n' = matched_rel_start`n'[1]
	bysort pidp partner_id (matched_min_dur`n'): 		replace matched_min_dur`n' = matched_min_dur`n'[1]
	bysort pidp partner_id (matched_years_obs`n'): 	replace matched_years_obs`n' = matched_years_obs`n'[1]
}

sort pidp partner_id year
browse pidp partner_id year treated rel_start_all relationship_duration  min_dur years_observed _id _n* matched_pidp* matched_partner_id* matched_rel_start* matched_min_dur* matched_years_obs*

// going to attempt to rescale all durations so start at min dur and just increment 1 - to make control and treated match
sort pidp partner_id relationship_duration

browse pidp partner_id relationship_duration treated

gen relationship_counter=.
replace relationship_counter = 0 if relationship_duration == min_dur
replace relationship_counter = relationship_counter[_n-1] + 1 if relationship_counter==. & pidp == pidp[_n-1] & partner_id == partner_id[_n-1]
tab relationship_counter, m

gen transition_counter=.
replace transition_counter = relationship_counter if relationship_duration == dur_transitioned
bysort pidp partner_id (transition_counter): replace transition_counter = transition_counter[1]

sort pidp partner_id relationship_duration
browse pidp partner_id year rel_start_all relationship_duration relationship_counter transition_counter year_transitioned dur_transitioned

save "$temp/UKHLS_psm_dataset_nn5.dta", replace // save here so I can start to create a lookup file

**************************************
* Now create treated only version
**************************************
keep if treated==1

//do they all have matches?
inspect matched_pidp* // okay no lolz
inspect pscore // okay, it's people that don't have propensity score, probably bc of missing data

drop if matched_pidp1==.

browse  pidp partner_id year rel_start_all relationship_duration  min_dur years_observed _id _n* _nn matched_pidp* matched_partner_id* matched_rel_start* matched_min_dur* matched_years_obs*

save "$temp/UKHLS_psm_treated.dta", replace  // will use this as base, then add control info, then resave

**************************************
* Now clean up full file for matching
**************************************
use "$temp/UKHLS_psm_dataset_nn5.dta", clear

keep pidp partner_id relationship_counter relationship_duration female_earn_pct_t female_hours_pct_t female_housework_pct_t

forvalues n=1/5{
	gen long matched_pidp`n' = pidp
	gen long matched_partner_id`n' = partner_id
	gen matched_true_dur`n' = relationship_duration
	gen matched_earn_pct`n' = female_earn_pct_t
	gen matched_hours_pct`n' = female_hours_pct_t
	gen matched_hw_pct`n' = female_housework_pct_t
}

// browse pidp partner_id relationship_counter matched_pidp1 matched_partner_id1

drop pidp partner_id relationship_duration female_earn_pct_t female_hours_pct_t female_housework_pct_t

save "$temp/UKHLS_psm_tomatch.dta", replace

**************************************
* Merge partner info
**************************************
use "$temp/UKHLS_psm_treated.dta", clear

forvalues n=1/5{
	merge m:1 matched_pidp`n' matched_partner_id`n' relationship_counter using "$temp/UKHLS_psm_tomatch.dta", ///
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


sort pidp partner_id relationship_counter

browse pidp partner_id year rel_start_all relationship_counter relationship_duration has_control* matched_pidp* matched_partner_id* matched_rel_start* matched_years_obs* matched_true_dur* female_earn_pct_t female_hours_pct_t female_housework_pct_t matched_earn_pct* matched_hours_pct* matched_hw_pct*

save "$created_data/UKHLS_psm_matched.dta", replace

**************************************
* Preliminary analysis
**************************************
tab relationship_counter has_control1, m row
drop if relationship_counter >=10 // very few records and matches. might even need to remove more than that

browse pidp partner_id year rel_start_all relationship_counter relationship_duration has_control* matched_pidp* matched_partner_id* matched_rel_start* matched_years_obs* matched_true_dur* female_earn_pct_t female_hours_pct_t female_housework_pct_t matched_earn_pct* matched_hours_pct* matched_hw_pct*

gen duration_centered = relationship_counter - transition_counter

preserve

collapse (median) female_earn_pct_t female_hours_pct_t female_housework_pct_t matched_earn_pct* matched_hours_pct* matched_hw_pct*, by(duration_centered)

twoway (line female_earn_pct_t duration_centered if duration_centered>=-5) (line matched_earn_pct* duration_centered if duration_centered>=-5), legend(order(1 "Treated" 2 "Control1" 3 "Control2" 4 "Control3" 5 "Control4" 6 "Control5")) xtitle(`"Duration from Marital Transition"') // rows(1) position(6) - old legend characteristics

twoway (line female_hours_pct_t duration_centered if duration_centered>=-5) (line matched_hours_pct* duration_centered if duration_centered>=-5), legend(order(1 "Treated" 2 "Control1" 3 "Control2" 4 "Control3" 5 "Control4" 6 "Control5")) xtitle(`"Duration from Marital Transition"')

twoway (line female_housework_pct_t duration_centered if duration_centered>=-5) (line matched_hw_pct* duration_centered if duration_centered>=-5), legend(order(1 "Treated" 2 "Control1" 3 "Control2" 4 "Control3" 5 "Control4" 6 "Control5")) xtitle(`"Duration from Marital Transition"')

restore

**************************************
**# * Actual analysis with matched data
**************************************
* Need to get these stacked instead of next to each other, so can create a column for treatment and interact, like I do below...do I have to cut, rename, and append?
* Yeah, definitely will need to add other characteristics so can control? Or is that all taken care of in propensity score anyway?

browse pidp partner_id year rel_start_all relationship_counter transition_counter duration_centered matched_pidp* matched_partner_id* matched_rel_start* matched_years_obs* matched_true_dur* female_earn_pct_t female_hours_pct_t female_housework_pct_t matched_earn_pct* matched_hours_pct* matched_hw_pct*

// get treated that I will then append control on to

preserve

keep pidp partner_id year rel_start_all treated relationship_counter transition_counter duration_centered female_earn_pct_t female_hours_pct_t female_housework_pct_t birth_after_trans own_child nchild_dv xw_anychild_dv

// create variables that I'll add to the control ones
gen control_n=.
gen treated_pidp=.
gen treated_partner=.

save "$temp/UKHLS_psm_base.dta", replace

restore

// get control

forvalues n=1/5{
	
	preserve

	keep pidp partner_id matched_pidp`n' matched_partner_id`n' year matched_rel_start`n' relationship_counter transition_counter duration_centered matched_earn_pct`n' matched_hours_pct`n' matched_hw_pct`n' birth_after_trans own_child nchild_dv xw_anychild_dv

	gen treated = 0
	gen control_n = `n'
	rename (pidp partner_id) (treated_pidp treated_partner)
	rename (matched_pidp`n' matched_partner_id`n' matched_rel_start`n' matched_earn_pct`n' matched_hours_pct`n' matched_hw_pct`n') ///
	(pidp partner_id rel_start_all female_earn_pct_t female_hours_pct_t female_housework_pct_t)

	save "$temp/UKHLS_psm_control_nn`n'.dta", replace

	restore

}

// now append
use "$temp/UKHLS_psm_base.dta", clear
forvalues n=1/5{
	append using "$temp/UKHLS_psm_control_nn`n'.dta"
}

save "$created_data/UKHLS_psm_matched_long.dta", replace

// 
**# START HERE FOR ANALYSIS

use "$created_data/UKHLS_psm_matched_long.dta", clear

keep if duration_centered>=-3 & duration_centered <=5
gen duration_pos = duration_centered + 4 // can't be negative

******************************
* OLS
******************************

// unadjusted regression
regress female_earn_pct_t treated##i.duration_pos
margins duration_pos#treated
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5")xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

regress female_hours_pct_t treated##i.duration_pos
margins duration_pos#treated
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Paid Work Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

regress female_housework_pct_t treated##i.duration_pos
margins duration_pos#treated
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Housework Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

******************************
* Growth Curves
******************************

// is this the same or different as growth curve models? This is following Kapelle 2022. I think this is better. They are directionally similar; this better controls for time dependence within individuals.
egen couple_id = group(pidp partner_id)

mixed female_earn_pct_t treated##i.duration_pos || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1)) // so is this how I graph the curve
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))


mixed female_hours_pct_t treated##i.duration_pos || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1)) // so is this how I graph the curve
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5", labsize(vsmall)) xtitle(`"Duration from Marital Transition"', size(small)) ytitle("Women's % of Total Paid Work Hours", size(small)) legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned") size(vsmall)) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink)) xline(4, lpattern(dash) lwidth(thin) lcolor(pink)) ylabel(0.3(0.05)0.55, labsize(vsmall)) xsize(4)  


mixed female_housework_pct_t treated##i.duration_pos || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1)) // so is this how I graph the curve
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5", labsize(vsmall)) xtitle(`"Duration from Marital Transition"', size(small)) ytitle("Women's % of Total Housework Hours", size(small)) legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")  size(vsmall)) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink)) xline(4, lpattern(dash) lwidth(thin) lcolor(pink)) ylabel(0.6(0.05)0.75, labsize(vsmall)) xsize(4)  

// Attempting to add parenthood

mixed female_earn_pct_t treated##i.duration_pos##i.birth_after_trans || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1) birth_after_trans=(0 1)) // so is this how I graph the curve
marginsplot

mixed female_earn_pct_t treated##i.duration_pos##i.own_child || couple_id: duration_pos // alt measurement
margins, at(duration_pos=(1(1)9) treated=(0 1) own_child=(0 1))
marginsplot


mixed female_earn_pct_t treated##i.duration_pos if birth_after_trans==0 || couple_id: duration_pos 
margins, at(duration_pos=(1(1)9) treated=(0 1))
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

mixed female_earn_pct_t treated##i.duration_pos if birth_after_trans==1 || couple_id: duration_pos 
margins, at(duration_pos=(1(1)9) treated=(0 1))
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

// do I actually want same treated group for all?

gen birth_adj = birth_after_trans
replace birth_adj = 0 if treated==0 

mixed female_earn_pct_t treated##i.duration_pos##i.birth_adj || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1) birth_adj=(0 1))
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 3 "CF Married" 4 "Child Married")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot3opts(lcolor(pink) mcolor(pink)) ci3opts(lcolor(pink)) plot4opts(lcolor(pink%30) mcolor(pink%30)) ci4opts(lcolor(pink%30))

mixed female_hours_pct_t treated##i.duration_pos##i.birth_adj || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1) birth_adj=(0 1))
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5", labsize(vsmall)) xtitle(`"Duration from Marital Transition"', size(small)) ytitle("Women's % of Total Paid Work Hrs", size(small)) legend(rows(1) position(bottom) order(1 "Cohab" 3 "CF Married" 4 "Child Married") size(vsmall)) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot3opts(lcolor(pink) mcolor(pink)) ci3opts(lcolor(pink)) plot4opts(lcolor(pink%30) mcolor(pink%30)) ci4opts(lcolor(pink%30)) xline(4, lpattern(dash) lwidth(thin) lcolor(pink)) ylabel(0.2(0.05)0.55, labsize(vsmall)) xsize(4) 

mixed female_housework_pct_t treated##i.duration_pos##i.birth_adj || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)9) treated=(0 1) birth_adj=(0 1))
marginsplot, xlabel( 1 "-3" 2 "-2" 3 "-1" 4 "Transition" 5 "1" 6 "2" 7 "3" 8 "4" 9 "5", labsize(vsmall)) xtitle(`"Duration from Marital Transition"', size(small)) ytitle("Women's % of Total Housework Hrs", size(small)) legend(rows(1) position(bottom) order(1 "Cohab" 3 "CF Married" 4 "Child Married") size(vsmall)) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot3opts(lcolor(pink) mcolor(pink)) ci3opts(lcolor(pink)) plot4opts(lcolor(pink%30) mcolor(pink%30)) ci4opts(lcolor(pink%30)) xline(4, lpattern(dash) lwidth(thin) lcolor(pink))  ylabel(0.5(0.05)0.80, labsize(vsmall)) xsize(4)  
 


/* This was initial really crude attempt
********************************************************************************
**# Option 2: keep groups of same length and recenter control, then weight?!
* Using IPW based on PSM above?
********************************************************************************
gen keep_flag=0
replace keep_flag = 1 if treated==1 & duration_cohab >=-4 & duration_cohab<=5 & years_observed>=5
replace keep_flag = 1 if treated==0 & duration_cohab>=0 & duration_cohab<=9 & years_observed>=5

unique pidp partner_id if keep_flag==1, by(treated)

tab duration_cohab treated if keep_flag==1

gen recenter_dur = duration_cohab if treated==1
replace recenter_dur = duration_cohab - 4 if treated==0

tab recenter_dur treated if keep_flag==1, m

keep if keep_flag==1
gen recenter_dur_pos = recenter_dur+5

gen ipw=.
replace ipw=1/pscore if treated==1
replace ipw=1/(1-pscore) if treated==0

browse pidp partner_id treated recenter_dur pscore ipw psm female_earn_pct_t female_hours_pct_t female_housework_pct_t if keep_flag==1

tabstat pscore ipw, by(treated)

set scheme cleanplots

// unadjusted
regress female_earn_pct_t treated##i.recenter_dur_pos 
margins recenter_dur_pos#treated
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

regress female_hours_pct_t treated##i.recenter_dur_pos 
margins recenter_dur_pos#treated
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Paid Work Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

regress female_housework_pct_t treated##i.recenter_dur_pos 
margins recenter_dur_pos#treated
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Housework Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

// adjusted
local controls "i.xw_ethn_dv age_all age_all_sp i.hiqual_dv i.hiqual_dv_sp couple_earnings_t rel_start_all nchild_dv i.home_owner"

regress female_earn_pct_t treated##i.recenter_dur_pos `controls' [pweight=ipw]
margins recenter_dur_pos#treated // does this work?? like is it this simple?
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

regress female_hours_pct_t treated##i.recenter_dur_pos  `controls' [pweight=ipw]
margins recenter_dur_pos#treated // does this work??
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Paid Work Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

regress female_housework_pct_t treated##i.recenter_dur_pos `controls'  [pweight=ipw]
margins recenter_dur_pos#treated // does this work??
marginsplot, xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "Transition" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Housework Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")  plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))
*/