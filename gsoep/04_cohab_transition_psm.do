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

********************************************************************************
* Add descriptive statistics and descriptive exploration
********************************************************************************
unique pid partner_id_pl, by(treated) 
unique pid partner_id_pl if treated==0
unique pid partner_id_pl if treated==1

// some small descriptives
tabstat age_woman age_man couple_earnings_net couple_educ_gp home_owner kids_in_hh, by(treated)

********************************************************************************
**# Option 1: fixed effects just on treated?
********************************************************************************
// so first need a var that is pre v. post treated. can I just use marital status?
egen couple_id = group(pid partner_id_pl)
 
tab marst_defacto treated, m
tab year_transitioned treated, m
tab marst_defacto if treated==1 & syear >= year_transitioned

gen married = 0
replace married = 1 if marst_defacto==1

tabstat female_earn_pct_gr female_earn_pct_net female_hours_pct_t female_housework_pct_wk female_housework_pct_avg female_childcare_pct_wk female_childcare_pct_avg if treated==1, by(married) // without controls, this is what I'd be estimating
tabstat female_earn_pct_net female_hours_pct_t female_housework_pct_avg female_childcare_pct_avg if treated==1, by(married) 

regress female_earn_pct_net i.married if treated==1
regress female_hours_pct_t i.married if treated==1
regress female_housework_pct_avg i.married if treated==1
regress female_childcare_pct_avg i.married if treated==1

ttest female_earn_pct_net if treated==1, by(married) 

local controls "i.isced97_pg i.isced97_pg_sp i.where_germany_pl age age_sp couple_earnings_net i.housing_status_hl num_children_hl rel_start_all"
regress female_earn_pct_net i.married `controls' if treated==1
margins married
// regress female_earn_pct_net i.married `controls' i.couple_id if treated==1
// margins married

xtset couple_id

xtreg female_earn_pct_net i.married `controls' if treated==1, fe // this exactly matches above
margins married

// regress female_earn_pct_net i.married i.relationship_duration `controls' i.couple_id if treated==1
// margins married

local controls "i.isced97_pg i.isced97_pg_sp i.where_germany_pl age age_sp couple_earnings_net i.housing_status_hl num_children_hl rel_start_all"

xtreg female_earn_pct_net i.married i.relationship_duration `controls' if treated==1, fe
margins married
margins relationship_duration
marginsplot

xtreg female_earn_pct_net i.married dur `controls' if treated==1, fe
margins married
margins, at(dur=(-5(1)10))
marginsplot

xtreg female_earn_pct_net i.married##i.relationship_duration `controls' if treated==1, fe
margins married
margins married, at(relationship_duration=(0(1)10))
marginsplot

local controls "i.isced97_pg i.isced97_pg_sp i.where_germany_pl age age_sp couple_earnings_net i.housing_status_hl num_children_hl rel_start_all"

// do I use controls with fixed effects? I think because that controls for observed and the remained is unobserved? I think controls let hyou control for time-varying things whereas fixed effects net out time invariant? (hence why race is collinear?)
xtreg female_earn_pct_net i.married i.relationship_duration `controls' if treated==1 & relationship_duration<=15, fe
margins married

xtreg female_hours_pct_t i.married i.relationship_duration `controls' if treated==1 & relationship_duration<=15, fe
margins married

xtreg female_housework_pct_avg i.married i.relationship_duration `controls' if treated==1 & relationship_duration<=15, fe
margins married

xtreg female_childcare_pct_avg i.married i.relationship_duration `controls' if treated==1 & relationship_duration<=15, fe
margins married

********************************************************************************
**# Preliminary PSM (this is very crude)
********************************************************************************
// prob want to use characteristics at relationship start, which is relationship_duration==0 for both
bysort pid partner_id_pl: egen min_dur = min(relationship_duration)
tab min_dur, m // 98.6% observer year 1 or 2

logit treated i.isced97_pg i.isced97_pg_sp i.emplst_pg i.emplst_pg_sp i.psample_pl i.psample_pl_sp i.where_germany_pl age age_sp couple_earnings_net housework_daily_avg housework_daily_avg_sp i.housing_status_hl num_children_hl rel_start_all if relationship_duration==min_dur // PSM based on characteristics at start of cohab OR first observed
predict psm if relationship_duration==min_dur

bysort pid partner_id_pl: egen pscore = max(psm)
sort pid partner_id_pl syear
browse pid partner_id_pl relationship_duration psm pscore min_dur isced97_pg isced97_pg_sp where_germany_pl age age_sp couple_earnings_net housing_status_hl num_children_hl rel_start_all

// check for overlap
tabstat psm, by(treated)
sum psm if treated==0, det
sum psm if treated==1, det
tabstat pscore, by(treated)

set scheme cleanplots
twoway (histogram psm if treated==1, width(.02) color(pink%30)) (histogram psm if treated==0, width(.02) color(gs8%30)),  legend(order(1 "Treated" 2 "Control") rows(1) position(6)) xtitle("Propensity Score")
twoway (histogram pscore if treated==1, width(.02) color(pink%30)) (histogram pscore if treated==0, width(.02) color(gs8%30)),  legend(order(1 "Treated" 2 "Control") rows(1) position(6)) xtitle("Propensity Score")

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
