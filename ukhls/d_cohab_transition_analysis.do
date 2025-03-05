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

// stopped here 3/4/25 - NEED TO FINISH THIS ASAP

********************************************************************************
**# okay make analytical sample and recode duration relative to marital transition
********************************************************************************
// before dropping, get descriptive comparison of cohabitors to married couples
tabstat female_earn_pct_t female_hours_pct_t female_housework_pct_t, by(marital_status_defacto) 
ttest female_earn_pct_t, by(marital_status_defacto) 
ttest female_hours_pct_t, by(marital_status_defacto) 
ttest female_housework_pct_t, by(marital_status_defacto) 

keep if ever_transition==1 | always_cohab==1 // so keeping a "control" group - basically drops those always married

gen treated=.
replace treated=1 if ever_transition==1 & always_cohab!=1
replace treated=0 if always_cohab==1

tab year_transitioned ever_transition, m

gen relationship_duration = int_year - rel_start_all
drop if relationship_duration < 0

gen duration_cohab=.
replace duration_cohab = int_year - year_transitioned if treated==1
replace duration_cohab = relationship_duration if treated==0

tab duration_cohab, m

browse pidp partner_id int_year treated marital_status_defacto marr_trans rel_start_all year_transitioned relationship_duration duration_cohab

// create couple-level education
tab college_degree college_degree_sp

gen couple_educ_gp=0
replace couple_educ_gp = 1 if college_degree==1 | college_degree_sp==1
replace couple_educ_gp=. if college_degree==. & college_degree_sp==.

// home_owners
recode tenure_dv (-9/-7=.)(1/2=1)(3/8=0), gen(home_owner)
tab home_owner, m
tab tenure_dv home_owner, m

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

********************************************************************************
**# Some descriptive statistics
* (Not yet revisited)
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

tab had_birth
unique pidp partner_id  if had_birth==1 // use this for % experiencing a birth
unique pidp partner_id  if had_birth==1 & duration_cohab==0
tab had_birth if duration_cohab==0

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
**# Descriptive analysis
* (not yet revisited)
********************************************************************************
/* descriptive

** total sample
preserve

collapse (median) paid_wife_pct_ot paid_earn_pct unpaid_wife_pct , by(duration_cohab) 
twoway line paid_wife_pct_ot duration_cohab if duration_cohab>=-5 & duration_cohab <=10
// twoway line paid_wife_pct_ot duration_cohab if duration_cohab>=-10 & duration_cohab <=15
twoway line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10
twoway line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10
// twoway line unpaid_wife_pct duration_cohab if duration_cohab>=-10 & duration_cohab <=15

twoway (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10) (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10)
twoway (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10) (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) 

restore

** split by presence of children
preserve

collapse (median) paid_wife_pct_ot paid_earn_pct unpaid_wife_pct, by(duration_cohab kids_in_hh)  // results for kids don't make sense, because need to scale it to TIME from children, because right now, it's all muddled. so doing NO KIDS in HH to show the results has the advantage of like OKAY it's not JUST because of children, but need to more formally investigate the role of children.

twoway line paid_wife_pct_ot duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & kids_in_hh==0
twoway line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & kids_in_hh==0

twoway (line paid_wife_pct_ot duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & kids_in_hh==0) (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & kids_in_hh==0)

twoway (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & kids_in_hh==0) (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & kids_in_hh==0, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) 

twoway (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & kids_in_hh==1) (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & kids_in_hh==1, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) 

twoway (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10) (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) by(kids_in_hh, graphregion(margin(tiny)))

restore

** split by having a college degree (either, for now))
preserve

collapse (median) paid_wife_pct_ot paid_earn_pct unpaid_wife_pct, by(duration_cohab couple_educ_gp)

line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==0
line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==1
line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==0
line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==1

// by type of labor
twoway (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==0) (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==1), legend(order(1 "No College" 2 "College") rows(1) position(6))
twoway (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==0) (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==1), legend(order(1 "No College" 2 "College") rows(1) position(6))

// by degree
twoway (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==0) (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==0, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6))
twoway (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==1) (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==1, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6))

// lol is it crazy to do all? bc it's the same trend, but starting positions are different
twoway (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==0, lpattern(dash)) (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==1) (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==0, lpattern(dash) yaxis(2)) (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==1, yaxis(2)), legend(order(1 "No Paid" 2 "College Paid" 3 "No Unpaid" 4 "College Unpaid") rows(1) position(6))

twoway (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==0, lcolor(green)) (line paid_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==1, lcolor(blue)) (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==0, lpattern(dash) lcolor(green)) (line unpaid_wife_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=10 & couple_educ_gp==1, lpattern(dash) lcolor(blue)), legend(order(1 "No Paid" 2 "College Paid" 3 "No Unpaid" 4 "College Unpaid") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"% Female Contributions"')

restore

// other charts
tab duration_cohab hh_earn_type if duration_cohab>=-5 & duration_cohab <=10, row nofreq
tab duration_cohab unpaid_dol if duration_cohab>=-5 & duration_cohab <=10, row nofreq
tab duration_cohab earn_housework if duration_cohab>=-5 & duration_cohab <=10, row nofreq
// tab duration_cohab hours_housework if duration_cohab>=-5 & duration_cohab <=10, row nofreq
*/

********************************************************************************
**# Option 1: fixed effects just on treated?
********************************************************************************
// so first need a var that is pre v. post treated. can I just use marital status?
egen couple_id = group(pidp partner_id)
 
tab marital_status_defacto treated, m
tab year_transitioned treated, m
tab marital_status_defacto if treated==1 & int_year >= year_transitioned
tab marital_status_defacto if treated==1 & int_year < year_transitioned

gen married = 0
replace married = 1 if marital_status_defacto==1

tabstat female_earn_pct_t female_hours_pct_t female_housework_pct_t if treated==1, by(married) // without controls, this is what I'd be estimating

regress female_earn_pct_t i.married if treated==1
regress female_hours_pct_t i.married if treated==1
regress female_housework_pct_t i.married if treated==1

ttest female_earn_pct_t if treated==1, by(married) 
ttest female_hours_pct_t if treated==1, by(married) 
ttest female_housework_pct_t if treated==1, by(married) 

local controls "i.xw_ethn_dv age_all age_all_sp hiqual_dv hiqual_dv_sp couple_earnings_t rel_start_all nchild_dv i.home_owner"

regress female_earn_pct_t i.married `controls' if treated==1
margins married

// regress female_earn_pct_t i.married `controls' i.couple_id if treated==1
// margins married

xtset couple_id

xtreg female_earn_pct_t i.married `controls' if treated==1, fe // this exactly matches above, except above won't estimate standard errors so def use this
margins married

// exploring right duration measures
local controls "i.xw_ethn_dv age_all age_all_sp hiqual_dv hiqual_dv_sp couple_earnings_t rel_start_all nchild_dv i.home_owner"

xtreg female_earn_pct_t i.married i.relationship_duration `controls' if treated==1, fe
margins married
margins relationship_duration
marginsplot

xtreg female_earn_pct_t i.married duration_cohab `controls' if treated==1, fe
margins married
margins, at(dur=(-5(1)10))
marginsplot

xtreg female_earn_pct_t i.married##i.relationship_duration `controls' if treated==1, fe
margins married
margins married, at(relationship_duration=(0(1)10))
marginsplot


// models to use for gender workshop / prelim analysis
local controls "i.xw_ethn_dv age_all age_all_sp hiqual_dv hiqual_dv_sp couple_earnings_t rel_start_all nchild_dv i.home_owner"

// do I use controls with fixed effects? I think because that controls for observed and the remained is unobserved? I think controls let hyou control for time-varying things whereas fixed effects net out time invariant? (hence why race is collinear?)
xtreg female_earn_pct_t i.married i.relationship_duration `controls' if treated==1 & relationship_duration<=15, fe
margins married

xtreg female_hours_pct_t i.married i.relationship_duration `controls' if treated==1 & relationship_duration<=15, fe
margins married

xtreg female_housework_pct_t i.married i.relationship_duration `controls' if treated==1 & relationship_duration<=15, fe
margins married

********************************************************************************
**# Preliminary PSM (this is very crude)
********************************************************************************
// prob want to use characteristics at relationship start, which is relationship_duration==0 for both
bysort pidp partner_id: egen min_dur = min(relationship_duration)
tab min_dur, m // 70% observed year 1 or 2

logit treated i.hiqual_dv i.hiqual_dv_sp i.xw_memorig i.xw_memorig_sp i.xw_ethn_dv i.xw_ethn_dv_sp age_all age_all_sp i.jbstat i.jbstat_sp couple_earnings_t i.home_owner nchild_dv rel_start_all if relationship_duration==min_dur // PSM based on characteristics at start of cohab OR first observed
predict psm if relationship_duration==min_dur

bysort pidp partner_id: egen pscore = max(psm)
sort pidp partner_id int_year

// check for overlap
tabstat psm, by(treated)
sum psm if treated==0, det
sum psm if treated==1, det
tabstat pscore, by(treated)

twoway (histogram psm if treated==1, width(.02) color(blue%30)) (histogram psm if treated==0, width(.02) color(red%30)),  legend(order(1 "Treated" 2 "Control") rows(1) position(6))

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
