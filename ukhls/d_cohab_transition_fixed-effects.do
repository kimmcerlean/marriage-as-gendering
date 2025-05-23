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

// browse pidp partner_id int_year rel_start_all marital_status_defacto marr_trans ever_transition year_transitioned
// make sure dates look right

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

gen relationship_duration = int_year - rel_start_all
drop if relationship_duration < 0

gen duration_cohab=.
replace duration_cohab = int_year - year_transitioned if treated==1
replace duration_cohab = relationship_duration if treated==0

tab duration_cohab, m

browse pidp partner_id int_year treated marital_status_defacto marr_trans rel_start_all year_transitioned relationship_duration duration_cohab


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

xtset couple_id year // have to use year instead of int_year because sometimes consecutive waves are in same year

tab marital_status_defacto treated, m
tab year_transitioned treated, m
tab marital_status_defacto if treated==1 & int_year >= year_transitioned
tab marital_status_defacto if treated==1 & int_year < year_transitioned

gen married = 0
replace married = 1 if marital_status_defacto==1

// other prep steps
tab duration_cohab treated, m
tab relationship_duration treated, m

keep if duration_cohab >=-5 & duration_cohab<=8
gen duration_pos = duration_cohab+6

// data exploration
tabstat female_earn_pct_t female_hours_pct_t female_housework_pct_t, by(duration_cohab) 
 
preserve

collapse (median) female_earn_pct_t female_hours_pct_t female_housework_pct_t, by(duration_cohab)

twoway (line female_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=8) ///
(line female_hours_pct_t duration_cohab if duration_cohab>=-5 & duration_cohab <=8) ///
(line female_housework_pct_t duration_cohab if duration_cohab>=-5 & duration_cohab <=8), ///
legend(order(1 "Earnings" 2 "Work Hours" 3 "Housework Hours") rows(1) position(6)) xscale(range(-5 8)) xlabel(-5(1)8)  xline(0)

restore

 
preserve

collapse (median) female_earn_pct_t female_hours_pct_t female_housework_pct_t, by(duration_cohab treated)

twoway (line female_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=8 & treated==0, lcolor(green) lpattern(dash)) ///
(line female_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=8 & treated==1, lcolor(green) lpattern(solid)) 		///
(line female_hours_pct_t duration_cohab if duration_cohab>=-5 & duration_cohab <=8 & treated==0, lcolor(blue) lpattern(dash)) 		///
(line female_hours_pct_t duration_cohab if duration_cohab>=-5 & duration_cohab <=8 & treated==1, lcolor(blue) lpattern(solid)) 	///
(line female_housework_pct_t duration_cohab if duration_cohab>=-5 & duration_cohab <=8 & treated==0, lcolor(pink) lpattern(dash)) 	///
(line female_housework_pct_t duration_cohab if duration_cohab>=-5 & duration_cohab <=8 & treated==1, lcolor(pink) lpattern(solid)), 	///
legend(order(1 "Earnings: Control" 2 "Earnings: Treated" 3 "Work Hours: Control" 4 "Work Hours: Treated" 5 "HW Hours: Control" 6 "HW Hours: Treated") ///
rows(2) position(6)) xscale(range(-5 8)) xlabel(-5(1)8)  xline(0)

restore


// preliminary exploration models
tabstat female_earn_pct_t female_hours_pct_t female_housework_pct_t if treated==1, by(married) // without controls, this is what I'd be estimating

regress female_earn_pct_t i.married if treated==1
margins married
regress female_hours_pct_t i.married if treated==1
margins married
regress female_housework_pct_t i.married if treated==1
margins married

ttest female_earn_pct_t if treated==1, by(married) 
ttest female_hours_pct_t if treated==1, by(married) 
ttest female_housework_pct_t if treated==1, by(married) 

global controls "i.xw_ethn_dv age_all age_all_sp i.hiqual_dv i.hiqual_dv_sp couple_earnings_t rel_start_all nchild_dv i.home_owner"

regress female_earn_pct_t i.married $controls if treated==1
margins married

// regress female_earn_pct_t i.married $controls i.couple_id if treated==1
// margins married

xtreg female_earn_pct_t i.married $controls if treated==1, fe // this exactly matches above, except above won't estimate standard errors so def use this
margins married

// exploring right duration measures
xtreg female_earn_pct_t i.married i.relationship_duration $controls if treated==1, fe
margins married
margins relationship_duration
marginsplot

xtreg female_earn_pct_t i.married duration_cohab $controls if treated==1, fe
margins married
margins, at(dur=(-5(1)10))
marginsplot

xtreg female_earn_pct_t i.married##i.relationship_duration $controls if treated==1, fe
margins married
margins married, at(relationship_duration=(0(1)10))
marginsplot

************************************
**# Compare contrast OLS v. FE
************************************
// with both controls AND control group
regress female_earn_pct_t i.married $controls
margins married
xtreg female_earn_pct_t i.married $controls, fe
margins married

regress female_hours_pct_t i.married $controls
margins married
xtreg female_hours_pct_t i.married $controls, fe
margins married

regress female_housework_pct_t i.married $controls
margins married
xtreg female_housework_pct_t i.married $controls, fe
margins married

// with controls, but NO control group
regress female_earn_pct_t i.married $controls if treated==1
margins married
xtreg female_earn_pct_t i.married $controls if treated==1, fe
margins married

regress female_hours_pct_t i.married $controls if treated==1
margins married
xtreg female_hours_pct_t i.married $controls if treated==1, fe
margins married

regress female_housework_pct_t i.married $controls if treated==1
margins married
xtreg female_housework_pct_t i.married $controls  if treated==1, fe
margins married

// no controls but with control group
regress female_earn_pct_t i.married
margins married
xtreg female_earn_pct_t i.married, fe
margins married

regress female_hours_pct_t i.married
margins married
xtreg female_hours_pct_t i.married, fe
margins married

regress female_housework_pct_t i.married
margins married
xtreg female_housework_pct_t i.married, fe
margins married

// no controls OR control group
regress female_earn_pct_t i.married if treated==1
margins married
xtreg female_earn_pct_t i.married if treated==1, fe
margins married

regress female_hours_pct_t i.married if treated==1
margins married
xtreg female_hours_pct_t i.married if treated==1, fe
margins married

regress female_housework_pct_t i.married if treated==1
margins married
xtreg female_housework_pct_t i.married if treated==1, fe
margins married

************************************
**# FEIS v. FE
************************************
// so I feel like this is what matches Zhou and Kan. How do I add the TIME element though??
// and decide if I keep never treated or not (Bruderl / Schechtl def do; I feel like Z&K do as well?)

tab hiqual_dv, gen(educ_r)
tab hiqual_dv_sp, gen(educ_sp)

// controls  AND control group
local controls  "educ_r1 educ_r2 educ_r3 educ_r4 educ_r5 educ_sp1 educ_sp2 educ_sp3 educ_sp4 educ_sp5 age_all age_all_sp couple_earnings_t nchild_dv home_owner" // educ_r6 educ_sp6 i.xw_ethn_dv hiqual_dv hiqual_dv_sp

xtreg female_earn_pct_t i.married `controls', fe
xtfeis female_earn_pct_t married `controls', slope(relationship_duration) cluster(couple_id) 

xtreg female_hours_pct_t i.married `controls', fe
xtfeis female_hours_pct_t married `controls', slope(relationship_duration) cluster(couple_id) 

xtreg female_housework_pct_t i.married `controls', fe
xtfeis female_housework_pct_t married `controls', slope(relationship_duration) cluster(couple_id) 

// with controls, but NO control group
local controls  "educ_r1 educ_r2 educ_r3 educ_r4 educ_r5 educ_sp1 educ_sp2 educ_sp3 educ_sp4 educ_sp5 age_all age_all_sp couple_earnings_t nchild_dv home_owner" // educ_r6 educ_sp6 i.xw_ethn_dv hiqual_dv hiqual_dv_sp

xtreg female_earn_pct_t i.married `controls' if treated==1, fe
xtfeis female_earn_pct_t married `controls' if treated==1, slope(relationship_duration) cluster(couple_id) 

xtreg female_hours_pct_t i.married `controls' if treated==1, fe
xtfeis female_hours_pct_t married `controls' if treated==1, slope(relationship_duration) cluster(couple_id) 

xtreg female_housework_pct_t i.married `controls' if treated==1, fe
xtfeis female_housework_pct_t married `controls' if treated==1, slope(relationship_duration) cluster(couple_id) 

// no controls but with control group
xtreg female_earn_pct_t i.married, fe
xtfeis female_earn_pct_t married, slope(relationship_duration) cluster(couple_id) 

xtreg female_hours_pct_t i.married, fe
xtfeis female_hours_pct_t married, slope(relationship_duration) cluster(couple_id) 

xtreg female_housework_pct_t i.married, fe
xtfeis female_housework_pct_t married, slope(relationship_duration) cluster(couple_id) 

// no controls OR control group
xtreg female_earn_pct_t i.married if treated==1, fe
xtfeis female_earn_pct_t married if treated==1, slope(relationship_duration) cluster(couple_id) 

xtreg female_hours_pct_t i.married if treated==1, fe
xtfeis female_hours_pct_t married if treated==1, slope(relationship_duration) cluster(couple_id) 

xtreg female_housework_pct_t i.married if treated==1, fe
xtfeis female_housework_pct_t married if treated==1, slope(relationship_duration) cluster(couple_id) 

// Hausman test for whether or not you need FEIS v. FE
local controls  "educ_r1 educ_r2 educ_r3 educ_r4 educ_r5 educ_sp1 educ_sp2 educ_sp3 educ_sp4 educ_sp5 age_all age_all_sp couple_earnings_t nchild_dv home_owner" // educ_r6 educ_sp6 i.xw_ethn_dv hiqual_dv hiqual_dv_sp

xtfeis female_earn_pct_t married `controls', slope(relationship_duration) cluster(couple_id) 
xtart, keep(married) 

xtfeis female_hours_pct_t married `controls', slope(relationship_duration) cluster(couple_id) 
xtart, keep(married) 

xtfeis female_housework_pct_t married `controls', slope(relationship_duration) cluster(couple_id) 
xtart, keep(married) 

************************************
**# COEFPLOT FOR FLOPS (May 2025)
************************************
// with both controls AND control group
global controls  "educ_r1 educ_r2 educ_r3 educ_r4 educ_r5 educ_sp1 educ_sp2 educ_sp3 educ_sp4 educ_sp5 age_all age_all_sp couple_earnings_t nchild_dv home_owner" 

** Earnings
regress female_earn_pct_t married $controls
est store e_b
xtreg female_earn_pct_t married $controls, fe
est store e_fe
xtfeis female_earn_pct_t married $controls, slope(relationship_duration) cluster(couple_id) 
est store e_feis

** Paid Work Hours
regress female_hours_pct_t married $controls
est store h_b
xtreg female_hours_pct_t married $controls, fe
est store h_fe
xtfeis female_hours_pct_t married $controls, slope(relationship_duration) cluster(couple_id) 
est store h_feis

** Housework
regress female_housework_pct_t married $controls
est store hw_b
xtreg female_housework_pct_t married $controls, fe
est store hw_fe
xtfeis female_housework_pct_t married $controls, slope(relationship_duration) cluster(couple_id) 
est store hw_feis

coefplot 	(h_b, label("OLS") lcolor("gs8") mcolor("gs8") ciopts(color("gs8"))) ///
			(h_fe,label("FE") lcolor("pink") mcolor("pink") ciopts(color("pink"))) ///
			(h_feis,label("FEIS") lcolor("pink%30") mcolor("pink%30") ciopts(color("pink%30"))), bylabel("Paid Work Hours") || ///
			(e_b, label("OLS")) (e_fe,label("FE")) (e_feis,label("FEIS")), bylabel("Earnings") || ///
			(hw_b, label("OLS")) (hw_fe,label("FE")) (hw_feis,label("FEIS")), bylabel("Housework") || ///
			, keep(married)  byopts(rows(1)) xsize(8) ysize(3) xline(0, lcolor(black) lstyle(solid)) levels(90) ///
			base coeflabels(married = "") xtitle(Change in Women's Share, size(small)) legend(rows(1)) ylabel(none)

coefplot 	(h_b, label("OLS") lcolor("gs8") mcolor("gs8") ciopts(color("gs8"))) ///
			(h_fe,label("FE") lcolor("pink") mcolor("pink") ciopts(color("pink"))) ///
			(h_feis,label("FEIS") lcolor("pink%30") mcolor("pink%30") ciopts(color("pink%30"))), bylabel("Paid Work Hours") || ///
			(hw_b, label("OLS")) (hw_fe,label("FE")) (hw_feis,label("FEIS")), bylabel("Housework") || ///
			, keep(married)  byopts(rows(1)) xsize(7) ysize(3) xline(0, lcolor(black) lstyle(solid)) levels(90) ///
			base coeflabels(married = "") xtitle(Change in Women's Share, size(small)) legend(position(bottom) rows(1)) ylabel(none)

************************************
* Working through impact functions
************************************
// okay, looking at the Happiness 3 replicate code, I see now. Duration only starts ONCE married
gen duration_married = 0
replace duration_married = duration_cohab + 1 if married==1

tab duration_married married
tab duration_married treated

xtreg female_earn_pct_t i.married c.duration_married##c.duration_married $controls, fe

* Plotting the marginal marriage effects (Conditional Effect Plot)
* See slides pdf 96: "reference point is average of all PYs prior to marriage" - only within treated
margins, at(married=(0 1) duration_married=(0(1)8)) contrast(atcontrast(r._at) lincom) noatlegend
marginsplot, recast(line) recastci(rline) yline(0, lcolor(black) lpatter(solid)) ///
   x(duration_married)                                                  /// yrsmarried is on the X-axis
   plot1opts(lstyle(none)) ci1opts(lstyle(none))                  /// omit graph for never-married
   plot2opts(lpattern(solid) lwidth(thick) lcolor(blue))     	  /// estimate for married
   ci2opts(lpattern(dash) lwidth(medthick) lcolor(green))         /// CI for estimate married
   ylabel(-.3(.1).5, grid angle(0) labsize(medium) format(%3.1f)) /// 
   xlabel(0(1)8, labsize(medium))                                ///
   xtitle("Years since marriage", size(large) margin(0 0 0 2))    ///
   ytitle("Change in Female Earnings Percent", size(large))   title(" ")        ///
   legend(pos(7) ring(0) row(1) order(2 "95%-CI") size(medlarge))     
   
  
// for dummy impact, though, they take essentially my duration centered, then lump ALL prior to -1 into -1
// then add 1 so -1 becomes 0 aka not yet married
// then assign control to 0 so it becomes "not yet married" AND "never married" - so this is the comparison - ALL YEARS NEVER MARRIED
tab duration_cohab treated
tab duration_cohab married

gen dummy_duration = .
replace dummy_duration = duration_cohab if treated==1
recode dummy_duration (min/-1=-1)
replace dummy_duration = dummy_duration+1
replace dummy_duration = 0 if treated==0

tab dummy_duration treated
tab dummy_duration married

xtreg female_earn_pct_t i.dummy_duration $controls, fe 
estimates store d1

xtfeis female_earn_pct_t i.dummy_duration $controls, slope(relationship_duration) cluster(couple_id)
est store d2

coefplot 	(d1, lcolor("red*1.2") mcolor("red*1.2") ciopts(color("red*1.2")) label("FE")) 		///
			(d2, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("FEIS")), 	///
			keep(*.dummy_duration) vertical yline(0, lpattern(solid)) recast(line) 			///
			coeflabels(1.dummy_duration="0" 2.dummy_duration="1" 3.dummy_duration="2" 		///
			4.dummy_duration="3" 5.dummy_duration="4" 6.dummy_duration="5"      			///
		    7.dummy_duration="6" 8.dummy_duration="7" 9.dummy_duration="8")        			///         
			ylabel(-.3(.1).5, grid angle(0) labsize(medium) format(%3.1f))        			///
			xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        			///
			ytitle("Change in Female Earnings %", size(medlarge)) 		  
		  
		  
// okay but then they go into other anticipation effects up to year -6, so this is where I could just use my duration positive?
// I just need to figure out where to put control - at time -5 or time 0 (they should be in reference category they say)

// this is with controls at time 0. okay this literally doesn't matter
tab duration_pos treated
gen duration_pos3 = duration_pos
replace duration_pos3 = 6 if treated==0
tab duration_pos3 treated

xtreg female_earn_pct_t ib5.duration_pos3 $controls, fe // ref group is time 0

coefplot, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) recast(line) lwidth(thick) lcolor(blue)  ///
          xline(5, lpatter(solid)) ciopts(recast(rline) lpattern(dash) lwidth(medthick) lcolor(green))   	/// 
		  ylabel(-.3(.1).5, grid angle(0) labsize(medium) format(%3.1f))        					///
		  coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		  4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		  8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"			///
		  12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  						///  
          xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
          ytitle("Change in Female Earnings %", size(medlarge)) 

************************************
**# IMPACT PLOTS FOR FLOPS (May 2025)
************************************
** Earnings
xtreg female_earn_pct_t ib5.duration_pos3 $controls, fe // ref group is time 0

coefplot, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) recast(line) lwidth(medthick) lcolor(pink)  ///
          xline(5, lpatter(solid)) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30))   	/// 
		  ylabel(-.2(.1).2, grid angle(0) labsize(medium) format(%3.1f))        					///
		  coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		  4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		  8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"			///
		  12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  						///  
          xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
          ytitle("Change in Women's Earnings %", size(medlarge)) 
		  	  
** Paid Work Hours
xtreg female_hours_pct_t ib5.duration_pos3 $controls, fe // ref group is time 0

coefplot, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) recast(line) lwidth(medthick) lcolor(pink) ///
          xline(5, lpatter(solid)) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30))   	/// 
		  ylabel(-.2(.1).2, grid angle(0) labsize(medium) format(%3.1f))        					///
		  coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		  4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		  8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"			///
		  12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  						///  
          xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
          ytitle("Change in Women's Paid Work Hrs %", size(medlarge)) 
		  	  
** Housework
xtreg female_housework_pct_t ib5.duration_pos3 $controls, fe // ref group is time 0

coefplot, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) recast(line) lwidth(medthick) lcolor(pink)  ///
          xline(5, lpatter(solid)) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30))   	/// 
		  ylabel(-.2(.1).2, grid angle(0) labsize(medium) format(%3.1f))        					///
		  coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		  4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		  8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"			///
		  12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  						///  
          xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
          ytitle("Change in Women's Housework Hrs %", size(medlarge)) 
		  
// Can I interact with having a birth? (AFTER marriage)
browse pidp partner_id int_year rel_start_all rel_end_all duration_pos3 year_transitioned year_first_birth age_youngest_child xw_anychild_dv nkids_dv nchild_dv

gen birth_after_trans=0
replace birth_after_trans = 1 if year_first_birth >= year_transitioned & year_first_birth!=9999 & xw_anychild_dv!=2 & xw_anychild_dv!=.

gen birth_interact = birth_after_trans
replace birth_interact = 0 if birth_after_trans==1 & inrange(duration_pos3,1,4)

browse pidp partner_id int_year rel_start_all rel_end_all birth_after_trans birth_interact  duration_pos3 year_transitioned year_first_birth age_youngest_child xw_anychild_dv nkids_dv nchild_dv

gen own_child = 0
replace own_child = 1 if nchild_dv > 0 & nchild_dv!=.

** Earnings
xtreg female_earn_pct_t ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins duration_pos3#birth_interact
marginsplot

xtreg female_earn_pct_t ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(birth_interact==0) post
est store e_birth0

xtreg female_earn_pct_t ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(birth_interact==1) post
est store e_birth1

coefplot (e_birth0, recast(line) lwidth(medthick) lcolor(pink) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30)) label("No Post-Marital Birth")) ///
		(e_birth1, recast(line) lwidth(medthick) lcolor(sand) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(sand)) label("Had Post-Marital Birth")) ///
		, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) xline(5, lpattern(solid))  	///
		ylabel(-.3(.1).3, grid angle(0) labsize(medium) format(%3.1f))        					///
		coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"		///
		12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  					///  
		xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
		ytitle("Change in Women's Earnings %", size(medlarge)) legend(position(bottom) rows(1))

// OH - instead of interacting, do I estimate seaprately?? (with the time invariant??)
xtreg female_earn_pct_t ib5.duration_pos3 $controls if birth_after_trans==0, fe // ref group is time 0
est store e_birth0_alt

xtreg female_earn_pct_t ib5.duration_pos3 $controls if birth_after_trans==1, fe // ref group is time 0
est store e_birth1_alt

coefplot (e_birth0_alt, recast(line) lwidth(medthick) lcolor(pink) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30)) label("No Post-Marital Birth")) ///
		(e_birth1_alt, recast(line) lwidth(medthick) lcolor(sand) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(sand)) label("Had Post-Marital Birth")) ///
		, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) xline(5, lpattern(solid))  	///
		ylabel(-.3(.1).3, grid angle(0) labsize(medium) format(%3.1f))        					///
		coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"		///
		12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  					///  
		xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
		ytitle("Change in Women's Earnings %", size(medlarge)) legend(position(bottom) rows(1))

// Children in HH - can't do children ever because it has to vary with time (because fixed effects)
xtreg female_earn_pct_t ib5.duration_pos3##i.own_child $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(own_child==0) post
est store e_kid0

xtreg female_earn_pct_t ib5.duration_pos3##i.own_child $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(own_child==1) post
est store e_kid1

coefplot (e_kid0, recast(line) lwidth(medthick) lcolor(pink) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30)) label("No Children in HH")) ///
		(e_kid1, recast(line) lwidth(medthick) lcolor(sand) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(sand)) label("Children in HH")) ///
		, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) xline(5, lpattern(solid))  	///
		ylabel(-.3(.1).3, grid angle(0) labsize(medium) format(%3.1f))        					///
		coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"		///
		12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  					///  
		xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
		ytitle("Change in Women's Earnings %", size(medlarge)) legend(position(bottom) rows(1))
		
** Paid Work Hours
xtreg female_hours_pct_t ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins duration_pos3#birth_interact
marginsplot

xtreg female_hours_pct_t ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(birth_interact==0) post
est store h_birth0

xtreg female_hours_pct_t ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(birth_interact==1) post
est store h_birth1

coefplot (h_birth0, recast(line) lwidth(medthick) lcolor(pink) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30)) label("No Post-Marital Birth")) ///
		(h_birth1, recast(line) lwidth(medthick) lcolor(sand) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(sand)) label("Had Post-Marital Birth")) ///
		, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) xline(5, lpattern(solid))  	///
		ylabel(-.3(.1).3, grid angle(0) labsize(medium) format(%3.1f))        					///
		coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"		///
		12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  					///  
		xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
		ytitle("Change in Women's Paid Work Hrs %", size(medlarge)) legend(position(bottom) rows(1))
		
** Housework Hours
xtreg female_housework_pct_t ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins duration_pos3#birth_interact
marginsplot

xtreg female_housework_pct_t ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(birth_interact==0) post
est store hw_birth0

xtreg female_housework_pct_t ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(birth_interact==1) post
est store hw_birth1

coefplot (hw_birth0, recast(line) lwidth(medthick) lcolor(pink) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30)) label("No Post-Marital Birth")) ///
		(hw_birth1, recast(line) lwidth(medthick) lcolor(sand) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(sand)) label("Had Post-Marital Birth")) ///
		, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) xline(5, lpattern(solid))  	///
		ylabel(-.3(.1).3, grid angle(0) labsize(medium) format(%3.1f))        					///
		coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"		///
		12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  					///  
		xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
		ytitle("Change in Women's Housework Hrs %", size(medlarge)) legend(position(bottom) rows(1))


************************************
* Old analyses
************************************
// models to use for gender workshop / prelim analysis
// don't match 100% because i reduced the duration pre / post being used

// do I use controls with fixed effects? I think because that controls for observed and the remained is unobserved? I think controls let hyou control for time-varying things whereas fixed effects net out time invariant? (hence why race is collinear?)
xtreg female_earn_pct_t i.married i.relationship_duration $controls if treated==1 & relationship_duration<=15, fe
margins married

xtreg female_hours_pct_t i.married i.relationship_duration $controls if treated==1 & relationship_duration<=15, fe
margins married

xtreg female_housework_pct_t i.married i.relationship_duration $controls if treated==1 & relationship_duration<=15, fe
margins married
