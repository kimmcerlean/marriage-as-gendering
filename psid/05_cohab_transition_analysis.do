********************************************************************************
********************************************************************************
* Project: Marriage as a Gendering Institution
* Owner: Kimberly McErlean
* Started: September 2024
* File: cohab_transition_analyis
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files actually conducts the analysis

********************************************************************************
* Import data and small final sample cleanup
********************************************************************************
use "$created_data/PSID_partners_cleaned.dta", clear

// first get partner_id before I drop before
gen id_head=.
replace id_head = unique_id if relationship==1 
bysort survey_yr main_fam_id FAMILY_INTERVIEW_NUM_ (id_head): replace id_head = id_head[1]

gen id_wife=.
replace id_wife = unique_id if relationship==2
bysort survey_yr main_fam_id FAMILY_INTERVIEW_NUM_ (id_wife): replace id_wife = id_wife[1]

sort survey_yr FAMILY_INTERVIEW_NUM_
browse unique_id main_fam_id FAMILY_INTERVIEW_NUM_ survey_yr relationship id_head id_wife

gen partner_id=.
replace partner_id = id_head if relationship==2 // so need opposite id
replace partner_id = id_wife if relationship==1

browse unique_id main_fam_id partner_id FAMILY_INTERVIEW_NUM_ survey_yr relationship id_head id_wife

// now work on deduplicating
tab SEX marital_status_updated if SEX_HEAD_==1
/* need to end up with this amount of respondents after the below
           | marital_status_update
    SEX OF |           d
INDIVIDUAL | Married (  Partnered |     Total
-----------+----------------------+----------
      Male |   163,449     11,543 |   174,992 
    Female |   163,429     11,499 |   174,928 
-----------+----------------------+----------
     Total |   326,878     23,042 |   349,920 

*/

drop if SEX_HEAD_!=1
tab rel_start_yr SEX, m col // is either one's data more reliable?

// keep only one respondent per household (bc all data recorded for all)
sort survey_yr FAMILY_INTERVIEW_NUM_  unique_id   
browse unique_id FAMILY_INTERVIEW_NUM_ survey_yr SEX marital_status_updated rel_start_yr female_earn_pct_t hh_earn_type_t female_hours_pct_t hh_hours_type_t wife_housework_pct_t housework_bkt_t

gen has_rel_info=0
replace has_rel_info=1 if rel_start_yr!=.

bysort survey_yr FAMILY_INTERVIEW_NUM_: egen rel_info = max(has_rel_info)
browse unique_id FAMILY_INTERVIEW_NUM_ survey_yr SEX marital_status_updated rel_info has_rel_info rel_start_yr female_earn_pct_t hh_earn_type_t female_hours_pct_t hh_hours_type_t wife_housework_pct_t housework_bkt_t

* first drop the partner WITHOUT rel info if at least one of them does
drop if has_rel_info==0 & rel_info==1

*then rank the remaining members
bysort survey_yr FAMILY_INTERVIEW_NUM_ : egen per_id = rank(unique_id) // so if there is only one member left after above, will get a 1
browse survey_yr FAMILY_INTERVIEW_NUM_  unique_id per_id

tab per_id // 1s should approximately total above
keep if per_id==1 | per_id==3

tab marital_status_updated
/* k pretty close

marital_status_upd |
              ated |      Freq.     Percent        Cum.
-------------------+-----------------------------------
Married (or pre77) |    163,608       93.39       93.39
         Partnered |     11,580        6.61      100.00
-------------------+-----------------------------------
             Total |    175,188      100.00

*/

// should I restrict to certain years? aka to help with the cohab problem? well probably should from a time standpoint... and to match to the british one, at least do 1990+?
tab survey_yr marital_status_updated
tab rel_start_yr marital_status_updated, m

unique unique_id, by(marital_status_updated) 
unique unique_id if rel_start_yr >= 1990, by(marital_status_updated) // nearly half of sample goes away. okay let's decide later...
keep if rel_start_yr >= 1990 // cohabitation not even reliably measured until mid-1980s

// restrict to working age?
tab AGE_HEAD_ employed_head, row
keep if (AGE_HEAD_>=18 & AGE_HEAD_<=60) &  (AGE_WIFE_>=18 & AGE_WIFE_<=60) // sort of drops off a cliff after 60?

// identify couples who ever transitioned to marriage - but need to do WITHIN a given relationship?! except do cohab and marriage of same couple have diff start / end dates? because can just sort by unique ID and rel start year?! because those cover all records. okay yeah, crap, they do, let's try to figure this out.
// how do I also get those partnered whole time? so I need two samples - cohabitors whole time and transitioners. Married whole time, I don't want.

sort unique_id survey_yr
browse unique_id partner_id survey_yr marital_status_updated marr_trans

tab marital_status_updated marr_trans, m

bysort unique_id partner_id (marr_trans): egen ever_transition = max(marr_trans)
gen year_transitioned=survey_yr if marr_trans==1
bysort unique_id partner_id (year_transitioned): replace year_transitioned = year_transitioned[1]

bysort unique_id partner_id: egen years_observed=count(survey_yr)
bysort unique_id partner_id (years_observed): replace years_observed = years_observed[1]

bysort unique_id partner_id: egen years_married=count(survey_yr) if marital_status_updated==1
bysort unique_id partner_id (years_married): replace years_married = years_married[1]

bysort unique_id partner_id: egen years_cohab=count(survey_yr) if marital_status_updated==2
bysort unique_id partner_id (years_cohab): replace years_cohab = years_cohab[1]

sort unique_id survey_yr
browse unique_id partner_id survey_yr years_observed years_married years_cohab ever_transition marital_status_updated marr_trans rel_start_yr  year_transitioned

gen always_married=0
replace always_married=1 if years_observed==years_married & years_married!=.

gen always_cohab=0
replace always_cohab=1 if years_observed==years_cohab & years_cohab!=.

tab always_cohab always_married, m
tab always_cohab ever_transition, m
tab always_married ever_transition, m

browse unique_id partner_id survey_yr always_married  always_cohab ever_transition marital_status_updated  years_observed years_married years_cohab  marr_trans rel_start_yr  year_transitioned

// can I just use ever transitioned? I think I did this because I hadn't added partner ID before
gen transition_flag=0
replace transition_flag=1 if ever_transition==1 & marital_status_updated==2 // keep all cohabs (except do have some risk that have multiple cohabs...)
replace transition_flag=1 if ever_transition==1 & marital_status_updated==1 & survey_yr >= year_transitioned // so only keep married AFTER year of transition. so might help weed out otehr marriages? that is why I want to test before I drop
replace transition_flag=0 if ever_transition==1 & rel_start_yr > year_transitioned+1 // soo if NEW relationship started post transition to marriage, shouldn't keep? that will rule out the multiple cohab issue I mention above - except will this knock out people if marriage year recorded as new year? so restrict to cohabs? add a one window buffer?

tab ever_transition transition_flag, m

********************************************************************************
**# okay make analytical sample and recode duration relative to marital transition
********************************************************************************
keep if transition_flag==1 | always_cohab==1 // so keeping a "control" group - basically drops those always married

browse unique_id partner_id survey_yr transition_flag always_cohab rel_start_yr year_transitioned relationship_duration marital_status_updated

gen duration_cohab=.
replace duration_cohab = survey_yr - year_transitioned if transition_flag==1
replace duration_cohab = relationship_duration if always_cohab==1

browse unique_id partner_id survey_yr transition_flag always_cohab rel_start_yr year_transitioned relationship_duration duration_cohab marital_status_updated

tab duration_cohab, m

recode duration_cohab(-24/-11=-5)(-10/-7=-4)(-6/-5=-3)(-4/-3=-2)(-2/-1=-1)(0=0)(1/2=1)(3/4=2)(5/6=3)(7/8=4)(9/10=5)(11/12=6)(13/20=7)(21/30=8), gen(dur) // smoothing (bc the switch to every other year makes this wonky)

********************************************************************************
**# Some descriptive statistics - have NOT revisited this
********************************************************************************
browse unique_id survey_yr year_transitioned rel_start_yr // some people could have two relationships? so uniques needs to be combo of id and relation year?! oh but the cohab / marriage are off, so actually do transition year?
unique unique_id
unique unique_id year_transitioned
unique unique_id rel_start_yr
unique unique_id if dur==0

sum duration_cohab if dur < 0 // average cohab duration
sum duration_cohab if dur > 0 & dur !=. // average marital duration

tab couple_educ_gp
tab couple_educ_gp if dur==0

tab children
tab children if dur==0

tab had_birth
unique unique_id if had_birth==1 // use this for % experiencing a birth
unique unique_id if had_birth==1 & dur==0
tab had_birth if dur==0

sum female_earn_pct
tab hh_earn_type
sum female_hours_pct
sum wife_housework_pct
tab housework_bkt
tab earn_housework

sum female_earn_pct if dur==0
tab hh_earn_type if dur==0
sum female_hours_pct if dur==0
sum wife_housework_pct if dur==0
tab housework_bkt if dur==0
tab earn_housework if dur==0

********************************************************************************
**# ANALYSIS (finally lol)
********************************************************************************
// descriptive

** total sample
preserve

collapse (median) female_earn_pct_t female_hours_pct_t wife_housework_pct_t, by(dur transition_flag)

// both types of labor: non-transitioners
twoway (line female_earn_pct_t dur if dur>=-4 & dur <=6 & transition_flag==0) (line wife_housework_pct_t dur if dur>=-4 & dur <=6 & transition_flag==0, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) 

// both types of labor: transitioners
twoway (line female_earn_pct_t dur if dur>=-4 & dur <=6 & transition_flag==1) (line wife_housework_pct_t dur if dur>=-4 & dur <=6 & transition_flag==1, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) 

// paid labor: compare the two
twoway (line female_earn_pct_t dur if dur>=-4 & dur <=6 & transition_flag==0) (line female_earn_pct_t dur if dur>=-4 & dur <=6 & transition_flag==1), legend(order(1 "Cohab" 2 "Transitioned") rows(1) position(6)) xtitle(`"Duration from Marital Transition"')

// paid labor: compare the two
twoway (line female_hours_pct_t dur if dur>=-4 & dur <=6 & transition_flag==0) (line female_hours_pct_t dur if dur>=-4 & dur <=6 & transition_flag==1), legend(order(1 "Cohab" 2 "Transitioned") rows(1) position(6)) xtitle(`"Duration from Marital Transition"')

// unpaid labor: compare the two
twoway (line wife_housework_pct_t dur if dur>=-4 & dur <=6 & transition_flag==0) (line wife_housework_pct_t dur if dur>=-4 & dur <=6 & transition_flag==1), legend(order(1 "Cohab" 2 "Transitioned") rows(1) position(6)) xtitle(`"Duration from Marital Transition"')

// one way to plot it all, don't like this
twoway (line female_earn_pct_t dur if dur>=-4 & dur <=6) (line wife_housework_pct_t dur if dur>=-4 & dur <=6, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) by(transition_flag)

restore

** total just those who transition (original analysis)
preserve

collapse (median) female_earn_pct_t female_hours_pct_t wife_housework_pct_t if transition_flag==1, by(dur)
twoway line female_earn_pct_t dur if dur>=-4 & dur <=6
twoway line female_hours_pct_t dur if dur>=-4 & dur <=6
twoway line wife_housework_pct_t dur if dur>=-4 & dur <=6

twoway (line female_earn_pct_t dur if dur>=-4 & dur <=6) (line wife_housework_pct_t dur if dur>=-4 & dur <=6)
twoway (line female_earn_pct_t dur if dur>=-4 & dur <=6) (line wife_housework_pct_t dur if dur>=-4 & dur <=6, yaxis(2))

twoway (line female_earn_pct_t dur if dur>=-4 & dur <=6) (line wife_housework_pct_t dur if dur>=-4 & dur <=6, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) 

twoway (line female_hours_pct_t dur if dur>=-4 & dur <=6) (line wife_housework_pct_t dur if dur>=-4 & dur <=6, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) 

restore


/// stopped here for now for 2/7/25 - nothing below here updated

** split by presence of children
preserve

collapse (median) female_earn_pct female_hours_pct wife_housework_pct, by(dur children)

twoway (line female_earn_pct dur if dur>=-4 & dur <=6 & children==0) (line wife_housework_pct dur if dur>=-4 & dur <=6 & children==0, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) 

twoway (line female_earn_pct dur if dur>=-4 & dur <=6 & children==1) (line wife_housework_pct dur if dur>=-4 & dur <=6 & children==1, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) 

twoway (line female_earn_pct dur if dur>=-4 & dur <=6) (line wife_housework_pct dur if dur>=-4 & dur <=6, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) by(children)

restore

** split by having a college degree (either, for now))
preserve

collapse (median) female_earn_pct female_hours_pct wife_housework_pct, by(dur couple_educ_gp)

line female_earn_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==0
line female_earn_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==1
line wife_housework_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==0
line wife_housework_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==1

// by type of labor
twoway (line female_earn_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==0) (line female_earn_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==1), legend(order(1 "No College" 2 "College") rows(1) position(6))
twoway (line wife_housework_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==0) (line wife_housework_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==1), legend(order(1 "No College" 2 "College") rows(1) position(6))

// by degree
twoway (line female_earn_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==0) (line wife_housework_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==0, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6))
twoway (line female_earn_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==1) (line wife_housework_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==1, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6))

// lol is it crazy to do all? bc it's the same trend, but starting positions are different
twoway (line female_earn_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==0, lpattern(dash)) (line female_earn_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==1) (line wife_housework_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==0, lpattern(dash) yaxis(2)) (line wife_housework_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==1, yaxis(2)), legend(order(1 "No Paid" 2 "College Paid" 3 "No Unpaid" 4 "College Unpaid") rows(1) position(6))

twoway (line female_earn_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==0, lcolor(green)) (line female_earn_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==1, lcolor(blue)) (line wife_housework_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==0, lpattern(dash) lcolor(green)) (line wife_housework_pct dur if dur>=-4 & dur <=6 & couple_educ_gp==1, lpattern(dash) lcolor(blue)), legend(order(1 "No Paid" 2 "College Paid" 3 "No Unpaid" 4 "College Unpaid") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"% Female Contributions"')

restore

// other charts
tab dur hh_earn_type, row nofreq
tab dur housework_bkt, row nofreq
tab dur earn_housework, row nofreq
// tab dur hours_housework, row nofreq