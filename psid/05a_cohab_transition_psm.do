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
* Another option - do what I did for divorce and use the family matrix instead?
* To ensure I have right cohabitation?

********************************************************************************
* Import data and small final sample cleanup
********************************************************************************
use "$created_data/PSID_partners_cleaned.dta", clear

egen couple_id = group(unique_id partner_id)

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
tab rel_start_all marital_status_updated, m

unique unique_id, by(marital_status_updated) 
unique unique_id if rel_start_yr >= 1990, by(marital_status_updated) // nearly half of sample goes away.
unique unique_id if rel_start_yr >= 1985, by(marital_status_updated)
keep if rel_start_yr >= 1985 // cohabitation not even reliably measured until mid-1980s

// restrict to working age?
tab AGE_HEAD_ employed_head, row
keep if (AGE_HEAD_>=18 & AGE_HEAD_<=60) &  (AGE_WIFE_>=18 & AGE_WIFE_<=60) // sort of drops off a cliff after 60?

// identify couples who ever transitioned to marriage - but need to do WITHIN a given relationship?! except do cohab and marriage of same couple have diff start / end dates (okay, I fixed this now in step 4) because can just sort by unique ID and rel start year?! because those cover all records.
// how do I also get those partnered whole time? so I need two samples - cohabitors whole time and transitioners. Married whole time, I don't want.

sort unique_id survey_yr
browse unique_id partner_id survey_yr marital_status_updated marr_trans

tab marital_status_updated marr_trans, m

bysort unique_id partner_id (marr_trans): egen ever_transition = max(marr_trans)
gen year_transitioned=survey_yr if marr_trans==1
gen dur_transitioned=relationship_duration if marr_trans==1
bysort unique_id partner_id (year_transitioned): replace year_transitioned = year_transitioned[1]
bysort unique_id partner_id (dur_transitioned): replace dur_transitioned = dur_transitioned[1]

bysort unique_id partner_id: egen years_observed=count(survey_yr)
bysort unique_id partner_id (years_observed): replace years_observed = years_observed[1]

bysort unique_id partner_id: egen years_married=count(survey_yr) if marital_status_updated==1
bysort unique_id partner_id (years_married): replace years_married = years_married[1]

bysort unique_id partner_id: egen years_cohab=count(survey_yr) if marital_status_updated==2
bysort unique_id partner_id (years_cohab): replace years_cohab = years_cohab[1]

sort unique_id partner_id survey_yr
browse unique_id partner_id survey_yr relationship_duration marital_status_updated years_observed years_married years_cohab ever_transition marr_trans rel_start_all  year_transitioned dur_transitioned

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
* before dropping, get descriptive comparison of cohabitors to married couples
********************************************************************************

tabstat female_earn_pct_t female_hours_pct_t wife_housework_pct_t, by(marital_status_updated) 
ttest female_earn_pct_t, by(marital_status_updated) 
ttest female_hours_pct_t, by(marital_status_updated) 
ttest wife_housework_pct_t, by(marital_status_updated) 

unique unique_id partner_id, by(marital_status_updated) 
unique unique_id partner_id if marital_status_updated==1
unique unique_id partner_id if marital_status_updated==2

// % ever transition
tab marital_status_updated ever_transition, row
tab ever_transition if transition_flag==1 | always_cohab==1

// some small descriptives
tabstat AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 couple_educ_gp home_owner children, by(marital_status_updated)

********************************************************************************
**# okay make analytical sample and recode duration relative to marital transition
********************************************************************************

keep if transition_flag==1 | always_cohab==1 // so keeping a "control" group - basically drops those always married

browse unique_id partner_id survey_yr transition_flag always_cohab rel_start_all year_transitioned relationship_duration marital_status_updated

gen treated=.
replace treated=1 if ever_transition==1
replace treated=0 if always_cohab==1

gen duration_cohab=.
replace duration_cohab = survey_yr - year_transitioned if transition_flag==1
replace duration_cohab = relationship_duration if always_cohab==1

browse unique_id partner_id survey_yr transition_flag always_cohab rel_start_yr year_transitioned relationship_duration duration_cohab marital_status_updated

tab duration_cohab, m

recode duration_cohab(-24/-11=-5)(-10/-7=-4)(-6/-5=-3)(-4/-3=-2)(-2/-1=-1)(0=0)(1/2=1)(3/4=2)(5/6=3)(7/8=4)(9/10=5)(11/12=6)(13/20=7)(21/30=8), gen(dur) // smoothing (bc the switch to every other year makes this wonky)

unique unique_id, by(treated)
unique unique_id partner_id, by(treated)
unique unique_id partner_id if treated==0
unique unique_id partner_id if treated==1

tab years_observed treated, m col
unique unique_id if treated==0, by(years_observed) // like if I want to use 4 years even, there are only 186 control uniques...oh duh, kim, you sum all of them 4+ DUH because this is constant
unique unique_id if treated==1, by(years_observed) // 190 treated at 4 years. so, is this actually even going to work on is this too small of a group

********************************************************************************
**# Some descriptive statistics
* # have NOT really revisited this and need to automate:
* # need to pull first PRE match to show differences in characteristics
* # then, post match to show balance
********************************************************************************
browse unique_id survey_yr year_transitioned rel_start_yr // some people could have two relationships? so uniques needs to be combo of id and relation year?! oh but the cohab / marriage are off, so actually do transition year?
unique unique_id
unique unique_id year_transitioned
unique unique_id rel_start_yr
unique unique_id if dur==0

unique unique_id partner_id, by(treated)

sum duration_cohab if dur < 0 // average cohab duration
sum duration_cohab if dur > 0 & dur !=. // average marital duration

tab treated couple_educ_gp, row // so, very unbalanced
tab treated educ_type, row // so, very unbalanced
tab treated educ_type if relationship_duration==0, row

tab raceth_head_fixed treated, col

tabstat AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 couple_educ_gp home_owner children, by(treated)

tab treated children, row
tab treated children if relationship_duration==0, row m 

tab had_birth
unique unique_id if had_birth==1 // use this for % experiencing a birth
unique unique_id if had_birth==1 & dur==0
tab had_birth if dur==0

tabstat female_earn_pct_t1 female_earn_pct_t female_hours_pct_t1 female_hours_pct_t wife_housework_pct_t1 wife_housework_pct_t, by(treated)
// interestingly, these actually don't really differ across groups. I'd expect to see a difference? with treated being more traditional?
tabstat female_earn_pct_t1 female_earn_pct_t female_hours_pct_t1 female_hours_pct_t wife_housework_pct_t1 wife_housework_pct_t if relationship_duration==0, by(treated)
tabstat female_earn_pct_t1 female_earn_pct_t female_hours_pct_t1 female_hours_pct_t wife_housework_pct_t1 wife_housework_pct_t if dur==5, by(treated) // okay, so yes, the differences appear at later durs

tab hh_earn_type_t1 treated, col
tab housework_bkt_t treated, col
tab earn_housework_t treated, col


********************************************************************************
**# DESCRIPTIVE ANALYSIS
********************************************************************************
/*/ descriptive

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
*/


********************************************************************************
********************************************************************************
********************************************************************************
** PROPENSITY SCORE MATCHING EXPLORATION
********************************************************************************
********************************************************************************
********************************************************************************
* First, do some sample things
// want to observe in relatively early year of relationship
bysort unique_id partner_id: egen min_dur = min(relationship_duration)
tab min_dur, m // 90% observed by second year. Should I drop if above that?

keep if min_dur <=5

// for treated, need to observe at least one year in each state
tab years_observed treated, m
tab years_married treated, m
tab years_cohab treated, m

browse unique_id partner_id survey_yr marital_status_updated treated relationship_duration year_transitioned years_observed years_married years_cohab
keep if treated==0 | (treated==1 & years_married>=1 & years_married!=. & years_cohab>=1 & years_cohab!=.)

// for control, do we want to restrict to min of 2 years? or is one sufficient? 
// (I am loosely following Kapelle and she says just one valid wealth wave is needed)

********************************************************************************
**# Preliminary PSM (this is very crude)
********************************************************************************
// prob want to use characteristics at relationship start, which is relationship_duration==0 for both

// if missing on ANY, cannot estimate. see how much of a problem this is. Particularly worried about off years. Does min dur work because it is definitely a year I observed? Or could it be during an off year so I should adjust?
// okay education wife has the most at around 10%. then education husband and housework have like 5%. Come back to this...
inspect educ_head_est educ_wife_est raceth_head_fixed raceth_wife_fixed sample_type AGE_HEAD_ AGE_WIFE_ ft_pt_t1_head ft_pt_t1_wife couple_earnings_t1 housework_head housework_wife home_owner NUM_CHILDREN_ rel_start_all if relationship_duration==min_dur

logit treated i.educ_head_est i.educ_wife_est i.raceth_head_fixed i.raceth_wife_fixed i.sample_type AGE_HEAD_ AGE_WIFE_ i.ft_pt_t1_head i.ft_pt_t1_wife couple_earnings_t1 housework_head housework_wife i.home_owner NUM_CHILDREN_ rel_start_all if relationship_duration==min_dur // PSM based on characteristics at start of cohab OR first observed. will decide if want to restrict min dur
predict psm if relationship_duration==min_dur

// alt estimator
// pscore treated educ_head_est educ_wife_est raceth_head_fixed raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ i.ft_pt_t1_head i.ft_pt_t1_wife couple_earnings_t1 home_owner NUM_CHILDREN_ rel_start_yr if relationship_duration==min_dur, pscore(psm_alt) logit 

bysort unique_id partner_id: egen pscore = max(psm) // here there is about 20% missing
sort unique_id partner_id survey_yr
browse unique_id partner_id relationship_duration psm pscore min_dur educ_head_est educ_wife_est raceth_head_fixed raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 home_owner NUM_CHILDREN_ rel_start_yr

// check for overlap
tabstat psm, by(treated)
sum psm if treated==0, det
sum psm if treated==1, det
tabstat pscore, by(treated)

set scheme cleanplots
twoway (histogram psm if treated==1, width(.02) color(pink%30)) (histogram psm if treated==0, width(.02) color(gs8%30)),  legend(order(1 "Treated" 2 "Control") rows(1) position(6)) xtitle("Propensity Score")
twoway (histogram pscore if treated==1, width(.02) color(blue%30)) (histogram pscore if treated==0, width(.02) color(red%30)),  legend(order(1 "Treated" 2 "Control") rows(1) position(6))

browse treated psm educ_head_est educ_wife_est raceth_head_fixed raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 home_owner NUM_CHILDREN_ rel_start_yr if relationship_duration==0

// for now, estimate female_earn_pct_t female_hours_pct_t year_transitioned?? BUT how do I get the control group? do I need to do this by duration?
tabstat female_hours_pct_t, by(treated)

// teffects psmatch (re78) (treat age agesq agecube educ edusq marr nodegree black hisp re74 re75 u74 u75 interaction1, logit), atet gen(pstub_cps) nn(5) // atet = aTT caliper(0.1)

********************************************************************************
**# Option 1a: Use PSMATCH2 to match AT BASELINE
********************************************************************************
// I am just playing around. I really want to match at baseline, but want to see how this works / what sort of outputs I get
// YES this is what I want because I get the IDs of the matches, so I can then use their transition duration

/*
//
Just one match/
psmatch2 treated, pscore(psm) // psm is just at min dur; pscore is where I filled it in for all durs based on that. SO, using psm IS matching at baseline (I think)
sort _id
browse unique_id partner_id treated psm pscore rel_start_yr relationship_duration min_dur year_transitioned duration_cohab dur transition_flag relationship_duration _pscore _treated _support _weight _id _n1 _nn _pdif _Unique if _treated!=.

sort unique_id partner_id relationship_duration
browse unique_id partner_id survey_yr marital_status_updated treated psm pscore rel_start_yr relationship_duration min_dur year_transitioned duration_cohab dur transition_flag relationship_duration _pscore _treated _support _weight _id _n1 _nn _pdif _Unique ///
if inlist(unique_id, 1762033, 5858039, 5812009, 2252031, 2067173, 2067173, 2067173, 808030, 5853006, 5035002, 5940031, 351035, 931031, 2053030, 1302035, 2509030)
// if inlist(_id, 32, 154, 1152, 1762, 2224, 2224, 2224, 2291, 2303, 2315, 2572, 2926, 3387, 3388, 3389, 3570) // they get a different ID for each duration, so if I want to view all rows, need to use unique

// In retrospect - if min dur is on an off survey year, aka a lot of missing, should I update? Or did I only use full survey years? I want to reduce the amount of missing in the PSM
// okay duh only the treated get matches
// syntax from help file:
// sort _id
// g x_of_match = x[_n1]
*/

// nearest neighbor matching
psmatch2 treated, pscore(psm) neighbor(5) 
sort _id
browse unique_id treated psm pscore rel_start_yr min_dur marr_trans ever_transition year_transitioned transition_flag relationship_duration _pscore _treated _support _weight _id _n1 _n2 _n3 _n4 _n5 _nn _pdif _Unique
browse unique_id partner_id treated psm pscore rel_start_yr min_dur marr_trans ever_transition year_transitioned transition_flag relationship_duration _pscore _treated _support _weight _id _n1 _n2 _n3 _n4 _n5 _nn _pdif _Unique if _treated!=.

tab _weight treated
tab treated _nn if relationship_duration == min_dur, m

sort unique_id partner_id relationship_duration
browse unique_id partner_id survey_yr marital_status_updated treated psm pscore rel_start_yr relationship_duration min_dur year_transitioned duration_cohab dur transition_flag relationship_duration _pscore _treated _support _weight _id _n1 _n2 _n3 _n4 _n5 _nn _pdif _Unique ///
if inlist(unique_id, 5031, 498002, 1538174, 1644031, 1776004, 1807030, 2890030, 3368007, 5002005, 5171006, 6062003, 6750032)

// let's create a lookup file of ids for the controls
preserve

keep _id unique_id partner_id rel_start_all min_dur years_observed
rename _id _n1
gen _n2 = _n1
gen _n3 = _n1
gen _n4 = _n1
gen _n5 = _n1
rename unique_id matched_unique_id
rename partner_id matched_partner_id
rename rel_start_all matched_rel_start
rename min_dur matched_min_dur
rename years_observed matched_years_obs

// save "$temp/psm_id_lookup.dta", replace // file for just 1 control match
save "$temp/psm_id_lookup_nn5.dta", replace

restore

// now merge on the control info
*Try to make this a loop through the 5 (_n1 to _n5)
forvalues n=1/5{
	merge m:1 _n`n' using "$temp/psm_id_lookup_nn5.dta"
	drop if _merge==2
	drop _merge
	
	rename (matched_unique_id matched_partner_id matched_rel_start matched_min_dur matched_years_obs) ///
	(matched_unique_id`n' matched_partner_id`n' matched_rel_start`n' matched_min_dur`n' matched_years_obs`n')

	bysort unique_id partner_id (matched_unique_id`n'): 	replace matched_unique_id`n' = matched_unique_id`n'[1]
	bysort unique_id partner_id (matched_partner_id`n'): 	replace matched_partner_id`n' = matched_partner_id`n'[1]
	bysort unique_id partner_id (matched_rel_start`n'): 	replace matched_rel_start`n' = matched_rel_start`n'[1]
	bysort unique_id partner_id (matched_min_dur`n'): 		replace matched_min_dur`n' = matched_min_dur`n'[1]
	bysort unique_id partner_id (matched_years_obs`n'): 	replace matched_years_obs`n' = matched_years_obs`n'[1]
}

sort unique_id partner_id survey_yr
browse unique_id partner_id survey_yr treated rel_start_all relationship_duration  min_dur years_observed _id _n* matched_unique_id* matched_partner_id* matched_rel_start* matched_min_dur* matched_years_obs*

// going to attempt to rescale all durations so start at min dur and just increment 1 - to make control and treated match
sort unique_id partner_id relationship_duration

gen relationship_counter=.
replace relationship_counter = 0 if relationship_duration == min_dur
replace relationship_counter = relationship_counter[_n-1] + 1 if relationship_counter==. & unique_id == unique_id[_n-1] & partner_id == partner_id[_n-1]
tab relationship_counter, m

gen transition_counter=.
replace transition_counter = relationship_counter if relationship_duration == dur_transitioned
bysort unique_id partner_id (transition_counter): replace transition_counter = transition_counter[1]

browse unique_id partner_id survey_yr rel_start_all relationship_duration relationship_counter transition_counter year_transitioned dur_transitioned

save "$temp/PSID_psm_dataset_nn5.dta", replace // save here so I can start to create a lookup file
// save "$temp/PSID_psm_dataset.dta", replace // file for just 1 control match

**************************************
* Now create treated only version
**************************************
keep if treated==1

//do they all have matches?
inspect matched_unique_id* // okay no lolz
inspect pscore // okay, it's people that don't have propensity score, probably bc of missing data

drop if matched_unique_id1==.

browse unique_id partner_id survey_yr rel_start_all relationship_duration  min_dur years_observed _id _n* _nn matched_unique_id* matched_partner_id* matched_rel_start* matched_min_dur* matched_years_obs*

save "$temp/PSID_psm_treated.dta", replace  // will use this as base, then add control info, then resave

**************************************
* Now clean up full file for matching
**************************************
use "$temp/PSID_psm_dataset_nn5.dta", clear

keep unique_id partner_id relationship_counter relationship_duration female_earn_pct_t female_hours_pct_t wife_housework_pct_t

forvalues n=1/5{
	gen matched_unique_id`n' = unique_id
	gen matched_partner_id`n' = partner_id
	gen matched_true_dur`n' = relationship_duration
	gen matched_earn_pct`n' = female_earn_pct_t
	gen matched_hours_pct`n' = female_hours_pct_t
	gen matched_hw_pct`n' = wife_housework_pct_t
}

drop unique_id partner_id relationship_duration female_earn_pct_t female_hours_pct_t wife_housework_pct_t

save "$temp/PSID_psm_tomatch.dta", replace

**************************************
* Merge partner info
**************************************
use "$temp/PSID_psm_treated.dta", clear

forvalues n=1/5{
	merge m:1 matched_unique_id`n' matched_partner_id`n' relationship_counter using "$temp/PSID_psm_tomatch.dta", ///
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


sort unique_id partner_id relationship_counter

browse unique_id partner_id survey_yr rel_start_all relationship_counter relationship_duration has_control* matched_unique_id* matched_partner_id* matched_rel_start* matched_years_obs* matched_true_dur* female_earn_pct_t female_hours_pct_t wife_housework_pct_t matched_earn_pct* matched_hours_pct* matched_hw_pct*

save "$created_data/PSID_psm_matched.dta", replace


**************************************
* Preliminary analysis
**************************************
tab relationship_counter has_control1, m
drop if relationship_counter >=10 // very few records and matches. might even need to remove more than that

browse unique_id partner_id survey_yr rel_start_all relationship_counter transition_counter matched_unique_id* matched_partner_id* matched_rel_start* matched_years_obs* matched_true_dur* female_earn_pct_t* female_hours_pct_t* wife_housework_pct_t* matched_earn_pct* matched_hours_pct* matched_hw_pct*

gen duration_centered = relationship_counter - transition_counter

preserve

collapse (median) female_earn_pct_t female_hours_pct_t wife_housework_pct_t matched_earn_pct* matched_hours_pct* matched_hw_pct*, by(duration_centered)

twoway (line female_earn_pct_t duration_centered if duration_centered>=-5) (line matched_earn_pct* duration_centered if duration_centered>=-5), legend(order(1 "Treated" 2 "Control1" 3 "Control2" 4 "Control3" 5 "Control4" 6 "Control5")) xtitle(`"Duration from Marital Transition"') // rows(1) position(6) - old legend characteristics

twoway (line female_hours_pct_t duration_centered if duration_centered>=-5) (line matched_hours_pct* duration_centered if duration_centered>=-5), legend(order(1 "Treated" 2 "Control1" 3 "Control2" 4 "Control3" 5 "Control4" 6 "Control5")) xtitle(`"Duration from Marital Transition"')

twoway (line wife_housework_pct_t duration_centered if duration_centered>=-5) (line matched_hw_pct* duration_centered if duration_centered>=-5), legend(order(1 "Treated" 2 "Control1" 3 "Control2" 4 "Control3" 5 "Control4" 6 "Control5")) xtitle(`"Duration from Marital Transition"')

restore


**************************************
**# * Actual analysis with matched data
**************************************
* Need to get these stacked instead of next to each other, so can create a column for treatment and interact, like I do below...do I have to cut, rename, and append?
* Yeah, definitely will need to add other characteristics so can control? Or is that all taken care of in propensity score anyway?
* Think I do also need to do more controls and include several (at least 5?) per treated so the sample sizes are larger (since the controls generally have shorter durations)

browse unique_id partner_id survey_yr rel_start_all relationship_counter transition_counter duration_centered matched_unique_id* matched_partner_id* matched_rel_start* matched_years_obs* matched_true_dur* female_earn_pct_t female_hours_pct_t wife_housework_pct_t matched_earn_pct* matched_hours_pct* matched_hw_pct*

// get treated that I will then append control on to

preserve

keep unique_id partner_id survey_yr rel_start_all treated relationship_counter transition_counter duration_centered female_earn_pct_t female_hours_pct_t wife_housework_pct_t

// create variables that I'll add to the control ones
gen control_n=.
gen treated_unique=.
gen treated_partner=.

save "$temp/PSID_psm_base.dta", replace

restore

// get control

forvalues n=1/5{
	
	preserve

	keep unique_id partner_id matched_unique_id`n' matched_partner_id`n' survey_yr matched_rel_start`n' relationship_counter transition_counter duration_centered matched_earn_pct`n' matched_hours_pct`n' matched_hw_pct`n'

	gen treated = 0
	gen control_n = `n'
	rename (unique_id partner_id) (treated_unique treated_partner)
	rename (matched_unique_id`n' matched_partner_id`n' matched_rel_start`n' matched_earn_pct`n' matched_hours_pct`n' matched_hw_pct`n') ///
	(unique_id partner_id rel_start_all female_earn_pct_t female_hours_pct_t wife_housework_pct_t)

	save "$temp/PSID_psm_control_nn`n'.dta", replace

	restore

}

// now append
use "$temp/PSID_psm_base.dta", clear
forvalues n=1/5{
	append using "$temp/PSID_psm_control_nn`n'.dta"
}

save "$created_data/PSID_psm_matched_long.dta", replace

// 

keep if duration_centered>=-5 & duration_centered <=5
gen duration_pos = duration_centered + 6 // can't be negative

// unadjusted regression
regress female_earn_pct_t treated##i.duration_pos
margins duration_pos#treated
marginsplot, xlabel( 1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "Transition" 7 "1" 8 "2" 9 "3" 10 "4" 11 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

regress female_hours_pct_t treated##i.duration_pos
margins duration_pos#treated
marginsplot, xlabel( 1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "Transition" 7 "1" 8 "2" 9 "3" 10 "4" 11 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Paid Work Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

regress wife_housework_pct_t treated##i.duration_pos
margins duration_pos#treated
marginsplot, xlabel( 1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "Transition" 7 "1" 8 "2" 9 "3" 10 "4" 11 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Housework Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

// is this the same or different as growth curve models? This is following Kapelle 2022. I think this is better. They are directionally similar; this better controls for time dependence within individuals.
egen couple_id = group(unique_id partner_id)

mixed female_earn_pct_t treated##i.duration_pos || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)11) treated=(0 1)) // so is this how I graph the curve
marginsplot, xlabel( 1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "Transition" 7 "1" 8 "2" 9 "3" 10 "4" 11 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

mixed female_hours_pct_t treated##i.duration_pos || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)11) treated=(0 1)) // so is this how I graph the curve
marginsplot, xlabel( 1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "Transition" 7 "1" 8 "2" 9 "3" 10 "4" 11 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Paid Work Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

mixed wife_housework_pct_t treated##i.duration_pos || couple_id: duration_pos // constant == baseline. coefficient = change from baseline
margins, at(duration_pos=(1(1)11) treated=(0 1)) // so is this how I graph the curve
marginsplot, xlabel( 1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "Transition" 7 "1" 8 "2" 9 "3" 10 "4" 11 "5") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Housework Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")


 /*
********************************************************************************
**# Option 1b: keep groups of same length and recenter control, then weight?!
* Using IPW based on PSM above?
********************************************************************************
// okay, so need to start over with original file, so figure out how to make this happen...

gen keep_flag=0
replace keep_flag = 1 if treated==1 & dur >=-2 & dur<=4 & years_observed>=4
replace keep_flag = 1 if treated==0 & dur>=0 & dur<=6 & years_observed>=4

unique unique_id if keep_flag==1, by(treated)
tab dur treated if keep_flag==1

gen recenter_dur = dur if treated==1
replace recenter_dur = dur - 2 if treated==0

tab recenter_dur treated if keep_flag==1, m

gen recenter_dur_pos = recenter_dur+3

gen ipw=.
replace ipw=1/pscore if treated==1
replace ipw=1/(1-pscore) if treated==0

browse unique_id partner_id treated recenter_dur pscore ipw psm female_earn_pct_t female_hours_pct_t wife_housework_pct_t if keep_flag==1

tabstat pscore ipw, by(treated)

keep if keep_flag==1

gen earn_weight_treat = 0
replace earn_weight_treat = female_earn_pct_t*ipw if treated==1
gen earn_weight_control = 0
replace earn_weight_control = female_earn_pct_t*ipw if treated==0

tabstat earn_weight_treat earn_weight_control if keep_flag==1, by(recenter_dur) // I don't think any of this is right lol

set scheme cleanplots

// unadjusted
regress female_earn_pct_t treated##i.recenter_dur_pos 
margins recenter_dur_pos#treated
marginsplot, xlabel(1 "-2" 2 "-1" 3 "Transition" 4 "1" 5 "2" 6 "3" 7 "4") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

regress female_hours_pct_t treated##i.recenter_dur_pos 
margins recenter_dur_pos#treated
marginsplot, xlabel(1 "-2" 2 "-1" 3 "Transition" 4 "1" 5 "2" 6 "3" 7 "4") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Paid Work Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

regress wife_housework_pct_t treated##i.recenter_dur_pos 
margins recenter_dur_pos#treated
marginsplot, xlabel(1 "-2" 2 "-1" 3 "Transition" 4 "1" 5 "2" 6 "3" 7 "4") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Housework Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")

// adjusted
local controls "i.educ_head_est i.educ_wife_est i.raceth_head_fixed i.raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 i.home_owner NUM_CHILDREN_ rel_start_yr"

* (by interacting with duration, is this essentially the dummy impact function discussed by Ludwig and Bruderl? in my head, yes, so I actually did something right... but they don't use treatment / control; they just use within couple fixed-effects, so it's like my previous approach, but...different...)
regress female_earn_pct_t treated##i.recenter_dur_pos `controls' [pweight=ipw] 
margins recenter_dur_pos#treated // does this work?? like is it this simple?
marginsplot, xlabel(1 "-2" 2 "-1" 3 "Transition" 4 "1" 5 "2" 6 "3" 7 "4") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Earnings") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

regress female_hours_pct_t treated##i.recenter_dur_pos  `controls' [pweight=ipw]
margins recenter_dur_pos#treated // does this work??
marginsplot, xlabel(1 "-2" 2 "-1" 3 "Transition" 4 "1" 5 "2" 6 "3" 7 "4") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Couple Paid Work Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("") plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

regress wife_housework_pct_t treated##i.recenter_dur_pos `controls'  [pweight=ipw]
margins recenter_dur_pos#treated // does this work??
marginsplot, xlabel(1 "-2" 2 "-1" 3 "Transition" 4 "1" 5 "2" 6 "3" 7 "4") xtitle(`"Duration from Marital Transition"') ytitle("Women's % of Total Housework Hours") legend(rows(1) position(bottom) order(1 "Cohab" 2 "Transitioned")) title("")  plot1opts(lcolor(gs8) mcolor(gs8)) ci1opts(lcolor(gs8)) plot2opts(lcolor(pink) mcolor(pink)) ci2opts(lcolor(pink))

// teffects instead
teffects ipwra (wife_housework_pct_t i.recenter_dur_pos) ///
 (treated i.educ_head_est i.educ_wife_est i.raceth_head_fixed i.raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 i.home_owner NUM_CHILDREN_ rel_start_yr, probit), atet osample(overlap)
 
tebalance summarize

** descriptive (not weighted)
preserve

collapse (median) female_earn_pct_t female_hours_pct_t wife_housework_pct_t if keep_flag==1, by(recenter_dur treated)

// paid labor: compare the two
twoway (line female_earn_pct_t recenter_dur if treated==0) (line female_earn_pct_t recenter_dur if treated==1), legend(order(1 "Cohab" 2 "Transitioned") rows(1) position(6)) xtitle(`"recenter_duration from Marital Transition"')

// paid labor: compare the two
twoway (line female_hours_pct_t recenter_dur if treated==0) (line female_hours_pct_t recenter_dur if treated==1), legend(order(1 "Cohab" 2 "Transitioned") rows(1) position(6)) xtitle(`"recenter_duration from Marital Transition"')

// unpaid labor: compare the two
twoway (line wife_housework_pct_t recenter_dur if treated==0) (line wife_housework_pct_t recenter_dur if treated==1), legend(order(1 "Cohab" 2 "Transitioned") rows(1) position(6)) xtitle(`"recenter_duration from Marital Transition"')

// one way to plot it all, don't like this
twoway (line female_earn_pct_t recenter_dur) (line wife_housework_pct_t recenter_dur, yaxis(2)), legend(order(1 "Paid Labor" 2 "Unpaid Labor") rows(1) position(6)) xtitle(`"Duration from Marital Transition"') ytitle(`"Paid Labor"') ylabel(, valuelabel) ytitle(`"Unpaid Labor"', axis(2)) by(treated)

restore

********************************************************************************
**# Option 1c: Use PSM as time-varying covariate (see Kupzyk and Beal)
* want to see how different this is from using PSM as a weight instead (as above)
********************************************************************************
*/
