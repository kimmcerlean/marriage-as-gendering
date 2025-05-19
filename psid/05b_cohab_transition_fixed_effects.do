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
unique unique_id if rel_start_all >= 1990, by(marital_status_updated) // nearly half of sample goes away.
unique unique_id if rel_start_all >= 1985, by(marital_status_updated)
keep if rel_start_all >= 1985 // cohabitation not even reliably measured until mid-1980s

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
bysort unique_id partner_id: egen min_dur = min(relationship_duration)

keep if transition_flag==1 | always_cohab==1 // so keeping a "control" group - basically drops those always married

browse unique_id partner_id survey_yr transition_flag always_cohab rel_start_all year_transitioned relationship_duration marital_status_updated

gen treated=.
replace treated=1 if ever_transition==1
replace treated=0 if always_cohab==1

gen duration_cohab=.
replace duration_cohab = survey_yr - year_transitioned if transition_flag==1
replace duration_cohab = relationship_duration if always_cohab==1

browse unique_id partner_id survey_yr transition_flag always_cohab rel_start_all year_transitioned relationship_duration duration_cohab marital_status_updated

tab duration_cohab, m

recode duration_cohab(-24/-11=-5)(-10/-7=-4)(-6/-5=-3)(-4/-3=-2)(-2/-1=-1)(0=0)(1/2=1)(3/4=2)(5/6=3)(7/8=4)(9/10=5)(11/12=6)(13/20=7)(21/30=8), gen(dur) // smoothing (bc the switch to every other year makes this wonky)

// alernative - recode duration so in increments of 1 again (like I did before? so survey waves?)
sort unique_id partner_id survey_yr
gen relationship_counter=.
replace relationship_counter = 0 if relationship_duration == min_dur
replace relationship_counter = relationship_counter[_n-1] + 1 if relationship_counter==. & unique_id == unique_id[_n-1] & partner_id == partner_id[_n-1]
tab relationship_counter, m

gen transition_counter=.
replace transition_counter = relationship_counter if relationship_duration == dur_transitioned
bysort unique_id partner_id (transition_counter): replace transition_counter = transition_counter[1]
replace transition_counter = 0 if treated==0

gen duration_centered = relationship_counter - transition_counter
tab duration_centered treated, m

browse  unique_id partner_id survey_yr rel_start_all min_dur relationship_duration relationship_counter dur duration_centered duration_cohab  year_transitioned transition_counter dur_transitioned

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
********************************************************************************
********************************************************************************
**# FIXED EFFECTS EXPLORATION
********************************************************************************
********************************************************************************
********************************************************************************
// xtset the data here since I will need this for all of below
drop if couple_id==.

egen tag = tag(couple_id survey_yr)
egen ndistinct = total(tag), by(couple_id) // eventually drop those with less than 2-3 of data (need 3+ for FEIS and 2+ for FE)

xtset couple_id survey_yr

// restrict to just treated? ugh, reading the Ludwig and Bruderl / Schechtl / Zhou and Khan, I am not sure.
// keep if treated==1
keep if duration_center >=-5 & duration_centered<=8
gen duration_pos = duration_center+6

// quick data exploration
tabstat female_earn_pct_t female_hours_pct_t wife_housework_pct_t, by(duration_centered) 
 
preserve

collapse (median) female_earn_pct_t female_hours_pct_t wife_housework_pct_t, by(duration_centered)

twoway (line female_earn_pct duration_centered if duration_centered>=-5 & duration_centered <=8) ///
(line female_hours_pct_t duration_centered if duration_centered>=-5 & duration_centered <=8) ///
(line wife_housework_pct_t duration_centered if duration_centered>=-5 & duration_centered <=8), ///
legend(order(1 "Earnings" 2 "Work Hours" 3 "Housework Hours") rows(1) position(6)) xscale(range(-5 8)) xlabel(-5(1)8)  xline(0)

restore

// preliminary models
* so first need a var that is pre v. post treated. can I just use marital status?
tab marital_status_updated treated, m
tab year_transitioned treated, m
tab marital_status_updated if treated==1 & survey_yr >= year_transitioned

sort unique_id partner_id survey_yr
browse unique_id partner_id survey_yr marital_status_updated treated marr_trans year_transitioned // so treated transition_flag and ever_transition are the same thing at this point

gen married = 0
replace married = 1 if marital_status_updated==1

tabstat female_earn_pct_t female_hours_pct_t wife_housework_pct_t, by(married) // without controls, this is what I'd be estimating? yes
regress female_earn_pct_t i.married
regress female_hours_pct_t i.married
regress wife_housework_pct_t i.married

ttest female_earn_pct_t, by(married) // also same as regression
ttest female_hours_pct_t, by(married) // also same as regression
ttest wife_housework_pct_t, by(married) // also same as regression

// does this match the descriptive over time?
regress female_earn_pct_t i.duration_pos
margins duration_pos
marginsplot

regress female_hours_pct_t i.duration_pos
margins duration_pos
marginsplot

regress wife_housework_pct_t i.duration_pos
margins duration_pos
marginsplot

// now, start to add fixed effects
regress female_earn_pct_t i.married

* First, validate these are the same (is this essentially estimating a first difference?)
regress female_earn_pct_t i.married i.couple_id // -.0054635
margins married

xtreg female_earn_pct_t i.married, fe // this exactly matches above: -.0054635 (without survey yr); -.0054635 (with survey yr - okay so is same
margins married

xtreg female_hours_pct_t i.married, fe
margins married

xtreg wife_housework_pct_t i.married, fe
margins married

* Do I want to add this with duration instead?
regress female_earn_pct_t i.duration_pos if treated==1
margins duration_pos
marginsplot

xtreg female_earn_pct_t i.duration_pos  if treated==1,  fe // wait, these are SO different
margins duration_pos
marginsplot

xtreg female_earn_pct_t i.duration_pos##i.married, fe
margins duration_pos#married
marginsplot

xtreg female_earn_pct_t i.duration_pos##i.treated, fe // this doesn't work because non-treated don't have observations pre duration 5
margins duration_pos#treated
marginsplot

xtreg female_hours_pct_t i.duration_pos, fe
margins duration_pos
marginsplot

xtreg female_hours_pct_t i.duration_pos##i.married, fe
margins duration_pos#married
marginsplot

xtreg wife_housework_pct_t i.duration_pos, fe
margins duration_pos
marginsplot

xtreg wife_housework_pct_t i.duration_pos##i.married, fe
margins duration_pos#married
marginsplot
margins duration_pos

* Do I also need to add controls?
local controls "i.educ_head_est i.educ_wife_est i.raceth_head_fixed i.raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 i.home_owner NUM_CHILDREN_ rel_start_yr"

xtreg female_earn_pct_t i.married, fe
margins married

xtreg female_earn_pct_t i.married if treated==1, fe // confused on how these are the same. Is it because married isn't time varying in the control group so it ignores?
// so how is control group used then??
margins married

xtreg female_earn_pct_t i.married `controls', fe
margins married

xtreg female_earn_pct_t i.married `controls' i.relationship_duration, fe
margins married

local controls "i.educ_head_est i.educ_wife_est i.raceth_head_fixed i.raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 i.home_owner NUM_CHILDREN_ rel_start_yr"

xtreg female_hours_pct_t i.married, fe
margins married

xtreg female_hours_pct_t i.married `controls', fe
margins married

xtreg female_hours_pct_t i.married `controls' i.relationship_duration, fe
margins married
margins relationship_duration
marginsplot

local controls "i.educ_head_est i.educ_wife_est i.raceth_head_fixed i.raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 i.home_owner NUM_CHILDREN_ rel_start_yr"

xtreg wife_housework_pct_t i.married, fe
margins married

xtreg wife_housework_pct_t i.married `controls', fe
margins married

xtreg wife_housework_pct_t i.married `controls' i.relationship_duration, fe
margins married

************************************
* What happens when I try xtfeis
************************************
// see also Happiness5 files from Bruderl / Ludwig lecture for more info here

tab duration_pos,gen(dur)
xtfeis female_earn_pct_t married ib6.duration_pos, slope(AGE_WIFE_) cluster(couple_id)
est store feis1

xtfeis female_earn_pct_t married dur1 dur2 dur3 dur4 dur5 dur7 dur8 dur9 dur10 dur11 dur12 dur13 dur14, slope(AGE_WIFE_) cluster(couple_id)
est store feis2

coefplot feis2, keep(married dur*)

xtfeis female_earn_pct_t married dur1 dur2 dur3 dur4 dur5 dur7 dur8 dur9 dur10 dur11 dur12 dur13 dur14 if treated==1, slope(AGE_WIFE_) cluster(couple_id)
est store feis3

coefplot feis3, keep(married dur*)

* Confused because it seems like Ludwig puts all of the control at 0
gen dur_alt = duration_pos
replace dur_alt = 6 if treated==0 // should it be 6 actually, because that is real 0?
tab dur_alt, gen(dur_alt)

xtfeis female_earn_pct_t married dur_alt1 dur_alt2 dur_alt3 dur_alt4 dur_alt5 dur_alt7 dur_alt8 dur_alt9 dur_alt10 dur_alt11 dur_alt12 dur_alt13 dur_alt14, slope(AGE_WIFE_) cluster(couple_id) // this appears to match the above where I restricted just to treated...
est store feis4

coefplot feis4, keep(married dur_alt*)

* but then they don't have the treatment variable in model?

xtfeis female_earn_pct_t dur_alt1 dur_alt2 dur_alt3 dur_alt4 dur_alt6 dur_alt7 dur_alt8 dur_alt9 dur_alt10 dur_alt11 dur_alt12 dur_alt13 dur_alt14, slope(AGE_WIFE_) cluster(couple_id) // this appears to match the above where I restricted just to treated...
est store feis5
coefplot feis5, keep(dur_alt*)

// I am so confused
xtreg female_earn_pct_t ib5.dur_alt  if treated==1,  fe
margins dur_alt
marginsplot

xtreg female_earn_pct_t  dur_alt1 dur_alt2 dur_alt3 dur_alt4 dur_alt6 dur_alt7 dur_alt8 dur_alt9 dur_alt10 dur_alt11 dur_alt12 dur_alt13 dur_alt14 $controls, fe
est store fe5
margins dur_alt
marginsplot

coefplot feis5 fe5, keep(dur_alt*)

************************************
* Working through impact functions
************************************
// okay, looking at the Happiness 3 replicate code, I see now. Duration only starts ONCE married
gen duration_married = 0
replace duration_married = duration_centered + 1 if married==1

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
tab duration_centered treated
tab duration_centered married

gen dummy_duration = .
replace dummy_duration = duration_centered if treated==1
recode dummy_duration (min/-1=-1)
replace dummy_duration = dummy_duration+1
replace dummy_duration = 0 if treated==0

tab dummy_duration treated
tab dummy_duration married

xtreg female_earn_pct_t i.dummy_duration $controls, fe 
estimates store d1

xtfeis female_earn_pct_t i.dummy_duration $controls, slope(relationship_duration) cluster(couple_id)
est store d2

coefplot 	(d1, lcolor("red*1.2") mcolor("red*1.2") ciopts(color("red*1.2"))) 				///
			(d2, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue"))),				///
			keep(*.dummy_duration) vertical yline(0, lpattern(solid)) recast(line) 			///
			coeflabels(1.dummy_duration="0" 2.dummy_duration="1" 3.dummy_duration="2" 		///
			4.dummy_duration="3" 5.dummy_duration="4" 6.dummy_duration="5"      			///
		    7.dummy_duration="6" 8.dummy_duration="7" 9.dummy_duration="8")        			///         
			ylabel(-.3(.1).5, grid angle(0) labsize(medium) format(%3.1f))        			///
			xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        			///
			ytitle("Change in Female Earnings %", size(medlarge)) 		  
		  
coefplot 	(d1, lcolor("red*1.2") mcolor("red*1.2") ciopts(recast(rline) lpattern(dash) lcolor("red*1.2"))) 				///
			(d2, lcolor("eltblue") mcolor("eltblue") ciopts(recast(rline) lpattern(dash) lcolor("eltblue"))),				///
			keep(*.dummy_duration) vertical yline(0, lpattern(solid)) recast(line) 			///
			coeflabels(1.dummy_duration="0" 2.dummy_duration="1" 3.dummy_duration="2" 		///
			4.dummy_duration="3" 5.dummy_duration="4" 6.dummy_duration="5"      			///
		    7.dummy_duration="6" 8.dummy_duration="7" 9.dummy_duration="8")        			///         
			ylabel(-.3(.1).5, grid angle(0) labsize(medium) format(%3.1f))        			///
			xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        			///
			ytitle("Change in Female Earnings %", size(medlarge)) 		  
		  
		  
xtreg female_hours_pct_t i.dummy_duration $controls, fe 
est store d3
xtfeis female_hours_pct_t i.dummy_duration $controls, slope(relationship_duration) cluster(couple_id)
est store d4

coefplot 	(d3, lcolor("red*1.2") mcolor("red*1.2") ciopts(color("red*1.2"))) 				///
			(d4, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue"))),				///
			keep(*.dummy_duration) vertical yline(0, lpattern(solid)) recast(line) 			///
			coeflabels(1.dummy_duration="0" 2.dummy_duration="1" 3.dummy_duration="2" 		///
			4.dummy_duration="3" 5.dummy_duration="4" 6.dummy_duration="5"      			///
		    7.dummy_duration="6" 8.dummy_duration="7" 9.dummy_duration="8")        			///         
			ylabel(-.3(.1).5, grid angle(0) labsize(medium) format(%3.1f))        			///
			xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        			///
			ytitle("Change in Female Hours %", size(medlarge)) 		  
		  

		  
xtreg wife_housework_pct_t i.dummy_duration $controls, fe 
est store d5
xtfeis wife_housework_pct_t i.dummy_duration $controls, slope(relationship_duration) cluster(couple_id)
est store d6

coefplot 	(d5, lcolor("red*1.2") mcolor("red*1.2") ciopts(color("red*1.2"))) 				///
			(d6, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue"))),				///
			keep(*.dummy_duration) vertical yline(0, lpattern(solid)) recast(line) 			///
			coeflabels(1.dummy_duration="0" 2.dummy_duration="1" 3.dummy_duration="2" 		///
			4.dummy_duration="3" 5.dummy_duration="4" 6.dummy_duration="5"      			///
		    7.dummy_duration="6" 8.dummy_duration="7" 9.dummy_duration="8")        			///         
			ylabel(-.3(.1).5, grid angle(0) labsize(medium) format(%3.1f))        			///
			xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        			///
			ytitle("Change in Female HW %", size(medlarge)) 		  
		  
// okay but then they go into other anticipation effects up to year -6, so this is where I could just use my duration positive?
// I just need to figure out where to put control - at time -5 or time 0 (they should be in reference category they say)
tab duration_pos treated
gen duration_pos2 = duration_pos
replace duration_pos2 = 1 if treated==0
tab duration_pos2 treated

xtreg female_earn_pct_t i.duration_pos2 $controls, fe 

coefplot, keep(*.duration_pos2) vertical yline(0, lpattern(solid)) recast(line) lwidth(thick) lcolor(blue)  ///
          xline(6, lpatter(solid)) ciopts(recast(rline) lpattern(dash) lwidth(medthick) lcolor(green))   	/// 
		  ylabel(-.3(.1).5, grid angle(0) labsize(medium) format(%3.1f))        					///
		  coeflabels(1.duration_pos2="-5" 2.duration_pos2="-4" 3.duration_pos2="-3" 				///
		  4.duration_pos2="-2" 5.duration_pos2="-1" 6.duration_pos2="0" 7.duration_pos2="1" 		///
		  8.duration_pos2="2" 9.duration_pos2="3" 10.duration_pos2="4" 11.duration_pos2="5"			///
		  12.duration_pos2="6" 13.duration_pos2="7" 14.duration_pos2="8") 	  						///  
          xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
          ytitle("Change in Female Earnings %", size(medlarge)) 
		  

xtreg female_earn_pct_t ib5.duration_pos2 $controls, fe 
		  
************************************
* Comparisons
************************************
// so I feel like this is what matches Zhou and Kan. How do I add the TIME element though??
// and decide if I keep never treated or not (Bruderl / Schechtl def do; I feel like Z&K do as well?)

// Earnings
global controls "i.educ_head_est i.educ_wife_est i.raceth_head_fixed i.raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 i.home_owner NUM_CHILDREN_ rel_start_yr"

regress female_earn_pct_t married $controls if treated==1
est store e0

xtreg female_earn_pct_t married $controls if treated==1, fe
est store e1

xtreg female_earn_pct_t married $controls, fe
est store e1a

xtfeis female_earn_pct_t married $controls if treated==1, slope(AGE_WIFE_) cluster(couple_id)
est store e2

xtfeis female_earn_pct_t married $controls if treated==1, slope(relationship_duration) cluster(couple_id) // should it actually be relationship duration? doesn't seem to matter
est store e3

xtfeis female_earn_pct_t married $controls, slope(relationship_duration) cluster(couple_id) // should it actually be relationship duration? doesn't seem to matter
est store e3a

xtreg female_earn_pct_t married if treated==1, fe
est store e4

xtreg female_earn_pct_t married, fe
est store e4a

coefplot (e0, label("Base")) (e1,label("FE")) (e1a,label("FE w/ control")) (e2,label("FEIS, age")) (e3,label("FEIS, dur")) ///
(e3,label("FEIS, dur w/ control")) (e4,label("FE Treat + No Controls")) (e4a,label("FE Both + No Controls")), keep(married)

estimates table e0 e1 e1a e3 e3a e4 e4a, b(%7.3f) se(%7.4f) stfmt(%6.0f) stats(N N_clust) ///
                keep(married)
				
				// so there is a section in the Bruderl lecture and code (see Happiness3 Regression and pdf page 92) on the importance of controls and
				// a. suggests taht the way I am implementing is, in fact, correct - literally just the diff of treated==1 v. no restriction on treated
				// and b. that seems to validate that i should include the control, but it only matters when it comes to time-varying variables...
				// so, I am not confused (re: implementation)
				// still big question - letting effects vary across time post treatment...

// Hours
local controls "i.educ_head_est i.educ_wife_est i.raceth_head_fixed i.raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 i.home_owner NUM_CHILDREN_ rel_start_yr"

regress female_hours_pct_t married $controls if treated==1
est store h0

xtreg female_hours_pct_t married $controls if treated==1, fe
est store h1

xtfeis female_hours_pct_t married $controls if treated==1, slope(AGE_WIFE_) cluster(couple_id)
est store h2

coefplot (h0, label("Base")) (h1,label("FE")) (h2,label("FEIS")), keep(married)

// Housework
local controls "i.educ_head_est i.educ_wife_est i.raceth_head_fixed i.raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 i.home_owner NUM_CHILDREN_ rel_start_yr"

regress wife_housework_pct_t married $controls if treated==1
est store hw0

xtreg wife_housework_pct_t married $controls if treated==1, fe
est store hw1

xtfeis wife_housework_pct_t married $controls if treated==1, slope(AGE_WIFE_) cluster(couple_id)
est store hw2

xtfeis wife_housework_pct_t married $controls if treated==1, slope(relationship_duration) cluster(couple_id)
est store hw3

coefplot (hw0, label("Base")) (hw1,label("FE")) (hw2,label("FEIS")) (hw3,label("FEIS, dur")), keep(married)

// Hausman test for whether or not you need FEIS v. FE
global controls "i.educ_head_est i.educ_wife_est i.raceth_head_fixed i.raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 i.home_owner NUM_CHILDREN_ rel_start_yr"
xtfeis female_earn_pct_t married $controls if treated==1, slope(AGE_WIFE_) cluster(couple_id)
// xtart // this is giving errors
xtart, keep(married) // requests that the ART be conducted only for the specified subset of common coefficients of Model A and Model B.

xtfeis female_hours_pct_t married $controls if treated==1, slope(AGE_WIFE_) cluster(couple_id)
xtart, keep(married) 

xtfeis wife_housework_pct_t married $controls if treated==1, slope(AGE_WIFE_) cluster(couple_id)
xtart, keep(married) // so this one is significant

// Including controls
xtfeis female_earn_pct_t married $controls, slope(relationship_duration) cluster(couple_id)
xtart, keep(married) // requests that the ART be conducted only for the specified subset of common coefficients of Model A and Model B.

xtfeis female_hours_pct_t married $controls, slope(relationship_duration) cluster(couple_id)
xtart, keep(married) 

xtfeis wife_housework_pct_t married $controls, slope(relationship_duration) cluster(couple_id)
xtart, keep(married) // so this one is significant


************************************
* Old analyses
************************************

* Is this what was in gender workshop preso? no these don't perfectly match...but I did fix the relationship duration that was previously incorrect
local controls "i.educ_head_est i.educ_wife_est i.raceth_head_fixed i.raceth_wife_fixed AGE_HEAD_ AGE_WIFE_ couple_earnings_t1 i.home_owner NUM_CHILDREN_ rel_start_yr"
// do I use controls with fixed effects? I think because that controls for observed and the remained is unobserved? I think controls let hyou control for time-varying things whereas fixed effects net out time invariant? (hence why race is collinear?)
xtreg female_earn_pct_t i.married i.relationship_duration `controls' if treated==1 & relationship_duration<=15, fe
margins married

xtreg female_hours_pct_t i.married i.relationship_duration `controls' if treated==1 & relationship_duration<=15, fe
margins married

xtreg wife_housework_pct_t i.married i.relationship_duration `controls' if treated==1 & relationship_duration<=15, fe
margins married
