********************************************************************************
********************************************************************************
* Project: Relationship Growth Curves
* Owner: Kimberly McErlean
* Started: September 2024
* File: couple_sample_recodes
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files takes sample of couples and recodes to get ready for analysis

********************************************************************************
**# import orig data, merge to marital history, and create nec relationship variables
********************************************************************************
use "$created_data/PSID_partners.dta", clear // created step 2

// merge onto relationship history (incorporates marital history + attempts to identify cohabitation)
merge m:1 unique_id using "$created_data/psid_composition_history.dta" // try this for now - created step 3
rename partnered ever_partnered
drop partner_id // need to clean up some things I don't need in this file for now

drop if _merge==2
drop _merge

// HH change variables
gen moved = 0
replace moved = 1 if inlist(MOVED_,1,2) & inlist(SPLITOFF_,1,3) // moved in
replace moved = 2 if inlist(MOVED_,1,2) & inlist(SPLITOFF_,2,4) // splitoff
replace moved = 3 if inlist(MOVED_,5,6) // moved out
replace moved = 4 if MOVED_==1 & SPLITOFF_==0 // born
replace moved = 5 if MOVED_==7

label define moved 0 "no" 1 "Moved in" 2 "Splitoff" 3 "Moved out" 4 "Born" 5 "Died"
label values moved moved

gen change_yr=.
replace change_yr = MOVED_YEAR_ if MOVED_YEAR_ >0 & MOVED_YEAR_ <9000
replace change_yr = SPLITOFF_YEAR_ if SPLITOFF_YEAR_ >0 & SPLITOFF_YEAR_ <9000

gen permanent_attrit=0
replace permanent_attrit=1 if PERMANENT_ATTRITION==1 // attrited
replace permanent_attrit=2 if inlist(PERMANENT_ATTRITION,2,3) // marked as died
label define perm 0 "no" 1 "attrited" 2 "died"
label values permanent_attrit perm

// who is who
gen relationship = .
replace relationship = 1 if inlist(RELATION_,1,10) // head
replace relationship = 2 if inlist(RELATION_,2,20,22) // partner

label define relationship 1 "Head" 2 "Partner"
label values relationship relationship

/* okay, the other one I made I think is better, so not using this
// this is just for those where marital history not cutting it (either not in it OR cohab)
tab marital_status_updated rel_start
gen rel_start_yr_est = survey_yr if rel_start==1
unique rel_start_yr_est if rel_start==1, by(unique_id) gen(count_rel_est)
browse unique_id survey_yr rel_start_yr_est count_rel_est
bysort unique_id: egen rel_rank_est = rank(rel_start_yr_est)
// bysort unique_id (count_rel_est): replace count_rel_est = count_rel_est[1]
// bysort unique_id (rel_start_yr_est): replace rel_start_yr_est = rel_start_yr_est[1] if count_rel_est==1

sort unique_id survey_yr
browse unique_id survey_yr rel_start_yr_est count_rel_est rel_rank_est

forvalues r=1/6{
	gen rel_start_est`r'=.
	replace rel_start_est`r' = rel_start_yr_est if rel_rank_est==`r'
	bysort unique_id (rel_start_est`r'): replace rel_start_est`r' = rel_start_est`r'[1]
}

sort unique_id survey_yr
browse unique_id survey_yr rel_start_est* rel1_start rel2_start rel3_start rel4_start rel5_start // does what i created here match what i created in other file? seems liek yes, except I moved year up by 1 and i accounted for those who entered partnered
tab rel_start_est1, m
tab rel1_start, m // so I have way less missing with theother variable, I think because I accounted for those who entered partnered. so use these instead?
*/

// filling in marital history
browse unique_id survey_yr RELATION_ marital_status_updated marr_trans FIRST_MARRIAGE_YR_START mh_yr_married1 mh_yr_end1 mh_status1 mh_yr_married2 mh_yr_end2 mh_yr_married3 mh_yr_end3 mh_yr_married4 mh_yr_end4 in_marital_history // FAMILY_INTERVIEW_NUM_ so will compare what is provided in individual file, what is provided in MH, what I calculated based on observed transitions
tab  FIRST_MARRIAGE_YR_START if in_marital_history==0 // oh I think this is populated from marital history GAH

gen rel_number=.
forvalues r=1/9{
	replace rel_number=`r' if survey_yr >=mh_yr_married`r' & survey_yr <= mh_yr_end`r'
}
forvalues r=12/13{
	replace rel_number=`r' if survey_yr >=mh_yr_married`r' & survey_yr <= mh_yr_end`r'
}

/*
forvalues r=98/99{
	replace rel_number=`r' if survey_yr >=mh_yr_married`r' & survey_yr <= mh_yr_end`r'
}
*/

browse unique_id survey_yr marital_status_updated rel_number FIRST_MARRIAGE_YR_START mh_yr_married1 mh_yr_end1 mh_status1 mh_yr_married2 mh_yr_end2 mh_yr_married3 mh_yr_end3 mh_yr_married4 mh_yr_end4 in_marital_history

tab rel_number, m
tab rel_number in_marital_history, m // so about half of the missing are bc not in marital history, but still a bunch otherwise
tab rel_number marital_status_updated if in_marital_history==1, m // okay, so yes, the vast majority of missing are bc partnered, not married, so that makes sense.

gen rel_start_yr=.
gen rel_end_yr=.
gen rel_status=.

forvalues r=1/9{
	replace rel_start_yr=mh_yr_married`r' if rel_number==`r'
	replace rel_end_yr=mh_yr_end`r' if rel_number==`r'
	replace rel_status=mh_status`r' if rel_number==`r'
}
forvalues r=12/13{
	replace rel_start_yr=mh_yr_married`r' if rel_number==`r'
	replace rel_end_yr=mh_yr_end`r' if rel_number==`r'
	replace rel_status=mh_status`r' if rel_number==`r'
}

/*
forvalues r=98/99{
	replace rel_start_yr=mh_yr_married`r' if rel_number==`r'
	replace rel_end_yr=mh_yr_end`r' if rel_number==`r'
	replace rel_status=mh_status`r' if rel_number==`r'
}
*/

browse unique_id survey_yr marital_status_updated rel_number rel_start_yr rel_end_yr FIRST_MARRIAGE_YR_START mh_yr_married1 mh_yr_end1 mh_status1 mh_yr_married2 mh_yr_end2 mh_yr_married3 mh_yr_end3 mh_yr_married4 mh_yr_end4 in_marital_history

gen flag=0
replace flag=1 if rel_start_yr==. // aka need to add manually

// so, right now, official relationship start and end filled in for those in marital history and not cohabiting. let's figure out the rest
browse unique_id survey_yr marital_status_updated in_marital_history flag rel_start_yr rel_end_yr rel_start moved change_yr hh1_start hh2_start hh1_end hh2_end rel1_start rel1_end rel2_start rel2_end mh_yr_married1 mh_yr_end1 mh_yr_married2 mh_yr_end2
browse unique_id survey_yr marital_status_updated in_marital_history rel_start_yr rel_end_yr rel_start moved change_yr hh1_start hh2_start hh1_end hh2_end rel1_start rel1_end rel2_start rel2_end if flag==1

gen hhno_est=.
forvalues h=1/5{
	replace hhno_est=`h' if survey_yr >=hh`h'_start & survey_yr <= hh`h'_end
}

gen relno_est=.
forvalues r=1/5{
	replace relno_est=`r' if survey_yr >=rel`r'_start & survey_yr <= rel`r'_end
}

gen rel_start_yr_est=.
gen rel_end_yr_est =.
gen hh_start_yr_est=.
gen hh_end_yr_est=.

forvalues r=1/5{
	replace rel_start_yr_est=rel`r'_start if relno_est==`r'
	replace rel_end_yr_est=rel`r'_end if relno_est==`r'
	replace hh_start_yr_est=hh`r'_start if hhno_est==`r'
	replace hh_end_yr_est=hh`r'_end if hhno_est==`r'
}

egen max_start_yr_est = rowmax(hh_start_yr_est rel_start_yr_est)
egen max_end_yr_est = rowmax(hh_end_yr_est rel_end_yr_est)
egen min_start_yr_est = rowmin(hh_start_yr_est rel_start_yr_est)
egen min_end_yr_est = rowmin(hh_end_yr_est rel_end_yr_est)
// browse unique_id survey_yr max_start_yr_est max_end_yr_est hh_start_yr_est rel_start_yr_est hh_end_yr_est rel_end_yr_est

replace rel_start_yr = rel_start_yr_est if flag==1 & hh_start_yr_est==. // so use relationship if no HH info
replace rel_end_yr = rel_end_yr_est if flag==1 & hh_end_yr_est==. // so use relationship if no HH info

replace rel_start_yr = hh_start_yr_est if rel_start_yr_est==hh_start_yr_est & rel_start_yr_est!=. & hh_start_yr_est!=. & rel_start_yr==. // fill in if they match
replace rel_start_yr = hh_start_yr_est if (abs(rel_start_yr_est-hh_start_yr_est)==1 | abs(rel_start_yr_est-hh_start_yr_est)==2) & rel_start_yr_est!=. & hh_start_yr_est!=. & rel_start_yr==. // fill in if they are just a year or two off in either direction (bc of biennial surveys)
replace rel_start_yr = max_start_yr_est if rel_start_yr_est!=. & hh_start_yr_est!=. & rel_start_yr==. // I think the later date makes sense in these instances
replace rel_start_yr = survey_yr if rel_start==1 & rel_start_yr==. 
tab hh_start_yr_est if rel_start_yr==. , m
tab rel_start_yr_est if rel_start_yr==. , m

replace rel_end_yr = hh_end_yr_est if rel_end_yr_est==hh_end_yr_est & rel_end_yr_est!=. & hh_end_yr_est!=. & rel_end_yr==. // fill in if they match
replace rel_end_yr = hh_end_yr_est if (abs(rel_end_yr_est-hh_end_yr_est)==1 | abs(rel_end_yr_est-hh_end_yr_est)==2) & rel_end_yr_est!=. & hh_end_yr_est!=. & rel_end_yr==. // fill in if they are just a year off in either direction
replace rel_end_yr = hh_end_yr_est if rel_end_yr==. // use hh end if no rel end bc I think this better captures move outs then permanent attrits
replace rel_end_yr = rel_end_yr_est if rel_end_yr==. // then for rest, use rel end, okay these are all missing
tab hh_end_yr_est if rel_end_yr==. , m
tab rel_end_yr_est if rel_end_yr==. , m

/* to troubleshoot
browse unique_id survey_yr marital_status_updated rel_start_yr rel_end_yr rel_start hhno_est hh_start_yr_est hh_end_yr_est relno_est rel_start_yr_est rel_end_yr_est moved change_yr hh1_start hh2_start hh1_end hh2_end  rel1_start rel1_end rel2_start rel2_end if flag==1
browse unique_id survey_yr marital_status_updated rel_start_yr rel_end_yr rel_start hhno_est hh_start_yr_est hh_end_yr_est relno_est rel_start_yr_est rel_end_yr_est moved change_yr hh1_start hh2_start hh1_end hh2_end  rel1_start rel1_end rel2_start rel2_end rel3_start rel3_end if rel_start_yr==.
browse unique_id survey_yr marital_status_updated rel_start_yr rel_end_yr rel_start hhno_est hh_start_yr_est hh_end_yr_est relno_est rel_start_yr_est rel_end_yr_est moved change_yr hh1_start hh2_start hh1_end hh2_end  rel1_start rel1_end rel2_start rel2_end if rel_end_yr==.
*/

/* old code
// help
forvalues r=1/5{
	local s = `r'+1
	display `s'
	replace rel_start_yr = rel_start_est`r' if survey_yr >=rel_start_est`r' & survey_yr < mh_yr_end`s' & flag==1 // only for those where above didn't work
}
replace rel_start_yr = rel_start_est6 if survey_yr >=rel_start_est6 & flag==1 // 6 is max

// wait what if it ended?! come back to this. think it's fine because those rows should be gone, liek wouldn't be partnered anymore or would have a new end date (based on browse)

browse  unique_id survey_yr marital_status_updated flag rel_number rel_start_yr rel_end_yr rel_start_yr_est rel_start_est*
inspect rel_start_yr if in_marital_history==1
inspect rel_start_yr if marital_status_updated==1
inspect rel_start_yr if marital_status_updated==2

browse unique_id survey_yr marital_status_updated flag rel_number rel_start_yr mh_yr_married1 FIRST_MARRIAGE_YR_START FIRST_MARRIAGE_YR_HEAD_ LAST_MARRIAGE_YR_HEAD_ first_survey_yr

browse unique_id survey_yr marital_status_updated flag rel_number rel_start_yr mh_yr_married1 FIRST_MARRIAGE_YR_START FIRST_MARRIAGE_YR_HEAD_ LAST_MARRIAGE_YR_HEAD_ first_survey_yr if marital_status_updated==2

// okay will try another way to fill in more cohab
gen rel_start_est_cohab = survey_yr if survey_yr==first_survey_yr & marital_status_updated==2
bysort unique_id (rel_start_est_cohab): replace rel_start_est_cohab = rel_start_est_cohab[1]
sort unique_id survey_yr
browse unique_id survey_yr marital_status_updated flag rel_number rel_start_est_cohab rel_start_yr mh_yr_married1 FIRST_MARRIAGE_YR_START FIRST_MARRIAGE_YR_HEAD_ LAST_MARRIAGE_YR_HEAD_ first_survey_yr if marital_status_updated==2

replace rel_start_yr=rel_start_est_cohab if rel_start_yr==. & marital_status_updated==2
*/

gen relationship_duration = survey_yr - rel_start_yr

browse unique_id survey_yr marital_status_updated rel_start_yr relationship_duration

********************************************************************************
**# Other variable recodes now, like DoL and sociodemographics
* A lot of this code repurposed from union dissolution work - file 01a
********************************************************************************
// Education recode
browse unique_id survey_yr relationship SEX  EDUC1_HEAD_ EDUC_HEAD_ EDUC1_WIFE_ EDUC_WIFE_ YRS_EDUCATION_INDV COLLEGE_WIFE_ COLLEGE_HEAD_ COLLEGE_INDV_ BACHELOR_YR_WIFE_ BACHELOR_YR_HEAD_ BACHELOR_YR_INDV_  ENROLLED_WIFE_ ENROLLED_HEAD_ STUDENT_T1_INDV_ STUDENT_CURRENT_INDV_ // can also use yrs education but this is individual not HH, so need to match to appropriate person

/*
foreach var in EDUC1_HEAD_ EDUC_HEAD_ EDUC1_WIFE_ EDUC_WIFE_ YRS_EDUCATION_INDV COLLEGE_WIFE_ COLLEGE_HEAD_ COLLEGE_INDV_ ENROLLED_WIFE_ ENROLLED_HEAD_ STUDENT_T1_INDV_ STUDENT_CURRENT_INDV_ BACHELOR_YR_WIFE_ BACHELOR_YR_HEAD_ BACHELOR_YR_INDV_{
	tabstat `var', by(survey_yr) // want to see which variables asked when	
}

foreach var in YRS_EDUCATION_INDV COLLEGE_INDV_ STUDENT_T1_INDV_ STUDENT_CURRENT_INDV_ BACHELOR_YR_INDV_{
	tabstat `var', by(relationship) // are they asked for all?
}
*/

/*
educ1 until 1990, but educ started 1975, okay but then a gap until 1991? wife not asked 1969-1971 - might be able to fill in if she is in sample either 1968 or 1972? (match to the id). also look at yrs education (missing 69 and 74?)

codes are also different between the two, use educ1 until 1990, then educ 1991 post
early educ:
0. cannot read
1. 0-5th grade
2. 6-8th grade
3. 9-11 grade
4/5. 12 grade
6. college no degree
7/8. college / advanced degree
9. dk

later educ: years of education
*/

* clean up intermediary variables
label values YRS_EDUCATION_INDV .

gen hs_head=.
replace hs_head=1 if inlist(HS_GRAD_HEAD_,1,2)
replace hs_head=0 if HS_GRAD_HEAD_==3

gen hs_wife=.
replace hs_wife=1 if inlist(HS_GRAD_WIFE_,1,2)
replace hs_wife=0 if HS_GRAD_WIFE_==3

gen attended_college_head=.
replace attended_college_head= 0 if ATTENDED_COLLEGE_HEAD_==5
replace attended_college_head= 1 if ATTENDED_COLLEGE_HEAD_==1

gen attended_college_wife=.
replace attended_college_wife= 0 if ATTENDED_COLLEGE_WIFE_==5
replace attended_college_wife= 1 if ATTENDED_COLLEGE_WIFE_==1

gen completed_college_head=.
replace completed_college_head= 0 if COLLEGE_HEAD_==5
replace completed_college_head= 1 if COLLEGE_HEAD_==1
replace completed_college_head= 0 if attended_college_head==0

gen completed_college_wife=.
replace completed_college_wife= 0 if COLLEGE_WIFE_==5
replace completed_college_wife= 1 if COLLEGE_WIFE_==1
replace completed_college_wife= 0 if attended_college_wife==0

gen completed_college_indv=.
replace completed_college_indv= 0 if COLLEGE_INDV_==5
replace completed_college_indv= 1 if COLLEGE_INDV_==1

gen college_degree_head=.
replace college_degree_head=0 if HIGHEST_DEGREE_HEAD_==0
replace college_degree_head=1 if HIGHEST_DEGREE_HEAD_==1 // associates
replace college_degree_head=2 if inrange(HIGHEST_DEGREE_HEAD_,2,6) // bachelor's plus

gen college_degree_wife=.
replace college_degree_wife=0 if HIGHEST_DEGREE_WIFE_==0
replace college_degree_wife=1 if HIGHEST_DEGREE_WIFE_==1 // associates
replace college_degree_wife=2 if inrange(HIGHEST_DEGREE_WIFE_,2,6) // bachelor's plus

label define degree 0 "No Coll" 1 "Assoc" 2 "BA+"
label values college_degree_head college_degree_wife

tab attended_college_head completed_college_head, m
tab completed_college_head college_degree_head, m

replace NEW_HEAD_YEAR = 1900+NEW_HEAD_YEAR if NEW_HEAD_YEAR>0 & NEW_HEAD_YEAR<100
replace NEW_WIFE_YEAR = 1900+NEW_WIFE_YEAR if NEW_WIFE_YEAR>0 & NEW_WIFE_YEAR<100

recode EDUC1_WIFE_ (1/3=1)(4/5=2)(6=3)(7/8=4)(9=.)(0=.), gen(educ_wife_early)
recode EDUC1_HEAD_ (0/3=1)(4/5=2)(6=3)(7/8=4)(9=.), gen(educ_head_early)
recode EDUC_WIFE_ (1/11=1) (12=2) (13/15=3) (16/17=4) (99=.)(0=.), gen(educ_wife_1975)
recode EDUC_HEAD_ (0/11=1) (12=2) (13/15=3) (16/17=4) (99=.), gen(educ_head_1975)
recode YRS_EDUCATION_INDV (1/11=1) (12=2) (13/15=3) (16/17=4) (98/99=.)(0=.), gen(educ_completed) // okay this is hard to use because head / wife ONLY recorded against those specific ones so they don't always have values here

label define educ 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values educ_wife_early educ_head_early educ_wife_1975 educ_head_1975 educ_completed educ

browse unique_id survey_yr in_sample relationship YRS_EDUCATION_INDV educ_completed educ_head_early educ_head_1975 hs_head HS_GRAD_HEAD attended_college_head completed_college_head college_degree_head BACHELOR_YR_HEAD_ YR_EDUC_UPD_HEAD_ NEW_HEAD_ NEW_HEAD_YEAR if relationship==1 // using head right now to wrap my head around

* create final education variables - these are better
gen educ_head=.
replace educ_head=1 if hs_head==0
replace educ_head=2 if hs_head==1 & attended_college_head==0
replace educ_head=3 if hs_head==1 & attended_college_head==1 & completed_college_head==0
replace educ_head=3 if completed_college_head==1 & college_degree_head==1
replace educ_head=4 if completed_college_head==1 & college_degree_head==2

gen educ_head_est=. // this can help fill in some missing info
replace educ_head_est=educ_head_early if inrange(survey_yr,1968,1990)
replace educ_head_est=educ_head_1975 if inrange(survey_yr,1991,2021)

tab educ_head educ_head_est, m
tab educ_completed educ_head if relationship==1, m
tab educ_head_est educ_completed if educ_head==., m
replace educ_head = educ_completed if educ_head==. & educ_completed!=.
replace educ_head = educ_head_est if educ_head==. & educ_head_est!=.

browse unique_id survey_yr educ_head educ_head_est educ_completed YRS_EDUCATION_INDV  hs_head attended_college_head completed_college_head college_degree_head if relationship==1 

gen educ_wife=.
replace educ_wife=1 if hs_wife==0
replace educ_wife=2 if hs_wife==1 & attended_college_wife==0
replace educ_wife=3 if hs_wife==1 & attended_college_wife==1 & completed_college_wife==0
replace educ_wife=3 if completed_college_wife==1 & college_degree_wife==1
replace educ_wife=4 if completed_college_wife==1 & college_degree_wife==2

gen educ_wife_est=.
replace educ_wife_est=educ_wife_early if inrange(survey_yr,1968,1990)
replace educ_wife_est=educ_wife_1975 if inrange(survey_yr,1991,2021)
tab survey_yr educ_wife_est, m 

replace educ_wife = educ_completed if educ_wife==. & educ_completed!=.
replace educ_wife = educ_wife_est if educ_wife==. & educ_wife_est!=.

tab educ_wife educ_wife_est, m
tab educ_completed educ_wife if relationship==2, m
tab educ_wife_est educ_completed if educ_wife==., m

label values educ_head educ_wife educ_head_est educ_wife_est educ

gen college_wife=.
replace college_wife=0 if inrange(educ_wife,1,3)
replace college_wife=1 if educ_wife==4

gen college_head=.
replace college_head=0 if inrange(educ_head,1,3)
replace college_head=1 if educ_head==4
tab college_degree_head college_head, m

gen college_indv=.
replace college_indv=0 if inrange(educ_completed,1,3)
replace college_indv=1 if educ_completed==4

gen couple_educ_gp=.
replace couple_educ_gp = 0 if inlist(educ_head,1,2,3) & inlist(educ_wife,1,2,3)
replace couple_educ_gp = 1 if (college_wife==1 | college_head==1)

label define couple_educ 0 "Neither College" 1 "At Least One College"
label values couple_educ_gp couple_educ

gen educ_type=.
replace educ_type=1 if inlist(educ_head,1,2,3) & inlist(educ_wife,1,2,3)
replace educ_type=2 if educ_wife==4 & inlist(educ_head,1,2,3)
replace educ_type=3 if inlist(educ_wife,1,2,3) & educ_head==4
replace educ_type=4 if educ_wife==4 & educ_head==4

label define educ_type 1 "Neither College" 2 "Her College" 3 "His College" 4 "Both College"
label values educ_type educ_type

// income and division of paid labor
browse unique_id survey_yr FAMILY_INTERVIEW_NUM_ TAXABLE_T1_HEAD_WIFE TOTAL_INCOME_T1_FAMILY LABOR_INCOME_T1_HEAD WAGES_ALT_T1_HEAD WAGES_T1_HEAD LABOR_INCOME_T2_HEAD LABOR_INCOME_T1_WIFE_ WAGES_T1_WIFE_  LABOR_INCOME_T2_WIFE_

	// to use: WAGES_HEAD_ WAGES_WIFE_ -- wife not asked until 1993? okay labor income??
	// wages and labor income asked for head whole time. labor income wife 1968-1993, wages for wife, 1993 onwards

gen earnings_t1_wife=.
replace earnings_t1_wife = LABOR_INCOME_T1_WIFE_ if inrange(survey_yr,1968,1993)
replace earnings_t1_wife = WAGES_T1_WIFE_ if inrange(survey_yr,1994,2021)
replace earnings_t1_wife=. if earnings_t1_wife== 9999999

gen earnings_t1_head=.
replace earnings_t1_head = LABOR_INCOME_T1_HEAD if inrange(survey_yr,1968,1993)
replace earnings_t1_head = WAGES_T1_HEAD if inrange(survey_yr,1994,2021)
replace earnings_t1_head=. if earnings_t1_head== 9999999

egen couple_earnings_t1 = rowtotal(earnings_t1_wife earnings_t1_head)
browse unique_id survey_yr TAXABLE_T1_HEAD_WIFE couple_earnings_t1 earnings_t1_wife earnings_t1_head
	
gen female_earn_pct_t1 = earnings_t1_wife/(couple_earnings_t1)

gen hh_earn_type_t1=.
replace hh_earn_type_t1=1 if female_earn_pct_t1 >=.4000 & female_earn_pct_t1 <=.6000
replace hh_earn_type_t1=2 if female_earn_pct_t1 < .4000 & female_earn_pct_t1 >=0
replace hh_earn_type_t1=3 if female_earn_pct_t1 > .6000 & female_earn_pct_t1 <=1
replace hh_earn_type_t1=4 if earnings_t1_head==0 & earnings_t1_wife==0

label define hh_earn_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_earn_type_t1 hh_earn_type

sort unique_id survey_yr

gen hh_earn_type_t=.
replace hh_earn_type_t=hh_earn_type_t1[_n+1] if unique_id==unique_id[_n+1] & wave==wave[_n+1]-1
label values hh_earn_type_t hh_earn_type

gen female_earn_pct_t=.
replace female_earn_pct_t=female_earn_pct_t1[_n+1] if unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

browse unique_id survey_yr wave earnings_t1_head earnings_t1_wife hh_earn_type_t1 hh_earn_type_t female_earn_pct_t1 female_earn_pct_t

// hours instead of earnings	
browse unique_id survey_yr WEEKLY_HRS1_T1_WIFE_ WEEKLY_HRS_T1_WIFE_ WEEKLY_HRS1_T1_HEAD_ WEEKLY_HRS_T1_HEAD_

gen weekly_hrs_t1_wife = .
replace weekly_hrs_t1_wife = WEEKLY_HRS1_T1_WIFE_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_t1_wife = WEEKLY_HRS_T1_WIFE_ if survey_yr >=1994
replace weekly_hrs_t1_wife = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_T1_WIFE_,9,0)
replace weekly_hrs_t1_wife = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==1
replace weekly_hrs_t1_wife = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==2
replace weekly_hrs_t1_wife = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==3
replace weekly_hrs_t1_wife = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==4
replace weekly_hrs_t1_wife = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==5
replace weekly_hrs_t1_wife = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==6
replace weekly_hrs_t1_wife = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==7
replace weekly_hrs_t1_wife = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_T1_WIFE_ ==8
replace weekly_hrs_t1_wife=. if weekly_hrs_t1_wife==999

gen weekly_hrs_t1_head = .
replace weekly_hrs_t1_head = WEEKLY_HRS1_T1_HEAD_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_t1_head = WEEKLY_HRS_T1_HEAD_ if survey_yr >=1994
replace weekly_hrs_t1_head = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_T1_HEAD_,9,0)
replace weekly_hrs_t1_head = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==1
replace weekly_hrs_t1_head = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==2
replace weekly_hrs_t1_head = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==3
replace weekly_hrs_t1_head = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==4
replace weekly_hrs_t1_head = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==5
replace weekly_hrs_t1_head = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==6
replace weekly_hrs_t1_head = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==7
replace weekly_hrs_t1_head = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_T1_HEAD_ ==8
replace weekly_hrs_t1_head=. if weekly_hrs_t1_head==999

egen couple_hours_t1 = rowtotal(weekly_hrs_t1_wife weekly_hrs_t1_head)
gen female_hours_pct_t1 = weekly_hrs_t1_wife/couple_hours_t1

gen hh_hours_type_t1=.
replace hh_hours_type_t1=1 if female_hours_pct_t1 >=.4000 & female_hours_pct_t1 <=.6000
replace hh_hours_type_t1=2 if female_hours_pct_t1 <.4000
replace hh_hours_type_t1=3 if female_hours_pct_t1 >.6000 & female_hours_pct_t1!=.
replace hh_hours_type_t1=4 if weekly_hrs_t1_head==0 & weekly_hrs_t1_head==0

label define hh_hours_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_hours_type_t1 hh_hours_type

gen hh_hours_type_t=.
replace hh_hours_type_t=hh_hours_type_t1[_n+1] if unique_id==unique_id[_n+1] & wave==wave[_n+1]-1
label values hh_hours_type_t hh_hours_type

gen female_hours_pct_t=.
replace female_hours_pct_t=female_hours_pct_t1[_n+1] if unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

// browse unique_id survey_yr hh_hours_type_t1 hh_hours_type_t  female_hours_pct_t1 female_hours_pct_t

// housework hours - not totally sure if accurate prior to 1976 (asked annually not weekly). missing head/wife specific in 1968, 1975, 1982
browse unique_id survey_yr RELATION_ HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_INDV_ TOTAL_HOUSEWORK_T1_HW MOST_HOUSEWORK_T1 // total and most HW stopped after 1974

gen housework_head = HOUSEWORK_HEAD_
replace housework_head = (HOUSEWORK_HEAD_/52) if inrange(survey_yr,1968,1974)
replace housework_head = HOUSEWORK_INDV_ if relationship==1 & inrange(survey_yr,1968,1974) & HOUSEWORK_INDV_!=.
replace housework_head=. if inlist(housework_head,998,999)

gen housework_wife = HOUSEWORK_WIFE_
replace housework_wife = (HOUSEWORK_WIFE_/52) if inrange(survey_yr,1968,1974)
replace housework_wife = HOUSEWORK_INDV_ if relationship==2 & inrange(survey_yr,1968,1974) & HOUSEWORK_INDV_!=.
replace housework_wife=. if inlist(housework_wife,998,999)

gen total_housework_weekly = TOTAL_HOUSEWORK_T1_HW / 52

browse unique_id survey_yr relationship housework_head housework_wife HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_INDV_

egen couple_housework_t1 = rowtotal (housework_wife housework_head)
browse id survey_yr housework_head housework_wife couple_housework total_housework_weekly TOTAL_HOUSEWORK_T1_HW MOST_HOUSEWORK_T1

gen wife_housework_pct_t = housework_wife / couple_housework_t

gen housework_bkt_t=.
replace housework_bkt_t=1 if wife_housework_pct_t >=.4000 & wife_housework_pct_t <=.6000
replace housework_bkt_t=2 if wife_housework_pct_t >.6000 & wife_housework_pct_t!=.
replace housework_bkt_t=3 if wife_housework_pct_t <.4000
replace housework_bkt_t=4 if housework_wife==0 & housework_head==0

label define housework_bkt 1 "Dual HW" 2 "Female Primary" 3 "Male Primary" 4 "NA"
label values housework_bkt_t housework_bkt

sort id survey_yr
gen housework_bkt_t1=.
replace housework_bkt_t1=housework_bkt_t[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
label values housework_bkt_t1 housework_bkt

gen wife_housework_pct_t1=.
replace wife_housework_pct_t1=wife_housework_pct_t[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

// browse id survey_yr housework_bkt_t housework_bkt_t1 wife_housework_pct_t wife_housework_pct_t1

//  combined indicator of paid and unpaid, using HOURS
gen hours_housework_t=.
replace hours_housework_t=1 if hh_hours_type_t==1 & housework_bkt_t==1 // dual both (egal)
replace hours_housework_t=2 if hh_hours_type_t==1 & housework_bkt_t==2 // dual earner, female HM (second shift)
replace hours_housework_t=3 if hh_hours_type_t==2 & housework_bkt_t==2 // male BW, female HM (traditional)
replace hours_housework_t=4 if hh_hours_type_t==3 & housework_bkt_t==3 // female BW, male HM (counter-traditional)
replace hours_housework_t=5 if hours_housework_t==. & hh_hours_type_t!=. & housework_bkt_t!=. // all others

label define combined_dol 1 "Egal" 2 "Second Shift" 3 "Traditional" 4 "Counter Traditional" 5 "All others"
label values hours_housework combined_dol 

gen earn_housework_t=.
replace earn_housework_t=1 if hh_earn_type_t==1 & housework_bkt_t==1 // dual both (egal)
replace earn_housework_t=2 if hh_earn_type_t==1 & housework_bkt_t==2 // dual earner, female HM (second shift)
replace earn_housework_t=3 if hh_earn_type_t==2 & housework_bkt_t==2 // male BW, female HM (traditional)
replace earn_housework_t=4 if hh_earn_type_t==3 & housework_bkt_t==3 // female BW, male HM (counter-traditional)
replace earn_housework_t=5 if earn_housework_t==. & hh_earn_type_t!=. & housework_bkt_t!=. // all others

label values earn_housework combined_dol 

// employment
browse unique_id survey_yr EMPLOY_STATUS_HEAD_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS_WIFE_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_
// not numbered until 1994; 1-3 arose in 1994. codes match
// wife not asked until 1976?

gen employ_head=.
replace employ_head=0 if inrange(EMPLOY_STATUS_HEAD_,2,9)
replace employ_head=1 if EMPLOY_STATUS_HEAD_==1
gen employ1_head=.
replace employ1_head=0 if inrange(EMPLOY_STATUS1_HEAD_,2,8)
replace employ1_head=1 if EMPLOY_STATUS1_HEAD_==1
gen employ2_head=.
replace employ2_head=0 if EMPLOY_STATUS2_HEAD_==0 | inrange(EMPLOY_STATUS2_HEAD_,2,8)
replace employ2_head=1 if EMPLOY_STATUS2_HEAD_==1
gen employ3_head=.
replace employ3_head=0 if EMPLOY_STATUS3_HEAD_==0 | inrange(EMPLOY_STATUS3_HEAD_,2,8)
replace employ3_head=1 if EMPLOY_STATUS3_HEAD_==1

browse employ_head employ1_head employ2_head employ3_head
egen employed_head=rowtotal(employ_head employ1_head employ2_head employ3_head), missing
replace employed_head=1 if employed_head==2

gen employ_wife=.
replace employ_wife=0 if inrange(EMPLOY_STATUS_WIFE_,2,9)
replace employ_wife=1 if EMPLOY_STATUS_WIFE_==1
gen employ1_wife=.
replace employ1_wife=0 if inrange(EMPLOY_STATUS1_WIFE_,2,8)
replace employ1_wife=1 if EMPLOY_STATUS1_WIFE_==1
gen employ2_wife=.
replace employ2_wife=0 if EMPLOY_STATUS2_WIFE_==0 | inrange(EMPLOY_STATUS2_WIFE_,2,8)
replace employ2_wife=1 if EMPLOY_STATUS2_WIFE_==1
gen employ3_wife=.
replace employ3_wife=0 if EMPLOY_STATUS3_WIFE_==0 | inrange(EMPLOY_STATUS3_WIFE_,2,8)
replace employ3_wife=1 if EMPLOY_STATUS3_WIFE_==1

egen employed_wife=rowtotal(employ_wife employ1_wife employ2_wife employ3_wife), missing
replace employed_wife=1 if employed_wife==2

browse id survey_yr employed_head employed_wife employ_head employ1_head employ_wife employ1_wife

// problem is this employment is NOW not last year. I want last year? use if wages = employ=yes, then no? (or hours)
gen ft_pt_t1_head=.
replace ft_pt_t1_head = 0 if weekly_hrs_t1_head==0
replace ft_pt_t1_head = 1 if weekly_hrs_t1_head > 0 & weekly_hrs_t1_head<=35
replace ft_pt_t1_head = 2 if weekly_hrs_t1_head > 35 & weekly_hrs_t1_head < 999

gen ft_pt_t1_wife=.
replace ft_pt_t1_wife = 0 if weekly_hrs_t1_wife==0
replace ft_pt_t1_wife = 1 if weekly_hrs_t1_wife > 0 & weekly_hrs_t1_wife<=35
replace ft_pt_t1_wife = 2 if weekly_hrs_t1_wife > 35 & weekly_hrs_t1_wife < 999

/*
gen ft_pt_t_head=.
replace ft_pt_t_head = 0 if weekly_hrs_t_head==0
replace ft_pt_t_head = 1 if weekly_hrs_t_head > 0 & weekly_hrs_t_head<=35
replace ft_pt_t_head = 2 if weekly_hrs_t_head > 35 & weekly_hrs_t_head < 999

gen ft_pt_t_wife=.
replace ft_pt_t_wife = 0 if weekly_hrs_t_wife==0
replace ft_pt_t_wife = 1 if weekly_hrs_t_wife > 0 & weekly_hrs_t_wife<=35
replace ft_pt_t_wife = 2 if weekly_hrs_t_wife > 35 & weekly_hrs_t_wife < 999
*/

label define ft_pt 0 "Not Employed" 1 "PT" 2 "FT"
label values ft_pt_t1_head ft_pt_t1_wife ft_pt

gen ft_t1_head=0
replace ft_t1_head=1 if ft_pt_t1_head==2
replace ft_t1_head=. if ft_pt_t1_head==.

gen ft_t1_wife=0
replace ft_t1_wife=1 if ft_pt_t1_wife==2
replace ft_t1_wife=. if ft_pt_t1_wife==.

/*
gen ft_t_head=0
replace ft_t_head=1 if ft_pt_t_head==2
replace ft_t_head=. if ft_pt_t_head==.

gen ft_t_wife=0
replace ft_t_wife=1 if ft_pt_t_wife==2
replace ft_t_wife=. if ft_pt_t_wife==.
*/

// adding other controls right now
gen either_enrolled=0
replace either_enrolled = 1 if ENROLLED_WIFE_==1 | ENROLLED_HEAD_==1

// race / ethnicity
// drop if RACE_1_WIFE_==9 | RACE_1_HEAD_==9

browse unique_id survey_yr RACE_1_WIFE_ RACE_2_WIFE_ RACE_3_WIFE_ RACE_1_HEAD_ RACE_2_HEAD_ RACE_3_HEAD_ RACE_4_HEAD_
// wait race of wife not asked until 1985?! that's wild. also need to see if codes changed in between. try to fill in historical for wife if in survey in 1985 and prior.
/*
1968-1984: 1=White; 2=Negro; 3=PR or Mexican; 7=Other
1985-1989: 1=White; 2=Black; 3=Am Indian 4=Asian 7=Other; 8 =more than 2
1990-2003: 1=White; 2=Black; 3=Am India; 4=Asian; 5=Latino; 6=Other; 7=Other
2005-2021: 1=White; 2=Black; 3=Am India; 4=Asian; 5=Native Hawaiian/Pac Is; 7=Other

From SHELF:
Both summary measures were based on majority response across all available waves (with a small number of ties being broken by most recent report). 
Also good history on how the PSID collected race and when actually asked v. carried forward
so maybe rely on when asked and more recent, when measures more robust and self-reported (aka not by interviewer)
*/

gen race_1_head_rec=.
replace race_1_head_rec=1 if RACE_1_HEAD_==1
replace race_1_head_rec=2 if RACE_1_HEAD_==2
replace race_1_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_1_HEAD_==3)
replace race_1_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_1_HEAD_==4)
replace race_1_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_1_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_1_HEAD_==5)
replace race_1_head_rec=6 if RACE_1_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_1_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_1_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_1_HEAD_==8)

gen race_2_head_rec=.
replace race_2_head_rec=1 if RACE_2_HEAD_==1
replace race_2_head_rec=2 if RACE_2_HEAD_==2
replace race_2_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_2_HEAD_==3)
replace race_2_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_2_HEAD_==4)
replace race_2_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_2_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_2_HEAD_==5)
replace race_2_head_rec=6 if RACE_2_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_2_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_2_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_2_HEAD_==8)

gen race_3_head_rec=.
replace race_3_head_rec=1 if RACE_3_HEAD_==1
replace race_3_head_rec=2 if RACE_3_HEAD_==2
replace race_3_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_3_HEAD_==3)
replace race_3_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_3_HEAD_==4)
replace race_3_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_3_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_3_HEAD_==5)
replace race_3_head_rec=6 if RACE_3_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_3_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_3_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_3_HEAD_==8)

gen race_4_head_rec=.
replace race_4_head_rec=1 if RACE_4_HEAD_==1
replace race_4_head_rec=2 if RACE_4_HEAD_==2
replace race_4_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_4_HEAD_==3)
replace race_4_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_4_HEAD_==4)
replace race_4_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_4_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_4_HEAD_==5)
replace race_4_head_rec=6 if RACE_4_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_4_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_4_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_4_HEAD_==8)

gen race_1_wife_rec=.
replace race_1_wife_rec=1 if RACE_1_WIFE_==1
replace race_1_wife_rec=2 if RACE_1_WIFE_==2
replace race_1_wife_rec=3 if (inrange(survey_yr,1985,2019) & RACE_1_WIFE_==3)
replace race_1_wife_rec=4 if (inrange(survey_yr,1985,2019) & RACE_1_WIFE_==4)
replace race_1_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_1_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_1_WIFE_==5)
replace race_1_wife_rec=6 if RACE_1_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_1_WIFE_==6) | (inrange(survey_yr,2005,2019) & RACE_1_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_1_WIFE_==8)

gen race_2_wife_rec=.
replace race_2_wife_rec=1 if RACE_2_WIFE_==1
replace race_2_wife_rec=2 if RACE_2_WIFE_==2
replace race_2_wife_rec=3 if (inrange(survey_yr,1985,2019) & RACE_2_WIFE_==3)
replace race_2_wife_rec=4 if (inrange(survey_yr,1985,2019) & RACE_2_WIFE_==4)
replace race_2_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_2_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_2_WIFE_==5)
replace race_2_wife_rec=6 if RACE_2_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_2_WIFE_==6) | (inrange(survey_yr,2005,2019) & RACE_2_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_2_WIFE_==8)

gen race_3_wife_rec=.
replace race_3_wife_rec=1 if RACE_3_WIFE_==1
replace race_3_wife_rec=2 if RACE_3_WIFE_==2
replace race_3_wife_rec=3 if (inrange(survey_yr,1985,2019) & RACE_3_WIFE_==3)
replace race_3_wife_rec=4 if (inrange(survey_yr,1985,2019) & RACE_3_WIFE_==4)
replace race_3_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_3_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_3_WIFE_==5)
replace race_3_wife_rec=6 if RACE_3_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_3_WIFE_==6) | (inrange(survey_yr,2005,2019) & RACE_3_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_3_WIFE_==8)

gen race_4_wife_rec=.
replace race_4_wife_rec=1 if RACE_4_WIFE_==1
replace race_4_wife_rec=2 if RACE_4_WIFE_==2
replace race_4_wife_rec=3 if (inrange(survey_yr,1985,2021) & RACE_4_WIFE_==3)
replace race_4_wife_rec=4 if (inrange(survey_yr,1985,2021) & RACE_4_WIFE_==4)
replace race_4_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_4_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_4_WIFE_==5)
replace race_4_wife_rec=6 if RACE_4_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_4_WIFE_==6) | (inrange(survey_yr,2005,2021) & RACE_4_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_4_WIFE_==8)

browse unique_id race_1_head_rec race_2_head_rec race_3_head_rec race_4_head_rec

// based on first mention (that is one option they use in SHELF)
gen race_wife=race_1_wife_rec
replace race_wife=7 if race_2_wife_rec!=.

gen race_head=race_1_head_rec
replace race_head=7 if race_2_head_rec!=.

label define race 1 "White" 2 "Black" 3 "Indian" 4 "Asian" 5 "Latino" 6 "Other" 7 "Multi-racial"
label values race_wife race_head race

// ethnicity
gen hispanic_head=.
replace hispanic_head=0 if HISPANICITY_HEAD_==0
replace hispanic_head=1 if inrange(HISPANICITY_HEAD_,1,7)

gen hispanic_wife=.
replace hispanic_wife=0 if HISPANICITY_WIFE_==0
replace hispanic_wife=1 if inrange(HISPANICITY_WIFE_,1,7)

tab race_head hispanic_head, m

// combined
gen raceth_head=.
replace raceth_head=1 if race_head==1 & (hispanic_head==0 | hispanic_head==.)
replace raceth_head=2 if race_head==2
replace raceth_head=3 if hispanic_head==1 & race_head!=2 // hispanic, non-black
replace raceth_head=3 if race_head==5 & (hispanic_head==0 | hispanic_head==.)
replace raceth_head=4 if race_head==4 & (hispanic_head==0 | hispanic_head==.)
replace raceth_head=5 if inlist(race_head,3,6,7) & (hispanic_head==0 | hispanic_head==.)

tab raceth_head, m
tab race_head raceth_head, m

gen raceth_wife=.
replace raceth_wife=1 if race_wife==1 & (hispanic_wife==0 | hispanic_wife==.)
replace raceth_wife=2 if race_wife==2
replace raceth_wife=3 if hispanic_wife==1 & race_wife!=2 // hispanic, non-black
replace raceth_wife=3 if race_wife==5 & (hispanic_wife==0 | hispanic_wife==.)
replace raceth_wife=4 if race_wife==4 & (hispanic_wife==0 | hispanic_wife==.)
replace raceth_wife=5 if inlist(race_wife,3,6,7) & (hispanic_wife==0 | hispanic_wife==.)

label define raceth 1 "NH White" 2 "Black" 3 "Hispanic" 4 "NH Asian" 5 "NH Other"
labe values raceth_head raceth_wife raceth

// figure out how to make time invariant, re: SHELF
tab raceth_head in_sample, m
tab raceth_wife in_sample, m
browse unique_id survey_yr raceth_head raceth_wife

// bysort unique_id: egen raceth_head_fixed = median(raceth_head)
bysort unique_id: egen raceth_head_fixed = mode(raceth_head) // majority
tab raceth_head_fixed, m
gen last_race_head=raceth_head if survey_yr==last_survey_yr // tie break with last reported
bysort unique_id (last_race_head): replace last_race_head = last_race_head[1]
sort unique_id survey_yr
browse unique_id survey_yr last_survey_yr raceth_head raceth_head_fixed last_race_head
replace raceth_head_fixed=last_race_head if raceth_head_fixed==.
tab raceth_head if raceth_head_fixed==., m

bysort unique_id: egen raceth_wife_fixed = mode(raceth_wife) // majority
tab raceth_wife_fixed, m
gen last_race_wife=raceth_wife if survey_yr==last_survey_yr // tie break with last reported
bysort unique_id (last_race_wife): replace last_race_wife = last_race_wife[1]
sort unique_id survey_yr
browse unique_id survey_yr last_survey_yr raceth_wife raceth_wife_fixed last_race_wife
replace raceth_wife_fixed=last_race_wife if raceth_wife_fixed==.

// realizing - I shouldn't do this this way becauase the head / wife can change over time (one reason that head / wife might seemingly change over time rather than data errors for the same person)

// if partners same race
gen same_race=0
replace same_race=1 if raceth_head==raceth_wife & raceth_head!=.

// religion
tabstat RELIGION_WIFE_ RELIGION_HEAD_, by(survey_yr) // just to get a sense of when asked to start.
label values RELIGION_WIFE_ RELIGION_HEAD_ . // these values are v wrong
/* head was 1970-1977, 1979-2021. wife was 1976, 1985-2021
Okay, but some weird things with how asked: 
In 1979, when this question was reinstated in the questionnaire, values were not brought forward for families with unchanged Heads since 1977.
For those cases with the same Heads from 1977 through the present, please use 1977 religious preference, V5617
So, most missings after 1977 can be interpreted as no new head, so use 1977 value? Is this another that might help if I edit once I have the variables assigned to the focal person?
Okay, but I *think* starting in 1985, was asked to everyone again? Because number of 0s goes down and the note is gone. okay, carried forward again starting 1986.
So carry through 1977-1984 if in sample and same head / partner?

The codes changed wildly over the years?
1970-1984 - 0: No or Other, 1: Baptist, 2: Methodist, 3: Episcopalian, 4: Presbyterian, 5: Lutheran, 6: Unitarian, Mormon, and related, 7: Other Protestant, 8: Catholic, 9: Jewish
1985-1987 - 0: None, 1: Roman Catholic, 2: Jewish, 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 10: Other non-Christian, 11: LDS, 12: Jehvah's Witnesses
13: Greek Orthodox, 14: "Christian", 15: Unitarian, 16: Christian Science, 17: 7th day Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 99: NA/DK
-- in 1987, the label specifically says None, atheist, agnostic
1988-1993 - 0: None, atheist, agnostic, 1: Roman Catholic, 2: Jewish, 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 10: Other non-Christian, 11: LDS, 12: Jehvah's Witnesses
13: Greek Orthodox, 14: "Christian", 15: Unitarian, 16: Christian Science, 17: 7th day Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 21: Church of God, 22: United Church of Christ, 23: Reformed, 24: Disciples of Christ, 25: CHurches of Christ, 97: Other, 99: NA/DK
-- so, up to 20 is the same as above, just added 21-25.
1994-2017 - 0: None, 1: Catholic, 2: Jewish, 8: Protestant unspecified, 10: Other non-Christian, 13: Greek Orthodox, 97: Other, 98: DK, 99: NA // so these large categories do match above in terms of coding (like 8 is the same, 13, etc. just way less groups)
-- In 1994, DENOMINATION was added as a separate question, so all of the detail goes to a separate question (which I don't believe I pulled in at the moment). so, I guess decide if that is worth adding.
2019-2021 - 0: Inapp (no partner), 1: None, 2: Atheist, 3: Agnostic, 4: Roman Catholic, 5: Greek Orthodox, 6: Baptist, 7: Episcopalian, 8: Jehovah's Witness, 9: Lutheran, 10: Methodist, 11: Pentecostal, 12: Presbyterian, 13: Protestant unspecified, 14: Christian, unspecified, 15: Christian, non-denominational, 16: Jewish, 17: Muslim, 18: Buddhist, 19: Other non-christian, 20: Other protestant, 21: LDS, 22: Unitarian, 23: Christian Science, 24: Adventist, 25: Amish, 26: Quaker, 27: Church of God, 28: United Church of Christ, 29: Reformed, 30: Disciples of Christ, 31: Churches of Christ, 97: Other, 98: DK, 99: NA
-- lol so DENOMINATION ends in 2017 and is integrated BACK to this question lord and the codes change AGAIN.

Denomination
1994-2017 - 0: None, atheist, agnostic, not Protestant OR no spouse (this is a lot in one), 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 11: LDS, 12: Jehovah's witness, 14: Christian, 15: Unitarian, 16: Christian Science, 17: Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 21: Church of God, 22: United Church of Christ, 23: Reformed, 24: Disciples of Christ, 25: CHurches of Christ, 97: Other, 98: DK, 99: NA
-- so, I think aligns with how asked 1985-1993. I think if I combine the two I actually get all the same codes 0-25 (that's why some are missing)

This might be helpful: https://www.pewresearch.org/religion/2015/05/12/appendix-b-classification-of-protestant-denominations/
https://en.wikipedia.org/wiki/Protestantism_in_the_United_States#Mainline_Protestantism
https://www.thegospelcoalition.org/blogs/trevin-wax/quick-guide-christian-denominations/ - the big three are Eastern Orthodox; Catholic; Protestant
https://truthandgracecounseling.com/understanding-the-difference-between-evangelical-and-mainline-protestant-churches/
https://woollyscreamsmiracle.wordpress.com/evangelical-vs-mainline-protestant-denominations-an-overview/
-- ideally could have evangelical Protestantism, mainline Protestantism and historically black Protestantism
-- okay no because these denominations spain mainline and evangelical in their classification
*/
tab DENOMINATION_HEAD_ RELIGION_HEAD_ if inrange(survey_yr,1994,2017), m col // want to clarify how these map on so I can decide what catgories to use. so all of these are protestant denominations??

browse unique_id survey_yr RELIGION_HEAD_ DENOMINATION_HEAD_ RELIGION_WIFE_ DENOMINATION_WIFE_

gen religion_head=.
replace religion_head=0 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==0 // no religion
replace religion_head=0 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==0
replace religion_head=0 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==0
replace religion_head=0 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,1,2,3)
replace religion_head=1 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==8 // catholic
replace religion_head=1 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==1
replace religion_head=1 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==1
replace religion_head=1 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==4
replace religion_head=2 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==9 // jewish
replace religion_head=2 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==2
replace religion_head=2 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==2
replace religion_head=2 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==16
replace religion_head=3 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==1 // baptist
replace religion_head=3 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==3
replace religion_head=3 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==8 & DENOMINATION_HEAD_==3
replace religion_head=3 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==6
replace religion_head=4 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,2,3,4,5) // mainline protestant
replace religion_head=4 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD_,4,5,6,7,22,23,24)
replace religion_head=4 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==8 & inlist(DENOMINATION_HEAD_,4,5,6,7,22,23,24)
replace religion_head=4 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,7,9,10,12,28,29,30)
// replace religion_head=5 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // evangelical protestant - none in first waves
replace religion_head=5 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD_,17,18,21,25)
replace religion_head=5 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==8 & inlist(DENOMINATION_HEAD_,17,18,21,25)
replace religion_head=5 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,11,24,27,31)
replace religion_head=6 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==7 // other protestant
replace religion_head=6 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD_,8,9,19,20)
replace religion_head=6 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==8 & inlist(DENOMINATION_HEAD_,8,9,19,20,97,98,99)
replace religion_head=6 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,13,20,25,26)
// replace religion_head=7 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // eastern orthodox
replace religion_head=7 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==13
replace religion_head=7 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==13
replace religion_head=7 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==5
replace religion_head=8 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==6 // other christian
replace religion_head=8 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD_,11,12,14,15,16)
replace religion_head=8 if inrange(survey_yr,1994,2017) & inlist(DENOMINATION_HEAD_,11,12,14,15,16)
replace religion_head=8 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,8,14,15,21,22,23)
// replace religion_head=9 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // other non-christian
replace religion_head=9 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==10
replace religion_head=9 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==10
replace religion_head=9 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,17,18,19)
// replace religion_head=10 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // other other
replace religion_head=10 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==97
replace religion_head=10 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==97
replace religion_head=10 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==97
// replace religion_head=. if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // missing
replace religion_head=. if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==99
replace religion_head=. if inrange(survey_yr,1994,2017) & inlist(RELIGION_HEAD_,98,99)
replace religion_head=. if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,98,99)

gen religion_wife=.
replace religion_wife=0 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==0 // no religion
replace religion_wife=0 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==0
replace religion_wife=0 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==0
replace religion_wife=0 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,1,2,3)
replace religion_wife=1 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==8 // catholic
replace religion_wife=1 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==1
replace religion_wife=1 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==1
replace religion_wife=1 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==4
replace religion_wife=2 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==9 // jewish
replace religion_wife=2 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==2
replace religion_wife=2 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==2
replace religion_wife=2 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==16
replace religion_wife=3 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==1 // baptist
replace religion_wife=3 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==3
replace religion_wife=3 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==8 & DENOMINATION_WIFE_==3
replace religion_wife=3 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==6
replace religion_wife=4 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,2,3,4,5) // mainline protestant
replace religion_wife=4 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE_,4,5,6,7,22,23,24)
replace religion_wife=4 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==8 & inlist(DENOMINATION_WIFE_,4,5,6,7,22,23,24)
replace religion_wife=4 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,7,9,10,12,28,29,30)
// replace religion_wife=5 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // evangelical protestant - none in first waves
replace religion_wife=5 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE_,17,18,21,25)
replace religion_wife=5 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==8 & inlist(DENOMINATION_WIFE_,17,18,21,25)
replace religion_wife=5 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,11,24,27,31)
replace religion_wife=6 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==7 // other protestant
replace religion_wife=6 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE_,8,9,19,20)
replace religion_wife=6 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==8 & inlist(DENOMINATION_WIFE_,8,9,19,20,97,98,99)
replace religion_wife=6 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,13,20,25,26)
// replace religion_wife=7 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // eastern orthodox
replace religion_wife=7 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==13
replace religion_wife=7 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==13
replace religion_wife=7 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==5
replace religion_wife=8 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==6 // other christian
replace religion_wife=8 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE_,11,12,14,15,16)
replace religion_wife=8 if inrange(survey_yr,1994,2017) & inlist(DENOMINATION_WIFE_,11,12,14,15,16)
replace religion_wife=8 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,8,14,15,21,22,23)
// replace religion_wife=9 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // other non-christian
replace religion_wife=9 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==10
replace religion_wife=9 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==10
replace religion_wife=9 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,17,18,19)
// replace religion_wife=10 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // other other
replace religion_wife=10 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==97
replace religion_wife=10 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==97
replace religion_wife=10 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==97
// replace religion_wife=. if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // missing
replace religion_wife=. if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==99
replace religion_wife=. if inrange(survey_yr,1994,2017) & inlist(RELIGION_WIFE_,98,99)
replace religion_wife=. if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,98,99)

label define religion 0 "No religion" 1 "Catholic" 2 "Jewish" 3 "Baptist" 4 "Mainline Protestant" 5 "Evangelical Protestant" 6 "Other Protestant" 7 "Eastern Orthodox" 8 "Other Christian" 9 "Other Non-Christian" 10 "Other Other"
label values religion_head religion_wife religion
tab religion_head, m
tab RELIGION_HEAD_ religion_head, m

tab religion_wife, m
tab RELIGION_WIFE_ religion_wife, m

// any children - need to get more specific; think I need to append childbirth history also?!
gen children=.
replace children=0 if NUM_CHILDREN_==0
replace children=1 if NUM_CHILDREN_>=1 & NUM_CHILDREN_!=.

bysort unique_id: egen children_ever = max(NUM_CHILDREN_)
replace children_ever=1 if children_ever>0

// use incremental births? okay come back to this with childbirth history
recode NUM_BIRTHS(98/99=.) // okay this doesn't increment GAH. it must be ever?
recode BIRTHS_T1_BOTH_(8/9=.)
recode BIRTHS_T1_HEAD_(8/9=.)
recode BIRTHS_T1_WIFE_(8/9=.)
// or if num children goes up AND age of youngest child is 1 (lol it is coded 1 for newborn up to second birthday in most years) aka unique_id 1003 in 1973?!

browse unique_id survey_yr SEX NUM_CHILDREN_ AGE_YOUNG_CHILD_  BIRTHS_T1_BOTH_ BIRTHS_T1_HEAD_ BIRTHS_T1_WIFE_  NUM_BIRTHS // okay so these are new births in last year, but not asksed until 1986 GAH

gen had_birth=0
replace had_birth=1 if NUM_CHILDREN_ == NUM_CHILDREN_[_n-1]+1 & AGE_YOUNG_CHILD_==1 & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

gen had_first_birth=0
replace had_first_birth=1 if had_birth==1 & (survey_yr==FIRST_BIRTH_YR | survey_yr==FIRST_BIRTH_YR+1) // think sometimes recorded a year late

gen had_first_birth_alt=0
replace had_first_birth_alt=1 if NUM_CHILDREN_==1 & NUM_CHILDREN_[_n-1]==0 & AGE_YOUNG_CHILD_==1 & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
browse unique_id survey_yr SEX NUM_CHILDREN_ AGE_YOUNG_CHILD_  had_birth had_first_birth had_first_birth_alt FIRST_BIRTH_YR

// also use FIRST_BIRTH_YR to say whether pre / post marital

// some age things
gen year_birth = survey_yr - AGE_INDV
// browse unique_id survey_yr SEX year_birth  AGE_INDV AGE_HEAD_ AGE_WIFE_

gen yr_born_head = survey_yr - AGE_HEAD_
gen yr_born_wife = survey_yr- AGE_WIFE_

gen age_mar_head = rel_start_yr -  yr_born_head
gen age_mar_wife = rel_start_yr -  yr_born_wife
browse unique_id survey_yr relationship SEX yr_born_head  yr_born_wife  year_birth AGE_INDV AGE_HEAD_ AGE_WIFE_ rel_start_yr age_mar_head age_mar_wife

save "$created_data/PSID_partners_cleaned.dta", replace
