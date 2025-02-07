********************************************************************************
********************************************************************************
* Project: Marriage as a Gendering Institution
* Owner: Kimberly McErlean
* Started: September 2024
* File: get_data
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files gets the data from the PSID and reorganizes it for analysis

********************************************************************************
* Take downloaded data, turn it into Stata format and rename
********************************************************************************
do "$PSID/J342304 (2021)/J342304.do" // note - this will need to be updated to wherever the raw data you downloaded is - this is directly provided by PSID
do "$PSID/J342304 (2021)/J342304_formats.do" // note - this will need to be updated to wherever the raw data you downloaded is - this is directly provided by PSID - also need to direct this file where to save: "$PSID\PSID_full.dta"
do "$code/00_rename_vars.do"

********************************************************************************
* Import data and reshape so it's long
********************************************************************************
use "$PSID/PSID_full_renamed.dta", clear
browse X1968_PERSON_NUM_1968 X1968_INTERVIEW_NUM_1968 // 30001 = interview; 30002 = person number

rename X1968_PERSON_NUM_1968 main_per_id
rename X1968_INTERVIEW_NUM_1968 main_fam_id

gen unique_id = (main_fam_id*1000) + main_per_id // (ER30001 * 1000) + ER30002
browse main_per_id main_fam_id unique_id

// want to see if I can make this time-fixed sample status
recode main_fam_id (1/2999 = 1 "SRC cross-section") (3001/3441 = 2 "Immigrant 97") (3442/3511 = 3 "Immigrant 99") (4001/4851 = 4 "Immigrant 17/19") (5001/6999  = 5 "1968 Census") (7001/9043 = 6 "Latino 90") (9044/9308 = 7 "Latino 92"), gen(sample_type) 
/* from FAQ:
You will need to look at the 1968 family interview number available in the individual-level files (variable ER30001).
SRC sample families have values less than 3000.
SEO sample families have values greater than 5000 and less than 7000.
Immigrant sample families have values greater than 3000 and less than 5000. (Values from 3001 to 3441 indicate that the original family was first interviewed in 1997; values from 3442 to 3511 indicate the original family was first interviewed in 1999; values from 4001-4851 indicate the original family was first interviewed in 2017; values from 4700-4851 indicate the original family was first interviewed in 2019.)
Latino sample families have values greater than 7000 and less than 9309. (Values from 7001 to 9043 indicate the original family was first interviewed in 1990; values from 9044 to 9308 indicate the original family was first interviewed in 1992.)
*/
tab sample_type, m

// and if original sample or not
tab SAMPLE,m
tab SAMPLE_STATUS_TYPE, m

/* from FAQ
okay, so this is covered already in sample variable
Follow status indicates whether we are interested in continuing to interview an individual. In general, sample members are always considered Followable. Non-Sample Members can be Followable too, if they represent a population of current interest. For example, we have in the past, followed such people as Non-Sample parents of sample children who were aged 25 or younger.

You can tell who is a sample member by looking at the individual's Person Number and Follow Status. Original Sample Members who were living in the original study FU in the first year of interviewing were given Person Numbers in the range of 001-019. Any Reference Person's (the term `Reference Person' has replaced `Head' in 2017) Spouse in the original interviewing year who was living in an institution was given a Person Number of 020. In addition, children of the Reference Person (and Spouse/Partner, if present) who were under age 25 and in an institution the first year were considered Original Sample members and given Person Numbers in the range 0021-029. All of these people are followable.

Individuals who were born into a sample family after the first interviewing year and have a sample parent are considered "born-in Sample Members" and receive Person Numbers in the range of 030-169. All born in sample members are followable.

Some individuals who qualify as sample members (because they have a sample parent) are not born into a study family, but move in later. These "Moved in Sample Members" have Person Numbers of 170 or greater and are Followable.

All other people who have ever lived in a PSID family are not sample individuals. They also receive Person Numbers of 170 or greater, but are not Followable.
*/

merge 1:1 unique_id using "$PSID/strata.dta", keepusing(stratum cluster)
drop if _merge==2
drop _merge

gen id=_n

local reshape_vars "RELEASE_ X1968_PERSON_NUM_ INTERVIEW_NUM_ RELATION_ AGE_INDV_ MARITAL_PAIRS_ MOVED_ YRS_EDUCATION_INDV_ TYPE_OF_INCOME_ TOTAL_MONEY_INCOME_ ANNUAL_HOURS_T1_INDV_ RELEASE_NUM2_ FAMILY_COMPOSITION_ AGE_HEAD_ AGE_WIFE_ SEX_HEAD_ AGE_YOUNG_CHILD_ RESPONDENT_WHO_ RACE_1_HEAD_ EMPLOY_STATUS_HEAD_ MARST_DEFACTO_HEAD_ WIDOW_LENGTH_HEAD_ WAGES_T1_HEAD_ FAMILY_INTERVIEW_NUM_ FATHER_EDUC_HEAD_ HRLY_RATE_T1_HEAD_ HRLY_RATE_T1_WIFE_ REGION_ NUM_CHILDREN_ CORE_WEIGHT_ ANNUAL_HOURS_T1_HEAD_ ANNUAL_HOURS_T1_WIFE_ LABOR_INCOME_T1_HEAD_ LABOR_INCOME_T1_WIFE_ TOTAL_INCOME_T1_FAMILY_ TAXABLE_T1_HEAD_WIFE_ EDUC1_HEAD_ EDUC1_WIFE_ WEEKLY_HRS1_T1_WIFE_ WEEKLY_HRS1_T1_HEAD_ TOTAL_HOUSEWORK_T1_HW_ RENT_COST_V1_ MORTGAGE_COST_ HOUSE_VALUE_ HOUSE_STATUS_ VEHICLE_OWN_ POVERTY_THRESHOLD_ SEQ_NUMBER_ RESPONDENT_ FAMILY_ID_SO_ COMPOSITION_CHANGE_ NEW_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_HEAD_ MOST_HOUSEWORK_T1_ AGE_OLDEST_CHILD_ FOOD_STAMPS_ HRLY_RATE_CURRENT_HEAD_ RELIGION_HEAD_ CHILDCARE_COSTS_ TRANSFER_INCOME_ WELFARE_JOINT_ NEW_WIFE_ FATHER_EDUC_WIFE_ MOTHER_EDUC_WIFE_ MOTHER_EDUC_HEAD_ TYPE_TAXABLE_INCOME_ TOTAL_INCOME_T1_INDV_ COLLEGE_HEAD_ COLLEGE_WIFE_ EDUC_HEAD_ EDUC_WIFE_ SALARY_TYPE_HEAD_ FIRST_MARRIAGE_YR_WIFE_ RELIGION_WIFE_ WORK_MONEY_WIFE_ EMPLOY_STATUS_WIFE_ SALARY_TYPE_WIFE_ HRLY_RATE_CURRENT_WIFE_ RESEPONDENT_WIFE_ WORK_MONEY_HEAD_ MARST_LEGAL_HEAD_ EVER_MARRIED_HEAD_ EMPLOYMENT_INDV_ STUDENT_T1_INDV_ BIRTH_YR_INDV_ COUPLE_STATUS_HEAD_ OTHER_ASSETS_ STOCKS_MF_ WEALTH_NO_EQUITY_ WEALTH_EQUITY_ VEHICLE_VALUE_ RELATION_TO_HEAD_ NUM_MARRIED_HEAD_ FIRST_MARRIAGE_YR_HEAD_ FIRST_MARRIAGE_END_HEAD_ FIRST_WIDOW_YR_HEAD_ FIRST_DIVORCE_YR_HEAD_ FIRST_SEPARATED_YR_HEAD_ LAST_MARRIAGE_YR_HEAD_ LAST_WIDOW_YR_HEAD_ LAST_DIVORCE_YR_HEAD_ LAST_SEPARATED_YR_HEAD_ FAMILY_STRUCTURE_HEAD_ RACE_2_HEAD_ NUM_MARRIED_WIFE_ FIRST_MARRIAGE_END_WIFE_ FIRST_WIDOW_YR_WIFE_ FIRST_DIVORCE_YR_WIFE_ FIRST_SEPARATED_YR_WIFE_ LAST_MARRIAGE_YR_WIFE_ LAST_WIDOW_YR_WIFE_ LAST_DIVORCE_YR_WIFE_ LAST_SEPARATED_YR_WIFE_ FAMILY_STRUCTURE_WIFE_ RACE_1_WIFE_ RACE_2_WIFE_ STATE_ BIRTHS_T1_HEAD_ BIRTHS_T1_WIFE_ BIRTHS_T1_BOTH_ WELFARE_HEAD_1_ WELFARE_WIFE_1_ LABOR_INCOME_T1_INDV_ RELEASE_NUM_ WAGES_CURRENT_HEAD_ WAGES_CURRENT_WIFE_ WAGES_T1_WIFE_ RENT_COST_V2_ DIVIDENDS_HEAD_ DIVIDENDS_WIFE_ WELFARE_HEAD_2_ WELFARE_WIFE_2_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_ RACE_3_WIFE_ RACE_3_HEAD_ WAGES_ALT_T1_HEAD_ WAGES_ALT_T1_WIFE_ WEEKLY_HRS_T1_HEAD_ WEEKLY_HRS_T1_WIFE_ RACE_4_HEAD_ COR_IMM_WT_ ETHNIC_WIFE_ ETHNIC_HEAD_ CROSS_SECTION_FAM_WT_ LONG_WT_ CROSS_SECTION_WT_ CDS_ELIGIBLE_ TOTAL_HOUSING_ HEALTH_INSURANCE_FAM_ BANK_ASSETS_ LABOR_INC_J1_T1_HEAD_ TOTAL_WEEKS_T1_HEAD_ ANNUAL_HOURS2_T1_HEAD_ LABOR_INC_J1_T1_WIFE_ TOTAL_WEEKS_T1_WIFE_ ANNUAL_HOURS2_T1_WIFE_ WEEKLY_HRS_T2_HEAD_ LABOR_INC_J2_T1_HEAD_ LABOR_INC_J3_T1_HEAD_ LABOR_INC_J4_T1_HEAD_ LABOR_INC_J2_T1_WIFE_ LABOR_INC_J3_T1_WIFE_ LABOR_INC_J4_T1_WIFE_ DIVIDENDS_JOINT_ INTEREST_JOINT_ NUM_JOBS_T1_INDV_ BACHELOR_YR_INDV_ STUDENT_CURRENT_INDV_ COLLEGE_INDV_ SEX_WIFE_ BACHELOR_YR_WIFE_ ENROLLED_WIFE_ BACHELOR_YR_HEAD_ ENROLLED_HEAD_ WAGES2_T1_WIFE_ METRO_ CURRENTLY_WORK_HEAD_ CURRENTLY_WORK_WIFE_ EMPLOY_STATUS_T2_HEAD_ EMPLOY_STATUS_T2_WIFE_ WEEKLY_HRS_T2_WIFE_ START_YR_EMPLOYER_HEAD_ START_YR_EMPLOYER_WIFE_ START_YR_CURRENT_HEAD_ START_YR_CURRENT_WIFE_ START_YR_PREV_HEAD_ START_YR_PREV_WIFE_ YRS_CURRENT_EMPLOY_HEAD_ YRS_CURRENT_EMPLOY_WIFE_ LABOR_INCOME_T2_HEAD_ LABOR_INCOME_T2_WIFE_ WEEKLY_HRS_T2_INDV_ LABOR_INCOME_T2_INDV_ HOUSEWORK_INDV_ RACE_4_WIFE_ HISPANICITY_WIFE_ HISPANICITY_HEAD_ CHILDCARE_HEAD_ CHILDCARE_WIFE_ ADULTCARE_HEAD_ ADULTCARE_WIFE_ TOTAL_INCOME_T2_FAMILY_ WEEKS_WORKED_T2_INDV_ FOLLOW_STATUS_ NUM_IN_HH_ NUM_NONFU_IN_HH_ NEW_WIFE_YEAR_ MOVED_YEAR_ MOVED_MONTH_ SPLITOFF_YEAR_ SPLITOFF_MONTH_ DATA_RECORD_TYPE_ SPLITOFF_ NEW_HEAD_YEAR_ HS_GRAD_HEAD_ ATTENDED_COLLEGE_HEAD_ HIGHEST_DEGREE_HEAD_ HS_GRAD_WIFE_ ATTENDED_COLLEGE_WIFE_ HIGHEST_DEGREE_WIFE_ WHERE_EDUC_HEAD_ FOREIGN_DEG_HEAD_ WHERE_EDUC_WIFE_ FOREIGN_DEG_WIFE_ YR_EDUC_UPD_HEAD_ YR_EDUC_UPD_WIFE_ DENOMINATION_HEAD_ DENOMINATION_WIFE_"

reshape long `reshape_vars', i(id unique_id sample_type stratum cluster) j(survey_yr)

save "$temp/PSID_full_long.dta", replace
