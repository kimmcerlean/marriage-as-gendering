********************************************************************************
********************************************************************************
* Project: Marriage as Gendering
* Code owner: Kimberly McErlean
* Started: September 2024
* File name: c_match_partners.do
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file takes recoded individual data and matches partner data

********************************************************************************
* Create version of file for partner variables
********************************************************************************
use "$created_data/UKHLS_long_all_recoded.dta", clear

// just keep necessary variables

local partnervars "total_hours jbhrs jbot howlng aidhrs employed jbstat fimnlabgrs_dv nkids_dv age_youngest_child marital_status_legal marital_status_defacto partnered fihhmngrs_dv gor_dv xw_sex xw_memorig xw_sampst xw_racel_dv xw_anychild_dv xw_ethn_dv xw_xwdat_dv dob_year first_year_observed last_year_observed age_all hiqual_dv college_degree nchild_dv country_all rel_no current_rel_start_year current_rel_end_year current_rel_ongoing year_transitioned ivfio sampst hidp psu strata int_year istrtdaty year aidhh aidxhh husits hubuys hufrys huiron humops huboss year_first_birth rel_start_all rel_end_all status_all mh_*"

keep pidp survey wavename year `partnervars'

// rename them to indicate they are for spouse
foreach var in `partnervars'{
	rename `var' `var'_sp
}

// rename pidp to match the name I gave to partner pidp in main file to match
rename pidp partner_id // okay tried to update all to be pidp above - let's see if this will work

save "$temp/UKHLS_partner_lookup.dta", replace

********************************************************************************
** Now go back to original and merge on partner details
********************************************************************************
// first, restrict sample to those in relationships
use "$created_data/UKHLS_long_all_recoded.dta", clear

inspect partner_id if partnered==0 
inspect partner_id if partnered==1

keep if partnered==1
drop if partner_id==.
drop if rel_start_all==. // will get removed anyway with start date restrictions

merge m:1 partner_id wavename using "$temp/UKHLS_partner_lookup.dta"
drop if _merge==2

gen partner_match=0
replace partner_match=1 if _merge==3
drop _merge

drop if partner_match==0 // because need their info

// let's make sure the matching worked
sort hidp year
browse pidp wavename hidp marital_status_defacto partner_id partner_match total_hours total_hours_sp howlng howlng_sp
sort pidp year

drop sex // using cross-wave version and want to rename for ease
rename xw_sex sex
rename xw_sex_sp sex_sp

tab sex sex_sp, m
drop if sex==sex_sp
drop if sex==. | sex_sp==.

********************************************************************************
** Create couple-level variables
********************************************************************************
** First, create gendered versions of each variables

// earnings
gen gross_earnings_man = fimnlabgrs_dv if sex==1
replace gross_earnings_man = fimnlabgrs_dv_sp if sex==2

gen gross_earnings_woman = fimnlabgrs_dv if sex==2
replace gross_earnings_woman = fimnlabgrs_dv_sp if sex==1

// hours
gen weekly_hrs_man = total_hours if sex==1
replace weekly_hrs_man = total_hours_sp if sex==2

gen weekly_hrs_woman = total_hours if sex==2
replace weekly_hrs_woman = total_hours_sp if sex==1

// housework
gen housework_man = howlng if sex==1
replace housework_man = howlng_sp if sex==2

gen housework_woman = howlng if sex==2
replace housework_woman = howlng_sp if sex==1

** Division of labor variables

// earnings
egen couple_earnings_t = rowtotal(gross_earnings_man gross_earnings_woman)
gen female_earn_pct_t = gross_earnings_woman/(couple_earnings_t)

gen hh_earn_type_t=.
replace hh_earn_type_t=1 if female_earn_pct_t >=.4000 & female_earn_pct_t <=.6000
replace hh_earn_type_t=2 if female_earn_pct_t < .4000 & female_earn_pct_t >=0
replace hh_earn_type_t=3 if female_earn_pct_t > .6000 & female_earn_pct_t <=1
replace hh_earn_type_t=4 if gross_earnings_man==0 & gross_earnings_woman==0

label define hh_paid_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_earn_type_t hh_paid_type

// hours
egen couple_hours_t = rowtotal(weekly_hrs_man weekly_hrs_woman)
gen female_hours_pct_t = weekly_hrs_woman/couple_hours_t

gen hh_hours_type_t=.
replace hh_hours_type_t=1 if female_hours_pct_t >=.4000 & female_hours_pct_t <=.6000
replace hh_hours_type_t=2 if female_hours_pct_t <.4000
replace hh_hours_type_t=3 if female_hours_pct_t >.6000 & female_hours_pct_t!=.
replace hh_hours_type_t=4 if weekly_hrs_man==0 & weekly_hrs_woman==0

label values hh_hours_type_t hh_paid_type

tab hh_earn_type_t, m
tab hh_hours_type_t, m

// housework
egen couple_housework_t = rowtotal (housework_man housework_woman)
gen female_housework_pct_t = housework_woman / couple_housework_t

gen housework_bkt_t=.
replace housework_bkt_t=1 if female_housework_pct_t >=.4000 & female_housework_pct_t <=.6000
replace housework_bkt_t=2 if female_housework_pct_t >.6000 & female_housework_pct_t!=.
replace housework_bkt_t=3 if female_housework_pct_t <.4000
replace housework_bkt_t=1 if housework_man==0 & housework_woman==0 // na in dual bc so small

label define unpaid_bkt 1 "Dual HW" 2 "Female Primary" 3 "Male Primary" 4 "NA"
label values housework_bkt_t unpaid_bkt

tab housework_bkt_t, m
tab wavename housework_bkt_t, m row

** Other variables
// children
gen kids_in_hh=0
replace kids_in_hh=1 if nchild_dv > 0 | nchild_dv_sp > 0 // binary as to whether or not they have kids currently

tab nchild_dv nchild_dv_sp, m // pretty well aligned

** Relationship variables

// fix dates if don't match between partners
tab marital_status_defacto marital_status_defacto_sp, m // okay almost perfectly align
drop rel_start_all rel_end_all // let's do these here again once I update partner data; think this makes more sense

sort pidp year
browse pidp partner_id year marital_status_defacto ever_transition current_rel_start_year current_rel_end_year current_rel_start_year_sp current_rel_end_year_sp first_couple_year last_couple_year

gen rel_start_match = .
replace rel_start_match = 0 if current_rel_start_year!=current_rel_start_year_sp & current_rel_start_year!=. & current_rel_start_year_sp!=.
replace rel_start_match = 1 if current_rel_start_year==current_rel_start_year_sp & current_rel_start_year!=. & current_rel_start_year_sp!=.

gen rel_end_match = .
replace rel_end_match = 0 if current_rel_end_year!=current_rel_end_year_sp & current_rel_end_year!=. & current_rel_end_year_sp!=.
replace rel_end_match = 1 if current_rel_end_year==current_rel_end_year_sp & current_rel_end_year!=. & current_rel_end_year_sp!=.

tab rel_start_match, m
tab rel_end_match, m

gen current_rel_start = .
replace current_rel_start = current_rel_start_year if rel_start_match==1
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_start_year==first_couple_year
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_start_year_sp==first_couple_year
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_start_year!=. & current_rel_start_year_sp==.
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_start_year==. & current_rel_start_year_sp!=.
replace current_rel_start = current_rel_start_year if current_rel_start==. & current_rel_start_year < current_rel_start_year_sp
replace current_rel_start = current_rel_start_year_sp if current_rel_start==. & current_rel_start_year_sp < current_rel_start_year
replace current_rel_start = first_couple_year if current_rel_start==. & current_rel_start_year==. & current_rel_start_year_sp==.

inspect current_rel_start current_rel_start_year current_rel_start_year_sp

gen current_rel_end = .
replace current_rel_end = current_rel_end_year if rel_end_match==1
replace current_rel_end = current_rel_end_year if current_rel_end==. & current_rel_end_year==last_couple_year
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & current_rel_end_year_sp==last_couple_year
replace current_rel_end = current_rel_end_year if current_rel_end==. & current_rel_end_year!=. & current_rel_end_year_sp==.
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & current_rel_end_year==. & current_rel_end_year_sp!=.
replace current_rel_end = last_couple_year if current_rel_end==. & current_rel_end_year==. & current_rel_end_year_sp==.
replace current_rel_end = current_rel_end_year if current_rel_end==. & current_rel_end_year < current_rel_end_year_sp
replace current_rel_end = current_rel_end_year_sp if current_rel_end==. & current_rel_end_year_sp < current_rel_end_year

inspect current_rel_end current_rel_end_year current_rel_end_year_sp

browse pidp partner_id year marital_status_defacto ever_transition current_rel_start current_rel_end current_rel_start_year current_rel_end_year current_rel_start_year_sp current_rel_end_year_sp first_couple_year last_couple_year

// fix cohab / marriage dates (so continuous)
bysort pidp partner_id: egen rel_start_all = min(current_rel_start)
bysort pidp partner_id: egen rel_end_all = max(current_rel_end)

sort pidp year
browse pidp partner_id year marital_status_defacto ever_transition marr_trans rel_start_all rel_end_all current_rel_start current_rel_end

save "$created_data/ukhls_matched_couples.dta", replace
