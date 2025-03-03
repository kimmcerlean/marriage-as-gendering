********************************************************************************
********************************************************************************
* Project: Marriage as Gendering
* Owner: Kimberly McErlean
* Started: March 2025
* File: match_partners
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file takes recoded individual data and matches partner data

********************************************************************************
* Create version of file for partner variables
********************************************************************************
use "$created_data/gsoep_individuals_recoded.dta", clear

local keepvars "sex_pl age birthyr_pl marst_defacto partnered_total employed_binary laborforce_pg emplst_pg weekly_work_hrs gross_income_lm net_income_lm housework_daily_avg housework_weekdays housework_saturdays housework_sundays childcare_daily_avg childcare_weekdays childcare_saturdays childcare_sundays last_degree_pg college_type_pg isced97_pg where_germany_pl num_children_hl num_children_u16_hl firstyr_survey_pl lastyr_survey_pl psample_pl rel_no current_rel_start_year current_rel_end_year"

keep pid syear `keepvars'

foreach var in `keepvars'{
	rename `var' `var'_sp
}

rename pid partner_id_pl

save "$temp/partner_lookup.dta", replace

********************************************************************************
** Now go back to original and merge on partner details
********************************************************************************
// first, restrict sample to those in relationships
use "$created_data/gsoep_individuals_recoded.dta", clear

inspect partner_id_pl if partnered_total==0
inspect partner_id_pl if partnered_total==1

keep if partnered_total==1
drop if inlist(partner_id_pl,.,.n,.s)

// first, get spouse sample status to check next match
merge 1:1 partner_id_pl syear using "$temp/ppathl_partnerstatus.dta", keepusing(status_sp)
drop if _merge==2
drop _merge

// now, merge on partner characteristics
merge 1:1 partner_id_pl syear using "$temp/partner_lookup.dta"
drop if _merge==2

tab status_sp _merge, m // yes - all of the master only are no interview
drop _merge

sort pid syear
browse pid partner_id_pl syear partnered_total marst_defacto status_sp sex_pl sex_pl_sp

fre status_sp
keep if status_sp == 1 // so only keep if in sample

tab sex_pl sex_pl_sp, m
drop if sex_pl == sex_pl_sp
drop if sex_pl ==. | sex_pl_sp == .

********************************************************************************
** Create couple-level variables
********************************************************************************
** First, create gendered versions of each variables
// earnings
gen gross_earnings_man = gross_income_lm if sex_pl==1
replace gross_earnings_man = gross_income_lm_sp if sex_pl==2

gen gross_earnings_woman = gross_income_lm if sex_pl==2
replace gross_earnings_woman = gross_income_lm_sp if sex_pl==1

gen net_earnings_man = net_income_lm if sex_pl==1
replace net_earnings_man = net_income_lm_sp if sex_pl==2

gen net_earnings_woman = net_income_lm if sex_pl==2
replace net_earnings_woman = net_income_lm_sp if sex_pl==1

// hours
gen weekly_hrs_man = weekly_work_hrs if sex_pl==1
replace weekly_hrs_man = weekly_work_hrs_sp if sex_pl==2

gen weekly_hrs_woman = weekly_work_hrs if sex_pl==2
replace weekly_hrs_woman = weekly_work_hrs_sp if sex_pl==1

// housework
gen hw_weekdays_man = housework_weekdays if sex_pl==1
replace hw_weekdays_man = housework_weekdays_sp if sex_pl==2

gen hw_weekdays_woman = housework_weekdays if sex_pl==2
replace hw_weekdays_woman = housework_weekdays_sp if sex_pl==1

gen hw_avg_man = housework_daily_avg if sex_pl==1
replace hw_avg_man = housework_daily_avg_sp if sex_pl==2

gen hw_avg_woman = housework_daily_avg if sex_pl==2
replace hw_avg_woman = housework_daily_avg_sp if sex_pl==1

// childcare
gen cc_weekdays_man = childcare_weekdays if sex_pl==1
replace cc_weekdays_man = childcare_weekdays_sp if sex_pl==2

gen cc_weekdays_woman = childcare_weekdays if sex_pl==2
replace cc_weekdays_woman = childcare_weekdays_sp if sex_pl==1

gen cc_avg_man = childcare_daily_avg if sex_pl==1
replace cc_avg_man = childcare_daily_avg_sp if sex_pl==2

gen cc_avg_woman = childcare_daily_avg if sex_pl==2
replace cc_avg_woman = childcare_daily_avg_sp if sex_pl==1

** Division of labor variables

// earnings
egen couple_earnings_gross = rowtotal(gross_earnings_man gross_earnings_woman)
gen female_earn_pct_gr = gross_earnings_woman/(couple_earnings_gross)

egen couple_earnings_net = rowtotal(net_earnings_man net_earnings_woman)
gen female_earn_pct_net = net_earnings_woman/(couple_earnings_net)

browse pid partner_id_pl syear couple_earnings_gross gross_earnings_man gross_earnings_woman female_earn_pct_gr couple_earnings_net net_earnings_man net_earnings_woman female_earn_pct_net

tabstat female_earn_pct_gr female_earn_pct_net, stats(mean sd p50 count) // so means are similar, median net is lower. but net is real? so use net for now? though uk is gross I think?

gen hh_earn_type_t=.
replace hh_earn_type_t=1 if female_earn_pct_net >=.4000 & female_earn_pct_net <=.6000
replace hh_earn_type_t=2 if female_earn_pct_net < .4000 & female_earn_pct_net >=0
replace hh_earn_type_t=3 if female_earn_pct_net > .6000 & female_earn_pct_net <=1
replace hh_earn_type_t=4 if net_earnings_man==0 & net_earnings_woman==0

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
egen couple_housework_weekday = rowtotal (hw_weekdays_man hw_weekdays_woman)
egen couple_housework_avg = rowtotal (hw_avg_man hw_avg_woman)

gen female_housework_pct_wk = hw_weekdays_woman / couple_housework_weekday
gen female_housework_pct_avg = hw_avg_woman / couple_housework_avg

tabstat female_housework_pct_wk female_housework_pct_avg, stats(mean sd p50 count) // quite similar, average has more values non-missing. let's use that for now

gen housework_bkt_t=.
replace housework_bkt_t=1 if female_housework_pct_avg >=.4000 & female_housework_pct_avg <=.6000
replace housework_bkt_t=2 if female_housework_pct_avg >.6000 & female_housework_pct_avg!=.
replace housework_bkt_t=3 if female_housework_pct_avg <.4000
replace housework_bkt_t=1 if hw_avg_man==0 & hw_avg_woman==0 // na in dual bc so small

label define unpaid_bkt 1 "Dual HW" 2 "Female Primary" 3 "Male Primary" 4 "NA"
label values housework_bkt_t unpaid_bkt

tab housework_bkt_t, m

// childcare
egen couple_childcare_weekday = rowtotal (cc_weekdays_man cc_weekdays_woman)
egen couple_childcare_avg = rowtotal (cc_avg_man cc_avg_woman)

gen female_childcare_pct_wk = cc_weekdays_woman / couple_childcare_weekday
gen female_childcare_pct_avg = cc_avg_woman / couple_childcare_avg

tabstat female_childcare_pct_wk female_childcare_pct_avg, stats(mean sd p50 count)

gen childcare_bkt_t=.
replace childcare_bkt_t=1 if female_childcare_pct_avg >=.4000 & female_childcare_pct_avg <=.6000
replace childcare_bkt_t=2 if female_childcare_pct_avg >.6000 & female_childcare_pct_avg!=.
replace childcare_bkt_t=3 if female_childcare_pct_avg <.4000
replace childcare_bkt_t=4 if cc_avg_man==0 & cc_avg_woman==0 // here will leave NA split out

label values childcare_bkt_t unpaid_bkt

tab childcare_bkt_t, m

** Relationship variables
// fix dates if don't match between partners
bysort pid partner_id_pl: egen first_couple_year = min(syear)
bysort pid partner_id_pl: egen last_couple_year = max(syear)

browse pid partner_id_pl syear current_rel_start_year current_rel_start_year_sp current_rel_end_year current_rel_end_year_sp
gen rel_start_match = .
replace rel_start_match = 0 if current_rel_start_year!=current_rel_start_year_sp & current_rel_start_year!=. & current_rel_start_year_sp!=.
replace rel_start_match = 1 if current_rel_start_year==current_rel_start_year_sp & current_rel_start_year!=. & current_rel_start_year_sp!=.

gen rel_end_match = .
replace rel_end_match = 0 if current_rel_end_year!=current_rel_end_year_sp & current_rel_end_year!=. & current_rel_end_year_sp!=.
replace rel_end_match = 1 if current_rel_end_year==current_rel_end_year_sp & current_rel_end_year!=. & current_rel_end_year_sp!=.

tab rel_start_match, m
tab rel_end_match, m

sort pid partner_id_pl syear
browse pid partner_id_pl syear current_rel_start_year current_rel_start_year_sp current_rel_end_year current_rel_end_year_sp first_couple_year last_couple_year if rel_start_match==0 | rel_start_match==0 

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
browse pid partner_id_pl syear current_rel_start_year current_rel_start_year_sp  first_couple_year if current_rel_start==.
browse pid partner_id_pl syear current_rel_start current_rel_start_year current_rel_start_year_sp  first_couple_year

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
browse pid partner_id_pl syear current_rel_end_year current_rel_end_year_sp  last_couple_year if current_rel_end==.

browse pid partner_id_pl syear current_rel_start current_rel_start_year current_rel_start_year_sp current_rel_end current_rel_end_year current_rel_end_year_sp

// fix cohab / marriage dates (so continuous)
bysort pid partner_id_pl: egen rel_start_all = min(current_rel_start)
bysort pid partner_id_pl: egen rel_end_all = max(current_rel_end)

browse pid partner_id_pl syear marst_defacto ever_transition marr_trans rel_start_all rel_end_all current_rel_start current_rel_end
 
save "$created_data/gsoep_matched_couples.dta", replace

// eventually: keep after certain start date. keep only certain ages. deduplicate.
