********************************************************************************
********************************************************************************
* Project: Marriage as Gendering
* Owner: Kimberly McErlean
* Started: March 2025
* File: individual_recodes
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files gets the merged individual-level data and makes so relevant new variables

use "$temp/individ_data.dta", clear
label language EN

********************************************************************************
* DoL and Demographic Variables
********************************************************************************
// Nneed to do some variable cleanup / recoding / inspecting for core variables before merging with partner info
// per codebook: "the plausibility [of pgen] is validated longitudinally, making the data superior to pl in most situations"

// employment
* Status
label define employment 1 "FT" 2 "PT" 3 "Training" 4 "Marginally employed" 5 "PT retired" ///
6 "Military (vol)" 7 "FSJ/FOeJ/BFD" 8 "Disabled" 9 "Not employed" 10 "Internship" 11 "Military (comp)" 12 "short-time work"
label values employment employment
tab employment, m // harmonized employment variable (labels currently in German)

tab emplst_pg, m
tab emplst_pg employment, m // okay these correspond quite well. use pg version bc a. simpler and b. in English. can probably recode further also
tab laborforce_pg, m // this is more why not working.
tab emplst_pg laborforce_pg, m // so really a breakdown of 5 above

gen employed_binary=.
replace employed_binary = 0 if inlist(emplst_pg,5,6)
replace employed_binary = 1 if inlist(emplst_pg,1,2,3,4,7)  // hmm in labor force, they consider vocational training as employed. let's see if work hours recorded

tab emplst_pg employed_binary, m
tab laborforce_pg employed_binary, m row

* Work hours
inspect hours_per_wk // Actual hours per week, including overtime
inspect work_hours_pg // actual work time per week
inspect work_hours_agreed_pg // agreed upon work time
// note, for all of these, I believe missing if not employed, so want to replace those with 0. (the .n specifically I think not just .?)

tabstat hours_per_wk work_hours_pg, stats(mean p25 p50 p75 sd n) // okay, these are generally quite similar. the latter has less missing

inspect work_ot // do you work overtime (1997-2021)
inspect hours_ot // hours of overtime last month (all except 1984, 1985, 1987)
inspect work_ot_lastmonth // did you work OT last month same as above
inspect overtime_pg // overtime per week

inspect working_weekdays_v1 // 1984-1991. tine spent working - not sure if comparable to hours per week, so will check. okay this is daily work hours, not weekly
inspect working_weekdays_v2 // 1990
inspect working_weekdays_v3 // 1990
inspect working_weekdays_v4 // 1992-2021

browse pid sex_pl syear partnered_total employed_binary emplst_pg hours_per_wk hours_ot work_hours_pg work_hours_agreed_pg overtime_pg working_weekdays_v1 working_weekdays_v2 working_weekdays_v3 working_weekdays_v4

tab work_hours_pg emplst_pg, m
tab work_hours_pg employed_binary, m col

gen weekly_work_hrs = .
replace weekly_work_hrs = 0 if employed_binary==0
replace weekly_work_hrs = work_hours_pg if employed_binary==1 & work_hours_pg!=.
replace weekly_work_hrs = hours_per_wk if employed_binary==1 & weekly_work_hrs==. & hours_per_wk!=.
tab weekly_work_hrs employed_binary, m col

* Earnings
inspect gross_income_lastmonth // gross income last month (harmonized - all)
inspect gross_labor_inc_pg // imputed gross labor income in the previous month generated for all SOEP respondents who are employed in a main job in each respective wave.  (all waves)
inspect net_labor_inc_pg // imputed net labor income in the previous month in a main job of all persons in SOEP working in the respective wave. (all waves)
inspect gross_salary_ly	// gross amount of wages, salary prev. yr (1990-2021)
inspect net_wages_ly // net amount of wages, salary prev. year (2000-2020) - mostly missing
inspect gross_salary2_ly // gross amount 2nd job, previous year (1990-2021) - mostly missing

tabstat gross_income_lastmonth gross_labor_inc_pg net_labor_inc_pg gross_salary_ly, by(employed_binary) stats(mean p25 p50 p75 sd n) // okay, the two versions of gross income are generally quite similar. ofc, net income is smaller. the two pg versions have much better coverage, but the two gross align when both there

browse pid sex_pl syear partnered_total employed_binary emplst_pg gross_income_lastmonth  gross_labor_inc_pg net_labor_inc_pg gross_salary_ly net_wages_ly gross_salary2_ly

gen gross_income_lm = .
replace gross_income_lm = 0 if employed_binary==0 // same thing, want earnings to be 0 if not employed
replace gross_income_lm = gross_labor_inc_pg if employed_binary==1

gen net_income_lm = .
replace net_income_lm = 0 if employed_binary==0
replace net_income_lm = net_labor_inc_pg if employed_binary==1

tabstat weekly_work_hrs gross_income_lm net_income_lm, by(employed_binary)

// unpaid labor
* Housework
browse pid sex_pl syear partnered_total housework_weekdays housework_saturdays housework_sundays repair_weekdays leisure_weekdays errands_weekdays // note that weekdays have been asked always, sat and sun inconsistent and generally in odd years only
// then there are some specific categories: repair_weekdays, leisure_weekdays, errands_weekdays (also have sat and sunday versions)
tab housework_weekdays, m // about 2.6% missing
tab housework_saturdays, m // more than half missing
tab housework_sundays, m // just under half missing
tab repair_weekdays, m // generally good coverage
tab leisure_weekdays, m // generally good coverage
tab errands_weekdays, m // less coverage

egen housework_daily_avg = rowmean(housework_weekdays housework_saturdays housework_sundays)

browse pid sex_pl syear partnered_total housework_daily_avg housework_weekdays housework_saturdays housework_sundays repair_weekdays leisure_weekdays errands_weekdays 
tabstat housework_daily_avg housework_weekdays housework_saturdays housework_sundays

*Carework
tab childcare_weekdays, m // about 2.5% missing
tab childcare_saturdays, m
tab childcare_sundays, m

egen childcare_daily_avg = rowmean(childcare_weekdays childcare_saturdays childcare_sundays)
tabstat childcare_daily_avg childcare_weekdays childcare_saturdays childcare_sundays // childcare is higher on weekends

browse pid sex_pl syear partnered_total childcare_daily_avg childcare_weekdays childcare_saturdays childcare_sundays // childcare weekdays always since 1985, again sat and sun inconsistent.
// lots of 0s, but I don't yet have child / hh comp / birth variables to see if correlated

// Other demographics
* Age
tab birthyr_pl, m // dob
tab age, m

* Education
fre last_degree_pg // school leaving degree. most comprehensive
fre college_type_pg  // type of college degree (doesn't have clear relationship to above, but is 80% missing bc inapplicable)
// mostly if last degree is upper secondary or isced is higher education
fre isced11_pg // 55% missing
fre isced97_pg // <2% missing
tab last_degree_pg isced97_pg, m
tab isced97_pg college_type_pg, m // okay, yes, if isced is higher ed, then this is filled in

browse pid age last_degree_pg college_type_pg isced97_pg isced11_pg // will use isced as education for now, but not really sure what to do about education cross-nationally...

* Race / ethnicity / nativity (or similar)
tab syear where_germany_pl, m // this is at least east v. west germany. Analyze separately? Only about 20% East because only available after 1989
unique pid, by(where_germany_pl)

* HH comp
// realizing I currently have no variables about this like kids in HH, etc. look if hl or other file is needed GAH. oh maybe biol actually
merge m:1 hid syear using "$temp/hl_cleaned.dta"
drop if _merge==2
drop _merge

tab num_children_hl, m
replace num_children_hl = 0 if num_children_hl==.n 

tab childcare_weekdays num_children_u16_hl, m col // so yeah, 92% of "No"s are 0 for childcare

* Sample info
browse pid syear firstyr_contact_pl firstyr_survey_pl lastyr_contact_pl lastyr_survey_pl
fre firstyr_contact_pl // Year First Contacted, Netto=10-99
fre firstyr_survey_pl // Year First Surveyed, Netto=10-99
fre lastyr_contact_pl // Year Of Last Contact, Netto=10-99 // oh I guess these might differ if they kept trying to contact after non-response? but survey is when they actually have info
fre lastyr_survey_pl // Year Of Last Survey, Netto=10-99
fre psample_pl	// which sample are they a part of - this will be good for imputation.
fre status_detailed_pl // I am not sure if I just have interview in my sample atm? like, ppathl might have non-response that will be useful info?
fre status_pl // yeah bc 99%+ here are interview (I think because I used pl as my base). so, not everyone has the same amount of waves right now

// save "$created_data/gsoep_individuals_recoded.dta", replace // temp save, want to see if I can get HH characteristics

********************************************************************************
* Marital History (at least to get current relationship start date)
********************************************************************************
// let's do my normal relationship transitions
sort pid syear

*enter
gen rel_start=0
replace rel_start=1 if partnered_total==1 & partnered_total[_n-1]==0 & pid==pid[_n-1] & syear==syear[_n-1]+1

*exit
gen rel_end=0
replace rel_end=1 if partnered_total==0 & partnered_total[_n-1]==1 & pid==pid[_n-1] & syear==syear[_n-1]+1

gen rel_end_pre=0
replace rel_end_pre=1 if partnered_total==1 & partnered_total[_n+1]==0 & pid==pid[_n+1] & syear==syear[_n+1]-1

*cohab to marr
gen marr_trans=0
replace marr_trans=1 if marst_defacto==1 & marst_defacto[_n-1]==2 & pid==pid[_n-1] & syear==syear[_n-1]+1 & partner_id_pl==partner_id_pl[_n-1]

bysort pid partner_id_pl: egen ever_transition = max(marr_trans)
gen year_transitioned = . 
replace year_transitioned = syear if marr_trans==1
bysort pid partner_id_pl (year_transitioned): replace year_transitioned = year_transitioned[1] if ever_transition == 1
tab year_transitioned ever_transition, m

sort pid syear

tab rel_start chg_newpartner, m
tab rel_start chg_married, m
tab rel_start chg_cohabit, m

browse pid partner_id_pl syear partnered_total marst_defacto rel_start rel_end_pre marr_trans ever_transition year_transitioned chg_newpartner chg_newpartner_month chg_newpartner_month_py chg_married chg_married_month chg_married_month_py chg_cohabit chg_cohabit_month chg_cohabit_month_py

// merge on to my crudely created partner history
merge m:1 pid using "$temp/biocouplm_wide.dta"
drop if _merge==2
drop _merge

browse pid partner_id_pl syear partnered_total marst_defacto rel_start rel_end_pre marr_trans ever_transition year_transitioned mh_rel_type1 mh_beginy1 mh_endy1 mh_rel_type2 mh_beginy2 mh_endy2
tab mh_beginy1  // one problem - this doesn't go back in time, so earliest start date is 1984 GAH. okay, this is fine for now, just will need to update I think.

gen rel_no = .
forvalues m=1/25{
	replace rel_no = `m' if mh_rel_type`m'==marst_defacto & inlist(marst_defacto,1,2) & syear>=mh_beginy`m' & syear<=mh_endy`m'
	replace rel_no = `m' if rel_no==. & mh_rel_type`m'==. & inlist(marst_defacto,1,2) & syear>=mh_beginy`m' & syear<=mh_endy`m'
}

tab rel_no marst_defacto, m col

gen current_rel_start_year=.
gen current_rel_end_year=.

forvalues m=1/25{
	replace current_rel_start_year = mh_beginy`m' if rel_no==`m'
	replace current_rel_end_year = mh_endy`m' if rel_no==`m'
}

browse pid partner_id_pl syear partnered_total marst_defacto rel_no current_rel_start_year current_rel_end_year rel_start rel_end_pre marr_trans ever_transition year_transitioned mh_rel_type1 mh_beginy1 mh_endy1 mh_rel_type2 mh_beginy2 mh_endy2  mh_rel_type3 mh_beginy3 mh_endy3

tab current_rel_start_year marst_defacto, m col
tab current_rel_end_year marst_defacto, m col

// gah do I also want to add on fertility history? (see file biol)
// hold off on this for the moment, need to make progress for gender workshop

save "$created_data/gsoep_individuals_recoded.dta", replace 
