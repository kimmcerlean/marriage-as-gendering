********************************************************************************
********************************************************************************
* Project: Marriage as Gendering
* Code owner: Kimberly McErlean
* Started: September 2024
* File name: 1_create_couple_sample.do
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file restricts full UKHLS sample to individuals in eligible couples
* This will form basis of sample that I will then merge to individual file to get people's info before and after relationship to use for imputation (but will not use those years in analysis - but I want to maximze the info I use in imputation since I have the longitudinal data

** Eligibility
* Partnered
* Have partner ID
* Non-missing relationship start date
* Relationship started after 1991 (first year of survey)
* Relationship started when respondent at least 18 (hmm this will be problematic because do I want the same for men AND women?)

// to leave HERE for imputation and decide later
* Restrict to ELIGIBLE couples (see notes below on how i match pidps - some earlier relationships can be in here temporarily)
* Did relationship have to start a certain # of years from last interview date to allow for a min # of observations?
* Do I have to observe at relationship start (duration of 0 or 1)?
* Later: set upper age limit as well, but do once imputed because want to use that info where possible.
* Later: restrict to those with minimum number of observations (I guess I COULD literally remove everyone with just one observation here) - I could do later but then will I lose the info on like they only had one year (because the other years will be imputed). Let's ASSESS based on sample. Is this actually common? but for my analysis, I literally need multiple observations
* Later: restrict to certain durations as well (but need to do after imputation to use that info for imputation)

********************************************************************************
* Create list of individuals in eligible couples to match on to main file
********************************************************************************
use "$created_data/UKHLS_long_all_recoded.dta", clear

*****************
*1. restrict to couples with partner IDs and start dates
*****************

inspect partner_id if partnered==0 
inspect partner_id if partnered==1

keep if partnered==1
drop if partner_id==. // will get removed later anyway because won't have partner match
drop if rel_start_all==. // will get removed anyway with start date restrictions


*****************
*2. then restrictions based on relationship dates
*****************
keep if rel_start_all >= 1989 // let's add 1989 as a buffer for now? e.g. if I follow Kapelle approach where just need first year observed, not nec. first year

// filling in false end dates to have for now (have flag - should I do something with this in imputation?! or PSM?!)
tab rel_end_all, m // it's only about 1.7% missing.
tab rel_end_all rel_end_flag, m // oh duh in last step I did this for transitioners already (bc it was going to mess up the start / end harmonization across cohab and marriage - actually seems like most of these got last year in survey so probably IS indicative of an ongoing relationship hence why not filled in)

tab status_all, m // so <1% missing so prob fine? if ongoing, update current_rel_end_year with last survey year? and call it attrited?
tab rel_end_all status_all, m // oh, the ongoing all already have an end date of last survey year. so it's the ones that ended that i don't know...use last couple year? 

browse pidp partner_id marital_status_defacto int_year rel_start_all rel_end_all status_all first_year_observed first_couple_year last_year_observed last_couple_year rel_end_year_est

// going to fill in end date with last couple year, made a flag for this in last step
replace rel_end_all = last_couple_year if rel_end_all==.

// do I want restrictions based on minimum observations? Let's create the variables now and see.
* need to add duration variable
gen dur = int_year - rel_start_all // I am using WAVE year not int year because this is going to cause problems later (when interview years overlap)
tab dur, m

gen dur_alt = year - rel_start_all // worried above might cause problems later  (when interview years overlap). but then causes problems here because some people have duration of -1 that is really 0. so use above to things like min dur, but might need to revisit this coding later when trying to actually estimate models because need discrete durations
tab dur_alt

* then, min and max duration
bysort pidp partner_id: egen min_dur = min(dur)
bysort pidp partner_id: egen max_dur = max(dur)

sort pidp year
browse pidp partner_id int_year marital_status_defacto rel_start_all rel_end_all status_all dur min_dur max_dur first_couple_year last_couple_year 

tab min_dur // so about half are 1-2. also should I drop those with min dur of less than 0??
tab min_dur marital_status_defacto, col // yes more observed at shorter durations for cohab, but still a decent amount at longer.
tab min_dur if rel_start_all >=1991 // because the ones prior to 1991 def can't be observed

unique pidp partner_id, by(min_dur)
unique pidp partner_id if rel_start_all >=1991, by(min_dur)

// should I at least drop if min dur is greater than like 10, because now we are just talking about a different life course stage (also assuming many of those will be married couples which isn't even relevant - except that's not super true per above) ugh IDK let's just impute all and decide later

*****************
*3. age at rel start
*****************
browse pidp partner_id year int_year age_all dob_year rel_start_all
gen age_rel_start = .
replace age_rel_start = rel_start_all - dob_year

drop if age_rel_start < 17 // do 17 because it's years, not months so it's not 100% precise. can revisit later if needed. this is so few people.

*****************
*4. do I want to restrict on Min # of observations here?
*****************
// let's assess scope and decide
bysort pidp partner_id: egen years_observed=count(year)

sort pidp year
browse pidp partner_id year years_observed rel_start_all rel_end_all

tab years_observed, m // okay so observations just 3% (I guess that uniques will match and scale down from there (because each is multipled by years obs))
unique pidp partner_id, by(years_observed) // so yeah it's actually 20% of all uniques... I mean I have to drop, right? unless I keep in control group? so probably impute just in case...but good news is I have this variable to keep for later

********************************************************************************
* Actually create list of couples
********************************************************************************

unique pidp
unique pidp partner_id 
egen couple_id = group(pidp partner_id)
// browse pidp partner_id couple_id
unique couple_id

// browse pidp partner_id rel_start_all rel_end_all rel_end_flag

// get list
preserve

collapse (first) rel_start_all rel_end_all status_all rel_no_est ever_transition year_transitioned year_transitioned_wave min_dur max_dur first_couple_year last_couple_year years_observed (max) rel_end_flag, by(pidp partner_id couple_id)

gen eligible_couple=1
rename couple_id eligible_couple_id
rename rel_start_all eligible_rel_start_year
rename rel_end_all eligible_rel_end_year
rename status_all eligible_rel_status
rename rel_no_est eligible_rel_no
rename rel_end_flag eligible_end_estimated

gen long eligible_partner = partner_id 
by pidp: egen num_rel = count(partner_id) // this is how many relationships in this time frame they are contributing, so not quite the same

browse if num_rel > 1

save "$created_data/ukhls_couple_list.dta", replace

restore

********************************************************************************
**# Now merge back on to data to create a filter for individuals
********************************************************************************
use "$created_data/UKHLS_long_all_recoded.dta", clear

// some variables to create
gen dur = int_year - rel_start_all // but need to retain this because time varying
// bysort pidp partner_id: egen min_dur = min(dur) // I will use the version frm the matched file below so I know it's consistent
// bysort pidp partner_id: egen max_dur = max(dur)  // I will use the version frm the matched file below so I know it's consistent

tab dur, m

merge m:1 pidp partner_id using "$created_data/ukhls_couple_list.dta", keepusing(num_rel eligible_couple eligible_couple_id eligible_rel_start_year eligible_rel_end_year eligible_rel_no eligible_rel_status eligible_partner  eligible_end_estimated min_dur max_dur years_observed) // so I actually want to just merge on pidp because i want to keep them potentially even if not partnered anymore. BUT, can indicate which relationship is the eligible one, if multiple. okay, this won't work because some have multiple partners, so will merge for the specific couple, then create a MAX indicator of whether that person is ever eligible 

drop if _merge==2
drop _merge

bysort pidp: egen ever_eligible = max(eligible_couple)
bysort pidp: egen max_eligible_rels = max(num_rel)
replace ever_eligible = 0 if ever_eligible==.
tab ever_eligible eligible_couple, m
tab partnered ever_eligible, m

sort pidp year
browse pidp partner_id int_year partnered ever_eligible max_eligible_rels num_rel eligible_couple eligible_partner rel_start_all eligible_rel_start_year rel_end_all eligible_rel_end_year eligible_rel_status eligible_couple_id min_dur max_dur ever_transition year_transitioned year_transitioned_wave first_couple_year last_couple_year // if max_eligible_rels > 1

keep if ever_eligible==1

// okay I need to revisit this because this is filling in partner info if the pidp is ever eligible, so I keep, but then these could be relationships OUTSIDE of eligibility (aka no eligible couple flag). I think I need to filter on that later? (or might get handled with min / max dur). but actually think I *don't* fill this info in? I think maybe I do it so I have it to match on the right relationship actually (if there are multiple). still struggling with whether or not I need this TBH, but think trying to get info to fill in maybe and keep track of like WHY I kept them via which relationship
tab partnered eligible_couple, m

/*
I actually DON'T want to do this. I think the below handles what I need it to handle, but let's see.

replace eligible_partner = partner_id if eligible_partner==. & partner_id !=.
replace eligible_rel_start_year = rel_start_all if eligible_rel_start_year==. & rel_start_all !=.
replace eligible_rel_end_year = rel_end_all if eligible_rel_end_year==. & rel_end_all !=.
replace eligible_rel_status = status_all if eligible_rel_status==. & status_all !=.
label values eligible_rel_status status
replace eligible_rel_no = rel_no_est if eligible_rel_no==. & rel_no_est !=.
*/

foreach var in eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_partner eligible_rel_no eligible_couple_id years_observed min_dur max_dur eligible_end_estimated ever_transition year_transitioned year_transitioned_wave first_couple_year last_couple_year{
	bysort pidp (`var'): replace `var' = `var'[1] if max_eligible_rels==1
}

sort pidp year
// https://www.stata.com/support/faqs/data-management/replacing-missing-values/

foreach var in eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_partner eligible_rel_no eligible_couple_id years_observed min_dur max_dur eligible_end_estimated ever_transition year_transitioned year_transitioned_wave first_couple_year last_couple_year{
	replace `var' = `var'[_n-1] if `var'==. & `var'[_n-1]!=. & pidp==pidp[_n-1] & max_eligible_rels > 1
	gsort pidp -year 
	replace `var' = `var'[_n-1] if `var'==. & `var'[_n-1]!=. & pidp==pidp[_n-1] & max_eligible_rels > 1
	sort pidp year
}

// examples: 76165, 110847, 114247

assert eligible_rel_start_year!=.
assert eligible_rel_end_year!=.
assert eligible_partner!=. 
assert eligible_rel_no!=.
// assert eligible_rel_status!=. // some don't have statuses; think bc ongoing (or attrited and so we don't know) - so that is what I try to fill in below
label define rel_status 0 "Ended" 1 "Ongoing" 99 "Attrition"
label values eligible_rel_status status_all rel_status

sort pidp year
browse pidp partner_id int_year partnered marital_status_defacto eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_rel_no rel_start_all rel_end_all status_all max_eligible_rels rel_no_est

gen post_marital_status=.
replace post_marital_status = marital_status_defacto if int_year > eligible_rel_end_year
label values post_marital_status marital_status_defacto

gen post_ended=.
replace post_ended = 1 if inlist(post_marital_status,3,4,5,6) // actually never married works as well bc that will be status for cohabitors
bysort pidp (post_ended): replace post_ended=post_ended[1]
sort pidp year

replace eligible_rel_status = 0 if post_ended==1 & eligible_rel_status==.
replace eligible_rel_status = 99 if eligible_rel_status==. & eligible_rel_end_year == last_year_observed // calling this "attrition" (but I guess that's the equivalent of ongoing?)

// browse pidp partner_id int_year eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status rel_start_all rel_end_all status_all max_eligible_rels if eligible_partner==.
// browse pidp partner_id int_year eligible_partner eligible_rel_start_year eligible_rel_end_year eligible_rel_status rel_start_all rel_end_all status_all max_eligible_rels if inlist(pidp,683763125, 687391802, 748014291, 749686407, 816866325)

browse pidp partner_id int_year marital_status_defacto post_marital_status post_ended eligible_partner eligible_rel_start_year eligible_rel_end_year rel_start_all rel_end_all status_all max_eligible_rels first_year_observed last_year_observed if eligible_rel_status==.

egen couple_id = group(pidp eligible_partner)
unique couple_id
unique pidp couple_id
unique pidp partner_id
unique pidp eligible_partner

gen relative_duration_v0 = int_year - eligible_rel_start_year
tab relative_duration_v0, m

// fix the duplicate duration issue (identified below) - when people are interviewed twice in same year
bysort pidp couple_id: egen duplicate_dur = rank(year), unique  // because waves aren't in right order have to use year (proxy for wave)
sort pidp year

gen wave_distance = year - year[_n-1] if pidp == pidp[_n-1] & eligible_partner == eligible_partner[_n-1] // because waves aren't in right order

gen relative_duration = relative_duration_v0 if duplicate_dur==1
// browse pidp eligible_partner int_year year wavename wave_distance eligible_rel_start_year relative_duration_v0 relative_duration duplicate_dur 
replace relative_duration = relative_duration[_n-1] + wave_distance if pidp == pidp[_n-1] & eligible_partner == eligible_partner[_n-1]  
browse pidp eligible_partner int_year year wavename wave_distance eligible_rel_start_year relative_duration_v0 relative_duration duplicate_dur 
replace relative_duration = relative_duration_v0 if relative_duration==.

duplicates tag couple_id relative_duration, generate(dup_count)
duplicates list couple_id relative_duration
	// browse pidp eligible_partner eligible_rel_start current_rel_start eligible_rel_no int_year year relative_duration_v0 duplicate_dur* couple_id if inlist(couple_id, 29480, 32150)
drop if dup_count==1 // weird intermittent relationships

gen relative_duration_alt = year - eligible_rel_start_year
browse pidp eligible_partner int_year year eligible_rel_start_year eligible_rel_end_year relative_duration_v0 relative_duration relative_duration_alt

// I want to keep people eventually 5 years before marriage then around 10 years after (prob less, though). so let's do up to relative duration 16?
keep if relative_duration >=-2
keep if relative_duration <=16

// do I want to drop extraneous variables? will that help reduce file size and therefore facilitate imputation in any way? Might not, but let's do it.
drop nmar_bh hgra racel_bh lvag14_bh sampst_bh ukborn jbttwt paygwc ccare dinner qfhigh school scend fenow feend racel racel_dv ethn_dv oprlg0 oprlg0ni oprlg1 ff_oprlg0 ff_oprlg0ni nmar lcoh lprnt lnprnt paygw huxpch hunurs oprlg1_bh race hhch12 hgr2r hoh sex birthy doby month emboost mh_partner1 mh_startm1 mh_endm1 mh_divorcey1 mh_divorcem1 mh_mrgend1 mh_cohend1 mh_ongoing1 mh_partner2 mh_startm2 mh_endm2 mh_divorcey2 mh_divorcem2 mh_mrgend2 mh_cohend2 mh_ongoing2 mh_partner3 mh_startm3 mh_endm3 mh_divorcey3 mh_divorcem3 mh_mrgend3 mh_cohend3 mh_ongoing3 mh_partner4 mh_startm4 mh_endm4 mh_divorcey4 mh_divorcem4 mh_mrgend4 mh_cohend4 mh_ongoing4 mh_partner5 mh_startm5 mh_endm5 mh_divorcey5 mh_divorcem5 mh_mrgend5 mh_cohend5 mh_ongoing5 mh_partner6 mh_startm6 mh_endm6 mh_divorcey6 mh_divorcem6 mh_mrgend6 mh_cohend6 mh_ongoing6 mh_partner7 mh_startm7 mh_endm7 mh_divorcey7 mh_divorcem7 mh_mrgend7 mh_cohend7 mh_ongoing7 mh_partner8 mh_startm8 mh_endm8 mh_divorcey8 mh_divorcem8 mh_mrgend8 mh_cohend8 mh_ongoing8 mh_partner9 mh_startm9 mh_endm9 mh_divorcey9 mh_divorcem9 mh_mrgend9 mh_cohend9 mh_ongoing9 mh_partner10 mh_startm10 mh_endm10 mh_divorcey10 mh_divorcem10 mh_mrgend10 mh_cohend10 mh_ongoing10 mh_partner11 mh_startm11 mh_endm11 mh_divorcey11 mh_divorcem11 mh_mrgend11 mh_cohend11 mh_ongoing11 mh_partner12 mh_startm12 mh_endm12 mh_divorcey12 mh_divorcem12 mh_mrgend12 mh_cohend12 mh_ongoing12 mh_partner13 mh_startm13 mh_endm13 mh_divorcey13 mh_divorcem13 mh_mrgend13 mh_cohend13 mh_ongoing13 mh_partner14 mh_startm14 mh_endm14 mh_divorcey14 mh_divorcem14 mh_mrgend14 mh_cohend14 mh_ongoing14 mh2_year_marr1 mh2_year_coh1 mh2_year_end1 mh2_how_end1 mh2_year_marr2 mh2_year_coh2 mh2_year_end2 mh2_how_end2 mh2_year_marr3 mh2_year_coh3 mh2_year_end3 mh2_how_end3 mh2_year_marr4 mh2_year_coh4 mh2_year_end4 mh2_how_end4 x_mh_status1 x_mh_status2 x_mh_status3 x_mh_status4 x_mh_status5 x_mh_status6 x_mh_status7 x_mh_status8 x_mh_status9 x_mh_status10 x_mh_status11 x_mh_status12 x_mh_status13 x_mh_status14 paygl paynl paygu_dv payg_dv paynu_dv payn_dv sex_dv age_dv intdatd_dv intdatm_dv doby_dv marstat_dv mastat_dv lcohnpi coh1bm coh1by coh1mr coh1em coh1ey lmar1m lmar1y cohab cohabn lmcbm1 lmcby41 currpart1 lmspm1 lmspy41 lmcbm2 lmcby42 currpart2 lmspm2 lmspy42 lmcbm3 lmcby43 currpart3 lmspm3 lmspy43 lmcbm4 lmcby44 currpart4 lmspm4 lmspy44 lmcbm5 lmcby45 currpart5 lmspm5 lmspy45 currmstat lmcbm6 lmcby46 currpart6 lmspm6 lmspy46 lmcbm7 lmcby47 currpart7 lmspm7 lmspy47 currcohby curmarrby mlstat_bh hubuys_bh hufrys_bh humops_bh huiron_bh hgsex hubuys_v0 hufrys_v0 huiron_v0 humops_v0 cohab1_yr cohab1_end_yr cohab2_yr cohab2_end_yr cohab3_yr cohab3_end_yr cohab4_yr cohab4_end_yr cohab5_yr cohab5_end_yr cohab6_yr cohab6_end_yr cohab7_yr cohab7_end_yr mh_est_div_yr1 mh_est_div_yr2 mh_est_div_yr3 mh_est_div_yr4 mh_est_div_yr5 mh_est_div_yr6 mh_est_div_yr7 mh_est_div_yr8 mh_est_div_yr9 mh_est_div_yr10 mh_est_div_yr11 mh_est_div_yr12 mh_est_div_yr13 mh_est_div_yr14 mh_div_yr1 mh_div_yr2 mh_div_yr3 mh_div_yr4 mh_div_yr5 mh_div_yr6 mh_div_yr7 mh_div_yr8 mh_div_yr9 mh_div_yr10 mh_div_yr11 mh_div_yr12 mh_div_yr13 mh_div_yr14 dur relative_duration_v0 duplicate_dur wave_distance relative_duration_alt 

save "$created_data/ukhls_eligible_for_imputation.dta", replace

********************************************************************************
**# Ensure data is rectangularized and attempt to fill in missing durations
********************************************************************************
// use "$created_data/ukhls_eligible_for_imputation.dta", clear

// do some figuring out of data first so I know how to update once rectangularized
unique pidp partner_id // partner Id missing a lot bc includes non-partnered years, so will be more (bc one missing record, one not)
unique pidp eligible_partner //  44759

// see if some variables are fixed or change
unique pidp // 42627
unique pidp country_all // 43511 instead of 23301 so barely changes 
unique pidp gor_dv // 46020 so changes more if I get more specific
quietly unique gor_dv if gor_dv!=., by(pidp) gen(country_change)
bysort pidp (country_change): replace country_change=country_change[1]
tab country_change, m

sort pidp year
browse pidp int_year relative_duration gor_dv country_all country_change 

unique pidp xw_ethn_dv // fixed because I got from cross-wave file
unique pidp xw_racel_dv // fixed because I got from cross-wave file
/* frome codebook re; race/ethn and their differences:
Respondents are asked the ethnic group question (racel or racel*t) only the first time they are interviewed.
in a few cases, racel is asked multiple times, and in those cases,
racel_dv prioritises the earliest report while ethn_dv prioritises the latest report.
*/

unique pidp hiqual_dv // some more movement here, but barely. 46743 v. 42627
quietly unique hiqual_dv if hiqual_dv!=., by(pidp) gen(educ_change)
bysort pidp (educ_change): replace educ_change=educ_change[1]
tab educ_change, m

// try to get at first relationship duration, but if not, prioritize earliest measurement (or should I do LATEST for completed measurement?) but I guess for matching purposes, I want to match at rel start, so this makes sense for now...
gen hiqual_fixed = hiqual_dv if educ_change==1
replace hiqual_fixed = hiqual_dv if hiqual_fixed==. & relative_duration == min_dur & inlist(min_dur,0,1)
bysort pidp (hiqual_fixed): replace hiqual_fixed=hiqual_fixed[1]

forvalues d=0/16{
	replace hiqual_fixed = hiqual_dv if relative_duration == `d' & hiqual_fixed==.
	bysort pidp (hiqual_fixed): replace hiqual_fixed=hiqual_fixed[1]
}

label define hiqual  1 "Degree" 2 "Other higher degree" 3 "A level" 4 "GCSE" 5 "Other qual" 9 "No qual"
label values hiqual_fixed hiqual

sort pidp year
browse pidp int_year relative_duration hiqual_dv hiqual_fixed educ_change

// need to figure out first birth year do I really need this? oh, i actually didn't even use in final imputation (see PSID files, I am dumb)
/* From codebook
Uses files W_CH1BY4 on data file W_INDRESP
W_LCHDOBY4 on datafile W_NATCHILD
W_MNPID WFNPID W_BIRTHY on W_INDALL
W_LCHBY4 on datafile W_NEWBORN
BW_CH1BY on datafile BW_INDRESP/
*/

// first see what missing data is like with the data that exists
misstable summarize jbhrs jshrs work_hours total_hours howlng aidhrs fimnlabgrs_dv jbstat employment_status employed hiqual_dv hiqual_fixed ever_parent nchild_dv nkids_dv hhsize age_youngest_child partnered marital_status_defacto fihhmngrs_dv xw_racel_dv xw_ethn_dv xw_bornuk_dv xw_ukborn country_all gor_dv urban_dv age_all dob_year year_first_birth xw_memorig xw_sampst respondent_self ivfio xw_sex tenure_dv housing_status_alt master_religion religion_est empstat_disabled disabled_est sr_health life_satisfaction empstat_retired any_aid aid_hours npens_dv npn_dv father_educ father_empstatus mother_educ mother_empstatus family_structure family_structure14_det hubuys hufrys huiron humops huboss husits eligible_end_estimated eligible_rel_start_year eligible_rel_no , showzeros all

// partnered_imp marital_status_imp 

// some variables causing problems that should be fixed within couples but are not
	// not working and IMPORTANT
	drop if dob_year==.
	
	quietly unique dob_year if dob_year!=., by(pidp) gen(dob_change)
	bysort pidp (dob_change): replace dob_change=dob_change[1]
	tab dob_change, m
	
	rename dob_year dob_year_v0
	bysort pidp: egen dob_year = min(dob_year_v0)
	
	sort pidp year
	browse pidp couple_id int_year dob_year age_all dob_change
	
	unique couple_id ever_transition 
	browse pidp eligible_partner couple_id int_year year marital_status_defacto marr_trans ever_transition year_transitioned*
	
	rename ever_transition ever_transition_v0
	bysort couple_id: egen ever_transition = max(ever_transition_v0) // this will resolve itself when I have sample restrictions but some people have multiple spells with the same partner and this is causing problems.
	tab ever_transition ever_transition_v0
	
/*
some people have two partners in a year. dropping those. need to do BEFORE I rectangularize or will cause problems later. No I am realizing the rectangularization is what creates this - because if people have multiple relationships, I sometimes create years that then overlap but their REAL relationships don't overlap. okay this is not actually a problem because this gets sorted once I rerestrict to the actual years of the relationship.
unique pidp couple_id
unique couple_id pidp eligible_partner
unique couple_id pidp eligible_partner min_dur max_dur first_couple_year last_couple_year

egen tag = tag(pidp year eligible_partner)
egen ndistinct = total(tag), by(pidp year)

tab ndistinct // people with multiple partners listed per year (example PIDPS: 163612085,4029691)
sort pidp int_year
browse pidp eligible_partner int_year year tag ndistinct eligible_rel_start_year relative_duration first_couple_year last_couple_year current_rel_start_year min_dur max_dur if ndistinct > 1
unique couple_id, by(ndistinct)

drop if ndistinct > 1
*/

	// not working and FINE: prior_divorce sampst plbornc pid religion_est. prior_divorce_year can be created from first_divorce_year that IS fine. religion will impute time-varying anyway. rest not using or have better version
 
************************************
**# here, we finally rectangularize
************************************

unique pidp
unique pidp eligible_partner
unique couple_id //  44758

drop if couple_id==. // this should have already been taken off but check

browse pidp eligible_partner int_year partnered partner_id eligible_rel_start_year relative_duration couple_id

gen orig_record = 1 // want to know if existed or new below

fillin couple_id relative_duration

// quick checks
tab relative_duration // yes, now it perfect aligns
unique couple_id, by(relative_duration)
bysort couple_id: egen rowcount = count(relative_duration)
tab rowcount, m // all should be 19

unique pidp eligible_rel_start_year
unique couple_id

************************************
* Updating missing variables when possible
************************************

// pull through fixed variables
foreach var in in_partner_history pidp eligible_couple_id eligible_rel_start_year eligible_rel_end_year eligible_rel_status eligible_rel_no min_dur max_dur years_observed eligible_partner ever_eligible max_eligible_rels first_divorce_year xw_xwdat_dv xw_memorig xw_sampst xw_sex xw_lvag16 xw_agelh xw_lvag14 xw_racel_dv xw_ukborn xw_bornuk_dv mother_empstatus mother_educ father_empstatus father_educ xw_coh1m_dv xw_coh1y_dv xw_evercoh_dv xw_lmar1m_dv xw_lmar1y_dv xw_evermar_dv xw_ch1by_dv xw_anychild_dv xw_ethn_dv  first_year_observed last_year_observed ever_parent year_first_birth num_religion family_structure family_structure14_det first_marr_yr first_cohab_yr first_cohab_end_yr eligible_end_estimated hiqual_fixed dob_year year_transitioned year_transitioned_wave ever_transition first_couple_year last_couple_year mh_status1 mh_starty1 mh_endy1 mh_status2 mh_starty2 mh_endy2 mh_status3 mh_starty3 mh_endy3 mh_status4 mh_starty4 mh_endy4 mh_status5 mh_starty5 mh_endy5 mh_status6 mh_starty6 mh_endy6 mh_status7 mh_starty7 mh_endy7 mh_status8 mh_starty8 mh_endy8 mh_status9 mh_starty9 mh_endy9 mh_status10 mh_starty10 mh_endy10 mh_status11 mh_starty11 mh_endy11 mh_status12 mh_starty12 mh_endy12 mh_status13 mh_starty13 mh_endy13 mh_status14 mh_starty14 mh_endy14 mh_ttl_spells mh_ttl_married mh_ttl_civil_partnership mh_ttl_cohabit mh_ever_married mh_ever_civil_partnership mh_ever_cohabit mh_lastintdate mh_lastinty mh_lastintm mh_hhorig{
	bysort couple_id (`var'): replace `var'=`var'[1] if `var'==.
}

replace int_year = eligible_rel_start_year + relative_duration if int_year==.
replace age_all = int_year - dob_year if age_all==.

sort pidp int_year
browse pidp eligible_partner int_year age_all dob_year partnered partner_id eligible_rel_start_year eligible_rel_end_year eligible_rel_no relative_duration couple_id orig_record

// some variables to create now that it's filled in
* parental status based on year of first birth
browse pidp int_year ever_parent xw_ch1by_dv year_first_birth

gen current_parent_status=.
replace current_parent_status = 0 if ever_parent==0
replace current_parent_status = 0 if ever_parent==1 & int_year < year_first_birth & year_first_birth!=0
replace current_parent_status = 1 if ever_parent==1 & int_year >= year_first_birth & year_first_birth!=0

tab ever_parent current_parent_status,m 
tab year_first_birth current_parent_status, m

* do same for divorce status
browse pidp int_year first_divorce_year mh_ttl_married marital_status_defacto
tab first_divorce_year mh_ttl_married, m
tab first_divorce_year in_partner_history, m

gen current_divorce_status = .
replace current_divorce_status = 0 if in_partner_history==1 & first_divorce_year==.
replace current_divorce_status = 0 if int_year < first_divorce_year & first_divorce_year!=.
replace current_divorce_status = 1 if int_year >= first_divorce_year & first_divorce_year!=.

tab first_divorce_year current_divorce_status, m

* now whether divorce generally prior to this rel. I tried this but fear it did not work
browse pidp couple_id int_year eligible_rel_start_year first_divorce_year mh_ttl_married marital_status_defacto current_divorce_status prior_divorce
rename prior_divorce prior_divorce_v0

gen prior_divorce=.
replace prior_divorce = 0 if in_partner_history==1 & first_divorce_year==.
replace prior_divorce = 0 if eligible_rel_start_year < first_divorce_year & first_divorce_year!=.
replace prior_divorce = 1 if eligible_rel_start_year >= first_divorce_year & first_divorce_year!=.

tab prior_divorce prior_divorce_v0, m
tab prior_divorce current_divorce_status, m

* first birth timing relative to relationship
gen birth_timing_rel = year_first_birth - eligible_rel_start_year if year_first_birth!=9999 & year_first_birth!=0
replace birth_timing_rel = 9999 if year_first_birth==9999

tab birth_timing_rel ever_parent, m

* Create binary version
	// browse pidp int_year ever_parent  year_first_birth eligible_rel_start_year birth_timing_rel
gen premarital_birth = .
replace premarital_birth = 0 if ever_parent==0
replace premarital_birth = 0 if ever_parent==1 & birth_timing_rel >=0 & birth_timing_rel<50
replace premarital_birth = 1 if ever_parent==1 & birth_timing_rel < 0

tab birth_timing_rel premarital_birth, m
tab ever_parent premarital_birth, m

// fill in respondent info with non-sample year when missing
gen respondent_info = respondent_self
replace respondent_info = 2 if respondent_self==.

label define resp 0 "proxy" 1 "self" 2 "non-sample"
label values respondent_info resp

// Can I fill in any - namely marital status / partnership status based on history variables? anything about children with birth history also?
browse pidp int_year partnered marital_status_defacto eligible_rel_start_year eligible_rel_end_year current_rel_start_year current_rel_end_year mh_*

gen partnered_imp=partnered
gen marital_status_imp=marital_status_defacto
label values marital_status_imp marital_status_defacto

forvalues y=1/14{
	replace partnered_imp = 1 if partnered_imp==. & int_year >= mh_starty`y' & int_year <= mh_endy`y'
	replace marital_status_imp = 1 if marital_status_imp==. & int_year >= mh_starty`y' & int_year <= mh_endy`y' & mh_status`y'==1 // marriage
	replace marital_status_imp = 2 if marital_status_imp==. & int_year >= mh_starty`y' & int_year <= mh_endy`y' & mh_status`y'==2 // cohab
}

replace partnered_imp=0 if partnered_imp==. & int_year < mh_starty1 // not partnered if prior to first rel date
replace partnered_imp=1 if partnered_imp==. & int_year >= eligible_rel_start_year & int_year <=eligible_rel_end_year // in case missing WITHIN relationship and not covered with above, will do this.
replace marital_status_imp=6 if marital_status_imp==. & int_year < mh_starty1 & mh_status1==1 // never married if prior to first rel date and it's a marriage

browse pidp int_year orig_record partnered_imp partnered marital_status_imp marital_status_defacto eligible_rel_start_year eligible_rel_end_year current_rel_start_year current_rel_end_year mh_*

inspect partnered_imp marital_status_imp partnered marital_status_defacto

// now see the missing again
misstable summarize jbhrs jshrs work_hours total_hours howlng aidhrs fimnlabgrs_dv jbstat employment_status employed hiqual_dv hiqual_fixed ever_parent nchild_dv nkids_dv hhsize age_youngest_child partnered marital_status_defacto partnered_imp marital_status_imp fihhmngrs_dv xw_racel_dv xw_ethn_dv xw_bornuk_dv xw_ukborn country_all gor_dv urban_dv age_all dob_year year_first_birth xw_memorig xw_sampst respondent_self ivfio xw_sex tenure_dv housing_status_alt master_religion religion_est empstat_disabled disabled_est sr_health life_satisfaction empstat_retired any_aid aid_hours npens_dv npn_dv father_educ father_empstatus mother_educ mother_empstatus family_structure family_structure14_det hubuys hufrys huiron humops huboss husits eligible_end_estimated eligible_rel_start_year eligible_rel_no first_divorce_year prior_divorce current_divorce_status current_parent_status birth_timing_rel premarital_birth respondent_info years_observed, showzeros all

save "$created_data/ukhls_couples_rectangular_long.dta", replace
