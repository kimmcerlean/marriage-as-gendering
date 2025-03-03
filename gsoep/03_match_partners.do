********************************************************************************
**# use ppathl to create base couple-level file to use as my master file 
* to merge rest of characteristics onto
********************************************************************************

use "$GSOEP/ppathl.dta", clear
label language EN

unique pid parid // 186572
tab syear, m

// sample status
sort pid syear parid 
browse pid syear parid partner netto 

/*Netto-codes 10-19 (and 29) define the respondents population of PGEN,
the codes 20-28 indicate children,
30-39 unit-non-responses in partially realized households,
and the codes 90-99 describe permanent (or temporary) dropouts.
Further differentiations point to the survey instruments (questionnaires). 
The Codes 10-39 describe the population in realized (and partially realized households).*/

recode netto (10/19=1)(20/29=2)(30/39=3)(40/99=0)(-3=0), gen(status)
label define status 0 "dropout" 1 "sample" 2 "youth" 3 "no int"
label values status status

unique pid, by(status) // sample: 106393 - this essentially almost exactly match pgen. do I want to add the 29 (refugees that are not 18?) prob not

// partner vars
replace parid=. if parid==-2
gen partnered=0
replace partnered=1 if inrange(partner,1,4)
replace partnered=. if partner==-2
inspect parid if partnered==1 // k most have a partner id
inspect parid if partnered==0

gen reltype=.
replace reltype=1 if partnered==1 & inlist(partner,1,3)
replace reltype=2 if partnered==1 & inlist(partner,2,4)
label define reltype 1 "married" 2 "cohab"
label values reltype reltype

browse pid syear parid partnered reltype partner status netto 

// relationship transitions
*enter
gen rel_start=0
replace rel_start=1 if partnered==1 & partnered[_n-1]==0 & pid==pid[_n-1] & syear==syear[_n-1]+1

*exit
gen rel_end=0
replace rel_end=1 if partnered==0 & partnered[_n-1]==1 & pid==pid[_n-1] & syear==syear[_n-1]+1

gen rel_end_pre=0
replace rel_end_pre=1 if partnered==1 & partnered[_n+1]==0 & pid==pid[_n+1] & syear==syear[_n+1]-1

*cohab to marr
gen marr_trans=0
replace marr_trans=1 if (reltype==1 & reltype[_n-1]==2) & pid==pid[_n-1] & syear==syear[_n-1]+1 & parid==parid[_n-1]

browse pid syear parid partnered reltype rel_start rel_end* marr_trans

// now restrict to partnered & insample - but first need to get partner in sample info
keep if partnered==1
keep if parid!=.

merge 1:1 parid syear using "$temp_gsoep/ppathl_partnerstatus.dta"
drop if _merge==2
drop _merge

// browse pid syear parid status status_sp rel_end_pre
tab status status_sp, m

// keep if status==1 | status_sp==1 // keep if at least one of them is in sample for now. can decide later if should only keep those where *both* in sample.
// alternatively, will I impute the years when they aren't in sample? let's NOT do this for now
drop if inlist(sex,-3,-1)
drop if inlist(sex_sp,-3,-1)
drop if sex==sex_sp & sex!=.

// now just clean up variables
keep pid syear hid cid sex sex_sp parid partner gebjahr todjahr eintritt erstbefr austritt letztbef psample status status_sp netto sampreg germborn partnered reltype rel_start rel_end rel_end_pre marr_trans prgroup pbleib phrf phrf0 phrf1

rename parid partner_id
rename gebjahr birthyr
rename todjahr deathyr
rename eintritt firstyr_contact
rename erstbefr firstyr_survey
rename austritt lastyr_contact
rename letztbef lastyr_survey
rename netto status_detailed
rename sampreg where_germany

// browse pid syear partner_id status status_sp rel_end_pre where_germany firstyr_contact firstyr_survey lastyr_contact lastyr_survey // want to see if this info recorded in non-sample years and it is. perhaps not true for variables in pl file; we will find out

save "$temp_gsoep/couples_ppathl.dta", replace
