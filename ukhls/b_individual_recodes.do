********************************************************************************
********************************************************************************
* Project: Marriage as Gendering
* Code owner: Kimberly McErlean
* Started: September 2024
* File name: b_individual_recodes.do
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This file recodes key variables to use in imputation / subsequence analysis


********************************************************************************
********************************************************************************
* Data prep steps
********************************************************************************
********************************************************************************

********************************************************************************
* Going to try to first update spouse id for BHPS so it's pidp NOT pid
********************************************************************************
use "$UKHLS/xwaveid_bh.dta", clear

keep pidp pid
rename pid sppid_bh
rename pidp partner_pidp_bh

save "$temp/spid_lookup.dta", replace

********************************************************************************
* Prep partner history file for later
********************************************************************************
use "$UKHLS_mh/phistory_wide.dta", clear

foreach var in status* partner* starty* startm* endy* endm* divorcey* divorcem* mrgend* cohend* ongoing* ttl_spells ttl_married ttl_civil_partnership ttl_cohabit ever_married ever_civil_partnership ever_cohabit lastintdate lastinty lastintm hhorig{
	rename `var' mh_`var' // renaming for ease of finding later, especially when matching partner info
}

save "$temp/partner_history_tomatch.dta", replace

********************************************************************************
* Backup partner history files
********************************************************************************
// see list on table 4 here: https://www.understandingsociety.ac.uk/documentation/mainstage/user-guides/main-survey-user-guide/list-of-data-files-and-their-descriptions/
// based on below, some of these are in indresp, but not the end dates I don't think?

** Marriage

local waves = "a f"

local i=1

foreach wave in `waves' {
	local waveno=`i'
	use "$UKHLS/`wave'_marriage.dta", clear
	
	foreach var in `wave'_lmary4 `wave'_lmcby4 `wave'_lmend `wave'_lmwwy4 `wave'_lmdvy4 `wave'_lmspy4{
		recode `var' (-9=.)(-2/-1=.)
	}
	
	gen year_end = `wave'_lmwwy4 if `wave'_lmend==2
	replace year_end = `wave'_lmspy4 if `wave'_lmend==1 & `wave'_lmspy4!=. & `wave'_lmspy4!=-8
	replace year_end = `wave'_lmdvy4 if year_end==. & `wave'_lmend==1 & `wave'_lmdvy4!=. & `wave'_lmdvy4!=-8
	replace year_end = `wave'_lmspy4 if year_end==. & `wave'_lmend==97 & `wave'_lmspy4!=. & `wave'_lmspy4!=-8
	replace year_end = `wave'_lmdvy4 if year_end==. & `wave'_lmend==97 & `wave'_lmdvy4!=. & `wave'_lmdvy4!=-8
	replace year_end = `wave'_lmwwy4 if year_end==. & `wave'_lmend==97 & `wave'_lmwwy4!=. & `wave'_lmwwy4!=-8
	
	gen how_end=.
	replace how_end = 1 if inlist(`wave'_lmend,1,97) // divorce / sep
	replace how_end = 2 if `wave'_lmend==2 // widowed
	
	label define how_end 1 "Dissolved" 2 "Widowed" 3 "Ongoing"
	label values how_end how_end
	
	keep pidp `wave'_marno `wave'_lmary4 `wave'_lmcby4 year_end how_end
	
	gen wave = "`wave'"
	
	rename `wave'_* *
	
	save "$temp/marriage_`wave'", replace
	
	local ++i
}

local waves = "bb bk bl" // coding switched in between

local i=1

foreach wave in `waves' {
	local waveno=`i'
	use "$UKHLS/`wave'_marriag.dta", clear
	
	foreach var in `wave'_lmary4 `wave'_lmcby4 `wave'_lmend `wave'_lmwwy4 `wave'_lmdvy4 `wave'_lmspy4{
		recode `var' (-9=.)(-2/-1=.)
	}
		
	gen year_end = `wave'_lmwwy4 if `wave'_lmend==1
	replace year_end = `wave'_lmspy4 if inlist(`wave'_lmend,2,3) & `wave'_lmspy4!=. & `wave'_lmspy4!=-8
	replace year_end = `wave'_lmdvy4 if year_end==. & inlist(`wave'_lmend,2,3) & `wave'_lmdvy4!=. & `wave'_lmdvy4!=-8
	
	gen how_end=.
	replace how_end = 1 if inlist(`wave'_lmend,2,3) // divorce / sep
	replace how_end = 2 if `wave'_lmend==1 // widowed
	replace how_end = 3 if `wave'_lmend==4 // still married
	
	label define how_end 1 "Dissolved" 2 "Widowed" 3 "Ongoing"
	label values how_end how_end
	
	keep pidp `wave'_marno `wave'_lmary4 `wave'_lmcby4 year_end how_end
	
	gen wave = "`wave'"
	
	rename `wave'_* *
	
	save "$temp/marriage_`wave'", replace
	
	local ++i
}


use "$temp/marriage_a", clear
append using "$temp/marriage_f"
append using "$temp/marriage_bb"
append using "$temp/marriage_bk"
append using "$temp/marriage_bl"

tab year_end how_end, m col

gen wavename=.
replace wavename = 1 if wave == "a"
replace wavename = 6 if wave == "f"
replace wavename = 16 if wave == "bb"
replace wavename = 25 if wave == "bk"
replace wavename = 26 if wave == "bl"

gen year=.
replace year=2009 if wavename==1
replace year=2014 if wavename==6
replace year=1992 if wavename==16
replace year=2001 if wavename==25
replace year=2002 if wavename==26

sort pidp marno 
unique pidp
browse

rename lmary4 mh2_year_marr
rename lmcby4 mh2_year_coh
rename year_end mh2_year_end
rename how_end mh2_how_end

drop wave
reshape wide mh2_year_marr mh2_year_coh mh2_year_end mh2_how_end, j(marno) i(pidp year wavename)

save "$temp/marriage_combined_wide.dta", replace

/*
** Cohabitation: I feel like these are all integrated into indresp?
use "$UKHLS/a_cohab.dta", clear
use "$UKHLS/f_cohab.dta", clear
use "$UKHLS/bb_cohabit.dta", clear

** in indresp - make sure I have all. but like none of these are END date
cohlby1 cohlby2 cohlby3 // year started reverse order (most recent to less recent) BH 08 (would that be h?) oh but these are in the main file, just only asked in wave bh 08
cohley1 cohley2 cohley3 // year ended
curmarrby // year current cohab started (wave 14) - these are only for new entrants
lcsby4 // year began cohab spell (1 6 Bh02 BH11 BH12)
lcsey4 // year ended
currcohby
lcmary4 // year of current marriage (1 6 BH11 BH12)
lcmspy4 // year separated (1 6 BH11 BH12)
lmar1y // year of first marriage (good coverage)

** xwlsten
lcmstat // wave marital status last changed
*/

********************************************************************************
* Get variables from cross wave files?
********************************************************************************
use "$UKHLS/xwavedat.dta", clear

// why does it still feel like no good race variables
tab racel_dv xwdat_dv, m col
tab racel_bh xwdat_dv, m col // very bad coverage
tab race_bh xwdat_dv, m col // very bad coverage

local xwave "xwdat_dv sex memorig sampst racel_dv ethn_dv coh1m_dv coh1y_dv evercoh_dv lmar1m_dv lmar1y_dv evermar_dv ch1by_dv anychild_dv"

foreach var in `xwave'{
	fre `var'
}

foreach var in `xwave'{
	recode `var' (-9/-1=.)
	rename `var' xw_`var' // renaming
}

keep pidp xw_* // to match later

save "$temp/xwave_tomatch.dta", replace 

********************************************************************************
********************************************************************************
**# Import data (created in step a) and do some data cleaning / recoding
********************************************************************************
********************************************************************************

use "$created_data/UKHLS_long_all.dta", clear
drop if pidp==. // think these are HHs that didn't match?

// Right now 1-13 are ukhls and 14-31 are bhps, so the wave order doesn't make a lot of sense. These aren't perfect but will work for now.
// okay i added interview characteristics, so use that?
browse pidp pid wavename intdatey intdaty_dv istrtdaty // istrtdaty seems the most comprehensive. the DV one is only UKHLS
// okay, but sometimes consecutive surveys are NOT consecutive years? https://www.understandingsociety.ac.uk/documentation/mainstage/survey-timeline/

gen year=.
replace year=2009 if wavename==1
replace year=2010 if wavename==2
replace year=2011 if wavename==3
replace year=2012 if wavename==4
replace year=2013 if wavename==5
replace year=2014 if wavename==6
replace year=2015 if wavename==7
replace year=2016 if wavename==8
replace year=2017 if wavename==9
replace year=2018 if wavename==10
replace year=2019 if wavename==11
replace year=2020 if wavename==12
replace year=2021 if wavename==13
replace year=2022 if wavename==14
replace year=1991 if wavename==15
replace year=1992 if wavename==16
replace year=1993 if wavename==17
replace year=1994 if wavename==18
replace year=1995 if wavename==19
replace year=1996 if wavename==20
replace year=1997 if wavename==21
replace year=1998 if wavename==22
replace year=1999 if wavename==23
replace year=2000 if wavename==24
replace year=2001 if wavename==25
replace year=2002 if wavename==26
replace year=2003 if wavename==27
replace year=2004 if wavename==28
replace year=2005 if wavename==29
replace year=2006 if wavename==30
replace year=2007 if wavename==31
replace year=2008 if wavename==32

// key DoL variables: husits howlng hubuys hufrys huiron humops jbhrs huboss (but in less waves of the old survey)
// key other variables: scend_dv jbstat mastat mastat_dv nchild_dv marstat age age_dv doby doby_dv qfhigh_dv hiqual_dv racel_dv ethn_dv sex sex_dv
// key ID variables: pidp hidp pno
// key linking variables: ppid ppno sppid sppno

** Demographic and related variables

// first, get variables from xwave file
merge m:1 pidp using "$temp/xwave_tomatch.dta" // only 4 didn't match
drop if _merge==2
drop _merge
// browse pidp year xw_*

gen survey=.
replace survey=1 if inrange(wavename,1,14)
replace survey=2 if inrange(wavename,15,32)
label define survey 1 "UKHLS" 2 "BHPS"
label values survey survey

tab wavename survey, m
tab survey xw_xwdat_dv, m

tab xw_memorig, m

// going to see if I can fill in missing sex variables
tab sex, m
recode sex (-9/0=.)
tab sex xw_sex, m

browse pidp year sex hgsex xw_sex
replace xw_sex = sex if xw_sex==. & sex!=.
replace xw_sex = hgsex if xw_sex==. & hgsex!=.
replace xw_sex = sex_dv if xw_sex==. & sex_dv!=.

tab xw_sex, m
recode xw_sex (-9/0=.)

tab age if survey==2, m
tab age_dv if survey==1, m

tab birthy, m
tab doby if survey==2, m
tab doby_dv if survey==1, m
browse pidp survey birthy doby age doby_dv age_dv

gen age_all=.
replace age_all = age if survey==2
replace age_all = age_dv if survey==1
replace age_all = . if age_all==-9
tab age_all, m

gen dob_year=.
replace dob_year = doby if survey==2
replace dob_year = doby_dv if survey==1
replace dob_year = . if inrange(dob_year,-9,-1)
replace dob_year = istrtdaty - age_all if dob_year==.
tab dob_year, m

replace age_all = istrtdaty - dob_year if age_all == .

// are these just respondents in sample? is there a sample status?
bysort pidp: egen first_year_observed = min(istrtdaty)
bysort pidp: egen last_year_observed = max(istrtdaty)
sort pidp year

browse pidp year istrtdaty wavename ivfio sex age_all dob_year first_year_observed last_year_observed

// education
// also need to figure out how to get college degree equivalent (see Musick et al 2020 - use the ISCED guidelines to identify bachelor's degree equivalents as the completion of tertiary education programs, excluding higher vocational programs
fre hiqual_dv // think need to use the component variable of this
tab hiqual_dv survey, m // both
replace hiqual_dv=. if inlist(hiqual_dv,-8,-9)
tab qfhigh_dv survey, m // this is only ukhls
// other educations: qfachi (bhps only) qfedhi (bhps only - details) qfhigh (not v good) qfhigh_dv (ukhls details) hiqual_dv hiqualb_dv (bhps only) isced11_dv (only UKHS, but lots of missing, which is sad bc I think it's what I nee/d?) isced (bhps, might be helpful?)

tab qfhigh_dv hiqual_dv
tab isced11_dv hiqual_dv // see what bachelors in isced is considered in hiqual // okay so bachelor's / master's in degree. some of bachelor's also in "other higher degree"
tab isced hiqual_dv // so here 5a and 6 are in degree. 5b is what is in other degree (vocational) - do I want that? I feel like Musick didn't include vocational.

gen college_degree=0
replace college_degree=1 if  hiqual_dv==1
replace college_degree=. if hiqual_dv==.

/*
Undergraduate degrees are either level 4, 5 or 6 qualifications, with postgraduate degrees sitting at level 7 or 8. In Scotland, awards are at level 9 or 10 for an undergraduate degree, and level 11 and 12 for master's and doctorates.
A bachelor's degree involves studying one, or sometimes two, subjects in detail. It's the most common undergraduate degree in the UK and is a level 6 qualification (level 9 or 10 in Scotland). 
*/

// these are the cross-wave race variables and are best to use.
tab xw_racel_dv survey, m
tab xw_ethn_dv survey, m // this one has less missing

// country
recode gor_dv (1/9=1)(10=2)(11=3)(12=4), gen(country_all)
replace country_all=. if inlist(country_all,-9,13)
label define country 1 "England" 2 "Wales" 3 "Scotland" 4 "N. Ireland"
label values country_all country

replace gor_dv = . if gor_dv==-9

// so many marital statuses STILL
tab mlstat survey, m // present legal marital status - has a lot of inapplicable, so doesn't seem right - think only for new people?
tab marstat survey, m  // legal marital status -- only for ukhls, doesn't seem to include cohabiting
tab marstat_dv survey, m // harmonized de facto - includes cohabiting, but only ukhls
tab mastat_dv survey, m // de facto - confused, because also only ukhls, so think above is better / cleaner?
tab currmstat survey, m // current legal marital status - many missings, think this wasn't added until later
tab mlstat_bh survey, m // present legal marital status - so this is bhps legal
tab mastat survey, m // marital status - okay this has cohabiting for bhps

fre mlstat_bh // 1 married, 2 sep, 3 divorced, 4 widowed, 5 never married
fre marstat // 1 never married, 2 married, 3 civil partnership, 4 separated, 5 divorced, 6 widowed, 7 sep civil, 8 divorced civil, 9 widow civil
gen marital_status_legal=. // use mlstat_bh for bhps; marstat for ukhls, but need to recode because not currently on same scale
* populate for bhps
replace marital_status_legal = 1 if survey==2 & mlstat_bh==1
replace marital_status_legal = 2 if survey==2 & mlstat_bh==2
replace marital_status_legal = 3 if survey==2 & mlstat_bh==3
replace marital_status_legal = 4 if survey==2 & mlstat_bh==4
replace marital_status_legal = 5 if survey==2 & mlstat_bh==5
* populate for ukhls
replace marital_status_legal = 1 if survey==1 & inlist(marstat,2,3) // married, including civil partner
replace marital_status_legal = 2 if survey==1 & inlist(marstat,4,7) // sep, including civil
replace marital_status_legal = 3 if survey==1 & inlist(marstat,5,8) // divorced, including civil
replace marital_status_legal = 4 if survey==1 & inlist(marstat,6,9) // widowed, including civil
replace marital_status_legal = 5 if survey==1 & marstat==1 // never married including civil
tab marital_status_legal, m // most of these missing are proxy interviews from ukhls
tab age_all marital_status_legal, m
tab mlstat_bh marital_status_legal, m
tab marstat marital_status_legal, m

label define marital_status_legal 1 "Married" 2 "Separated" 3 "Divorced" 4 "Widowed" 5 "Never Married"
label values marital_status_legal marital_status_legal

fre mastat // 1 married, 2 cohab, 3 widowed, 4 divorced, 5 separated, 6 never married
fre marstat_dv // 1 married, 2 cohab, 3 widowed, 4 divorced, 5 separated, 6 never married
gen marital_status_defacto=. // use mastat for bhps; marstat_dv for ukhls, but need to recode because not currently on same scale
* populate for bhps
replace marital_status_defacto = 1 if survey==2 & mastat==1
replace marital_status_defacto = 2 if survey==2 & mastat==2
replace marital_status_defacto = 3 if survey==2 & mastat==5
replace marital_status_defacto = 4 if survey==2 & mastat==4
replace marital_status_defacto = 5 if survey==2 & mastat==3
replace marital_status_defacto = 6 if survey==2 & mastat==6
* populate for ukhls
replace marital_status_defacto = 1 if survey==1 & marstat_dv==1
replace marital_status_defacto = 2 if survey==1 & marstat_dv==2
replace marital_status_defacto = 3 if survey==1 & marstat_dv==5
replace marital_status_defacto = 4 if survey==1 & marstat_dv==4
replace marital_status_defacto = 5 if survey==1 & marstat_dv==3
replace marital_status_defacto = 6 if survey==1 & marstat_dv==6

tab marital_status_defacto, m
tab age_all marital_status_defacto, m
tab mastat marital_status_defacto, m
tab marstat_dv marital_status_defacto, m

label define marital_status_defacto 1 "Married" 2 "Cohabiting" 3 "Separated" 4 "Divorced" 5 "Widowed" 6 "Never Married"
label values marital_status_defacto marital_status_defacto

gen partnered=.
replace partnered=0 if inrange(marital_status_defacto,3,6)
replace partnered=1 if inlist(marital_status_defacto,1,2)

inspect ppid
inspect ppid if partnered==1 // this is only ukhls
inspect ppid if partnered==1 & survey==1
inspect sppid if partnered==1 // okay this is also only ukhls and is just spouses
inspect sppid_bh if partnered==1 // okay this is bhps but includes spouses and partners
inspect sppid_bh if partnered==1 & survey==2 

merge m:1 sppid_bh using "$temp/spid_lookup.dta"
drop if _merge==2
browse survey pidp pid sppid_bh partner_pidp_bh _merge
inspect sppid_bh if partnered==1 & survey==2 
inspect partner_pidp_bh if partnered==1 & survey==2 
drop _merge

browse pidp wavename survey marital_status_defacto partnered ppid sppid sppid_bh

gen long partner_id = .
replace partner_id = ppid if survey==1
// replace partner_id = sppid_bh if survey==2
replace partner_id = partner_pidp_bh if survey==2 // okay will try to use pidp instead? I don't know if this is smart?
replace partner_id = . if partner_id <=0 // inapplicable / spouse not in HH

inspect partner_id
inspect partner_id if partnered==1

forvalues y=1991/2022{
	display `y'
	inspect partner_id if partnered==1 & year==`y'
}

sort pidp year
browse pidp wavename year survey marital_status_defacto partnered partner_id ppid partner_pidp_bh sppid_bh if partnered==1

********************************************************************************
** DoL variables
********************************************************************************

foreach var in howlng husits hubuys hufrys huiron humops huboss jbstat aidhh aidxhh{
	recode `var' (-10/-1=.)
}

// some better employment variables
fre jbstat
gen employed=.
replace employed=0 if inrange(jbstat,3,97)
replace employed=1 if inlist(jbstat,1,2)

recode jbhrs (-8=0)(-9=.)(-7/-1=.)

fre jbot
recode jbot (-8=0)(-9=.)(-7/-1=.)

egen total_hours=rowtotal(jbhrs jbot), missing

tabstat jbhrs jbot total_hours, by(jbstat)

sum howlng, detail
sum jbhrs, detail
sum jbhrs if employed==1, detail
tab husits, m
fre husits

// earnings 
recode fimnlabgrs_dv (-9=.)(-7=.) // Total personal monthly labour income gross
recode fimnlabnet_dv (-9=.)(-1=.) // net is only asked in UKHLS
recode paynu_dv (-9=.)(-7=.)(-8=0) // usual net pay per month: current job

tabstat fimnlabgrs_dv fimnlabnet_dv paynu_dv, by(year)

// HH incomes
tabstat fihhmngrs_dv fihhmnlabnet_dv fihhmnlabgrs_dv fihhml fihhyl grpay hhneti hhyneti hhyrlg hhyrln netlab, by(year)
// fihhmngrs_dv is only one asked in all waves, though I could probably combine a few? But this is gross household income: month before interview
recode fihhmngrs_dv (-9=.)

sort hidp year
browse hidp pidp year fimnlabgrs_dv paynu_dv fihhmngrs_dv
sort pidp year

// unpaid labor variables
foreach var in hubuys hufrys huiron humops{ // coding changed starting wave 12 (and then only asked every other year)
	gen `var'_v0 = `var'
	replace `var' = 1 if inlist(wavename,12,14) & inlist(`var'_v0,1,2) // 1 = mostly self
	replace `var' = 2 if inlist(wavename,12,14) & inlist(`var'_v0,4,5) // 2 = mostly partner
	replace `var' = 3 if inlist(wavename,12,14) & inlist(`var'_v0,3) // 3 = shared
	replace `var' = 97 if inlist(wavename,12,14) & inlist(`var'_v0,6,7) // 97 = other
	replace `var'=. if `var'_v0==8
	replace `var'=. if `var'==5
	tab `var', m
	
}

tab huboss, m
replace huboss=97 if huboss==4 // other was 4 in bhps

gen howlng_flag=0
replace howlng_flag = 1 if inlist(wavename,1,2,4,6,8,10,12,14) | inrange(wavename,16,32) // when howlng (continuous HW) is asked
// 1,2,4,6,8,10,12,BH02,BH03,BH04,BH05,BH06,BH07,BH08,BH09,BH10,BH11,BH12,BH13,BH14,BH15,BH16,BH17,BH18

gen housework_flag=0
replace housework_flag = 1 if inlist(wavename,2,4,6,8,10,12,14,15) | inrange(wavename,18,32) // when HW categorical vars are asked
// 2,4,6,8,10,12,BH01,BH04,BH05,BH06,BH07,BH08,BH09,BH10,BH11,BH12,BH13,BH14,BH15,BH16,BH17,BH18

tab aidhh survey, m
tab aidxhh survey, m
tab aidhrs survey, m
recode aidhrs (-8=0)(-10/-9=.)(-7/-1=.)

// child variables
foreach var in nch02_dv nch34_dv nch511_dv nch1215_dv agechy_dv nkids_dv{
	replace `var'=. if `var'==-9
}

egen nchild_015 = rowtotal(nch02_dv nch34_dv nch511_dv nch1215_dv), missing
browse nchild_dv nkids_dv nchild_015 nch02_dv nch34_dv nch511_dv nch1215_dv

tab agechy_dv nchild_dv, m
tab agechy_dv nchild_015, m
tab agechy_dv nkids_dv, m

tab xw_anychild_dv, m
tab xw_ch1by_dv xw_anychild_dv, m
tab agechy_dv xw_anychild_dv, m

gen year_first_birth = xw_ch1by_dv
replace year_first_birth = 9999 if xw_anychild_dv==2

gen age_youngest_child = agechy_dv
replace age_youngest_child = 9999 if agechy_dv==-8
tab age_youngest_child, m

tab husits if partnered==1, m
tab husits if nchild_dv!=0, m
tab wavename husits if nchild_dv!=0, m // still a lot of missing..oh, well, i guess they could have child AND not be partnered DUH
tab wavename husits if nchild_dv!=0 & partnered==1, m // still a lot of missing..oh, well, i guess they could have child AND not be partnered. okay still a lot of missing, might be proxy surveys
tab hubuys housework_flag if partnered==1, m col
tab hufrys housework_flag if partnered==1, m col
tab huiron housework_flag if partnered==1, m col
tab humops housework_flag if partnered==1, m col
tab huboss housework_flag if partnered==1, m col

browse pidp hidp wavename age_all partnered marital_status_defacto husits howlng hubuys hufrys huiron humops jbhrs

// let's do a check of the variables I will probably use in analysis (at least psm) so I can figure out any missings and if properly recoded
foreach var in jbhrs total_hours howlng aidhrs fimnlabgrs_dv jbstat employed hiqual_dv nchild_dv nkids_dv agechy_dv partnered marital_status_defacto fihhmngrs_dv xw_ethn_dv country_all dob_year year_first_birth xw_memorig xw_sampst ivfio xw_sex{
	inspect `var'
}

********************************************************************************
* Okay, let's add on marital history as well, so I can use this to get duration / relationship order
********************************************************************************
// main partner history
merge m:1 pidp using "$temp/partner_history_tomatch.dta", keepusing(mh_*)
tab marital_status_defacto _merge, row // so def some missing that shouldn't be... but not a lot (like coverage is 97-98% for those cohab / married, a little less for those dissolved)
drop if _merge==2

gen in_partner_history=0
replace in_partner_history=1 if _merge==3

drop _merge

// backup marital history
merge m:1 pidp using "$temp/marriage_combined_wide.dta", keepusing(mh2_*)
drop if _merge==2
drop _merge

	// browse pidp year mh_starty1 mh_endy1 mh_starty2 mh_endy2 mh2_year_marr1 mh2_year_end1 mh2_how_end1 mh2_year_marr4 mh2_year_end4 mh2_how_end4 mh2_year_marr2 mh2_year_marr3 // so sometimes mh2 year 4 is actually year 1 lolz (maybe most to least recent?)

// create necessary variables
sort pidp year
browse pidp year sex marital_status_defacto year partner_id mh_partner1 mh_status1 mh_starty1 mh_endy1 mh_partner2 mh_status2 mh_starty2 mh_endy2 mh_partner3 mh_status3  mh_starty3 mh_endy3 mh_partner4 mh_status4

label define status_new 1 "Marriage" 2 "Cohab"
foreach var in mh_status1 mh_status2 mh_status3 mh_status4 mh_status5 mh_status6 mh_status7 mh_status8 mh_status9 mh_status10 mh_status11 mh_status12 mh_status13 mh_status14{
	gen x_`var' = `var'
	replace `var' = 1 if inlist(`var',2,3)
	replace `var' = 2 if `var'==10
	label values `var' .
	label values `var' status_new
}

gen rel_no=. // add this to get lookup for current relationship start and end dates
forvalues r=1/14{
	replace rel_no = `r' if mh_status`r' == marital_status_defacto & mh_partner`r' == partner_id & partner_id!=.
}

// should this also be based on survey yr being between relationship start and end dates? Because of the people who are missing partner ids? But I guess they are not useful if missing partner ids? but at least to see what I can fill in because some have partner ids in main fiile but not in the marital history
gen rel_no_alt=.
forvalues r=1/14{
	replace rel_no_alt = `r' if istrtdaty >= mh_starty`r' & istrtdaty <= mh_endy`r' & mh_starty`r'!=. & mh_endy`r'!=.
}

replace rel_no = rel_no_alt if rel_no==. & partner_id!=.

browse pidp sex marital_status_defacto year partner_id rel_no mh_partner1 mh_status1 mh_starty1 mh_endy1 mh_partner2 mh_status2 mh_starty2 mh_endy2 mh_partner3 mh_status3  mh_starty3 mh_endy3 mh_partner4 mh_status4

tab rel_no if inlist(marital_status_defacto,1,2), m // so about 4% missing. come back to this - can I see if any started during the survey to at least get duration?
tab year rel_no if inlist(marital_status_defacto,1,2), m row // check that updated marital history file made this consistent again by year

sort pidp year
// browse pidp year marital_status_defacto

// manually created relationship transitions
*enter
gen rel_start=0
replace rel_start=1 if (inlist(marital_status_defacto,1,2) & inlist(marital_status_defacto[_n-1],3,4,5,6)) & pidp==pidp[_n-1] & year==year[_n-1]+1

*exit
gen rel_end=0
replace rel_end=1 if (inlist(marital_status_defacto,3,4,5,6) & inlist(marital_status_defacto[_n-1],1,2)) & pidp==pidp[_n-1] & year==year[_n-1]+1

gen rel_end_pre=0
replace rel_end_pre=1 if (inlist(marital_status_defacto,1,2) & inlist(marital_status_defacto[_n+1],3,4,5,6)) & pidp==pidp[_n+1] & year==year[_n+1]-1

*cohab to marr
gen marr_trans=0
replace marr_trans=1 if (marital_status_defacto==1 & marital_status_defacto[_n-1]==2) & pidp==pidp[_n-1] & partner_id==partner_id[_n-1] & year==year[_n-1]+1

// browse pidp sex year marital_status_defacto partner_id rel_no rel_start rel_end rel_end_pre marr_trans mh_partner1 mh_status1 mh_starty1 mh_endy1 mh_partner2 mh_status2 mh_starty2 mh_endy2 mh_partner3 mh_status3  mh_starty3 mh_endy3 mh_partner4 mh_status4

// I am going to create individual level versions of these variables for now and then check in next file against partners to make sure they match.
gen current_rel_start_year=.
gen current_rel_start_month=.
gen current_rel_end_year=.
gen current_rel_end_month=.
gen current_rel_ongoing=.
// gen current_rel_how_end=.
gen current_rel_marr_end=.
gen current_rel_coh_end=.

forvalues r=1/14{
	replace current_rel_start_year = mh_starty`r' if rel_no==`r'
	replace current_rel_start_month = mh_startm`r' if rel_no==`r'
	replace current_rel_end_year = mh_endy`r' if rel_no==`r'
	replace current_rel_end_month = mh_endm`r' if rel_no==`r'
	replace current_rel_ongoing = mh_ongoing`r' if rel_no==`r'
	// replace current_rel_how_end = mrgend`r' if rel_no==`r' & status`r'==1 // if marriage - okay this actually won't work because the codes are different between marriage and cohab
	// replace current_rel_how_end = cohend`r' if rel_no==`r' & status`r'==2 // if cohab
	replace current_rel_marr_end = mh_mrgend`r' if rel_no==`r'
	replace current_rel_coh_end = mh_cohend`r' if rel_no==`r'
}

replace current_rel_start_year=. if current_rel_start_year==-9
replace current_rel_start_month=. if current_rel_start_month==-9
replace current_rel_end_year=. if current_rel_end_year==-9
replace current_rel_end_month=. if current_rel_end_month==-9

label values current_rel_ongoing ongoing
label values current_rel_marr_end mrgend
label values current_rel_coh_end cohend

browse pidp year marital_status_defacto partner_id rel_no current_rel_start_year current_rel_start_month current_rel_end_year current_rel_end_month current_rel_ongoing rel_start rel_end rel_end_pre current_rel_marr_end current_rel_coh_end mh_partner1 mh_status1 mh_starty1 mh_startm1 mh_endy1 mh_endm1 mh_divorcey1 mh_divorcem1 mh_mrgend1 mh_cohend1 mh_ongoing1 mh_partner2 mh_status2 mh_starty2 mh_startm2 mh_endy2 mh_endm2 mh_divorcey2 mh_divorcem2 mh_mrgend2 mh_cohend2 mh_ongoing2

// check start dates if I use my manually created ones - to help with later info
gen rel_start_year_est = istrtdaty if rel_start==1
bysort pidp partner_id (rel_start_year_est): replace rel_start_year_est=rel_start_year_est[1]  if partner_id!=. // if partner id is missing, this actually doesn't work well

gen rel_end_year_est = istrtdaty if rel_end_pre==1
bysort pidp partner_id (rel_end_year_est): replace rel_end_year_est=rel_end_year_est[1]  if partner_id!=. // if partner id is missing, this actually doesn't work well

sort pidp year

gen rel_start_check=.
replace rel_start_check=0 if current_rel_start_year!=rel_start_year_est & current_rel_start_year!=. & rel_start_year_est!=.
replace rel_start_check=1 if current_rel_start_year==rel_start_year_est & current_rel_start_year!=. & rel_start_year_est!=.

tab rel_start_check // okay not actually a lot of congruence (even when they are both non-missing). this is problematic if the first year observed they are already in a rel. like most are just missing...

replace current_rel_start_year = rel_start_year_est if current_rel_start_year==. & rel_start_year_est!=. & partner_id!=. & istrtdaty>=rel_start_year_est
replace current_rel_end_year = rel_end_year_est if current_rel_end_year==. & rel_end_year_est!=. & partner_id!=. & istrtdaty<=rel_end_year_est

// check how many relationships have the proper info
tab current_rel_start_year if partner_id!=. , m // about 5% missing
tab current_rel_start_year if inlist(marital_status_defacto,1,2), m // about 6% missing
tab current_rel_start_year partnered, m col
tab current_rel_end_year partnered, m col

// for those with missing, maybe if only 1 spell, use that info? as long as the status matches and the interview date is within the confines of the spell?
// this might have been covered in my "rel no alt" but let's try
browse pidp istrtdaty marital_status_defacto partner_id rel_no current_rel_start_year rel_start_year_est current_rel_end_year rel_start rel_end rel_end_pre mh_ttl_spells mh_partner1 mh_status1 mh_starty1 mh_startm1 mh_endy1 mh_endm1 mh_divorcey1 mh_divorcem1 mh_mrgend1 mh_cohend1 mh_ongoing1 mh_partner2 mh_status2 mh_starty2 mh_startm2 mh_endy2 mh_endm2 mh_divorcey2 mh_divorcem2 mh_mrgend2 mh_cohend2 mh_ongoing2

gen rel_no_orig=rel_no

forvalues r=1/14{
	replace current_rel_start_year = mh_starty`r' if rel_no==. & partner_id!=. & inlist(marital_status_defacto,1,2) & marital_status_defacto==mh_status`r' & istrtdaty>=mh_starty`r' & istrtdaty<=mh_endy`r'
	replace current_rel_start_month = mh_startm`r' if rel_no==. & partner_id!=. & inlist(marital_status_defacto,1,2) & marital_status_defacto==mh_status`r' & istrtdaty>=mh_starty`r' & istrtdaty<=mh_endy`r'
	replace current_rel_end_year = mh_endy`r' if rel_no==. & partner_id!=. & inlist(marital_status_defacto,1,2) & marital_status_defacto==mh_status`r' & istrtdaty>=mh_starty`r' & istrtdaty<=mh_endy`r'
	replace current_rel_end_month = mh_endm`r' if rel_no==. & partner_id!=. & inlist(marital_status_defacto,1,2) & marital_status_defacto==mh_status`r' & istrtdaty>=mh_starty`r' & istrtdaty<=mh_endy`r'
	replace current_rel_ongoing = mh_ongoing`r' if rel_no==. & partner_id!=. & inlist(marital_status_defacto,1,2) & marital_status_defacto==mh_status`r' & istrtdaty>=mh_starty`r' & istrtdaty<=mh_endy`r'

	replace rel_no=`r' if rel_no==. & partner_id!=. & inlist(marital_status_defacto,1,2) & marital_status_defacto==mh_status`r' & istrtdaty>=mh_starty`r' & istrtdaty<=mh_endy`r' // okay yes this didn't add any
}

// can I use the dates from the main file, not marital history? or the cross-wave file?
tabstat lmar1y coh1by coh1ey lmcby41 lmcby42 lmspy41 lmspy42, by(year)

by pidp: egen first_marr_yr = max(lmar1y)
by pidp: egen first_cohab_yr = max(coh1by)
by pidp: egen first_cohab_end_yr = max(coh1ey)

forvalues c=1/7{
	by pidp: egen cohab`c'_yr = max(lmcby4`c')
	by pidp: egen cohab`c'_end_yr = max(lmspy4`c')
}

tab lmar1y survey if current_rel_start_year==., m
tab first_marr_yr survey if current_rel_start_year==., m
tab xw_lmar1y_dv survey if current_rel_start_year==., m
tab xw_coh1y_dv survey if current_rel_start_year==., m

sort pidp year
browse pidp istrtdaty marital_status_defacto partner_id rel_no mh_ttl_spells mh_ttl_married mh_ttl_cohabit xw_lmar1y_dv xw_coh1y_dv lmar1y first_marr_yr coh1by coh1ey first_cohab_yr lmcby41 lmcby42 mh_starty1 mh_endy1 mh_starty2 mh_endy2 if current_rel_start_year==. & partnered==1

replace current_rel_start_year = xw_lmar1y_dv if current_rel_start_year==. & marital_status_defacto==1 & mh_ttl_married==1 & inrange(xw_lmar1y_dv, 1900,2024) // if married and only has one marriage, use this date?
replace current_rel_start_year = first_marr_yr if current_rel_start_year==. & marital_status_defacto==1 & mh_ttl_married==1 & inrange(first_marr_yr, 1900,2024)
replace current_rel_start_year = xw_coh1y_dv if current_rel_start_year==. & marital_status_defacto==2 & mh_ttl_cohabit==1 & inrange(xw_coh1y_dv, 1900,2024) 
replace current_rel_start_year = first_cohab_yr if current_rel_start_year==. & marital_status_defacto==2 & mh_ttl_cohabit==1 & inrange(first_cohab_yr, 1900,2024) // same for cohab
replace current_rel_start_year = cohab1_yr if current_rel_start_year==. & marital_status_defacto==2 & mh_ttl_cohabit==1 & inrange(cohab1_yr, 1900,2024) 
replace current_rel_end_year = first_cohab_end_yr if current_rel_end_year==. & marital_status_defacto==2 & mh_ttl_cohabit==1 & inrange(first_cohab_end_yr, 1900,2024) 
replace current_rel_end_year = cohab1_end_yr if current_rel_end_year==. & marital_status_defacto==2 & mh_ttl_cohabit==1 & inrange(cohab1_end_yr, 1900,2024)
// replace current_rel_start_year = mh_starty1 if current_rel_start_year==. & mh_ttl_spells==1 & inrange(mh_starty1, 1900,2024) & inlist(marital_status_defacto,1,2) // this is giving me errors for cohabitors sometimes
// replace current_rel_end_year = mh_endy1 if current_rel_end_year==. & mh_ttl_spells==1 & inrange(mh_endy1, 1900,2024) & inlist(marital_status_defacto,1,2)

sort pidp year
browse pidp year mh_ttl_cohabit rel_no rel_no_alt rel_no_orig current_rel_start_year current_rel_end_year rel_start rel_end rel_end_pre xw_coh1y_dv coh1by coh1ey cohab1_yr cohab2_yr cohab1_end_yr cohab2_end_yr if marital_status_defacto==2  & (current_rel_start_year==. | current_rel_end_year==.)
browse pidp year mh_ttl_married rel_no current_rel_start_year current_rel_end_year rel_start rel_end rel_end_pre xw_lmar1y_dv lmar1y mh2_year_marr1 mh2_year_end1 mh2_how_end1 mh2_year_marr4 mh2_year_end4 mh2_how_end4 mh2_year_marr3 mh2_year_end3 mh2_year_marr2 mh2_year_end2 mh_status1 mh_starty1 mh_endy1 if marital_status_defacto==1 & (current_rel_start_year==. | current_rel_end_year==.)

tab current_rel_start_year marital_status_defacto if partnered==1, m col
tab current_rel_end_year marital_status_defacto if partnered==1, m col

// browse pidp istrtdaty marital_status_defacto partner_id rel_no current_rel_start_year current_rel_end_year rel_start rel_end rel_end_pre mh_ttl_spells mh_partner1 mh_status1 mh_starty1 mh_startm1 mh_endy1 mh_endm1 xw_lmar1y_dv xw_coh1y_dv mh_divorcey1 mh_divorcem1 mh_mrgend1 mh_cohend1 mh_ongoing1 first_marr_yr first_cohab_yr mh_partner2 mh_status2 mh_starty2 mh_startm2 mh_endy2 mh_endm2 mh_divorcey2 mh_divorcem2 mh_mrgend2 mh_cohend2 mh_ongoing2

// what things are correlated with missing relationship start year?
gen missing_rel_start=.
replace missing_rel_start = 0 if partnered==1 & current_rel_start_year!=.
replace missing_rel_start = 1 if partnered==1 & current_rel_start_year==.

gen missing_rel_end=.
replace missing_rel_end = 0 if partnered==1 & current_rel_end_year!=.
replace missing_rel_end = 1 if partnered==1 & current_rel_end_year==.

tab sampst missing_rel_start, row // codebook says psm and tsm might have more missing, that is true (We are aware that the information about past partnership history is incomplete or missing for new entrants and those who asked someone else to complete their interview on their behalf (proxy interviews))
tab sampst missing_rel_end, row 
tab ivfio missing_rel_start, row // and proxy interviews, so that is also true (96% of full interviews have a rel start)
tab ivfio missing_rel_end, row 
tab college_degree missing_rel_start, row // quite evenly distrbuted
tab college_degree missing_rel_end, row 
tab country_all missing_rel_start, row // quite evenly distributed
tab employed missing_rel_start, row // quite evenly distributed
tab age_all missing_rel_start, row
// okay so it really is about the type of sample / interview, not really the people themselves. so I think fine to just leave as missing for now?

// final clean up of file before saving
gen int_year = istrtdaty 
replace int_year = year if istrtdaty < 0 // missing / dk, so not very helpful...

// also need to make sure the start / end dates are congruent if the couple transitioned from cohabitation to marriage gAH
bysort pidp partner_id: egen ever_transition = max(marr_trans) if partnered==1
gen year_transitioned = int_year if marr_trans==1 // note, year transition is first year marrriage. so years prior are cohab, then that year onward is marriage
bysort pidp partner_id (year_transitioned): replace year_transitioned = year_transitioned[1]

sort pidp year
browse pidp partner_id int_year marital_status_defacto marr_trans ever_transition year_transitioned if partnered==1

// some cohab dates missing so the above is messing things up for marriage to cohab (start date after rel start)
bysort pidp partner_id: egen first_couple_year = min(int_year) if partnered==1
bysort pidp partner_id: egen last_couple_year = max(int_year) if partnered==1

sort pidp year
browse pidp partner_id int_year marital_status_defacto first_couple_year last_couple_year current_rel_start_year current_rel_end_year marr_trans if ever_transition==1

// updating dates specifically for those who transition because it will mess up the codes otherwise. add a flag to denote that I did still
gen rel_end_flag=.
replace rel_end_flag=0 if current_rel_end_year!=.
replace rel_end_flag=1 if current_rel_end_year==.

gen rel_start_flag=.
replace rel_start_flag=0 if current_rel_start_year!=.
replace rel_start_flag=1 if current_rel_start_year==.

replace current_rel_start_year = first_couple_year if current_rel_start_year==. & ever_transition==1 & marital_status_defacto==2
replace current_rel_end_year = last_couple_year if current_rel_end_year==. & ever_transition==1 & marital_status_defacto==1

bysort pidp partner_id: egen rel_start_all = min(current_rel_start_year) if partnered==1
bysort pidp partner_id: egen rel_end_all = max(current_rel_end_year) if partnered==1
bysort pidp partner_id: egen status_all = max(current_rel_ongoing) if partnered==1

capture label define status 0 "ended" 1 "ongoing"
label values status_all status

sort pidp year
replace rel_end_all = rel_end_all[_n-1] if rel_end_all==. & rel_start_all == rel_start_all[_n-1] & pidp==pidp[_n-1]

browse pidp partner_id int_year marital_status_defacto first_couple_year last_couple_year rel_start_all rel_end_all status_all current_rel_start_year current_rel_end_year marr_trans ever_transition

gen date_check=0
replace date_check = 1 if rel_end_all < rel_start_all & rel_end_all!=. & rel_start_all!=.

// browse pidp partner_id int_year marital_status_defacto rel_no first_couple_year last_couple_year rel_start_all rel_end_all status_all current_rel_start_year current_rel_end_year mh_starty1 mh_endy1 mh_starty2 mh_endy2 if date_check==1

replace rel_start_all = first_couple_year if date_check==1 & rel_start_all > last_couple_year
replace rel_end_all = last_couple_year if date_check==1 & rel_end_all < first_couple_year

save "$created_data/UKHLS_long_all_recoded.dta", replace

// note: this file is still NOT restricted in any way
unique pidp // 118405, 807942 total py
unique pidp partner_id // 135496	
unique pidp, by(xw_sex) // 56230 m, 62170 w
unique pidp, by(partnered) // 59583 0, 73581 1
unique pidp partner_id if partnered==1 & current_rel_start_year==. // 8124 couples. does that feel like too many to be missing?
unique pidp partner_id if partnered==1 & current_rel_end_year==. //  8150 couples. does that feel like too many to be missing?
// unique pidp partner_id if partnered==1, by(current_rel_start_year)