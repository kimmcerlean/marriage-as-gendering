********************************************************************************
********************************************************************************
* Project: Marriage as Gendering
* Owner: Kimberly McErlean
* Started: March 2025
* File: cohab_transition_analysis
********************************************************************************
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* This files actually conducts the analysis

use "$created_data/gsoep_matched_couples.dta", clear

********************************************************************************
* Final sample restrictions and things
********************************************************************************
// deduplicate
tab marst_defacto
tab marst_defacto if sex_pl==1

/* need to end up with this amount of respondents after the below

  marst_defacto |      Freq.     Percent        Cum.
----------------+-----------------------------------
        Married |    215,977       89.24       89.24
      Partnered |     26,039       10.76      100.00
----------------+-----------------------------------
          Total |    242,016      100.00


*/

// keep only one respondent per household (bc all data recorded for all)
sort hid pid syear
browse hid pid partner_id_pl syear sex_pl sex_pl_sp

bysort syear hid : egen per_id = rank(pid)
browse hid pid partner_id_pl syear per_id sex_pl sex_pl_sp

tab per_id, m // 1s should approximately total above
keep if inlist(per_id,1,3,5,7)

tab marst_defacto
/*
  marst_defacto |      Freq.     Percent        Cum.
----------------+-----------------------------------
        Married |    215,841       89.18       89.18
      Partnered |     26,183       10.82      100.00
----------------+-----------------------------------
          Total |    242,024      100.00
*/

// now only keep relationships that started after a certain date
tab syear marst_defacto, m
tab rel_start_all marst_defacto, m

unique pid partner_id_pl, by(marst_defacto) 
unique pid partner_id_pl if rel_start_all >=1985, by(marst_defacto) // I def don't want 1984
unique pid partner_id_pl if rel_start_all >=1990, by(marst_defacto)

keep if rel_start_all >= 1985 // consistent with psid except ukhls only started 1991 so think I will need to revisit this...

// variables to get treated and control
bysort pid partner_id_pl: egen years_observed=count(syear)

bysort pid partner_id_pl: egen years_married=count(syear) if marst_defacto==1
bysort pid partner_id_pl (years_married): replace years_married = years_married[1]

bysort pid partner_id_pl: egen years_cohab=count(syear) if marst_defacto==2
bysort pid partner_id_pl (years_cohab): replace years_cohab = years_cohab[1]

sort pid partner_id_pl syear
browse pid partner_id_pl syear years_observed years_married years_cohab ever_transition marst_defacto marr_trans rel_start_all year_transitioned

gen always_married=0
replace always_married=1 if years_observed==years_married & years_married!=.

gen always_cohab=0
replace always_cohab=1 if years_observed==years_cohab & years_cohab!=.

	// none of these should overlap
	tab always_cohab always_married, m
	tab always_cohab ever_transition, m
	tab always_married ever_transition, m
	
browse pid partner_id_pl syear years_observed years_married years_cohab ever_transition marst_defacto marr_trans rel_start_all year_transitioned if ever_transition==1 & (always_cohab==1 | always_married==1)
	
// only keep certain ages
tab age employed_binary, row
keep if (age>=18 & age<=60) &  (age_sp>=18 & age_sp<=60) // sort of drops off a cliff after 60?

// create couple-level education
tab isced97_pg isced97_pg_sp, m

gen couple_educ_gp=0
replace couple_educ_gp = 1 if isced97_pg==6 | isced97_pg_sp==6
replace couple_educ_gp=. if isced97_pg==. & isced97_pg_sp==.

tab couple_educ_gp, m

// better detailed educ
gen educ_recode=.
replace educ_recode=1 if inlist(isced97_pg,0,1,2)
replace educ_recode=2 if isced97_pg==3
replace educ_recode=3 if isced97_pg==4
replace educ_recode=4 if isced97_pg==5
replace educ_recode=5 if isced97_pg==6

gen educ_recode_sp=.
replace educ_recode_sp=1 if inlist(isced97_pg_sp,0,1,2)
replace educ_recode_sp=2 if isced97_pg_sp==3
replace educ_recode_sp=3 if isced97_pg_sp==4
replace educ_recode_sp=4 if isced97_pg_sp==5
replace educ_recode_sp=5 if isced97_pg_sp==6

label define educ 1 "low" 2 "middle voc" 3 "voc + ab" 4 "higher voc" 5 "higher educ"
label values educ_recode educ_recode_sp educ

// home_owners
recode housing_status_hl (.n=.)(.s=.)(1/2=0)(3=1)(4/5=0), gen(home_owner)
tab home_owner, m
tab housing_status_hl home_owner, m

// children
gen kids_in_hh=.
replace kids_in_hh=0 if num_children_hl==0 & num_children_hl_sp==0
replace kids_in_hh=1 if (num_children_hl>0 & num_children_hl<100) | (num_children_hl_sp>0 & num_children_hl_sp<100)

tab kids_in_hh, m
tab num_children_hl kids_in_hh, m

// ugh I forgot birth history before (added to step 1, but not merging on there for now - will do later when i inevitably revisit all of this)
merge m:1 pid using "$temp/biobirth_cleaned.dta"
drop if _merge==2
drop _merge

tab any_births, m
tab first_birth_year any_births, m

// age
gen age_woman = age if sex_pl==2
replace age_woman = age_sp if sex_pl==1

gen age_man = age if sex_pl==1
replace age_man = age_sp if sex_pl==2


* so first need a var that is pre v. post treated. can I just use marital status?
gen married = 0
replace married = 1 if marst_defacto==1


********************************************************************************
* before dropping, get descriptive comparison of cohabitors to married couples
********************************************************************************

tabstat female_earn_pct_net female_hours_pct_t female_housework_pct_avg female_childcare_pct_avg, by(marst_defacto) 
ttest female_earn_pct_net, by(marst_defacto) 
ttest female_hours_pct_t, by(marst_defacto) 
ttest female_housework_pct_avg, by(marst_defacto) 
ttest female_childcare_pct_avg, by(marst_defacto) 

unique pid partner_id_pl, by(marst_defacto) 
unique pid partner_id_pl if marst_defacto==1
unique pid partner_id_pl if marst_defacto==2

// % ever transition
tab marst_defacto ever_transition, row
unique pid partner_id_pl if marst_defacto==2, by(ever_transition) // this probably most accurate

// some small descriptives
tabstat age_woman age_man couple_earnings_net couple_educ_gp home_owner kids_in_hh, by(marst_defacto)

// and these to use in models
regress female_earn_pct_net married
est store e0

regress female_hours_pct_t married
est store h0

regress female_housework_pct_avg married
est store hw0

********************************************************************************
**# okay make analytical sample and recode duration relative to marital transition
********************************************************************************
keep if ever_transition==1 | always_cohab==1 // so keeping a "control" group - basically drops those always married

gen treated=.
replace treated=1 if ever_transition==1 & always_cohab!=1
replace treated=0 if always_cohab==1

tab year_transitioned ever_transition, m

gen relationship_duration = syear - rel_start_all

gen duration_cohab=.
replace duration_cohab = syear - year_transitioned if treated==1
replace duration_cohab = relationship_duration if treated==0

tab duration_cohab, m

browse pid partner_id_pl syear treated marst_defacto year_transitioned relationship_duration duration_cohab

********************************************************************************
* Add descriptive statistics and descriptive exploration
********************************************************************************
unique pid partner_id_pl, by(treated) 
unique pid partner_id_pl if treated==0
unique pid partner_id_pl if treated==1

// some small descriptives (for workshop)
unique pid partner_id_pl, by(treated)
tab treated
tabstat age_woman age_man couple_earnings_net couple_educ_gp home_owner kids_in_hh, by(treated)

********************************************************************************
**# Option 1: fixed effects just on treated?
********************************************************************************
// so first need a var that is pre v. post treated. can I just use marital status?
egen couple_id = group(pid partner_id_pl)

xtset couple_id syear
 
tab marst_defacto treated, m
tab year_transitioned treated, m
tab marst_defacto if treated==1 & syear >= year_transitioned

// gen married = 0
// replace married = 1 if marst_defacto==1

// other prep steps
tab duration_cohab treated, m
tab relationship_duration treated, m

keep if duration_cohab >=-5 & duration_cohab<=8
gen duration_pos = duration_cohab+6

// data exploration
tabstat female_earn_pct_net female_hours_pct_t female_housework_pct_avg, by(duration_cohab) 
 
preserve

collapse (median) female_earn_pct_net female_hours_pct_t female_housework_pct_avg, by(duration_cohab)

twoway (line female_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=8) ///
(line female_hours_pct_t duration_cohab if duration_cohab>=-5 & duration_cohab <=8) ///
(line female_housework_pct_avg duration_cohab if duration_cohab>=-5 & duration_cohab <=8), ///
legend(order(1 "Earnings" 2 "Work Hours" 3 "Housework Hours") rows(1) position(6)) xscale(range(-5 8)) xlabel(-5(1)8)  xline(0)

restore

 
preserve

collapse (median) female_earn_pct_net female_hours_pct_t female_housework_pct_avg, by(duration_cohab treated)

twoway (line female_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=8 & treated==0, lcolor(green) lpattern(dash)) ///
(line female_earn_pct duration_cohab if duration_cohab>=-5 & duration_cohab <=8 & treated==1, lcolor(green) lpattern(solid)) 		///
(line female_hours_pct_t duration_cohab if duration_cohab>=-5 & duration_cohab <=8 & treated==0, lcolor(blue) lpattern(dash)) 		///
(line female_hours_pct_t duration_cohab if duration_cohab>=-5 & duration_cohab <=8 & treated==1, lcolor(blue) lpattern(solid)) 	///
(line female_housework_pct_avg duration_cohab if duration_cohab>=-5 & duration_cohab <=8 & treated==0, lcolor(pink) lpattern(dash)) 	///
(line female_housework_pct_avg duration_cohab if duration_cohab>=-5 & duration_cohab <=8 & treated==1, lcolor(pink) lpattern(solid)), 	///
legend(order(1 "Earnings: Control" 2 "Earnings: Treated" 3 "Work Hours: Control" 4 "Work Hours: Treated" 5 "HW Hours: Control" 6 "HW Hours: Treated") ///
rows(2) position(6)) xscale(range(-5 8)) xlabel(-5(1)8)  xline(0)

restore


// preliminary exploration models
// local controls "i.isced97_pg i.isced97_pg_sp i.where_germany_pl age age_sp couple_earnings_net i.housing_status_hl num_children_hl rel_start_all"
tab educ_recode, gen(educ_r)
tab educ_recode_sp, gen(educ_sp)
gen east=.
replace east=0 if where_germany_pl==1
replace east=1 if where_germany_pl==2

global controls "educ_r2 educ_r3 educ_r4 educ_r5 educ_sp2 educ_sp3 educ_sp4 educ_sp5 east age age_sp couple_earnings_net home_owner num_children_hl rel_start_all" // refs: educ_r1 educ_sp1

tabstat female_earn_pct_gr female_earn_pct_net female_hours_pct_t female_housework_pct_wk female_housework_pct_avg female_childcare_pct_wk female_childcare_pct_avg if treated==1, by(married) // without controls, this is what I'd be estimating
tabstat female_earn_pct_net female_hours_pct_t female_housework_pct_avg female_childcare_pct_avg if treated==1, by(married) 

regress female_earn_pct_net i.married if treated==1
margins married
regress female_hours_pct_t i.married if treated==1
margins married
regress female_housework_pct_avg i.married if treated==1
margins married
regress female_childcare_pct_avg i.married if treated==1
margins married

ttest female_earn_pct_net if treated==1, by(married) 

regress female_earn_pct_net i.married $controls if treated==1
margins married
// regress female_earn_pct_net i.married $controls i.couple_id if treated==1
// margins married

xtreg female_earn_pct_net i.married $controls if treated==1, fe // this exactly matches above
margins married

// regress female_earn_pct_net i.married i.relationship_duration $controls i.couple_id if treated==1
// margins married

xtreg female_earn_pct_net i.married i.relationship_duration $controls if treated==1, fe
margins married
margins relationship_duration
marginsplot

// xtreg female_earn_pct_net i.married dur $controls if treated==1, fe
// margins married
// margins, at(dur=(-5(1)10))
// marginsplot

xtreg female_earn_pct_net i.married##i.relationship_duration $controls if treated==1, fe
margins married
margins married, at(relationship_duration=(0(1)10))
marginsplot

************************************
**# Compare contrast OLS v. FE
************************************
// with both controls AND control group
regress female_earn_pct_net i.married $controls
margins married
xtreg female_earn_pct_net i.married $controls, fe
margins married

regress female_hours_pct_t i.married $controls
margins married
xtreg female_hours_pct_t i.married $controls, fe
margins married

regress female_housework_pct_avg i.married $controls
margins married
xtreg female_housework_pct_avg i.married $controls, fe
margins married

regress female_childcare_pct_avg i.married $controls
margins married
xtreg female_childcare_pct_avg i.married $controls, fe
margins married

// with controls, but NO control group
regress female_earn_pct_net i.married $controls if treated==1
margins married
xtreg female_earn_pct_net i.married $controls if treated==1, fe
margins married

regress female_hours_pct_t i.married $controls if treated==1
margins married
xtreg female_hours_pct_t i.married $controls if treated==1, fe
margins married

regress female_housework_pct_avg i.married $controls if treated==1
margins married
xtreg female_housework_pct_avg i.married $controls  if treated==1, fe
margins married

regress female_childcare_pct_avg i.married $controls if treated==1
margins married
xtreg female_childcare_pct_avg i.married $controls  if treated==1, fe
margins married

// no controls but with control group
regress female_earn_pct_net i.married
margins married
xtreg female_earn_pct_net i.married, fe
margins married

regress female_hours_pct_t i.married
margins married
xtreg female_hours_pct_t i.married, fe
margins married

regress female_housework_pct_avg i.married
margins married
xtreg female_housework_pct_avg i.married, fe
margins married

regress female_childcare_pct_avg i.married
margins married
xtreg female_childcare_pct_avg i.married, fe
margins married

// no controls OR control group
regress female_earn_pct_net i.married if treated==1
margins married
xtreg female_earn_pct_net i.married if treated==1, fe
margins married

regress female_hours_pct_t i.married if treated==1
margins married
xtreg female_hours_pct_t i.married if treated==1, fe
margins married

regress female_housework_pct_avg i.married if treated==1
margins married
xtreg female_housework_pct_avg i.married if treated==1, fe
margins married

regress female_childcare_pct_avg i.married if treated==1
margins married
xtreg female_childcare_pct_avg i.married if treated==1, fe
margins married

************************************
**# FEIS v. FE
************************************
// so I feel like this is what matches Zhou and Kan. How do I add the TIME element though??
// and decide if I keep never treated or not (Bruderl / Schechtl def do; I feel like Z&K do as well?)

global controls "educ_r2 educ_r3 educ_r4 educ_r5 educ_sp2 educ_sp3 educ_sp4 educ_sp5 east age age_sp couple_earnings_net home_owner num_children_hl" // refs: educ_r1 educ_sp1

// controls  AND control group
xtreg female_earn_pct_net i.married $controls, fe
xtfeis female_earn_pct_net married $controls, slope(relationship_duration) cluster(couple_id) 

xtreg female_hours_pct_t i.married $controls, fe
xtfeis female_hours_pct_t married $controls, slope(relationship_duration) cluster(couple_id) 

xtreg female_housework_pct_avg i.married $controls, fe
xtfeis female_housework_pct_avg married $controls, slope(relationship_duration) cluster(couple_id) 

xtreg female_childcare_pct_avg i.married $controls, fe
xtfeis female_childcare_pct_avg married $controls, slope(relationship_duration) cluster(couple_id) 

// with controls, but NO control group
xtreg female_earn_pct_net i.married $controls if treated==1, fe
xtfeis female_earn_pct_net married $controls if treated==1, slope(relationship_duration) cluster(couple_id) 

xtreg female_hours_pct_t i.married $controls if treated==1, fe
xtfeis female_hours_pct_t married $controls if treated==1, slope(relationship_duration) cluster(couple_id) 

xtreg female_housework_pct_avg i.married $controls if treated==1, fe
xtfeis female_housework_pct_avg married $controls if treated==1, slope(relationship_duration) cluster(couple_id) 

xtreg female_childcare_pct_avg i.married $controls if treated==1, fe
xtfeis female_childcare_pct_avg married $controls if treated==1, slope(relationship_duration) cluster(couple_id) 

// no controls but with control group
xtreg female_earn_pct_net i.married, fe
xtfeis female_earn_pct_net married, slope(relationship_duration) cluster(couple_id) 

xtreg female_hours_pct_t i.married, fe
xtfeis female_hours_pct_t married, slope(relationship_duration) cluster(couple_id) 

xtreg female_housework_pct_avg i.married, fe
xtfeis female_housework_pct_avg married, slope(relationship_duration) cluster(couple_id) 

xtreg female_childcare_pct_avg i.married, fe
xtfeis female_childcare_pct_avg married, slope(relationship_duration) cluster(couple_id) 

// no controls OR control group
xtreg female_earn_pct_net i.married if treated==1, fe
xtfeis female_earn_pct_net married if treated==1, slope(relationship_duration) cluster(couple_id) 

xtreg female_hours_pct_t i.married if treated==1, fe
xtfeis female_hours_pct_t married if treated==1, slope(relationship_duration) cluster(couple_id) 

xtreg female_housework_pct_avg i.married if treated==1, fe
xtfeis female_housework_pct_avg married if treated==1, slope(relationship_duration) cluster(couple_id) 

xtreg female_childcare_pct_avg i.married if treated==1, fe
xtfeis female_childcare_pct_avg married if treated==1, slope(relationship_duration) cluster(couple_id) 

// Hausman test for whether or not you need FEIS v. FE
xtfeis female_earn_pct_net married $controls, slope(relationship_duration) cluster(couple_id) 
xtart, keep(married) 

xtfeis female_hours_pct_t married $controls, slope(relationship_duration) cluster(couple_id) 
xtart, keep(married) 

xtfeis female_housework_pct_avg married $controls, slope(relationship_duration) cluster(couple_id) 
xtart, keep(married) 

xtfeis female_childcare_pct_avg married $controls, slope(relationship_duration) cluster(couple_id) 
xtart, keep(married) 

************************************
**# COEFPLOT FOR FLOPS (May 2025)
************************************
// with both controls AND control group
global controls "educ_r2 educ_r3 educ_r4 educ_r5 educ_sp2 educ_sp3 educ_sp4 educ_sp5 east age age_sp couple_earnings_net home_owner num_children_hl" // refs: educ_r1 educ_sp1

** Earnings
regress female_earn_pct_net married $controls
est store e_b
xtreg female_earn_pct_net married $controls, fe
est store e_fe
xtfeis female_earn_pct_net married $controls, slope(relationship_duration) cluster(couple_id) 
est store e_feis

** Paid Work Hours
regress female_hours_pct_t married $controls
est store h_b
xtreg female_hours_pct_t married $controls, fe
est store h_fe
xtfeis female_hours_pct_t married $controls, slope(relationship_duration) cluster(couple_id) 
est store h_feis

** Housework
regress female_housework_pct_avg married $controls
est store hw_b
xtreg female_housework_pct_avg married $controls, fe
est store hw_fe
xtfeis female_housework_pct_avg married $controls, slope(relationship_duration) cluster(couple_id) 
est store hw_feis

coefplot 	(h_b, label("OLS") lcolor("gs8") mcolor("gs8") ciopts(color("gs8"))) ///
			(h_fe,label("FE") lcolor("pink") mcolor("pink") ciopts(color("pink"))) ///
			(h_feis,label("FEIS") lcolor("pink%30") mcolor("pink%30") ciopts(color("pink%30"))), bylabel("Paid Work Hours") || ///
			(e_b, label("OLS")) (e_fe,label("FE")) (e_feis,label("FEIS")), bylabel("Earnings") || ///
			(hw_b, label("OLS")) (hw_fe,label("FE")) (hw_feis,label("FEIS")), bylabel("Housework") || ///
			, keep(married)  byopts(rows(1)) xsize(8) ysize(3) xline(0, lcolor(black) lstyle(solid)) levels(90) ///
			base coeflabels(married = "") xtitle(Change in Women's Share, size(small)) legend(rows(1)) ylabel(none)

coefplot 	(h_b, label("OLS") lcolor("gs8") mcolor("gs8") ciopts(color("gs8"))) ///
			(h_fe,label("FE") lcolor("pink") mcolor("pink") ciopts(color("pink"))) ///
			(h_feis,label("FEIS") lcolor("pink%30") mcolor("pink%30") ciopts(color("pink%30"))), bylabel("Paid Work Hours") || ///
			(hw_b, label("OLS")) (hw_fe,label("FE")) (hw_feis,label("FEIS")), bylabel("Housework") || ///
			, keep(married)  byopts(rows(1)) xsize(7) ysize(3) xline(0, lcolor(black) lstyle(solid)) levels(90) ///
			base coeflabels(married = "") xtitle(Change in Women's Share, size(small)) legend(position(bottom) rows(1)) ylabel(none)

// paid work
coefplot 	(h0, label("cross-sectional") lcolor("black") mcolor("black") ciopts(color("black"))) ///
			(h_b, label("OLS") lcolor("gs8") mcolor("gs8") ciopts(color("gs8"))) ///
			(h_fe,label("FE") lcolor("pink") mcolor("pink") ciopts(color("pink"))) ///
			(h_feis,label("FEIS") lcolor("pink%30") mcolor("pink%30") ciopts(color("pink%30"))), ///
			keep(married)  byopts(rows(1)) xsize(3.5) xline(0, lcolor(gs4) lwidth(thin) lpattern(dash)) ///
			base coeflabels(married = "")  levels(95) xtitle(Change in Women's Paid Work Share, size(small)) ///
			legend(position(bottom) rows(1)) ylabel(none) xlabel(-.11(.01).01) // ysize(3) 
			
// housework			
coefplot 	(hw0, label("cross-sectional") lcolor("black") mcolor("black") ciopts(color("black"))) ///
			(hw_b, label("OLS") lcolor("gs8") mcolor("gs8") ciopts(color("gs8"))) ///
			(hw_fe,label("FE") lcolor("pink") mcolor("pink") ciopts(color("pink"))) ///
			(hw_feis,label("FEIS") lcolor("pink%30") mcolor("pink%30") ciopts(color("pink%30"))), ///
			keep(married)  byopts(rows(1)) xsize(3.5) xline(0, lcolor(gs4) lwidth(thin) lpattern(dash)) ///
			base coeflabels(married = "")  levels(95) xtitle(Change in Women's Housework Share, size(small)) ///
			legend(position(bottom) rows(1)) ylabel(none) xlabel(-.01(.01).11) // ysize(3) 
		
/*
// paid work
coefplot 	(h0, label("cross-sectional") lcolor("black") mcolor("black") ciopts(color("black"))) ///
			(h_b, label("OLS") lcolor("white") mcolor("white") ciopts(color("white"))) ///
			(h_fe,label("FE") lcolor("white") mcolor("white") ciopts(color("white"))) ///
			(h_feis,label("FEIS") lcolor("white") mcolor("white") ciopts(color("white"))), ///
			keep(married)  byopts(rows(1)) xsize(3.5) xline(0, lcolor(gs4) lwidth(thin) lpattern(dash)) ///
			base coeflabels(married = "")  levels(95) xtitle(Change in Women's Paid Work Share, size(small)) ///
			legend(position(bottom) rows(1)) ylabel(none) xlabel(-.11(.01).01) // xlabel(-.06(.01).01) // ysize(3) 
			
// housework			
coefplot 	(hw0, label("cross-sectional") lcolor("black") mcolor("black") ciopts(color("black"))) ///
			(hw_b, label("OLS") lcolor("white") mcolor("white") ciopts(color("white"))) ///
			(hw_fe,label("FE") lcolor("white") mcolor("white") ciopts(color("white"))) ///
			(hw_feis,label("FEIS") lcolor("white") mcolor("white") ciopts(color("white"))), ///
			keep(married)  byopts(rows(1)) xsize(3.5) xline(0, lcolor(gs4) lwidth(thin) lpattern(dash)) ///
			base coeflabels(married = "")  levels(95) xtitle(Change in Women's Housework Share, size(small)) ///
			legend(position(bottom) rows(1)) ylabel(none) xlabel(-.01(.01).11) // xlabel(-.01(.01).06) // ysize(3) 
*/
************************************
* Working through impact functions
************************************
// okay, looking at the Happiness 3 replicate code, I see now. Duration only starts ONCE married
gen duration_married = 0
replace duration_married = duration_cohab + 1 if married==1

tab duration_married married
tab duration_married treated

xtreg female_earn_pct_net i.married c.duration_married##c.duration_married $controls, fe

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
tab duration_cohab treated
tab duration_cohab married

gen dummy_duration = .
replace dummy_duration = duration_cohab if treated==1
recode dummy_duration (min/-1=-1)
replace dummy_duration = dummy_duration+1
replace dummy_duration = 0 if treated==0

tab dummy_duration treated
tab dummy_duration married

xtreg female_earn_pct_net i.dummy_duration $controls, fe 
estimates store d1

xtfeis female_earn_pct_net i.dummy_duration $controls, slope(relationship_duration) cluster(couple_id)
est store d2

coefplot 	(d1, lcolor("red*1.2") mcolor("red*1.2") ciopts(color("red*1.2")) label("FE")) 		///
			(d2, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("FEIS")), 	///
			keep(*.dummy_duration) vertical yline(0, lpattern(solid)) recast(line) 			///
			coeflabels(1.dummy_duration="0" 2.dummy_duration="1" 3.dummy_duration="2" 		///
			4.dummy_duration="3" 5.dummy_duration="4" 6.dummy_duration="5"      			///
		    7.dummy_duration="6" 8.dummy_duration="7" 9.dummy_duration="8")        			///         
			ylabel(-.3(.1).5, grid angle(0) labsize(medium) format(%3.1f))        			///
			xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        			///
			ytitle("Change in Female Earnings %", size(medlarge)) 		  
		  
		  
// okay but then they go into other anticipation effects up to year -6, so this is where I could just use my duration positive?
// I just need to figure out where to put control - at time -5 or time 0 (they should be in reference category they say)

// this is with controls at time 0. okay this literally doesn't matter
tab duration_pos treated
gen duration_pos3 = duration_pos
replace duration_pos3 = 6 if treated==0
tab duration_pos3 treated

xtreg female_earn_pct_net ib5.duration_pos3 $controls, fe // ref group is time 0

coefplot, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) recast(line) lwidth(thick) lcolor(blue)  ///
          xline(5, lpatter(solid)) ciopts(recast(rline) lpattern(dash) lwidth(medthick) lcolor(green))   	/// 
		  ylabel(-.3(.1).5, grid angle(0) labsize(medium) format(%3.1f))        					///
		  coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		  4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		  8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"			///
		  12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  						///  
          xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
          ytitle("Change in Female Earnings %", size(medlarge)) 

************************************
**# IMPACT PLOTS FOR FLOPS (May 2025)
************************************
** Earnings
xtreg female_earn_pct_net ib5.duration_pos3 $controls, fe // ref group is time 0

coefplot, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) recast(line) lwidth(medthick) lcolor(pink)  ///
          xline(5, lpatter(solid)) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30))   	/// 
		  ylabel(-.2(.1).2, grid angle(0) labsize(medium) format(%3.1f))        					///
		  coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		  4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		  8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"			///
		  12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  						///  
          xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
          ytitle("Change in Women's Earnings %", size(medlarge)) 
		  	  
** Paid Work Hours
xtreg female_hours_pct_t ib5.duration_pos3 $controls, fe // ref group is time 0

coefplot, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) recast(line) lwidth(medthick) lcolor(pink) ///
          xline(5, lpatter(solid)) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30))   	/// 
		  ylabel(-.2(.1).2, grid angle(0) labsize(medium) format(%3.1f))        					///
		  coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		  4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		  8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"			///
		  12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  						///  
          xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
          ytitle("Change in Women's Paid Work Hrs %", size(medlarge)) 
		  	  
** Housework
xtreg female_housework_pct_avg ib5.duration_pos3 $controls, fe // ref group is time 0

coefplot, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) recast(line) lwidth(medthick) lcolor(pink)  ///
          xline(5, lpatter(solid)) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30))   	/// 
		  ylabel(-.2(.1).2, grid angle(0) labsize(medium) format(%3.1f))        					///
		  coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		  4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		  8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"			///
		  12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  						///  
          xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
          ytitle("Change in Women's Housework Hrs %", size(medlarge)) 
		  
// Can I interact with having a birth? (AFTER marriage)
browse pid partner_id_pl syear rel_start_all marst_defacto marr_trans year_transitioned any_births first_birth_year num_children_hl num_children_u16_hl kids_in_hh

gen birth_after_trans=0
replace birth_after_trans = 1 if first_birth_year >= year_transitioned & first_birth_year!=9999 & any_births!=0 & any_births!=.

gen birth_interact = birth_after_trans
replace birth_interact = 0 if birth_after_trans==1 & inrange(duration_pos3,1,4)

browse pid partner_id_pl syear rel_start_all marst_defacto marr_trans birth_after_trans birth_interact year_transitioned any_births first_birth_year num_children_hl num_children_u16_hl kids_in_hh

** Earnings
xtreg female_earn_pct_net ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins duration_pos3#birth_interact
marginsplot

xtreg female_earn_pct_net ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(birth_interact==0) post
est store e_birth0

xtreg female_earn_pct_net ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(birth_interact==1) post
est store e_birth1

coefplot (e_birth0, recast(line) lwidth(medthick) lcolor(pink) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30)) label("No Post-Marital Birth")) ///
		(e_birth1, recast(line) lwidth(medthick) lcolor(sand) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(sand)) label("Had Post-Marital Birth")) ///
		, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) xline(5, lpattern(solid))  	///
		ylabel(-.3(.1).3, grid angle(0) labsize(medium) format(%3.1f))        					///
		coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"		///
		12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  					///  
		xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
		ytitle("Change in Women's Earnings %", size(medlarge)) legend(position(bottom) rows(1))

// OH - instead of interacting, do I estimate seaprately?? (with the time invariant??)
xtreg female_earn_pct_net ib5.duration_pos3 $controls if birth_after_trans==0, fe // ref group is time 0
est store e_birth0_alt

xtreg female_earn_pct_net ib5.duration_pos3 $controls if birth_after_trans==1, fe // ref group is time 0
est store e_birth1_alt

coefplot (e_birth0_alt, recast(line) lwidth(medthick) lcolor(pink) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30)) label("No Post-Marital Birth")) ///
		(e_birth1_alt, recast(line) lwidth(medthick) lcolor(sand) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(sand)) label("Had Post-Marital Birth")) ///
		, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) xline(5, lpattern(solid))  	///
		ylabel(-.3(.1).3, grid angle(0) labsize(medium) format(%3.1f))        					///
		coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"		///
		12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  					///  
		xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
		ytitle("Change in Women's Earnings %", size(medlarge)) legend(position(bottom) rows(1))

// Children in HH - can't do children ever because it has to vary with time (because fixed effects)
xtreg female_earn_pct_net ib5.duration_pos3##i.kids_in_hh $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(kids_in_hh==0) post
est store e_kid0

xtreg female_earn_pct_net ib5.duration_pos3##i.kids_in_hh $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(kids_in_hh==1) post
est store e_kid1

coefplot (e_kid0, recast(line) lwidth(medthick) lcolor(pink) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30)) label("No Children in HH")) ///
		(e_kid1, recast(line) lwidth(medthick) lcolor(sand) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(sand)) label("Children in HH")) ///
		, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) xline(5, lpattern(solid))  	///
		ylabel(-.3(.1).3, grid angle(0) labsize(medium) format(%3.1f))        					///
		coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"		///
		12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  					///  
		xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
		ytitle("Change in Women's Earnings %", size(medlarge)) legend(position(bottom) rows(1))
		
** Paid Work Hours
xtreg female_hours_pct_t ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins duration_pos3#birth_interact
marginsplot

xtreg female_hours_pct_t ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(birth_interact==0) post
est store h_birth0

xtreg female_hours_pct_t ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(birth_interact==1) post
est store h_birth1

coefplot (h_birth0, recast(line) lwidth(medthick) lcolor(pink) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30)) label("No Post-Marital Birth")) ///
		(h_birth1, recast(line) lwidth(medthick) lcolor(sand) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(sand)) label("Had Post-Marital Birth")) ///
		, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) xline(5, lpattern(solid))  	///
		ylabel(-.3(.1).3, grid angle(0) labsize(medium) format(%3.1f))        					///
		coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"		///
		12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  					///  
		xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
		ytitle("Change in Women's Paid Work Hrs %", size(medlarge)) legend(position(bottom) rows(1))
		
** Housework Hours
xtreg female_housework_pct_avg ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins duration_pos3#birth_interact
marginsplot

xtreg female_housework_pct_avg ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(birth_interact==0) post
est store hw_birth0

xtreg female_housework_pct_avg ib5.duration_pos3##i.birth_interact $controls, fe // ref group is time 0
margins, dydx(duration_pos3) at(birth_interact==1) post
est store hw_birth1

coefplot (hw_birth0, recast(line) lwidth(medthick) lcolor(pink) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(pink%30)) label("No Post-Marital Birth")) ///
		(hw_birth1, recast(line) lwidth(medthick) lcolor(sand) ciopts(recast(rline) lpattern(dash) lwidth(thin) lcolor(sand)) label("Had Post-Marital Birth")) ///
		, keep(*.duration_pos3) vertical yline(0, lpattern(solid)) xline(5, lpattern(solid))  	///
		ylabel(-.3(.1).3, grid angle(0) labsize(medium) format(%3.1f))        					///
		coeflabels(1.duration_pos3="-5" 2.duration_pos3="-4" 3.duration_pos3="-3" 				///
		4.duration_pos3="-2" 5.duration_pos3="-1" 6.duration_pos3="0" 7.duration_pos3="1" 		///
		8.duration_pos3="2" 9.duration_pos3="3" 10.duration_pos3="4" 11.duration_pos3="5"		///
		12.duration_pos3="6" 13.duration_pos3="7" 14.duration_pos3="8") 	  					///  
		xtitle("Years since marriage", size(medlarge) margin(0 0 0 2))        					///
		ytitle("Change in Women's Housework Hrs %", size(medlarge)) legend(position(bottom) rows(1))

************************************
* Old analyses
************************************
// models to use for gender workshop / prelim analysis
// don't match 100% because i reduced the duration pre / post being used

// do I use controls with fixed effects? I think because that controls for observed and the remained is unobserved? I think controls let hyou control for time-varying things whereas fixed effects net out time invariant? (hence why race is collinear?)
xtreg female_earn_pct_net i.married i.relationship_duration $controls if treated==1 & relationship_duration<=15, fe
margins married

xtreg female_hours_pct_t i.married i.relationship_duration $controls if treated==1 & relationship_duration<=15, fe
margins married

xtreg female_housework_pct_avg i.married i.relationship_duration $controls if treated==1 & relationship_duration<=15, fe
margins married

xtreg female_childcare_pct_avg i.married i.relationship_duration $controls if treated==1 & relationship_duration<=15, fe
margins married
