
/*##################################################################
# Partial replication files for "Parties as Disciplinarians: Charisma and Commitment Problems in Programmatic Campaigning" 
#by James R. Hollyer, Marko Klasnja and Rocio Titiunik, forthcoming at AJPS in 2021.
##################################################################*/

/* This do-file replicates the cross-national results in the paper and the supplementary 
	appendix that are based on the DALP data. 
	
	For the file to run, the following user-written programs are required: 
	
	-matmap- (can be installed by typing 'ssc install matmap')
	-frmttable- (can be found by typing 'search sg97_5', then clicking on the package link 
		and following the instructions to install). */
	
clear *
	
*** defining programs that will prep the data for analyses and format the results

* inputting data and setting up covariates
program def setup
	use "cross-national-data", clear
	do "cross-national-designate-controls.do"
end

* generating and formatting predicted values from models testing Hypothesis 1
program define getmargins_overall
	args x v /* x = volatility measure, v = dv number */
	qui sum `x' if e(sample), d
	local lov = r(p25)
	local hiv = r(p75)
	margins, at(ev_total = (`lov' `hiv')) post
	mat tab = r(table)
		mat res`v' = J(1,6,.)
		mat pval`v' = J(1,6,1)
		mat stars`v' = J(1,6,0)
		* low volatility
		mat res`v'[1,1] = tab[1,1]
		mat res`v'[1,2] = tab[2,1]
		* high volatility
		mat res`v'[1,3] = tab[1,2]
		mat res`v'[1,4] = tab[2,2]
		* difference
		lincom _b[2._at]-_b[1._at]
		mat res`v'[1,5] = r(estimate)
		mat res`v'[1,6] = r(se)
		local df = r(df)
		local est = r(estimate)
		local se = r(se)
		local pv = ttail(`df',abs(`est'/`se'))*2
		mat pval`v'[1,5] = `pv'
		local cols = colsof(pval`v')
		local rows = rowsof(pval`v')
		forval i = 1/`rows' {
			forval j = 1/`cols' {
				local pval = pval`v'[`i',`j']
				if `pval' <= .10 & `pval' > .05 {
					matrix stars`v'[`i',`j'] = 1
					}
				if `pval' <= .05 & `pval' > .01 {
					matrix stars`v'[`i',`j'] = 2 
					}
				if `pval' <= .01 {
					matrix stars`v'[`i',`j'] = 3
				}
			}
		}
end

* generating and formatting predicted values from models testing Hypotheses 2 and 3
program define getmargins
	args x z v /* x = volatility measure, z = group attachment var, 
				v = dv number */
	qui sum `z' if e(sample), d
	local hia = r(p90)
	qui sum `x' if e(sample), d
	local lov = r(p25)
	local hiv = r(p75)
	margins, at(`z' = (0 `hia') `x' = (`lov' `hiv')) post
	mat tab = r(table)
	mat res`v' = J(3,6,.)
	mat pval`v' = J(3,6,1)
	mat stars`v' = J(3,6,0)
	* low-alpha party, low volatility
	mat res`v'[1,1] = tab[1,1]
	mat res`v'[1,2] = tab[2,1]
	* low-alpha party, hi volatility
	mat res`v'[1,3] = tab[1,2]
	mat res`v'[1,4] = tab[2,2]
	* high-alpha party, low volatility
	mat res`v'[2,1] = tab[1,3]
	mat res`v'[2,2] = tab[2,3]
	* high-alpha party, hi volatility
	mat res`v'[2,3] = tab[1,4]
	mat res`v'[2,4] = tab[2,4]
	* differences
	* low-alpha parties, high vs. low volatility
	lincom _b[2._at]-_b[1._at]
	mat res`v'[1,5] = r(estimate)
	mat res`v'[1,6] = r(se)
	local df = r(df)
	local est = r(estimate)
	local se = r(se)
	local pv = ttail(`df',abs(`est'/`se'))*2
	mat pval`v'[1,5] = `pv'
	* high-alpha parties, high vs. low volatility
	lincom _b[4._at]-_b[3._at]
	mat res`v'[2,5] = r(estimate)
	mat res`v'[2,6] = r(se)
	local df = r(df)
	local est = r(estimate)
	local se = r(se)
	local pv = ttail(`df',abs(`est'/`se'))*2
	mat pval`v'[2,5] = `pv'
	* high- vs. low-alpha parties, low volatility
	lincom _b[3._at]-_b[1._at]
	mat res`v'[3,1] = r(estimate)
	mat res`v'[3,2] = r(se)
	mat pval`v'[3,1] = r(p)
	* high- vs. low-alpha parties, high volatility
	lincom _b[4._at]-_b[2._at]
	mat res`v'[3,3] = r(estimate)
	mat res`v'[3,4] = r(se)
	mat pval`v'[3,3] = r(p)
	* no need for diff-of-diff
	local cols = colsof(pval`v')
	local rows = rowsof(pval`v')
	forval i = 1/`rows' {
		forval j = 1/`cols' {
			local pval = pval`v'[`i',`j']
			if `pval' <= .10 & `pval' > .05 {
				matrix stars`v'[`i',`j'] = 1
				}
			if `pval' <= .05 & `pval' > .01 {
				matrix stars`v'[`i',`j'] = 2 
				}
			if `pval' <= .01 {
				matrix stars`v'[`i',`j'] = 3
			}
		}
	}
end

* collecting model coefficients for formatting
program define getcoefs
	args j /* j = dv number */
	mat obs`j' = e(N)
	* get number of countries
	mat cn`j' = e(N_clust)
	mat coef`j' = e(b)
	mat var`j' = vecdiag(e(V))
	matmap var`j' se`j', m(sqrt(@))
	mat pval_`j' = r(table)
	local C = colsof(pval_`j')
	mat pval_`j' = pval_`j'[4..4,1..`C']
	mat pval_`j' = pval_`j''
	mat coef`j' = coef`j'', se`j''
end

* elements for creating tables of coefficients (main specifications)
program define coeftable_main
	args z w /* z = number of columns, w = 0/1 if first two columns
				should have empty interaction term cells */
	cap mat drop coef
	if `z' == 1 {
		mat coef = coef1
		mat pval = pval_1
		mat obs = obs1
		mat cn = cn1
		}
	if `z' == 2 {
		mat coef = coef1,coef2
		mat pval = pval_1,pval_2
		mat obs = obs1,obs2
		mat cn = cn1,cn2
		}
	if `z' == 3 {
		mat coef = coef1,coef2,coef3
		mat pval = pval_1,pval_2,pval_3
		mat obs = obs1,obs2,obs3
		mat cn = cn1,cn2,cn3
		}
	if `z' == 4 {
		mat coef = coef1,coef2,coef3,coef4
		mat pval = pval_1,pval_2,pval_3,pval_4
		mat obs = obs1,obs2,obs3,obs4
		mat cn = cn1,cn2,cn3,cn4
		}		
	if `z' == 6 {
		mat coef = coef1,coef2,coef3,coef4,coef5,coef6
		mat pval = pval_1,pval_2,pval_3,pval_4,pval_5,pval_6
		mat obs = obs1,obs2,obs3,obs4,obs5,obs6
		mat cn = cn1,cn2,cn3,cn4,cn5,cn6
		}
	clear
	svmat coef
	svmat pval
	gen names = ""
	local names : rownames coef
	local k = 1
	foreach j of local names {
		replace names = "`j'" in `k'
		local k = `k' + 1
		}
	egen base = rowtotal(coef*)
	drop if base == 0
	drop base
	local m 2 3 4 6 7 8 9 11 12 13 14 15 17 18 19 20 22 23 24 26
	gen order = .
	local k = 1
	foreach j of local m {
		replace order = `j' in `k'
		local k = `k' + 1
		}
	local n = _N
	local newn = `n'+6
	set obs `newn'
	local m 1 5 10 16 21 25
	local k = 1
	foreach j of local m {
		local newn = `n'+`k'
		replace order = `j' in `newn'
		local k = `k' + 1
		}
	sort order
	mkmat coef*, mat(coef)
	if `w' == 1 & `z' > 3 {
		forval i = 1/4 {
			mat coef[3,`i'] = .
			mat coef[4,`i'] = .
			}
		}
	if `w' == 1 & `z' == 3 {
		forval i = 1/2 {
			mat coef[3,`i'] = .
			mat coef[4,`i'] = .
			}
		}
	forval i = 1/`z' {
		gen empty`i' = .
		order empty`i', after(pval`i')
		}
	mkmat pval1-empty`z', mat(pval)
	local cols = colsof(pval)
	local rows = rowsof(pval)
	mat stars = J(`rows',`cols',0)
	forval i = 1/`rows' {
		forval j = 1/`cols' {
			local pval = pval[`i',`j']
		if `pval' <= .10 & `pval' > .05 {
			matrix stars[`i',`j'] = 1
			}
		if `pval' <= .05 & `pval' > .01 {
			matrix stars[`i',`j'] = 2 
			}
		if `pval' <= .01 {
			matrix stars[`i',`j'] = 3
			}
		}
	}
end

* elements for creating tables of coefficients (alternative specifications)
program define coeftable_robust
	clear
	forval i = 1/5 {
		scalar R = rowsof(coef`i')-1
		mat coef`i' = coef`i'[1..R,1..2]
		mat pval_`i' = pval_`i'[1..R,1..1]
		}
	forval i = 1/5 {
		svmat coef`i'
		}
	forval i = 1/5 {
		svmat pval_`i'
		}
	mat obs = obs1,obs2,obs3,obs4,obs5
	mat cn = cn1,cn2,cn3,cn4,cn5
	gen names = ""
	local names : rownames coef5
	local k = 1
	foreach j of local names {
		replace names = "`j'" in `k'
		local k = `k' + 1
		}
	egen base = rowtotal(coef*)
	drop if base == 0
	drop base
	local m 2 3 4 6 7 8 9 11 12 13 14 15 17 18 19 20 22 23 24
	gen order = .
	local k = 1
	foreach j of local m {
		replace order = `j' in `k'
		local k = `k' + 1
		}
	local n = _N
	local newn = `n'+5
	set obs `newn'
	local m 1 5 10 16 21
	local k = 1
	foreach j of local m {
		local newn = `n'+`k'
		replace order = `j' in `newn'
		local k = `k' + 1
		}
	sort order
	mkmat coef*, mat(coef)
	forval i = 1/5 {
		gen empty`i' = .
		order empty`i', after(pval_`i')
		}
	mkmat pval_1-empty5, mat(pval)
	local cols = colsof(pval)
	local rows = rowsof(pval)
	mat stars = J(`rows',`cols',0)
	forval i = 1/`rows' {
		forval j = 1/`cols' {
			local pval = pval[`i',`j']
		if `pval' <= .10 & `pval' > .05 {
			matrix stars[`i',`j'] = 1
			}
		if `pval' <= .05 & `pval' > .01 {
			matrix stars[`i',`j'] = 2 
			}
		if `pval' <= .01 {
			matrix stars[`i',`j'] = 3
			}
		}
	}
end

*----------------------------------------------------------------------------------------*/
/****** Results in the paper ******/
*----------------------------------------------------------------------------------------*/

*----------------------------------------------------------------------------------------*/
** Figure 3: Electoral Volatility, Personalism, and Programmaticness—Raw Correlations
*----------------------------------------------------------------------------------------*/
setup
twoway (scatter personal ev_total, msymbol(o) mcolor(gs4%30)) ///
	(lfit personal ev_total, lcolor(red)), ///
	scheme(s1mono) yline(2.5, lwidth(thin)) xline(.4, lwidth(thin)) ylabel(1(.5)4) ///
	legend(off) aspect(1) ytitle(Personalism Score) xtitle(Volatility Index) ///
	title({bf:Personalism}) name(p1, replace) 
twoway (scatter programmatic ev_total, msymbol(o) mcolor(gs4%30)) ///
	(lfit programmatic ev_total, lcolor(red)), ///
	scheme(s1mono) yline(2.5, lwidth(thin)) xline(.4, lwidth(thin)) ylabel(1(.5)4) ///
	legend(off) aspect(1) ytitle(Programaticness Score) xtitle(Volatility Index) ///
	title({bf:Programmaticness}) name(p2, replace)
gr combine p1 p2, scheme(s1mono) rows(1) xsize(10) ysize(5) 

*----------------------------------------------------------------------------------------*/
** Figure 4: Electoral Volatility, Ethnic Parties, and Electoral Strategies—Raw Correlations
*----------------------------------------------------------------------------------------*/
twoway (scatter personal ev_total if ethnic == 0, msymbol(o) jitter(10) mcolor(gs4%30)) ///
	(lfit personal ev_total if ethnic == 0, lcolor(red)), ///
	scheme(s1mono) yline(2.5, lwidth(thin)) xline(.4, lwidth(thin)) ylabel(1(.5)4) ///
	legend(off) aspect(1) ytitle(Personalism Score) xtitle(Volatility Index) ///
	title({bf:Non-Ethnic Parties}) name(p1, replace) 
twoway (scatter personal ev_total if ethnic == 1, msymbol(o) jitter(10) mcolor(gs4%30)) ///
	(lfit personal ev_total if ethnic == 1, lcolor(red)), ///
	scheme(s1mono) yline(2.5, lwidth(thin)) xline(.4, lwidth(thin)) ylabel(1(.5)4) ///
	legend(off) aspect(1) ytitle(Personalism Score) xtitle(Volatility Index) ///
	title({bf:Ethnic Parties}) name(p2, replace) 
gr combine p1 p2, scheme(s1mono) rows(1) xcommon ycommon ///
	title({bf:Personalism}, size(medium)) name(t1, replace)
twoway (scatter programmatic ev_total if ethnic == 0, msymbol(o) jitter(10) mcolor(gs4%30)) ///
	(lfit programmatic ev_total if ethnic == 0, lcolor(red)), ///
	scheme(s1mono) yline(2.5, lwidth(thin)) xline(.4, lwidth(thin)) ylabel(1(.5)4) ///
	legend(off) aspect(1) ytitle(Programmaticness Score) xtitle(Volatility Index) ///
	title({bf:Non-Ethnic Parties}) name(p1, replace) 
twoway (scatter programmatic ev_total if ethnic == 1, msymbol(o) jitter(10) mcolor(gs4%30)) ///
	(lfit programmatic ev_total if ethnic == 1, lcolor(red)), ///
	scheme(s1mono) yline(2.5, lwidth(thin)) xline(.4, lwidth(thin)) ylabel(1(.5)4) ///
	legend(off) aspect(1) ytitle(Programmaticness Score) xtitle(Volatility Index) ///
	title({bf:Ethnic Parties}) name(p2, replace) 
gr combine p1 p2, scheme(s1mono) rows(1) xcommon ycommon ///
	title({bf:Programmaticness}, size(medium)) name(t2, replace)
gr combine t1 t2, scheme(s1mono) rows(2) xcommon xsize(10) ysize(11.5)

*----------------------------------------------------------------------------------------*/
** Figure 5: Electoral Volatility, Ideological Extremism, and Electoral Strategies—Raw Correlations
*----------------------------------------------------------------------------------------*/
gen xtrm = (extremist2 >= 3) if extremist2 ~= .
* top panel
twoway (scatter personal ev_total if xtrm == 0, msymbol(o) jitter(10) mcolor(gs4%30)) ///
	(lfit personal ev_total if xtrm == 0, lcolor(red)), ///
	scheme(s1mono) yline(2.5, lwidth(thin)) xline(.4, lwidth(thin)) ylabel(1(.5)4) ///
	legend(off) aspect(1) ytitle(Personalism Score) xtitle(Volatility Index) ///
	title({bf:Moderate Parties}) name(p1, replace) 
twoway (scatter personal ev_total if xtrm == 1, msymbol(o) jitter(10) mcolor(gs4%30)) ///
	(lfit personal ev_total if xtrm == 1, lcolor(red)), ///
	scheme(s1mono) yline(2.5, lwidth(thin)) xline(.4, lwidth(thin)) ylabel(1(.5)4) ///
	legend(off) aspect(1) ytitle(Personalism Score) xtitle(Volatility Index) ///
	title({bf:Extreme Parties}) name(p2, replace) 
gr combine p1 p2, scheme(s1mono) rows(1) xcommon ycommon ///
	title({bf:Personalism}, size(medium)) name(t1, replace)
* bottom panel
twoway (scatter programmatic ev_total if xtrm == 0, msymbol(o) jitter(10) mcolor(gs4%30)) ///
	(lfit programmatic ev_total if xtrm == 0, lcolor(red)), ///
	scheme(s1mono) yline(2.5, lwidth(thin)) xline(.4, lwidth(thin)) ylabel(1(.5)4) ///
	legend(off) aspect(1) ytitle(Programmaticness Score) xtitle(Volatility Index) ///
	title({bf:Moderate Parties}) name(p1, replace) 
twoway (scatter programmatic ev_total if xtrm == 1, msymbol(o) jitter(10) mcolor(gs4%30)) ///
	(lfit programmatic ev_total if xtrm == 1, lcolor(red)), ///
	scheme(s1mono) yline(2.5, lwidth(thin)) xline(.4, lwidth(thin)) ylabel(1(.5)4) ///
	legend(off) aspect(1) ytitle(Programmaticness Score) xtitle(Volatility Index) ///
	title({bf:Extreme Parties}) name(p2, replace) 
gr combine p1 p2, scheme(s1mono) rows(1) xcommon ycommon ///
	title({bf:Programmaticness}, size(medium)) name(t2, replace)
gr combine t1 t2, scheme(s1mono) rows(2) xcommon xsize(10) ysize(11.5)

*----------------------------------------------------------------------------------------*/
** Table 1: Electoral Volatility, Group Attachments, and Electoral Strategies
*----------------------------------------------------------------------------------------*/
setup
* top panel---overall
reg personal ev_total $p_controls $controls, cluster(ccode)
getmargins_overall ev_total 1
reg programmatic ev_total $p_controls $controls, cluster(ccode)
getmargins_overall ev_total 2
* middle panel---ethnicity
qui reg personal i.ethnic##c.ev_total $p_controls $controls, cluster(ccode)
getmargins ev_total ethnic 3
qui reg programmatic i.ethnic##c.ev_total $p_controls $controls, cluster(ccode)
getmargins ev_total ethnic 4
* bottom panel---ideology
qui reg personal c.extremist2##c.ev_total $p_controls $controls, cluster(ccode)
getmargins ev_total extremist2 5
qui reg programmatic c.extremist2##c.ev_total $p_controls $controls, cluster(ccode)
getmargins ev_total extremist2 6
* formatting
mat empty = J(1,12,.)
mat rese0 = res1,res2
mat rese1 = res3,res4
mat rese1 = empty \ rese1
mat rese2 = res5,res6
mat rese2 = empty \ rese2
mat rese = rese0 \ rese1 \ rese2
mat emptys = J(1,12,0)
mat starse0 = stars1,stars2
mat starse1 = stars3,stars4
mat starse1 = emptys \ starse1
mat starse2 = stars5,stars6
mat starse2 = emptys \ starse2
mat starse = starse0 \ starse1 \ starse2
frmttable, clear
frmttable, statmat(rese) substat(1) sdec(2) annotate(starse) asymbol($^{\dagger}$,*,**) ///
	rtitles("\hspace{1em} Overall" \ "" \ "\textbf{Ethnicity}" \ "" \ ///
			"\hspace{1em} Non-ethnic party" \ "" \ "\hspace{1em} Ethnic party" \ "" \ ///
			"\hspace{1em} Difference" \ "" \ ///
			"\textbf{Ideology}" \ "" \ "\hspace{1em} Moderate party" \ "" \ ///
			"\hspace{1em} Extremist party" \ "" \ "\hspace{1em} Difference" \ "") ///
	ctitles("","\textbf{Personalism}","","","\textbf{Programmaticness}","","" \ ///
		"","Volatility","","Difference","Volatility","","Difference" \ ///
		"","Low","High","","Low","High","") ///
	multicol(1,2,3;1,5,3;2,2,2;2,5,2) noblankrows ///
	title("\caption{Electoral Volatility, Group Attachments, and Electoral Strategies\label{tab:margins}}\leavevmode") ///
	titlfont(normalsize)
	

*----------------------------------------------------------------------------------------*/
/****** Results in the appendix ******/
*----------------------------------------------------------------------------------------*/

*----------------------------------------------------------------------------------------*/
** Figure C1: Patterns of missing observations
*----------------------------------------------------------------------------------------*/
setup
misstable patterns programmatic personal ev_total ctot ethnic extremist2 $p_controls ///
	pluralty pr cl mdmh system demsys polity gdppc gini ethfrac, asis replace clear
egen sum = total(_freq)
gen percent = round((_freq/sum)*100,.1)
tostring percent, gen(pct) force
replace pct = substr(pct,1,3)
replace pct = substr(pct,1,2) if percent < 1
gsort -percent
forval i = 1/14 {
    loc t`i' = pct in `i'
}
drop _freq sum percent pct pr
foreach x in programmatic personal partysize polity {
	gen `x' = 1
}
order personal programmatic, first
order partysize, after(extremist2)
order polity, after(demsys)
gen n = _n
local j = 1
foreach v of varlist personal-ethfrac {
    rename `v' v`j'
	local j = `j'+1
}
reshape long v, i(n) j(j)
label drop _all
lab def n 1 `t1' 2 `t2' 3 `t3' 4 `t4' 5 `t5' 6 `t6' 7 `t7' 8 `t8' 9 `t9' 10 `t10' ///
	11 `t11' 12 `t12' 13 `t13' 14 `t14' 
lab val n n 
twoway (scatter j n if v == 0, mcolor(red) msize(huge) msymbol(S)) ///
  (scatter j n if v == 1, mcolor(green) msize(huge) msymbol(S)), ///
  yscale(reverse range(1(1)16) lstyle(none)) xscale(range(1(1)14) lstyle(none) alt) ///
  legend(off) xlabel(1(1)14, valuel noticks labgap(1.5) labsize(medsmall)) xtitle(Share of obs. (%)) ///
  ylabel(1 "Personalism" 2 "Programmaticness" 3 "Volatility" 4 "Commodity ToT" ///
		 5 "Ethnic party" 6 "Party extremism" 7 "Party size" 8 "Links w/ unions" ///
		 9 "Links w/ business" 10 "Links w/ relig. orgs." 11 "Electoral rule" ///
		 12 "District magnitude" 13 "Chief exec. system" 14 "System tenure" ///
		 15 "Polity score" 16 "GDP per capita" 17 "Gini index" 18 "Ethnic fractionalization", ///
		 angle(0) labsize(medsmall) noticks nogrid labgap(2)) ytitle("") ///
		 graphregion(color(white))

*----------------------------------------------------------------------------------------*/
** Table C3: Summary statistics
*----------------------------------------------------------------------------------------*/
setup
tab cl, gen(cl)
gen mixed = (pluralty == 0 & pr == 0) if pluralty ~= . & pr ~= .
tab system, gen(sys)
mat sumstat = J(22,5,.)
local j = 1
foreach v in programmatic personal ev_total ctot ethnic extremist2 $p_controls ///
	pluralty pr cl1 cl2 mdmh sys1 sys3 demsys polity gdppc gini ethfrac {
		qui sum `v'
		mat sumstat[`j',1] = r(mean)
		mat sumstat[`j',2] = r(sd)
		mat sumstat[`j',3] = r(min)
		mat sumstat[`j',4] = r(max)
		qui tab `v'
		mat sumstat[`j',5] = r(r)
		local j = `j' + 1
	}
frmttable, statmat(sumstat) substat(0) sdec(2,2,2,2,0) ///
	rtitles("Programmaticness" \ "Personalism" \ "Electoral volatility" \ "Commodity terms of trade" \ ///
		"Ethnic party" \ "Party extremism" \ "Party size" \ "Links w/ unions" \ "Links w/ business" \ ///
		"Links w/ relig. orgs." \ "Plurality" \ "PR" \ "Open list" \ "Closed list" \ "District magnitude" \ ///
		"Presidential system" \ "Parliamentary system" \ "System tenure" \ "Polity score" \ ///
		 "GDP per capita" \ "Gini index" \ "Ethnic fractionalization") ///
	ctitles("","Mean","St. dev.","Min.","Max.","No. values") ///
	title("\caption{Summary statistics\label{tab:sumstat}}\leavevmode") ///
	titlfont(normalsize)

*----------------------------------------------------------------------------------------*/
** Table D1: Electoral Volatility, Group Attachments, and Electoral Strategies—OLS Results
*----------------------------------------------------------------------------------------*/
setup
reg personal c.ev_total##d $p_controls $controls, cluster(ccode)
getcoefs 1
reg programmatic c.ev_total##d $p_controls $controls, cluster(ccode)
getcoefs 2
reg personal c.ev_total##c.ethnic $p_controls $controls, cluster(ccode)
getcoefs 3
reg programmatic c.ev_total##c.ethnic $p_controls $controls, cluster(ccode)
getcoefs 4
reg personal c.ev_total##c.extremist2 $p_controls $controls, cluster(ccode)
getcoefs 5
reg programmatic c.ev_total##c.extremist2 $p_controls $controls, cluster(ccode)
getcoefs 6
coeftable_main 6 1
frmttable, clear
frmttable, statmat(coef) substat(1) sdec(2) annotate(stars) asymbol($^{\dagger}$,*,**) ///
	rtitles("\textbf{Key variables}" \ "" \ "\hspace{1em} Electoral volatility" \ "" \ ///
	"\hspace{1em} Group attachment" \ "" \ "\hspace{1em} Volatility $\times$ group attachment" \ "" \ ///
	"\textbf{Party-level covariates}" \ "" \ "\hspace{1em} Party size (vote share in prev. elec.)" \ "" \ ///
	"\hspace{1em} Links w/ unions" \ "" \ "\hspace{1em} Links w/ business" \ "" \ ///
	"\hspace{1em} Links w/ religious orgs." \ "" \ "\textbf{Electoral system covariates}" \ "" \ ///
	"\hspace{1em} Plurality" \ "" \ "\hspace{1em} Proportional representation" \ "" \ ///
	"\hspace{1em} Closed list" \ "" \ "\hspace{1em} Open list" \ "" \ ///
	"\hspace{1em} Average district magnitude (lower house)" \ "" \ "\textbf{Political system covariates}" \ "" \ ///
	"\hspace{1em} Presidential system" \ "" \ "\hspace{1em} Parliamentary system" \ "" \ ///
	"\hspace{1em} System tenure" \ "" \ "\hspace{1em} Polity score" \ "" \ "\textbf{Other country-level covariates}" \ "" \ ///
	"\hspace{1em} GDP per capita, PPP (logged)" \ "" \ "\hspace{1em} Gini index" \ "" \ ///
	"\hspace{1em} Ethnic fractionalization" \ "" \ "Constant" \ "")
frmttable, statmat(obs) substat(0) sdec(0) rtitle("Observations") append
frmttable, statfont(fs11) hlines(1001{0}11) vlines(000101{0}) ///
	ctitles("","Baseline model","","Extended model","","","" \ ///
			"","","","Ethnicity","","Ideology","" \ ///
			"","Personalism","Programmaticness","Personalism","Programmaticness","Personalism"
			"Programmaticness") ///
	multicol(1,2,2;1,4,4;2,4,2;2,6,2) noblankrows ///
	title("\caption{Electoral Volatility, Group Attachments, and Electoral Strategies---OLS Results\label{tab:coefs}}\leavevmode") ///
	titlfont(normalsize)

*----------------------------------------------------------------------------------------*/
** Table D2: Electoral Volatility, Group Attachments, and Electoral Strategies—Alternative Programmaticness Measure
*----------------------------------------------------------------------------------------*/
setup
reg programmatic2 c.ev_total##d $p_controls $controls, cluster(ccode)
getcoefs 1
reg programmatic2 c.ev_total##c.ethnic $p_controls $controls, cluster(ccode)
getcoefs 2
reg programmatic2 c.ev_total##c.extremist2 $p_controls $controls, cluster(ccode)
getcoefs 3
coeftable_main 3 1
frmttable, clear
frmttable, statmat(coef) substat(1) sdec(2) annotate(stars) asymbol($^{\dagger}$,*,**) ///
	rtitles("\textbf{Key variables}" \ "" \ "\hspace{1em} Electoral volatility" \ "" \ ///
	"\hspace{1em} Group attachment" \ "" \ "\hspace{1em} Volatility $\times$ group attachment" \ "" \ ///
	"\textbf{Party-level covariates}" \ "" \ "\hspace{1em} Party size (vote share in prev. elec.)" \ "" \ ///
	"\hspace{1em} Links w/ unions" \ "" \ "\hspace{1em} Links w/ business" \ "" \ ///
	"\hspace{1em} Links w/ religious orgs." \ "" \ "\textbf{Electoral system covariates}" \ "" \ ///
	"\hspace{1em} Plurality" \ "" \ "\hspace{1em} Proportional representation" \ "" \ ///
	"\hspace{1em} Closed list" \ "" \ "\hspace{1em} Open list" \ "" \ ///
	"\hspace{1em} Average district magnitude (lower house)" \ "" \ "\textbf{Political system covariates}" \ "" \ ///
	"\hspace{1em} Presidential system" \ "" \ "\hspace{1em} Parliamentary system" \ "" \ ///
	"\hspace{1em} System tenure" \ "" \ "\hspace{1em} Polity score" \ "" \ "\textbf{Other country-level covariates}" \ "" \ ///
	"\hspace{1em} GDP per capita, PPP (logged)" \ "" \ "\hspace{1em} Gini index" \ "" \ ///
	"\hspace{1em} Ethnic fractionalization" \ "" \ "Constant" \ "")
frmttable, statmat(obs) substat(0) sdec(0) rtitle("Observations") append
frmttable, statfont(fs11) hlines(101{0}11) vlines(011{0}) ///
	ctitles("","Baseline model","Extended model","" \ "","","Ethnicity","Ideology") multicol(1,3,2) noblankrows ///
	title("\caption{Electoral Volatility, Group Attachments, and Electoral Strategies---Alternative Programmaticness Measure\label{tab:prog2}}\leavevmode") ///
	titlfont(normalsize)

*----------------------------------------------------------------------------------------*/
** Table D3: Electoral Volatility, Group Attachments, and Electoral Strategies—Combined Group Attachment Measure
*----------------------------------------------------------------------------------------*/
setup
gen union = (unions > .5) if unions ~= .
sum extremist2, d
gen extremist2d = (extremist2 > r(p50)) if extremist2 ~= .
gen ga = (ethnic == 1 | union == 1 | extremist2d == 1) if union ~= . & extremist2d ~= . & ethnic ~= .
global p_controls partysize business religious
reg personal c.ev_total##i.ga $p_controls $controls, cluster(ccode)
getcoefs 1
reg programmatic c.ev_total##i.ga $p_controls $controls, cluster(ccode)
getcoefs 2
mat coef = coef1,coef2
mat pval = pval_1,pval_2
mat obs = obs1,obs2
clear
svmat coef
svmat pval
gen names = ""
local names : rownames coef
local k = 1
foreach j of local names {
	replace names = "`j'" in `k'
	local k = `k' + 1
	}
egen base = rowtotal(coef*)
drop if base == 0
drop base
local m 2 3 4 6 7 8 10 11 12 13 14 16 17 18 19 21 22 23 25
gen order = .
local k = 1
foreach j of local m {
	replace order = `j' in `k'
	local k = `k' + 1
	}
local n = _N
local newn = `n'+6
set obs `newn'
local m 1 5 9 15 20 24
local k = 1
foreach j of local m {
	local newn = `n'+`k'
	replace order = `j' in `newn'
	local k = `k' + 1
	}
sort order
mkmat coef*, mat(coef)
forval i = 1/2 {
	gen empty`i' = .
	order empty`i', after(pval`i')
	}
mkmat pval1-empty2, mat(pval)
local cols = colsof(pval)
local rows = rowsof(pval)
mat stars = J(`rows',`cols',0)
forval i = 1/`rows' {
	forval j = 1/`cols' {
		local pval = pval[`i',`j']
	if `pval' <= .10 & `pval' > .05 {
		matrix stars[`i',`j'] = 1
		}
	if `pval' <= .05 & `pval' > .01 {
		matrix stars[`i',`j'] = 2 
		}
	if `pval' <= .01 {
		matrix stars[`i',`j'] = 3
		}
	}
}
frmttable, clear
frmttable, statmat(coef) substat(1) sdec(2) annotate(stars) asymbol($^{\dagger}$,*,**) ///
	rtitles("\textbf{Key variables}" \ "" \ "\hspace{1em} Electoral volatility" \ "" \ ///
	"\hspace{1em} Combined group attachment" \ "" \ "\hspace{1em} Volatility $\times$ combined group attachment" \ "" \ ///
	"\textbf{Party-level covariates}" \ "" \ "\hspace{1em} Party size (vote share in prev. elec.)" \ "" \ ///
	"\hspace{1em} Links w/ business" \ "" \ ///
	"\hspace{1em} Links w/ religious orgs." \ "" \ "\textbf{Electoral system covariates}" \ "" \ ///
	"\hspace{1em} Plurality" \ "" \ "\hspace{1em} Proportional representation" \ "" \ ///
	"\hspace{1em} Closed list" \ "" \ "\hspace{1em} Open list" \ "" \ ///
	"\hspace{1em} Average district magnitude (lower house)" \ "" \ "\textbf{Political system covariates}" \ "" \ ///
	"\hspace{1em} Presidential system" \ "" \ "\hspace{1em} Parliamentary system" \ "" \ ///
	"\hspace{1em} System tenure" \ "" \ "\hspace{1em} Polity score" \ "" \ "\textbf{Other country-level covariates}" \ "" \ ///
	"\hspace{1em} GDP per capita, PPP (logged)" \ "" \ "\hspace{1em} Gini index" \ "" \ ///
	"\hspace{1em} Ethnic fractionalization" \ "" \ "Constant" \ "")
frmttable, statmat(obs) substat(0) sdec(0) rtitle("Observations") append
frmttable, statfont(fs11) hlines(11{0}11) vlines(011{0}) ///
	ctitles("","Personalism","Programmaticness") noblankrows ///
	title("\caption{Electoral Volatility, Group Attachments, and Electoral Strategies---Combined Group Attachment Measure\label{tab:ga}}\leavevmode") ///
	titlfont(normalsize)
frmttable, clear

*----------------------------------------------------------------------------------------*/
** Table D4: Results with Multi-Dimensional Measures of Ideological Extremism
*----------------------------------------------------------------------------------------*/
setup
reg personal c.ev_total##c.extremist2_2d $p_controls $controls, cluster(ccode)
getcoefs 1
reg programmatic c.ev_total##c.extremist2_2d $p_controls $controls, cluster(ccode)
getcoefs 2
reg personal c.ev_total##c.extremist2_3d $p_controls $controls, cluster(ccode)
getcoefs 3
reg programmatic c.ev_total##c.extremist2_3d $p_controls $controls, cluster(ccode)
getcoefs 4
coeftable_main 4 0
frmttable, clear
frmttable, statmat(coef) substat(1) sdec(2) annotate(stars) asymbol($^{\dagger}$,*,**) ///
	rtitles("\textbf{Key variables}" \ "" \ "\hspace{1em} Electoral volatility" \ "" \ ///
	"\hspace{1em} Ideological extremism" \ "" \ "\hspace{1em} Volatility $\times$ extremism" \ "" \ ///
	"\textbf{Party-level covariates}" \ "" \ "\hspace{1em} Party size (vote share in prev. elec.)" \ "" \ ///
	"\hspace{1em} Links w/ unions" \ "" \ "\hspace{1em} Links w/ business" \ "" \ ///
	"\hspace{1em} Links w/ religious orgs." \ "" \ "\textbf{Electoral system covariates}" \ "" \ ///
	"\hspace{1em} Plurality" \ "" \ "\hspace{1em} Proportional representation" \ "" \ ///
	"\hspace{1em} Closed list" \ "" \ "\hspace{1em} Open list" \ "" \ ///
	"\hspace{1em} Average district magnitude (lower house)" \ "" \ "\textbf{Political system covariates}" \ "" \ ///
	"\hspace{1em} Presidential system" \ "" \ "\hspace{1em} Parliamentary system" \ "" \ ///
	"\hspace{1em} System tenure" \ "" \ "\hspace{1em} Polity score" \ "" \ "\textbf{Other country-level covariates}" \ "" \ ///
	"\hspace{1em} GDP per capita, PPP (logged)" \ "" \ "\hspace{1em} Gini index" \ "" \ ///
	"\hspace{1em} Ethnic fractionalization" \ "" \ "Constant" \ "")
frmttable, statmat(obs) substat(0) sdec(0) rtitle("Observations") append
frmttable, statfont(fs11) hlines(101{0}11) vlines(0001{0}) ///
	ctitles("","Economic and social left-right","","Economic, social left-right, and nationalism","" \ ///
	"","Personalism","Programmaticness","Personalism","Programmaticness") ///
	multicol(1,2,2;1,4,2) ///
	title("\caption{Results with Multi-Dimensional Measures of Ideological Extremism\label{tab:multidim}}\leavevmode") ///
	titlfont(normalsize)
frmttable, clear

*----------------------------------------------------------------------------------------*/
** Table D5: Results with Detailed Electoral Rules Controls
*----------------------------------------------------------------------------------------*/
setup
reg personal c.ev_total##d $p_controls $controls2 $controls3 ///
	pluralty b3.cl mdmh b5.erule, cluster(ccode)
getcoefs 1
reg programmatic c.ev_total##d $p_controls $controls2 $controls3 ///
	pluralty b3.cl mdmh b5.erule, cluster(ccode)
getcoefs 2
reg personal c.ev_total##c.ethnic $p_controls $controls2 $controls3 ///
	pluralty b3.cl mdmh b5.erule, cluster(ccode)
getcoefs 3
reg programmatic c.ev_total##c.ethnic $p_controls $controls2 $controls3 ///
	pluralty b3.cl mdmh b5.erule, cluster(ccode)
getcoefs 4
reg personal c.ev_total##c.extremist2 $p_controls $controls2 $controls3 ///
	pluralty b3.cl mdmh b5.erule, cluster(ccode)
getcoefs 5
reg programmatic c.ev_total##c.extremist2 $p_controls $controls2 $controls3 ///
	pluralty b3.cl mdmh b5.erule, cluster(ccode)
getcoefs 6
clear
forval i = 1/6 {
	scalar R = rowsof(coef`i')-1
	mat coef`i' = coef`i'[1..R,1..2]
	mat pval_`i' = pval_`i'[1..R,1..1]
	}
mat obs = obs1,obs2,obs3,obs4,obs5,obs6
mat cn = cn1,cn2,cn3,cn4,cn5,cn6
forval i = 1/6 {
	svmat coef`i'
	svmat pval_`i'
	}
forval i = 2/3 {
	foreach x in 11 12 21 22 {
		replace coef`x' = . in `i'
		}
	}
gen names = ""
local names : rownames coef3
local k = 1
foreach j of local names {
	replace names = "`j'" in `k'
	local k = `k' + 1
	}
gen order2 = _n
* keep only the electoral rules variables
keep if order2 < 4 | order2 > 15
egen base = rowtotal(coef*)
drop if base == 0
drop base order2
local m 2 3 4 6 7 8 9 10 11 12 13 14
gen order = .
local k = 1
foreach j of local m {
	replace order = `j' in `k'
	local k = `k' + 1
	}
local n = _N
local newn = `n'+2
set obs `newn'
local m 1 5 
local k = 1
foreach j of local m {
	local newn = `n'+`k'
	replace order = `j' in `newn'
	local k = `k' + 1
	}
sort order
order coef11 coef12 coef21 coef22 coef31 coef32 coef41 coef42 coef51 coef52 coef61 coef62 ///
	pval_11 pval_21 pval_31 pval_41 pval_51 pval_61, first
mkmat coef*, mat(coef)
forval i = 1/6 {
	gen empty`i' = .
	order empty`i', after(pval_`i')
	}
mkmat pval_1-empty6, mat(pval)
local cols = colsof(pval)
local rows = rowsof(pval)
mat stars = J(`rows',`cols',0)
forval i = 1/`rows' {
	forval j = 1/`cols' {
		local pval = pval[`i',`j']
		if `pval' <= .10 & `pval' > .05 {
		matrix stars[`i',`j'] = 1
		}
		if `pval' <= .05 & `pval' > .01 {
			matrix stars[`i',`j'] = 2 
			}
		if `pval' <= .01 {
			matrix stars[`i',`j'] = 3
			}
		}
	}
frmttable, statmat(coef) substat(1) sdec(2) annotate(stars) asymbol($^{\dagger}$,*,**) ///
	rtitles("\textbf{Key variables}" \ "" \ "\hspace{1em} Electoral volatility" \ "" \ ///
	"\hspace{1em} Group attachment" \ "" \ "\hspace{1em} Volatility $\times$ group attachment" \ "" \ ///
	"\textbf{Electoral system covariates}" \ "" \ ///
	"\hspace{1em} Plurality" \ "" \ "\hspace{1em} Closed list" \ "" \ "\hspace{1em} Open list" \ "" \ ///
	"\hspace{1em} Average district magnitude (lower house)" \ "" \ ///
	"\hspace{1em} SMDP" \ "" \ "\hspace{1em} Two-round vote" \ "" \ "\hspace{1em} Alternative vote" \ "" \ ///
	"\hspace{1em} STV" \ "" \ "\hspace{1em} Mixed system" \ "")
frmttable, statmat(obs) substat(0) sdec(0) rtitle("Observations") append
frmttable, statfont(fs11) hlines(1001{0}11) vlines(000101{0}) ///
	ctitles("","Baseline model","","Extended model","","","" \ ///
			"","","","Ethnicity","","Ideology","" \ ///
			"","Personalism",,"Programmaticness","Personalism","Programmaticness","Personalism","Programmaticness") ///
	multicol(1,2,2;1,4,4;2,4,2;2,6,2) noblankrows ///
	title("\caption{Results with Detailed Electoral Rules Controls\label{tab:elecrules_robust_coefs}}\leavevmode") ///
	titlfont(normalsize)

*----------------------------------------------------------------------------------------*/
** Table D6: Economic Shocks, Group Attachments, and Electoral Strategies
*----------------------------------------------------------------------------------------*/
setup
reg personal c.ctot##d $p_controls $controls, cluster(ccode)
getcoefs 1
reg programmatic c.ctot##d $p_controls $controls, cluster(ccode)
getcoefs 2
reg personal c.ctot##c.ethnic $p_controls $controls, cluster(ccode)
getcoefs 3
reg programmatic c.ctot##c.ethnic $p_controls $controls, cluster(ccode)
getcoefs 4
reg personal c.ctot##c.extremist2 $p_controls $controls, cluster(ccode)
getcoefs 5
reg programmatic c.ctot##c.extremist2 $p_controls $controls, cluster(ccode)
getcoefs 6
coeftable_main 6 1
frmttable, clear
frmttable, statmat(coef) substat(1) sdec(2) annotate(stars) asymbol($^{\dagger}$,*,**) ///
	rtitles("\textbf{Key variables}" \ "" \ "\hspace{1em} Commodity terms of trade" \ "" \ ///
	"\hspace{1em} Group attachment" \ "" \ "\hspace{1em} Terms of trade $\times$ group attachment" \ "" \ ///
	"\textbf{Party-level covariates}" \ "" \ "\hspace{1em} Party size (vote share in prev. elec.)" \ "" \ ///
	"\hspace{1em} Links w/ unions" \ "" \ "\hspace{1em} Links w/ business" \ "" \ ///
	"\hspace{1em} Links w/ religious orgs." \ "" \ "\textbf{Electoral system covariates}" \ "" \ ///
	"\hspace{1em} Plurality" \ "" \ "\hspace{1em} Proportional representation" \ "" \ ///
	"\hspace{1em} Closed list" \ "" \ "\hspace{1em} Open list" \ "" \ ///
	"\hspace{1em} Average district magnitude (lower house)" \ "" \ "\textbf{Political system covariates}" \ "" \ ///
	"\hspace{1em} Presidential system" \ "" \ "\hspace{1em} Parliamentary system" \ "" \ ///
	"\hspace{1em} System tenure" \ "" \ "\hspace{1em} Polity score" \ "" \ "\textbf{Other country-level covariates}" \ "" \ ///
	"\hspace{1em} GDP per capita, PPP (logged)" \ "" \ "\hspace{1em} Gini index" \ "" \ ///
	"\hspace{1em} Ethnic fractionalization" \ "" \ "Constant" \ "")
frmttable, statmat(obs) substat(0) sdec(0) rtitle("Observations") append
frmttable, statfont(fs11) hlines(1001{0}11) vlines(000101{0}) ///
	ctitles("","Baseline model","","Extended model","","","" \ ///
			"","","","Ethnicity","","Ideology","" \ ///
			"","Personalism","Programmaticness","Personalism","Programmaticness","Personalism","Programmaticness") ///
	multicol(1,2,2;1,4,4;2,4,2;2,6,2) noblankrows ///
	title("\caption{Economic Shocks, Group Attachments, and Electoral Strategies\label{tab:coefs_ctot}}\leavevmode") ///
	titlfont(normalsize)

*----------------------------------------------------------------------------------------*/
** Table D7: Electoral Volatility and Commodity Terms of Trade Shocks
*----------------------------------------------------------------------------------------*/
setup
reg ev_total ctot, cl(ccode)
getcoefs 1
local n1 = e(N)
local cn1 = e(N_clust)
reg ev_total ctot $p_controls $controls, cl(ccode)
getcoefs 2
local n2 = e(N)
local cn2 = e(N_clust)
mat obs = `n1',`n2'
mat cn = `cn1',`cn2'
loc c1 = coef1[2,1]
loc c2 = coef1[2,2]
loc p1 = pval_1[2,1]
clear
svmat coef1
svmat coef2
svmat pval_1 
svmat pval_2
loc n = _N
replace coef11 = . in 2
replace coef12 = . in 2
replace coef11 = `c1' in `n'
replace coef12 = `c2' in `n'
replace pval_11 = . in 2
replace pval_11 = `p1' in `n'
gen names = ""
local names : rownames coef2
local k = 1
foreach j of local names {
	replace names = "`j'" in `k'
	local k = `k' + 1
	}
egen base = rowtotal(coef*)
drop if base == 0
drop base
mkmat coef*, mat(coef)
forval i = 1/2 {
	gen empty`i' = .
	order empty`i', after(pval_`i')
	}
mkmat pval_1-empty2, mat(pval)
local cols = colsof(pval)
local rows = rowsof(pval)
mat stars = J(`rows',`cols',0)
forval i = 1/`rows' {
	forval j = 1/`cols' {
		local pval = pval[`i',`j']
	if `pval' <= .10 & `pval' > .05 {
		matrix stars[`i',`j'] = 1
		}
	if `pval' <= .05 & `pval' > .01 {
		matrix stars[`i',`j'] = 2 
		}
	if `pval' <= .01 {
		matrix stars[`i',`j'] = 3
		}
	}
}
frmttable, clear
frmttable, statmat(coef) substat(1) sdec(2) annotate(stars) asymbol($^{\dagger}$,*,**) ///
	rtitles("Terms of trade index" \ "" \ "Party size (vote share in prev. elec.)" \ "" \ ///
	"Links w/ unions" \ "" \ "Links w/ business" \ "" \ "Links w/ religious orgs." \ "" \ ///
	"Plurality" \ "" \ "Proportional representation" \ "" \ "Closed list" \ "" \ ///
	"Open list" \ "" \ "Average district magnitude (lower house)" \ "" \ ///
	"Presidential system" \ "" \ "Parliamentary system" \ "" \ "System tenure" \ "" \ ///
	"Polity score" \ "" \ "GDP per capita, PPP (logged)" \ "" \ "Gini index" \ "" \ ///
	"Ethnic fractionalization" \ "" \ "Constant" \ "")
frmttable, statmat(obs) substat(0) sdec(0) rtitle("Observations") append
frmttable, statfont(fs11) hlines(11{0}11) ///
	ctitles("","(1)","(2)") ///
	title("\caption{Electoral Volatility and Commodity Terms of Trade Shocks\label{tab:ctot-volatility}}\leavevmode") ///
	titlfont(normalsize)
