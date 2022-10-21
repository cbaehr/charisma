/***********************************************

Replication code for article "Using Regression Discontinuity to Uncover the Personal Incumbency Advantag"
by Robert S. Erikson and Rocio Titiunik
November 13, 2014
************************************************/
clear all
set more off

insheet using EriksonTitiunik-QJPS-data.csv, comma clear

/*************************************************************
Table 1, Panel (A): Parametric regression estimation
*************************************************************/
/* Columns 1, 2, and 3, respectively */
reg dv cutpoint lagdv if use==1
outreg2 using table1-panelA.xls, replace excel

reg dv cutpoint lagdv year* if use==1
outreg2 using table1-panelA.xls, append
reg dv cutpoint lagdv year* pv if use==1
outreg2 using table1-panelA.xls, append

/*************************************************************
Table 1, Panel (B): Nonparametric regression estimation
*************************************************************/
/* RD estimate */
rdrobust demvotesh demmvlag if use==1, c(0) bwselect(CCT)

/* Divide by 2 to get personal incumbency advantage estimate */
local tau_RD = e(tau_cl)
matrix input CI_RD = (`e(ci_l_rb)',`e(ci_r_rb)')

local tau_PIA= `tau_RD'/2
local cir = `e(ci_r_rb)'/2
local cil = `e(ci_l_rb)'/2
matrix input CI_PIA = (`cil',`cir')

display as error "RD estimate is `tau_RD'"
display as error "Personal Incumbency advantage is `tau_PIA', with confidence interval [`cil',`cir']"

putexcel A1=("RD Estimate") B1=("Confidence Interval RD Estimate") D1=("Personal Incumbency Advantage (PIA)") E1=("Confidence Interval PIA") using table1-panelB.xls, replace
putexcel A2=(`tau_RD') B2=matrix(CI_RD) D2=(`tau_PIA') E2=matrix(CI_PIA) using table1-panelB.xls, modify
