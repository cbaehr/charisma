
/*##################################################################
# Partial replication files for "Parties as Disciplinarians: Charisma and Commitment Problems in Programmatic Campaigning" 
#by James R. Hollyer, Marko Klasnja and Rocio Titiunik, forthcoming at AJPS in 2021.
##################################################################*/


/* This do-file sets up some of the variables for formatting of results, and also 
	designates the covariates used in various cross-national analyses in the 
	paper and the supplementary appendix. */
	
* create placeholder interaction term in cross-national regressions for easier formatting
gen d = 1

replace gini = gini/100

* covariates
global p_controls partysize unions business religious
global controls pluralty i.pr b3.cl mdmh b1.system demsys polity gdppc gini ethfrac
global controls1 pluralty i.pr b3.cl mdmh
global controls2 b1.system demsys polity 
global controls3 gdppc gini ethfrac
