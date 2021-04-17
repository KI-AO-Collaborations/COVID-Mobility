
set more off
set matsize 11000
clear all

global homedir "/Volumes/GoogleDrive/Shared drives/Econ21320/data/clean"
cd "${homedir}"

use "covid_safety.dta", replace

*****************************
***** Merge First Stage *****
*****************************

preserve
    use "epidemic_rates.dta", replace
    replace country = "Laos" if country == "Lao People's Democratic Republic"
    replace country = "UK" if country == "United Kingdom"
    replace country = "Vietnam" if country == "Viet Nam"
    replace country = "South Korea" if country == "Republic of Korea"

    tempfile epi_rates
	save "`epi_rates'", replace
restore

merge 1:1 country using "`epi_rates'"
keep if _merge == 3
drop _merge

preserve
    use "healthcare.dta", replace
    * duplicates list // Check duplicates
    duplicates drop

    * duplicates tag country, gen(dup)
    * We can drop all duplicates because they're not countries we are using
    duplicates drop country, force

    replace country = "Czechia" if country == "Czech Republic"
    replace country = "UK" if country == "United Kingdom"

    tempfile healthcare_clean
	save "`healthcare_clean'", replace
restore

merge 1:1 country using "`healthcare_clean'"
keep if _merge == 3
drop _merge

******************************
***** Merge Second Stage *****
******************************

preserve
    use "covid_infections.dta", replace
    replace country = "UK" if country == "United Kingdom"
    replace country = "South Korea" if country == "Korea, South"
    replace country = "Taiwan" if country == "Taiwan*"
    replace country = "Myanmar" if country == "Burma"

    tempfile covid_is
	save "`covid_is'", replace
restore

merge 1:1 country using "`covid_is'"
keep if _merge == 3
drop _merge

preserve
    use "covid_deaths.dta", replace
    replace country = "UK" if country == "United Kingdom"
    replace country = "South Korea" if country == "Korea, South"
    replace country = "Taiwan" if country == "Taiwan*"
    replace country = "Myanmar" if country == "Burma"

    tempfile covid_ds
	save "`covid_ds'", replace
restore

merge 1:1 country using "`covid_ds'"
keep if _merge == 3
drop _merge

preserve
    use "gdp_per_capita.dta", replace
    replace country = "Czechia" if country == "Czech Republic"
    replace country = "UK" if country == "United Kingdom"
    replace country = "South Korea" if country == "Korea, Rep."
    replace country = "Laos" if country == "Lao PDR"
    replace country = "Slovakia" if country == "Slovak Republic"

    tempfile gdp
	save "`gdp'", replace
restore

merge 1:1 country using "`gdp'"
keep if _merge == 3
drop _merge

***********************
***** First Stage *****
***********************

*************************
***** Data Cleaning *****
*************************

qui su covid_safety
gen normalized_safety = (covid_safety - r(mean) ) / r(sd)
qui su hqi
gen normalized_hqi = (hqi - r(mean) ) / r(sd)
gen log_deaths = log(10000 * infection_deaths)

***********************************
***** First Stage Regressions *****
***********************************

reg normalized_safety log_deaths, robust
reg normalized_safety log_deaths normalized_hqi, robust
reg normalized_safety log_deaths normalized_hqi pop_thousands, robust

* Optimal to add up to cubic term
reg normalized_safety c.log_deaths##c.log_deaths normalized_hqi, robust
reg normalized_safety c.log_deaths##c.log_deaths##c.log_deaths normalized_hqi pop_thousands, robust
reg normalized_safety c.log_deaths##c.log_deaths##c.log_deaths pop_thousands, robust
reg normalized_safety c.log_deaths##c.log_deaths##c.log_deaths, robust

* Adding beyond cubic term doesn't add anything
reg normalized_safety c.log_deaths##c.log_deaths##c.log_deaths##c.log_deaths normalized_hqi pop_thousands, robust

* Healthcare quality and deaths appear correlated, but aren't once we control for square and cubic terms
reg log_deaths normalized_hqi c.log_deaths#c.log_deaths, robust
reg log_deaths normalized_hqi c.log_deaths#c.log_deaths#c.log_deaths c.log_deaths#c.log_deaths, robust
* Squared and cubic terms also uncorrelated
gen log_deaths_sq = log_deaths * log_deaths
gen log_deaths_cu = log_deaths * log_deaths * log_deaths
reg log_deaths_sq normalized_hqi c.log_deaths#c.log_deaths#c.log_deaths log_deaths, robust
reg log_deaths_cu normalized_hqi c.log_deaths#c.log_deaths log_deaths, robust

* Add in GDP
reg normalized_safety c.log_deaths##c.log_deaths##c.log_deaths normalized_hqi pop_thousands gdp_per_capita, robust

* Add in population^2
reg normalized_safety c.log_deaths##c.log_deaths##c.log_deaths normalized_hqi c.pop_thousands##c.pop_thousands gdp_per_capita, robust

* F-test
test c.log_deaths c.log_deaths#c.log_deaths c.log_deaths#c.log_deaths#c.log_deaths

************************
***** Second Stage *****
************************

*************************
***** Data Cleaning *****
*************************

gen log_covid_deaths = log(10000 * covid_deaths / (pop_thousands * 1000) )
gen log_covid_infections = log(10000 * covid_infections / (pop_thousands * 1000) )

*********************************
***** Endogenous Regression *****
*********************************

reg log_covid_deaths normalized_safety normalized_hqi pop_thousands gdp_per_capita, robust
reg log_covid_infections normalized_safety normalized_hqi pop_thousands gdp_per_capita, robust

***********************************
***** Reduced Form Regression *****
***********************************

reg log_covid_deaths c.log_deaths#c.log_deaths#c.log_deaths normalized_hqi pop_thousands gdp_per_capita, robust
reg log_covid_infections c.log_deaths#c.log_deaths#c.log_deaths normalized_hqi pop_thousands gdp_per_capita, robust

***************************
***** 2SLS Regression *****
***************************

ivregress 2sls log_covid_deaths normalized_hqi (normalized_safety = c.log_deaths#c.log_deaths#c.log_deaths), robust
ivreg2 log_covid_deaths normalized_hqi pop_thousands gdp_per_capita (normalized_safety = c.log_deaths##c.log_deaths##c.log_deaths), robust

ivregress 2sls log_covid_infections normalized_hqi (normalized_safety = c.log_deaths#c.log_deaths#c.log_deaths), robust
ivregress 2sls log_covid_infections normalized_hqi pop_thousands gdp_per_capita (normalized_safety = c.log_deaths#c.log_deaths#c.log_deaths), robust
