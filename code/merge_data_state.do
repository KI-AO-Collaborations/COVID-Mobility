
set more off
set matsize 11000
clear all

global homedir "/Volumes/GoogleDrive/Shared drives/Econ21320/data/clean"
cd "${homedir}"

use "all_mortality_by_state.dta", replace

gen date_fixed = dofc(date)
drop date
gen year = year(date_fixed)
gen week = week(date_fixed)

**************************************************
***** Merge Twitter Social Distancing Scores *****
**************************************************

preserve
    use "twitter_social_distancing_state.dta", replace

    gen date_fixed = dofc(date)
    drop date
    gen year = year(date_fixed)
    gen week = week(date_fixed)

    collapse (mean) twitter_social_distancing, by(state year week)

    su twitter_social_distancing
    replace twitter_social_distancing = (twitter_social_distancing - r(mean)) / r(sd)

    tempfile twitter
	save "`twitter'", replace
restore

merge 1:1 state year week using "`twitter'"
drop _merge

**************************************************
***** Merge Unacast Social Distancing Scores *****
**************************************************

preserve
    use "unacast_social_distancing_state.dta", replace

    gen date_fixed = dofc(date)
    drop date
    gen year = year(date_fixed)
    gen week = week(date_fixed)

    sort state year week
    gen distance_diff = 1
    by state year week: replace distance_diff = distance_diff[_n-1] * (1 + daily_distance_diff) if _n > 1
    replace distance_diff = distance_diff - 1
    gen visitation_diff = 1
    by state year week: replace visitation_diff = visitation_diff[_n-1] * (1 + daily_visitation_diff) if _n > 1
    replace visitation_diff = visitation_diff - 1

    collapse (last) distance_diff visitation_diff (mean) state_population, by(state year week)

    su distance_diff
    replace distance_diff = (distance_diff - r(mean)) / r(sd)
    su visitation_diff
    replace visitation_diff = (visitation_diff - r(mean)) / r(sd)

    tempfile unacast
	save "`unacast'", replace
restore

merge 1:1 state year week using "`unacast'"
*drop _merge

******************************
***** Generate Variables *****
******************************

sort state year week

by state: gen bartik = COVID_mortality_frac[_n-1] * ( (US_COVID_mortality_frac - US_COVID_mortality_frac[_n-1]) / US_COVID_mortality_frac[_n-1]) + (1 - COVID_mortality_frac[_n-1]) * ( (1 - US_COVID_mortality_frac - (1 - US_COVID_mortality_frac[_n-1]) ) / (1 - US_COVID_mortality_frac[_n-1]) )

by state: replace bartik = 0 if (US_COVID_mortality_frac == 0) & (US_COVID_mortality_frac[_n-1] == 0)
* Taking liberties to define bartik as next value if dividing by 0 but future fraction > 0
* by state: replace bartik = bartik[_n+1] if (US_COVID_mortality_frac > 0) & (US_COVID_mortality_frac[_n-1] == 0)

gen COVID_per_capita = COVID_mortality / state_population

encode state, gen(state_id)

by state: gen lagged_twitter = twitter_social_distancing[_n-1]
by state: gen lagged_dist = distance_diff[_n-1]
by state: gen lagged_visit = visitation_diff[_n-1]
by state: gen lagged_bartik = bartik[_n-1]

***************************
***** Run Regressions *****
***************************

**********************
***** Endogenous *****
**********************

reg COVID_mortality_frac lagged_twitter, robust
reg COVID_mortality_frac lagged_dist, robust
reg COVID_mortality_frac lagged_visit, robust

************************
***** Reduced Form *****
************************

reg COVID_mortality_frac lagged_bartik, robust

***********************
***** First Stage *****
***********************

reg twitter_social_distancing bartik, robust
reg distance_diff bartik, robust
reg visitation_diff bartik, robust

****************
***** 2SLS *****
****************

ivreg2 COVID_mortality_frac (lagged_twitter = lagged_bartik lagged_dist lagged_visit), robust

ivreg2 COVID_mortality_frac (lagged_dist = lagged_bartik), robust

ivreg2 COVID_mortality_frac (lagged_visit = lagged_bartik), robust
