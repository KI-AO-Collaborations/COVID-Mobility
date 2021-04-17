
set more off
set matsize 11000
clear all

global homedir "/Volumes/GoogleDrive/Shared drives/Econ21320/data/clean"
cd "${homedir}"

use "covid_deaths_us.dta", replace

gen date_fixed = date(date, "MD20Y")
drop date

* Aggregate from county level
collapse (sum) COVID_mortality, by(state date_fixed)

*********************************
***** Merge Infections Data *****
*********************************

preserve
    use "covid_infections_us.dta", replace

    gen date_fixed = date(date, "MD20Y")
    drop date

    * Aggregate from county level
    collapse (sum) COVID_infections, by(state date_fixed)

    tempfile inf
	save "`inf'", replace
restore

merge 1:1 state date_fixed using "`inf'"
drop _merge

*********************************
***** Merge Population Data *****
*********************************

preserve
    use "pop_us.dta", replace

    * Aggregate from county level
    collapse (sum) population, by(state)

    tempfile pop
	save "`pop'", replace
restore

merge m:1 state using "`pop'"
drop _merge

**************************************************
***** Merge Unacast Social Distancing Scores *****
**************************************************

preserve
    use "unacast_social_distancing_state.dta", replace

    gen date_fixed = dofc(date)
    drop date

    sort state date
    gen una_distance = daily_distance_diff + 1
    gen una_visitation = daily_visitation_diff + 1
    /* by state: replace una_distance = una_distance[_n-1] * (1 + daily_distance_diff) if _n > 1
    gen una_visitation = 1
    by state: replace una_visitation = una_visitation[_n-1] * (1 + daily_visitation_diff) if _n > 1

    by state: su una_distance
    by state: replace una_distance = (una_distance - r(mean)) / r(sd)
    by state: su una_visitation
    by state: replace una_visitation = (una_visitation - r(mean)) / r(sd) */

    * Normalize
    ren una_distance old_una_distance

    sort state
    by state: egen mob_mean = mean(old_una_distance)
    by state: egen mob_sd = sd(old_una_distance)
    by state: gen una_distance = (old_una_distance - mob_mean) / mob_sd

    drop mob_mean mob_sd old_una_distance

    ren una_visitation old_una_visitation

    sort state
    by state: egen mob_mean = mean(old_una_visitation)
    by state: egen mob_sd = sd(old_una_visitation)
    by state: gen una_visitation = (old_una_visitation - mob_mean) / mob_sd

    drop mob_mean mob_sd old_una_visitation

    tempfile unacast
	save "`unacast'", replace
restore

merge 1:1 state date_fixed using "`unacast'"
keep if _merge != 2
drop _merge

**************************************************
***** Merge Twitter Social Distancing Scores *****
**************************************************

preserve
    use "twitter_social_distancing_state.dta", replace

    gen date_fixed = dofc(date)
    drop date

    /* su twitter_social_distancing
    replace twitter_social_distancing = (twitter_social_distancing - r(mean)) / r(sd) */

    * Normalize
    ren twitter_social_distancing old_twitter

    sort state
    by state: egen mob_mean = mean(old_twitter)
    by state: egen mob_sd = sd(old_twitter)
    by state: gen twitter_social_distancing = (old_twitter - mob_mean) / mob_sd

    tempfile twitter
	save "`twitter'", replace
restore

merge 1:1 state date_fixed using "`twitter'"
keep if _merge != 2
drop _merge

sort state date

by state: replace twitter_social_distancing = twitter_social_distancing[_n-1] if missing(twitter_social_distancing) & (_n > 1)

********************************************************
***** Merge Descartes Lab Social Distancing Scores *****
********************************************************

preserve
    use "dl_social_distancing_state.dta", replace

    gen date_fixed = date(date, "YMD")
    drop date

    /* su dl_mobility
    replace dl_mobility = (dl_mobility - r(mean)) / r(sd) */

    * Normalize
    ren dl_mobility old_dl_mobility

    sort state
    by state: egen mob_mean = mean(old_dl_mobility)
    by state: egen mob_sd = sd(old_dl_mobility)
    by state: gen dl_mobility = (old_dl_mobility - mob_mean) / mob_sd

    drop mob_mean mob_sd old_dl_mobility

    tempfile dl
	save "`dl'", replace
restore

merge 1:1 state date_fixed using "`dl'"
keep if _merge != 2
drop _merge

*******************************
***** Merge Rainfall Data *****
*******************************

preserve
    use "rain_data.dta", replace

    gen date_fixed = date(date, "MD20Y")
    drop date

    tempfile rain
	save "`rain'", replace
restore

merge 1:1 state date_fixed using "`rain'"
keep if _merge != 2
drop _merge

*****************************
***** Merge Policy Data *****
*****************************

preserve
    use "policy_panel.dta", replace

    rename (Date State) (date_fixed state)

    tempfile policy
	save "`policy'", replace
restore

merge 1:1 state date_fixed using "`policy'"
keep if _merge != 2
drop _merge

*******************************
***** Some Final Cleaning *****
*******************************

gen year = year(date_fixed)
gen week = week(date_fixed)

format %td date_fixed

rename date_fixed date

egen state_id = group(state)

compress

save rain_merged_daily.dta, replace
