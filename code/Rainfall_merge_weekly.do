
set more off
set matsize 11000
clear all

global homedir "/Volumes/GoogleDrive/Shared drives/Econ21320/data/clean"
cd "${homedir}"

use "covid_deaths_us.dta", replace

gen date_fixed = date(date, "MD20Y")
drop date
rename date_fixed date

* Aggregate from county level
collapse (sum) COVID_mortality, by(state date)

gen week = week(date)

* Aggregate at weekly level
collapse (mean) COVID_mortality (first) date, by(state week)

*********************************
***** Merge Infections Data *****
*********************************

preserve
    use "covid_infections_us.dta", replace

    gen date_fixed = date(date, "MD20Y")
    drop date
    rename date_fixed date

    * Aggregate from county level
    collapse (sum) COVID_infections, by(state date)

    gen week = week(date)

    * Aggregate at weekly level
    collapse (mean) COVID_infections, by(state week)

    tempfile inf
	save "`inf'", replace
restore

merge 1:1 state week using "`inf'"
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

******************************************
***** Merge All-Cause Mortality Data *****
******************************************

preserve
    use "all_mortality_by_state.dta", replace

    gen date_fixed = dofc(date)
    drop date
    rename date_fixed date

    gen year = year(date)
    keep if year == 2020

    gen week = week(date)

    keep state week COVID_mortality_frac US_COVID_mortality US_total_mortality

    tempfile other_mort
	save "`other_mort'", replace
restore

merge m:1 state week using "`other_mort'"
keep if _merge != 2
drop _merge

**************************************************
***** Merge Unacast Social Distancing Scores *****
**************************************************

preserve
    use "unacast_social_distancing_state.dta", replace

    gen date_fixed = dofc(date)
    drop date
    rename date_fixed date

    sort state date
    gen una_distance = daily_distance_diff // 1
    by state: replace una_distance = una_distance + 1 // una_distance[_n-1] * (1 + daily_distance_diff) if _n > 1
    gen una_visitation = daily_visitation_diff // 1
    by state: replace una_visitation = una_visitation + 1 // una_visitation[_n-1] * (1 + daily_visitation_diff) if _n > 1

    gen week = week(date)

    * Aggregate at weekly level
    collapse (mean) una_distance una_visitation, by(state week)

    * Generate weekly percent difference (before normalize because can't take logs of negatives)
    gen una_pct_distance = (una_distance - una_distance[_n-1]) / una_distance[_n-1]
    gen una_pct_visitation = (una_visitation - una_visitation[_n-1]) / una_visitation[_n-1]
    gen una_log_distance = log(una_distance) - log(una_distance[_n-1])
    gen una_log_visitation = log(una_visitation) - log(una_visitation[_n-1])

    * Normalize
    /* by state: su una_distance
    by state: replace una_distance = (una_distance - r(mean)) / r(sd)
    by state: su una_visitation
    by state: replace una_visitation = (una_visitation - r(mean)) / r(sd) */

    ren una_distance old_una_distance

    by state: egen mob_mean = mean(old_una_distance)
    by state: egen mob_sd = sd(old_una_distance)
    by state: gen una_distance = (old_una_distance - mob_mean) / mob_sd

    drop mob_mean mob_sd old_una_distance

    ren una_visitation old_una_visitation

    by state: egen mob_mean = mean(old_una_visitation)
    by state: egen mob_sd = sd(old_una_visitation)
    by state: gen una_visitation = (old_una_visitation - mob_mean) / mob_sd

    drop mob_mean mob_sd old_una_visitation

    tempfile unacast
	save "`unacast'", replace
restore

merge 1:1 state week using "`unacast'"
keep if _merge != 2
drop _merge

**************************************************
***** Merge Twitter Social Distancing Scores *****
**************************************************

preserve
    use "twitter_social_distancing_state.dta", replace

    gen date_fixed = dofc(date)
    drop date
    rename date_fixed date

    gen year = year(date)
    keep if year == 2020

    gen week = week(date)

    * Aggregate at weekly level
    collapse (mean) twitter_social_distancing, by(state week)

    * Normalize
    /* by state: su twitter_social_distancing
    by state: replace twitter_social_distancing = (twitter_social_distancing - r(mean)) / r(sd) */
    
    ren twitter_social_distancing old_twitter

    by state: egen mob_mean = mean(old_twitter)
    by state: egen mob_sd = sd(old_twitter)
    by state: gen twitter_social_distancing = (old_twitter - mob_mean) / mob_sd

    drop mob_mean mob_sd old_twitter

    tempfile twitter
	save "`twitter'", replace
restore

merge 1:1 state week using "`twitter'"
keep if _merge != 2
drop _merge

********************************************************
***** Merge Descartes Lab Social Distancing Scores *****
********************************************************

preserve
    use "dl_social_distancing_state.dta", replace

    gen date_fixed = date(date, "YMD")
    drop date
    rename date_fixed date

    gen week = week(date)

    * Aggregate at weekly level
    collapse (mean) dl_mobility, by(state week)

    * Normalize
    /* by state: su dl_mobility
    by state: replace dl_mobility = (dl_mobility - r(mean)) / r(sd) */

    ren dl_mobility old_dl_mobility

    by state: egen mob_mean = mean(old_dl_mobility)
    by state: egen mob_sd = sd(old_dl_mobility)
    by state: gen dl_mobility = (old_dl_mobility - mob_mean) / mob_sd

    drop mob_mean mob_sd old_dl_mobility

    tempfile dl
	save "`dl'", replace
restore

merge 1:1 state week using "`dl'"
keep if _merge != 2
drop _merge

*******************************
***** Merge Rainfall Data *****
*******************************

preserve
    use "rain_data.dta", replace

    gen date_fixed = date(date, "MD20Y")
    drop date
    rename date_fixed date

    gen week = week(date)

    * Aggregate at weekly level
    collapse (mean) avg_precip, by(state week)

    tempfile rain
	save "`rain'", replace
restore

merge 1:1 state week using "`rain'"
keep if _merge != 2
drop _merge

*****************************
***** Merge Policy Data *****
*****************************

preserve
    use "policy_panel.dta", replace

    drop if State == ""

    rename (Date State) (date state)

    gen week = week(date)

    * Aggregate at weekly level
    collapse (max) EmergDec SchoolClose GathRestrictAny OtherBusinessClose RestaurantRestrict StayAtHome NEBusinessClose, by(state week)

    tempfile policy
	save "`policy'", replace
restore

merge 1:1 state week using "`policy'"
keep if _merge != 2
drop _merge

*******************************
***** Some Final Cleaning *****
*******************************

format %td date

egen state_id = group(state)

compress

save rain_merged_weekly.dta, replace
