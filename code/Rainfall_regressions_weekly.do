
set more off
set matsize 11000
clear all

global homedir "/Volumes/GoogleDrive/Shared drives/Econ21320/data/clean"
cd "${homedir}"

use "rain_merged_weekly.dta", replace

sort state date

rename twitter_social_distancing twitter_sd

gen COVID_mortality_pc = COVID_mortality / population
gen COVID_infections_pc = COVID_infections / population

foreach var in "COVID_mortality" "COVID_mortality_pc" "COVID_infections" "COVID_infections_pc" "avg_precip" "una_pct_distance" "una_log_distance" "una_distance" "dl_mobility" "twitter_sd" "EmergDec" "SchoolClose" "GathRestrictAny" "OtherBusinessClose" "RestaurantRestrict" "StayAtHome" "NEBusinessClose" {
    by state: gen d_`var' = `var' - `var'[_n-1]
    by state: gen d_log_`var' = log(`var') - log(`var'[_n-1])

    forval i=1/5 {
        by state: gen `var'_m`i' = `var'[_n-`i']
        by state: gen d_`var'_m`i' = d_`var'[_n-`i']
        by state: gen d_log_`var'_m`i' = d_log_`var'[_n-`i']
    }
}

* New Reduced Form:
reg d_COVID_mortality_pc d_avg_precip_m2 d_EmergDec_m2 d_SchoolClose_m2 d_GathRestrictAny_m2 d_RestaurantRestrict_m2 d_StayAtHome_m2 d_NEBusinessClose_m2 i.week, robust

***********************
***** First Stage *****
***********************

local f_stats = ""

foreach var in d_dl_mobility d_una_distance d_twitter_sd {
    local results_file = "../../results/regression_tables/rain_firststage_`var'"
    capture erase "`results_file'.ster"

    qui reg `var' d_avg_precip, robust cluster(state_id)
    qui estimates save "`results_file'", append

    qui test d_avg_precip
    local f = round(r(F), 0.001)
    local f: di %4.3fc `f'
    local f_stats = "`f_stats' `f'"

    qui reg `var' d_avg_precip i.state_id, robust cluster(state_id)
    qui estimates save "`results_file'", append

    qui test d_avg_precip
    local f = round(r(F), 0.001)
    local f: di %4.3fc `f'
    local f_stats = "`f_stats' `f'"

    qui reg `var' d_avg_precip EmergDec GathRestrictAny d_COVID_mortality_pc_m1 i.state_id, robust cluster(state_id)
    qui estimates save "`results_file'", append

    qui test d_avg_precip
    local f = round(r(F), 0.001)
    local f: di %4.3fc `f'
    local f_stats = "`f_stats' `f'"

    qui reg `var' d_avg_precip EmergDec GathRestrictAny d_COVID_mortality_pc_m1 i.state_id i.state_id#c.week i.state_id#c.week#c.week, robust cluster(state_id)
    qui estimates save "`results_file'", append

    qui test d_avg_precip
    local f = round(r(F), 0.001)
    local f: di %4.3fc `f'
    local f_stats = "`f_stats' `f'"
}
di "F-stats: `f_stats'"

***************************
***** Generate Tables *****
***************************

est clear

local all_res = ""

foreach var in d_dl_mobility d_una_distance d_twitter_sd {
    local vars = ""
    local i = 1
    foreach loc in "a_`var'" "b_`var'" "c_`var'" "d_`var'" {
        est use "../../results/regression_tables/rain_firststage_`var'", number(`i')
        est sto `loc'
        local vars = "`vars' `loc'"
        local i = `i' + 1
    }
    local all_res = "`all_res' `vars'"
}

estout `all_res' using "../../results/regression_tables/rain_firststage_all.tex", ///
cells("b(star fmt(%9.3fc))" "se(fmt(%9.3fc) par)") stats(N r2 r2_a, fmt(%9.0fc %9.3f %9.3f) ///
labels("\(N\)" "\(R^2\)" "Adj. \(R^2\)")) ///
msign(--) style(tex) collabels(,none) ///
substitute(\_ _) starlevel(* 0.10 ** 0.05 *** 0.01) replace ///
mlabels(,none) numbers mgroups("Descartes Lab" "Unacast" "Twitter", ///
pattern(1 0 0 0 1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span}) span) ///
prehead("") ///
posthead("\hline\addlinespace") prefoot("\addlinespace") ///
postfoot("") ///
keep(d_avg_precip EmergDec GathRestrictAny d_COVID_mortality_pc_m1 _cons) ///
order(d_avg_precip EmergDec GathRestrictAny d_COVID_mortality_pc_m1 _cons) ///
varlabels(d_avg_precip "\(\Delta\)Avg. Precipitation\(_{t}\)" ///
            EmergDec "Emergency Declaration\(_{t}\)" ///
            GathRestrictAny "Gathering Restriction (Any)\(_{t}\)" ///
            d_COVID_mortality_pc_m1 "\(\Delta\)COVID Mortality Per Capita\(_{t-1}\)" ///
            _cons "Constant")

****************
***** 2SLS *****
****************

foreach delay_bool in "_m" {
    forval delay_val=1/4 {
        local delay = "`delay_bool'`delay_val'"
        local delay_m1 = "`delay_bool'`=`delay_val'+1'"
        local log = "" // "_log"

        local results_file = "../../results/regression_tables/rain_secondstage_`delay'"
        capture erase "`results_file'.ster"

        local final = 0
        foreach var in "d`log'_dl_mobility" "d`log'_una_distance" "d`log'_twitter_sd" {
            if "`var'" == "d`log'_una_distance" {
                rename d`log'_dl_mobility`delay' temp
                rename d`log'_una_distance`delay' d`log'_dl_mobility`delay'
                local var = "d`log'_dl_mobility"
            }
            else if "`var'" == "d`log'_twitter_sd" {
                rename d`log'_dl_mobility`delay' d`log'_una_distance`delay'
                rename d`log'_twitter_sd`delay' d`log'_dl_mobility`delay'
                local var = "d`log'_dl_mobility"
                local final = 1
            }

            qui ivregress 2sls d_COVID_mortality_pc (`var'`delay' = d_avg_precip`delay'), robust cluster(state_id)
            qui estimates save "`results_file'", append

            qui ivregress 2sls d_COVID_mortality_pc i.state_id (`var'`delay' = d_avg_precip`delay'), robust cluster(state_id)
            qui estimates save "`results_file'", append

            qui ivregress 2sls d_COVID_mortality_pc EmergDec`delay' GathRestrictAny`delay' d_COVID_mortality_pc`delay_m1' i.state_id (`var'`delay' = d_avg_precip`delay'), robust cluster(state_id)
            qui estimates save "`results_file'", append

            qui ivregress 2sls d_COVID_mortality_pc EmergDec`delay' GathRestrictAny`delay' d_COVID_mortality_pc`delay_m1' i.state_id i.state_id#c.week (`var'`delay' = d_avg_precip`delay'), robust cluster(state_id)
            qui estimates save "`results_file'", append

            if "`final'" == "1" {
                rename d`log'_dl_mobility`delay' d`log'_twitter_sd`delay'
                rename temp d`log'_dl_mobility`delay'
            }
        }
    }
}

***************************
***** Generate Tables *****
***************************

foreach delay_bool in "_m" {
    forval delay_val=1/4 {
        local delay = "`delay_bool'`delay_val'"
        local delay_m1 = "`delay_bool'`=`delay_val'+1'"

        est clear

        local all_res = ""

        local i = 1
        foreach var in "d`log'_dl_mobility" "d`log'_una_distance" "d`log'_twitter_sd" {
            local vars = ""
            foreach loc in "a_`var'" "b_`var'" "c_`var'" "d_`var'" {
                est use "../../results/regression_tables/rain_secondstage_`delay'", number(`i')
                est sto `loc'
                local vars = "`vars' `loc'"
                local i = `i' + 1
            }
            local all_res = "`all_res' `vars'"
        }

        estout `all_res' using "../../results/regression_tables/rain_secondstage`delay'.tex", ///
        cells("b(star fmt(%9.3e))" "se(fmt(%9.3e) par)") stats(N r2 r2_a, fmt(%9.0fc %9.3f %9.3f) ///
        labels("\(N\)" "\(R^2\)" "Adj. \(R^2\)")) ///
        msign(--) style(tex) collabels(,none) ///
        substitute(\_ _) starlevel(* 0.10 ** 0.05 *** 0.01) replace ///
        mlabels(,none) numbers mgroups("Descartes Lab" "Unacast" "Twitter", ///
        pattern(1 0 0 0 1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span}) span) ///
        prehead("") ///
        posthead("\hline\addlinespace") prefoot("\addlinespace") ///
        postfoot("") ///
        keep(d`log'_dl_mobility`delay' EmergDec`delay' GathRestrictAny`delay' d_COVID_mortality_pc`delay_m1' _cons) ///
        order(d`log'_dl_mobility`delay' EmergDec`delay' GathRestrictAny`delay' d_COVID_mortality_pc`delay_m1' _cons) ///
        varlabels(d`log'_dl_mobility`delay' "\(\Delta\)Mobility\(_{t-`delay_val'}\)" ///
                    EmergDec`delay' "Emergency Declaration\(_{t-`delay_val'}\)" ///
                    GathRestrictAny`delay' "Gathering Restriction (Any)\(_{t-`delay_val'}\)" ///
                    d_COVID_mortality_pc`delay_m1' "\(\Delta\)COVID Mortality Per Capita\(_{t-`=`delay_val'+1'}\)" ///
                    _cons "Constant")
    }
}

*******************************************
***** Test Whether Coefficients Equal *****
*******************************************

preserve
    set more off

    expand 2
    sort state week
    gen number = 1
    by state week: replace number = number[_n-1] + 1 if _n > 1

    gen both_mobility_m3 = d_dl_mobility_m3
    replace both_mobility_m3 = d_una_distance_m3 if number == 2

    gen both_state_id = number + (state_id / 100)

    ivregress 2sls d_COVID_mortality_pc i.number#c.EmergDec_m3 i.number#c.GathRestrictAny_m3 i.number#c.d_COVID_mortality_pc_m4 i.number#i.state_id i.number (i.number#c.both_mobility_m3 = i.number#c.d_avg_precip_m3), robust cluster(both_state_id) noconstant

    test 1b.number#c.both_mobility_m3 = 2.number#c.both_mobility_m3
    * P-value: 0.4090 (can't reject the two coefficients are equal)
restore

***************************************
***** 2SLS with Measurement Error *****
***************************************

local results_file = "../../results/regression_tables/rain_secondstage_measurement_error"
capture erase "`results_file'.ster"

* OLS
qui reg d_COVID_mortality_pc d_una_distance_m3 EmergDec_m3 GathRestrictAny_m3 d_COVID_mortality_pc_m4 i.state_id, robust cluster(state_id)
qui estimates save "`results_file'", append
* 2SLS with rain IV
qui ivregress 2sls d_COVID_mortality_pc EmergDec_m3 GathRestrictAny_m3 d_COVID_mortality_pc_m4 i.state_id (d_una_distance_m3 = d_avg_precip_m3), robust cluster(state_id)
qui estimates save "`results_file'", append
* 2SLS with other measurement IV
qui ivregress 2sls d_COVID_mortality_pc EmergDec_m3 GathRestrictAny_m3 d_COVID_mortality_pc_m4 i.state_id (d_una_distance_m3 = d_dl_mobility_m3), robust cluster(state_id)
qui estimates save "`results_file'", append
/* * 2SLS with residualized other measurement IV (same results as non-residualized so we can ignore these results)
qui reg d_dl_mobility_m3 EmergDec_m3 GathRestrictAny_m3 d_COVID_mortality_pc_m4 i.state_id, robust cluster(state_id)
predict control_d_dl_mobility_m3, resid

ivregress 2sls d_COVID_mortality_pc EmergDec_m3 GathRestrictAny_m3 d_COVID_mortality_pc_m4 i.state_id (d_una_distance_m3 = control_d_dl_mobility_m3), robust cluster(state_id) */

***************************
***** Generate Tables *****
***************************

est clear

local all_res = ""

local i = 1
foreach loc in "a" "b" "c" {
    est use "../../results/regression_tables/rain_secondstage_measurement_error", number(`i')
    est sto `loc'
    local vars = "`vars' `loc'"
    local i = `i' + 1
}
local all_res = "`all_res' `vars'"

estout `all_res' using "../../results/regression_tables/rain_secondstage_measurement_error.tex", ///
cells("b(star fmt(%9.3e))" "se(fmt(%9.3e) par)") stats(N r2 r2_a, fmt(%9.0fc %9.3f %9.3f) ///
labels("\(N\)" "\(R^2\)" "Adj. \(R^2\)")) ///
msign(--) style(tex) collabels(,none) ///
substitute(\_ _) starlevel(* 0.10 ** 0.05 *** 0.01) replace ///
mlabels(,none) numbers mgroups("OLS" "2SLS" "Measurement Error", ///
pattern(1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span}) span) ///
prehead("") ///
posthead("\hline\addlinespace") prefoot("\addlinespace") ///
postfoot("") ///
keep(d_una_distance_m3 EmergDec_m3 GathRestrictAny_m3 d_COVID_mortality_pc_m4 _cons) ///
order(d_una_distance_m3 EmergDec_m3 GathRestrictAny_m3 d_COVID_mortality_pc_m4 _cons) ///
varlabels(d_una_distance_m3 "\(\Delta\)Mobility\(_{t-3}\)" ///
            EmergDec_m3 "Emergency Declaration\(_{t-3}\)" ///
            GathRestrictAny_m3 "Gathering Restriction (Any)\(_{t-3}\)" ///
            d_COVID_mortality_pc_m4 "\(\Delta\)COVID Mortality Per Capita\(_{t-4}\)" ///
            _cons "Constant")

************************
***** 2SLS + Lasso *****
************************

local results_file = "../../results/regression_tables/rain_secondstage_lasso"
capture erase "`results_file'.ster"

local final = 0
foreach var in "d_dl_mobility" "d_una_distance" "d_twitter_sd" {
    if "`var'" == "d_una_distance" {
        rename d_dl_mobility_m1 temp_m1
        rename d_dl_mobility_m3 temp_m3
        rename d_una_distance_m1 d_dl_mobility_m1
        rename d_una_distance_m3 d_dl_mobility_m3
        local var = "d_dl_mobility"
    }
    else if "`var'" == "d_twitter_sd" {
        rename d_dl_mobility_m1 d_una_distance_m1
        rename d_dl_mobility_m3 d_una_distance_m3
        rename d_twitter_sd_m1 d_dl_mobility_m1
        rename d_twitter_sd_m3 d_dl_mobility_m3
        local var = "d_dl_mobility"
        local final = 1
    }

    qui ivregress 2sls d_COVID_mortality_pc (d_dl_mobility_m1 d_dl_mobility_m3 = d_avg_precip_m1 d_avg_precip_m3), robust cluster(state_id)
    qui estimates save "`results_file'", append

    qui ivregress 2sls d_COVID_mortality_pc i.state_id (d_dl_mobility_m1 d_dl_mobility_m3 = d_avg_precip_m1 d_avg_precip_m3), robust cluster(state_id)
    qui estimates save "`results_file'", append

    qui ivregress 2sls d_COVID_mortality_pc EmergDec_m3 GathRestrictAny_m3 i.state_id (d_dl_mobility_m1 d_dl_mobility_m3 = d_avg_precip_m1 d_avg_precip_m3), robust cluster(state_id)
    qui estimates save "`results_file'", append

    if "`final'" == "1" {
        rename d_dl_mobility_m1 d_twitter_sd_m1
        rename d_dl_mobility_m3 d_twitter_sd_m3
        rename temp_m1 d_dl_mobility_m1
        rename temp_m3 d_dl_mobility_m3
    }
}

***************************
***** Generate Tables *****
***************************

est clear

local all_res = ""

local i = 1
foreach var in "d_dl_mobility" "d_una_distance" "d_twitter_sd" {
    local vars = ""
    foreach loc in "a_`var'" "b_`var'" "c_`var'" {
        est use "../../results/regression_tables/rain_secondstage_lasso", number(`i')
        est sto `loc'
        local vars = "`vars' `loc'"
        local i = `i' + 1
    }
    local all_res = "`all_res' `vars'"
}

estout `all_res' using "../../results/regression_tables/rain_secondstage_lasso.tex", ///
cells("b(star fmt(%9.3e))" "se(fmt(%9.3e) par)") stats(N r2 r2_a, fmt(%9.0fc %9.3f %9.3f) ///
labels("\(N\)" "\(R^2\)" "Adj. \(R^2\)")) ///
msign(--) style(tex) collabels(,none) ///
substitute(\_ _) starlevel(* 0.10 ** 0.05 *** 0.01) replace ///
mlabels(,none) numbers mgroups("Descartes Lab" "Unacast" "Twitter", ///
pattern(1 0 0 1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span}) span) ///
prehead("") ///
posthead("\hline\addlinespace") prefoot("\addlinespace") ///
postfoot("") ///
keep(d_dl_mobility_m1 d_dl_mobility_m3 EmergDec_m3 GathRestrictAny_m3 _cons) ///
order(d_dl_mobility_m1 d_dl_mobility_m3 EmergDec_m3 GathRestrictAny_m3 _cons) ///
varlabels(d_dl_mobility_m1 "\(\Delta\)Mobility\(_{t-1}\)" ///
            d_dl_mobility_m3 "\(\Delta\)Mobility\(_{t-3}\)" ///
            EmergDec_m3 "Emergency Declaration\(_{t-3}\)" ///
            GathRestrictAny_m3 "Gathering Restriction (Any)\(_{t-3}\)" ///
            _cons "Constant")
