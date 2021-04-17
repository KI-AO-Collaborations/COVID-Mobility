
set more off
set matsize 11000
clear all

global homedir "/Volumes/GoogleDrive/Shared drives/Econ21320/data/clean"
cd "${homedir}"

use "rain_merged_daily.dta", replace

sort state date

by state: gen d_COVID_mortality = log(COVID_mortality) - log(COVID_mortality[_n-1])
by state: gen d_COVID_mortality_m1 = d_COVID_mortality[_n-1]
by state: gen d_precip = avg_precip - avg_precip[_n-1]
by state: gen d_una = una_distance - una_distance[_n-1]
by state: gen d_dl = dl_mobility - dl_mobility[_n-1]
by state: gen d_twit = twitter_social_distancing - twitter_social_distancing[_n-1]

gen COVID_mortality_pc = COVID_mortality / population
by state: gen d_COVID_mortality_pc = COVID_mortality_pc - COVID_mortality_pc[_n-1]

by state: gen COVID_mortality_pc_m1 = COVID_mortality_pc[_n-1]
by state: gen d_COVID_mortality_pc_m1 = d_COVID_mortality_pc[_n-1]

by state: gen precip_m1 = avg_precip[_n-1]

***********************
***** First Stage *****
***********************

reg d_una d_precip d_COVID_mortality_m1 population i.state_id, robust
reg d_dl d_precip d_COVID_mortality_m1 population i.state_id, robust
reg d_twit d_precip d_COVID_mortality_m1 population i.state_id, robust

reg d_una c.d_precip##c.d_precip d_COVID_mortality_m1 population i.state_id, robust
reg d_dl c.d_precip##c.d_precip d_COVID_mortality_m1 population i.state_id, robust
reg d_twit c.d_precip##c.d_precip d_COVID_mortality_m1 population i.state_id, robust

reg d_una c.d_precip##c.d_precip c.d_COVID_mortality_m1##c.d_COVID_mortality_m1 population i.state_id, robust
reg d_dl c.d_precip##c.d_precip c.d_COVID_mortality_m1##c.d_COVID_mortality_m1 population i.state_id, robust
reg d_twit c.d_precip##c.d_precip c.d_COVID_mortality_m1##c.d_COVID_mortality_m1 population i.state_id, robust

reg d_una d_precip COVID_mortality_pc_m1 population i.state_id, robust
reg d_dl d_precip COVID_mortality_pc_m1 population i.state_id, robust
reg d_twit d_precip COVID_mortality_pc_m1 population i.state_id, robust

* Good specifications:
reg d_una d_precip COVID_mortality_pc_m1 population, robust
reg d_dl d_precip COVID_mortality_pc_m1 population, robust
reg d_twit d_precip COVID_mortality_pc_m1 population, robust

reg d_una d_precip d_COVID_mortality_pc_m1 population i.state_id, robust
reg d_dl d_precip d_COVID_mortality_pc_m1 population i.state_id, robust
reg d_twit d_precip d_COVID_mortality_pc_m1 population i.state_id, robust

* Good specifications:
reg d_una d_precip d_COVID_mortality_pc_m1 population, robust
reg d_dl d_precip d_COVID_mortality_pc_m1 population, robust
reg d_twit d_precip d_COVID_mortality_pc_m1 population, robust

* Testing with policy implementation
reg d_una d_precip d_COVID_mortality_pc_m1 GathRestrictAny i.state_id, robust
reg d_una d_precip d_COVID_mortality_pc_m1 OtherBusinessClose i.state_id, robust
reg d_una d_precip d_COVID_mortality_pc_m1 RetaurantRestrict i.state_id, robust
reg d_una d_precip d_COVID_mortality_pc_m1 NEBusinessClose i.state_id, robust

***********************
***** First Stage *****
***********************

* Here's what we'll show:
foreach var in d_una d_dl d_twit {
    local results_file = "../../results/regression_tables/rain_firststage_`var'"
    capture erase "`results_file'.ster"

    qui reg `var' d_precip precip_m1, robust
    qui estimates save "`results_file'", append
    qui reg `var' d_precip precip_m1 i.state_id, robust
    qui estimates save "`results_file'", append
    qui reg `var' d_precip precip_m1 d_COVID_mortality_pc_m1 i.state_id, robust
    qui estimates save "`results_file'", append
}

***************************
***** Generate Tables *****
***************************

est clear

local all_res = ""

foreach var in d_una d_dl d_twit {
    local vars = ""
    local i = 1
    foreach loc in "a_`var'" "b_`var'" "c_`var'" {
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
mlabels(,none) numbers mgroups("Unacast" "Descartes Lab" "Twitter", ///
pattern(1 0 0 1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span}) span) ///
prehead("") ///
posthead("\hline\addlinespace") prefoot("\addlinespace") ///
postfoot("") ///
keep(d_precip d_COVID_mortality_pc_m1 _cons) ///
order(d_precip d_COVID_mortality_pc_m1 _cons) ///
varlabels(d_precip "\(\Delta\)Precipitation\(_{t}\)" ///
            d_COVID_mortality_pc_m1 "\(\Delta\)COVID Mortality Per Capita\(_{t-1}\)" ///
            _cons "Constant")

*****************************
***** Prepare Variables *****
*****************************

sort state date

by state: gen d_precip_m1 = d_precip[_n-1]
by state: gen d_precip_m2 = d_precip[_n-2]
by state: gen d_una_m1 = d_una[_n-1] / 100000
by state: gen d_una_m2 = d_una[_n-2] / 100000
by state: gen d_dl_m1 = d_dl[_n-1] / 100000
by state: gen d_dl_m2 = d_dl[_n-2] / 100000
by state: gen d_twit_m1 = d_twit[_n-1] / 100000
by state: gen d_twit_m2 = d_twit[_n-2] / 100000
by state: gen d_COVID_mortality_pc_m2 = d_COVID_mortality_pc[_n-2]
by state: gen d_COVID_mortality_pc_m3 = d_COVID_mortality_pc[_n-3]

*********************************
***** Endogenous Regression *****
*********************************

* 1 period prior
reg d_COVID_mortality_pc d_una_m1, robust
reg d_COVID_mortality_pc d_dl_m1, robust
reg d_COVID_mortality_pc d_twit_m1, robust

reg d_COVID_mortality_pc d_una_m1 i.state_id, robust
reg d_COVID_mortality_pc d_dl_m1 i.state_id, robust
reg d_COVID_mortality_pc d_twit_m1 i.state_id, robust

reg d_COVID_mortality_pc d_una_m1 d_COVID_mortality_pc_m2 i.state_id, robust
reg d_COVID_mortality_pc d_dl_m1 d_COVID_mortality_pc_m2 i.state_id, robust
reg d_COVID_mortality_pc d_twit_m1 d_COVID_mortality_pc_m2 i.state_id, robust

* 2 periods prior
reg d_COVID_mortality_pc d_una_m2, robust
reg d_COVID_mortality_pc d_dl_m2, robust
reg d_COVID_mortality_pc d_twit_m2, robust

reg d_COVID_mortality_pc d_una_m2 i.state_id, robust
reg d_COVID_mortality_pc d_dl_m2 i.state_id, robust
reg d_COVID_mortality_pc d_twit_m2 i.state_id, robust

reg d_COVID_mortality_pc d_una_m2 d_COVID_mortality_pc_m3 i.state_id, robust
reg d_COVID_mortality_pc d_dl_m2 d_COVID_mortality_pc_m3 i.state_id, robust
reg d_COVID_mortality_pc d_twit_m2 d_COVID_mortality_pc_m3 i.state_id, robust

***********************************
***** Reduced Form Regression *****
***********************************

* 1 period prior
reg d_COVID_mortality_pc d_precip_m1, robust

reg d_COVID_mortality_pc d_precip_m1 i.state_id, robust

reg d_COVID_mortality_pc d_precip_m1 d_COVID_mortality_pc_m2 i.state_id, robust

* 2 periods prior
reg d_COVID_mortality_pc d_precip_m2, robust

reg d_COVID_mortality_pc d_precip_m2 i.state_id, robust

reg d_COVID_mortality_pc d_precip_m2 d_COVID_mortality_pc_m2 i.state_id, robust

***************************
***** 2SLS Regression *****
***************************

* 1 period prior
ivregress 2sls d_COVID_mortality_pc (d_una_m1 = d_precip_m1), robust
ivregress 2sls d_COVID_mortality_pc (d_dl_m1 = d_precip_m1), robust
ivregress 2sls d_COVID_mortality_pc (d_twit_m1 = d_precip_m1), robust

ivregress 2sls d_COVID_mortality_pc i.state_id (d_una_m1 = d_precip_m1), robust
ivregress 2sls d_COVID_mortality_pc i.state_id (d_dl_m1 = d_precip_m1), robust
ivregress 2sls d_COVID_mortality_pc i.state_id (d_twit_m1 = d_precip_m1), robust

ivregress 2sls d_COVID_mortality_pc d_COVID_mortality_pc_m2 i.state_id (d_una_m1 = d_precip_m1), robust
ivregress 2sls d_COVID_mortality_pc d_COVID_mortality_pc_m2 i.state_id (d_dl_m1 = d_precip_m1), robust
ivregress 2sls d_COVID_mortality_pc d_COVID_mortality_pc_m2 i.state_id (d_twit_m1 = d_precip_m1), robust

* 2 periods prior
ivregress 2sls d_COVID_mortality_pc (d_una_m2 = d_precip_m2), robust
ivregress 2sls d_COVID_mortality_pc (d_dl_m2 = d_precip_m2), robust
ivregress 2sls d_COVID_mortality_pc (d_twit_m2 = d_precip_m2), robust

ivregress 2sls d_COVID_mortality_pc i.state_id (d_una_m2 = d_precip_m2), robust
ivregress 2sls d_COVID_mortality_pc i.state_id (d_dl_m2 = d_precip_m2), robust
ivregress 2sls d_COVID_mortality_pc i.state_id (d_twit_m2 = d_precip_m2), robust

ivregress 2sls d_COVID_mortality_pc d_COVID_mortality_pc_m3 i.state_id (d_una_m2 = d_precip_m2), robust
ivregress 2sls d_COVID_mortality_pc d_COVID_mortality_pc_m3 i.state_id (d_dl_m2 = d_precip_m2), robust
ivregress 2sls d_COVID_mortality_pc d_COVID_mortality_pc_m3 i.state_id (d_twit_m2 = d_precip_m2), robust

************************
***** Second Stage *****
************************

local results_file = "../../results/regression_tables/rain_secondstage_m2"
capture erase "`results_file'.ster"

foreach var in d_una_m2 d_dl_m2 d_twit_m2 {
    if "`var'" == "d_dl_m2" {
        rename d_una_m2 temp
        rename d_dl_m2 d_una_m2
        local var = "d_una_m2"
    }
    else if "`var'" == "d_twit_m2" {
        rename d_una_m2 d_dl_m2
        rename d_twit_m2 d_una_m2
        local var = "d_una_m2"
        local final = "1"
    }

    qui ivregress 2sls d_COVID_mortality_pc (`var' = d_precip_m2), robust
    qui estimates save "`results_file'", append
    qui ivregress 2sls d_COVID_mortality_pc i.state_id (`var' = d_precip_m2), robust
    qui estimates save "`results_file'", append
    qui ivregress 2sls d_COVID_mortality_pc d_COVID_mortality_pc_m3 i.state_id (`var' = d_precip_m2), robust
    qui estimates save "`results_file'", append
    if "`final'" == "1" {
        rename d_una_m2 d_twit_m2
        rename temp d_una_m2
    }
}

***************************
***** Generate Tables *****
***************************

est clear

local all_res = ""

local i = 1
foreach var in d_una_m2 d_dl_m2 d_twit_m2 {
    local vars = ""
    foreach loc in "a_`var'" "b_`var'" "c_`var'" {
        est use "../../results/regression_tables/rain_secondstage_m2", number(`i')
        est sto `loc'
        local vars = "`vars' `loc'"
        local i = `i' + 1
    }
    local all_res = "`all_res' `vars'"
}

estout `all_res' using "../../results/regression_tables/rain_secondstage_m2.tex", ///
cells("b(star fmt(%9.3fc))" "se(fmt(%9.3fc) par)") stats(N r2 r2_a, fmt(%9.0fc %9.3f %9.3f) ///
labels("\(N\)" "\(R^2\)" "Adj. \(R^2\)")) ///
msign(--) style(tex) collabels(,none) ///
substitute(\_ _) starlevel(* 0.10 ** 0.05 *** 0.01) replace ///
mlabels(,none) numbers mgroups("Unacast" "Descartes Lab" "Twitter", ///
pattern(1 0 0 1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span}) span) ///
prehead("") ///
posthead("\hline\addlinespace") prefoot("\addlinespace") ///
postfoot("") ///
keep(d_una_m2 d_COVID_mortality_pc_m3 _cons) ///
order(d_una_m2 d_COVID_mortality_pc_m3 _cons) ///
varlabels(d_una_m2 "\(\Delta\)Mobility\(_{t-2}\)" ///
            d_COVID_mortality_pc_m3 "\(\Delta\)COVID Mortality Per Capita\(_{t-3}\)" ///
            _cons "Constant")
