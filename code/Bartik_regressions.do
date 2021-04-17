
set more off
set matsize 11000
clear all

global homedir "/Volumes/GoogleDrive/Shared drives/Econ21320/data/clean"
cd "${homedir}"

use "rain_merged_weekly.dta", replace

sort state week

gen US_other_mortality = US_total_mortality - US_COVID_mortality

by state: gen bartik = COVID_mortality_frac[_n-1] * (US_COVID_mortality - US_COVID_mortality[_n-1]) / US_COVID_mortality[_n-1] + (1 - COVID_mortality_frac[_n-1]) * (US_other_mortality -  US_other_mortality[_n-1]) / US_other_mortality[_n-1]

by state: replace bartik = (1 - COVID_mortality_frac[_n-1]) * (US_other_mortality -  US_other_mortality[_n-1]) / US_other_mortality[_n-1] if COVID_mortality_frac[_n-1] == 0

by state: gen d_COVID_mortality = log(COVID_mortality) - log(COVID_mortality[_n-1])
by state: gen d_una = una_distance - una_distance[_n-1]

ivregress 2sls d_COVID_mortality_pc d_COVID_mortality_pc_m3 i.state_id (d_una_m2 = bartik_m2), robust

ivregress 2sls d_COVID_mortality_pc i.state_id (d_COVID_mortality_pc_m2 = bartik_m2), robust
