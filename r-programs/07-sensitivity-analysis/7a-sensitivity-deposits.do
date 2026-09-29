* Sensitivity analysis 3.5.1: share of offshore wealth held in bank deposits
* Requires the repository's 0-setup.do globals: raw, work, fig, tables.
version 16.0
clear all
set more off

import excel "$raw/FGZ-raw-data.xlsx", sheet("T.A1") cellrange(A8:E28) clear
rename (A B C D E) (year world_gdp published_wealth securities deposits)
destring year world_gdp published_wealth securities deposits, replace
isid year
assert inrange(year, 2001, 2021)
assert !missing(year, world_gdp, published_wealth, securities, deposits)

* Original deposit share, calculated from original components (not rounded 20%).
gen double d0 = deposits/(securities + deposits)
gen double baseline = securities/(1-d0)
gen double baseline_published_gap = baseline - published_wealth
format d0 %8.4f
format baseline* %12.2f
list year d0 baseline published_wealth baseline_published_gap if abs(baseline_published_gap)>0.01, noobs

* Percentage-point perturbations of the original annual deposit share.
foreach scenario in m10 m5 p5 p10 {
    local shift = cond("`scenario'"=="m10",-.10,cond("`scenario'"=="m5",-.05,cond("`scenario'"=="p5",.05,.10)))
    gen double d_`scenario' = d0 + `shift'
    assert inrange(d_`scenario', 0, 1) & d_`scenario' < 1
    gen double wealth_`scenario' = securities/(1-d_`scenario')
    gen double gdp_`scenario' = 100*wealth_`scenario'/world_gdp
    gen double change_`scenario' = 100*(wealth_`scenario'/baseline-1)
}
gen double gdp_baseline = 100*baseline/world_gdp
gen double gdp_published = 100*published_wealth/world_gdp

label var gdp_m10 "Baseline -10 pp"
label var gdp_m5 "Baseline -5 pp"
label var gdp_baseline "Component-consistent baseline"
label var gdp_p5 "Baseline +5 pp"
label var gdp_p10 "Baseline +10 pp"
label var gdp_published "Published FGZ"

save "$work/sensitivity_deposits.dta", replace
export delimited using "$tables/sensitivity_deposits.csv", replace

twoway (line gdp_m10 year, lpattern(dash)) ///
       (line gdp_m5 year, lpattern(dash)) ///
       (line gdp_baseline year, lwidth(medthick)) ///
       (line gdp_p5 year, lpattern(dash)) ///
       (line gdp_p10 year, lpattern(dash)) ///
       (scatter gdp_published year, msymbol(oh) msize(vsmall)), ///
       ytitle("Offshore wealth (% of world GDP)") xtitle("Year") ///
       xlabel(2001(2)2021) ///
       legend(order(1 "-10 pp" 2 "-5 pp" 3 "Component baseline" 4 "+5 pp" 5 "+10 pp" 6 "Published FGZ") rows(2)) ///
       graphregion(color(white))
graph export "$fig/sensitivity_deposits.png", replace width(2400)
