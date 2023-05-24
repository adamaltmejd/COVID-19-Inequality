* Population
use "data\temp\population.dta", clear
generate population = 1

* Basic info
merge m:1 id using "data\temp\population_basic_info.dta"
keep if _merge == 3
drop _merge
generate age=(year-birth_year)-1
egen age_cat=cut(age), at(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120)

generate work_age = 0
replace work_age = 1 if age > 24 & age < 65

// Drop aged less than 25
drop if age < 25

// --------
// GROUPS

// INCOME
merge 1:1 id year using "data\temp\population_income_education.dta"
* Income quartiles
bysort birth_year year: egen income_qt=xtile(disposable_income), n(4)
drop _merge
keep if population == 1

// GEOGRAPHY
merge 1:1 id year using "data\temp\population_geography.dta"
destring municipality, replace
replace municipality = 0 if missing(municipality)
drop _merge
keep if population == 1

// --------
// OUTCOMES

// Covid outcomes are only defined for years 2020 and 2021.
merge 1:1 id year using "data\temp\outcome_covid_positive.dta"
replace covid_pos = 0 if missing(covid_pos) & inlist(year, 2020, 2021)
replace covid_pos = . if year == 2021 // Covid test data only available until March 2021
drop _merge
keep if population == 1

merge 1:1 id year using "data\temp\outcome_covid_hospitalized.dta"
replace covid_hosp = 0 if missing(covid_hosp) & inlist(year, 2020, 2021)
drop _merge
keep if population == 1

merge 1:1 id year using "data\temp\outcome_covid_dead.dta"
replace covid_dead = 0 if missing(covid_dead) & inlist(year, 2020, 2021)
drop _merge
keep if population == 1

merge 1:1 id year using "data\temp\outcome_covid_vaccinated.dta"
replace covid_vaccinated = 0 if missing(covid_vaccinated) & year == 2021 // Vaccinations started in 2021
generate covid_novacc = 1 - covid_vaccinated
drop _merge
keep if population == 1

// Indirect outcomes are defined for all years
merge 1:1 id year using "data\temp\outcome_dead_any_cause.dta"
replace dead_any_cause = 0 if missing(dead_any_cause)
drop _merge
keep if population == 1

merge 1:1 id year using "data\temp\outcome_antidepressants.dta"
replace antidepressants = 0 if missing(antidepressants)
drop _merge
keep if population == 1

merge 1:1 id year using "data\temp\outcome_sedatives.dta"
replace sedatives = 0 if missing(sedatives)
drop _merge
keep if population == 1

merge 1:1 id year using "data\temp\outcome_psych_1177.dta"
replace psych_1177 = 0 if missing(psych_1177)
replace psych_1177 = . if year == 2021 // Full 2021 not available
drop _merge
keep if population == 1

merge 1:1 id year using "data\temp\outcome_psych_outp.dta"
replace psych_outp = 0 if missing(psych_outp)
drop _merge
keep if population == 1

merge 1:1 id year using "data\temp\outcome_psych_hosp.dta"
replace psych_hosp = 0 if missing(psych_hosp)
drop _merge
keep if population == 1

merge 1:1 id year using "data\temp\outcome_psych_death.dta"
replace psych_death = 0 if missing(psych_death)
drop _merge
keep if population == 1

merge 1:1 id year using "data\temp\outcome_surgeries.dta"
replace surgery = 0 if missing(surgery)
drop _merge
keep if population == 1

merge 1:1 id year using "data\temp\outcome_disposable_income.dta"
replace dispinc_drop = 0 if missing(dispinc_drop)
replace dispinc_drop = . if work_age == 0 // Only defined for working age population
drop _merge
keep if population == 1

merge 1:1 id year using "data\temp\outcome_unemployment.dta"
replace unemployed = 0 if missing(unemployed)
replace unemployed = . if work_age == 0 // Only defined for workig age population
replace unemployed = . if year > 2020 // Unemployment spell data not available for 2021
drop _merge
keep if population == 1

// Not in employment (only defined for working age population)
generate not_in_emp = 1 if work_age == 1
replace not_in_emp = 0 if employed == 1 & work_age == 1

merge 1:1 id year using "data\temp\outcome_cancer.dta"
replace cancer = 0 if missing(cancer)
replace cancer = . if year == 2021 // Cancer data not available for 2021
drop _merge
keep if population == 1

// Create indicator for if the year is 2020-
gen year_2020 = 0
replace year_2020 = 1 if year >= 2020

// Encode region identifiers
encode region, generate(region_e)
drop region
rename region_e region

// Add interactions of outcomes
generate hosp_inc = dispinc_drop * covid_hosp
generate hosp_unemp = unemployed * covid_hosp
generate unemp_inc = dispinc_drop * unemployed
generate pos_inc = covid_pos * dispinc_drop
generate pos_unemp = covid_pos * dispinc_drop
generate cdead_hosp = covid_dead * covid_hosp

// Unique identifiers to save e(sample)
gen long uid = _n

save "data\data.dta", replace