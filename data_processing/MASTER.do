// WORKING DIR
cd "\\micro.intra\projekt\P1105$\P1105_Gem\research_projects\multidimensional_inequality"

/////////////////////
// DATA PROCESSING //
/////////////////////

// Create study population
do "population.do"
do "population_basic_info.do"
do "population_income_education.do"
do "population_geography.do"

// Outcomes
do "outcomes_covid.do"
do "outcomes_indirect.do"

// Merge
do "merge.do"

//////////////
// Analysis //
//////////////

use "data/data.dta", clear
drop if year > 2020 // Use only 2020 data

// All outcome variables
vl create all_outcomes = ( ///
	covid_pos covid_hosp covid_dead covid_novacc ///
	dead_any_cause antidepressants sedatives psych_1177 psych_outp psych_hosp psych_death ///
	surgery surg_dead_30d cancer cancer_dead_365d ///
	dispinc_drop unemployed not_in_emp ///
	hosp_inc hosp_unemp ///
)
// A subset of which is included in regressions (to save time)
vl create outcomes = ( ///
	covid_pos covid_hosp covid_dead ///
	dead_any_cause psych_outp ///
	surg_dead_30d cancer_dead_365d ///
	dispinc_drop unemployed ///
	hosp_inc hosp_unemp ///
)

// All groups
vl create groups = (country education income_qt male)

// Run main analysis
do "analysis_regressions.do"
do "analysis_margins.do"
do "analysis_export.do"

// Extra stats
collapse (mean) $all_outcomes, by (year)
rename * value*
rename valueyear year
reshape long value, i(year) j(outcome) string
export delimited "output/population_averages.csv", delimiter(,) quote replace

exit 0