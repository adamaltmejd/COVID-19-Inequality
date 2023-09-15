cd "\\micro.intra\projekt\P1105$\P1105_Gem\research_projects\multidimensional_inequality"

odbc load, exec("select P1105_LopNr_PersonNr as id, OrderCreatedDateTime from Inera_CK_EP WHERE OfferGroupName = 'Covid-19 PCR'")dsn("P1105") clear
gen tested = 1
gen year = yofd(dofc( OrderCreatedDateTime ))
drop OrderCreatedDateTime
collapse (max) tested, by(id year)
tempfile tested
save `tested'

use "data/data.dta", clear
keep if year == 2020
merge 1:1 id year using `tested'
drop _merge
keep if population == 1

// Test data is only available for a subset of regions
decode region, generate(region_s)
replace tested = 0 if missing(tested) & inlist(region_s, "01", "04", "08", "09", "10", "12")
replace tested = 0 if missing(tested) & inlist(region_s, "17", "18", "19", "20", "22", "23", "25")
replace tested = . if tested == 1 & inlist(region_s, "03", "05", "06", "07", "13", "14", "21", "24")

* Gender
reghdfe tested ib1.male, vce(cluster id) absorb(region age_cat)
margins i.male, grand nose

* Region of birth
reghdfe tested ib1.country, vce(cluster id) absorb(region age_cat) cformat(%8.5f)
margins i.country, grand nose

* Education
reghdfe tested ib1.education, vce(cluster id) absorb(region age_cat) cformat(%8.5f)
margins i.education, grand nose

* Income quartile
reghdfe tested ib1.income_qt, vce(cluster id) absorb(region age_cat) cformat(%8.5f)
margins i.income_qt, grand nose
