cd "\\micro.intra\projekt\P1105$\P1105_Gem\research_projects\multidimensional_inequality"
use "data/data.dta", clear
drop if year > 2021

gen cancer_death=0
replace cancer_death=1 if strpos(cause_of_death,"C")==1

*Regressions

*Male sex
reghdfe cancer_death ib0.year_group##ib1.male ib0.year_group##ib80.age_cat, vce(cluster id) absorb(region) 
margins i.male, over(year_group) grand nose

*Country of birth
reghdfe cancer_death ib0.year_group##ib1.country ib0.year_group##ib80.age_cat, vce(cluster id) absorb(region) 
margins i.country, over(year_group) grand nose

*Education
reghdfe cancer_death ib0.year_group##ib1.education ib0.year_group##ib80.age_cat, vce(cluster id) absorb(region) 
margins i.education, over(year_group) grand nose

*Inkomst
reghdfe cancer_death ib0.year_group##ib1.income_qt ib0.year_group##ib80.age_cat, vce(cluster id) absorb(region) 
margins i.income_qt, over(year_group) grand nose

*Detected cancers

*Male sex
reghdfe cancer ib0.year_group##ib1.male ib0.year_group##ib80.age_cat, vce(cluster id) absorb(region) 
margins i.male, over(year_group) grand nose

*Country of birth
reghdfe cancer ib0.year_group##ib1.country ib0.year_group##ib80.age_cat, vce(cluster id) absorb(region) 
margins i.country, over(year_group) grand nose

*Education
reghdfe cancer ib0.year_group##ib1.education ib0.year_group##ib80.age_cat, vce(cluster id) absorb(region) 
margins i.education, over(year_group) grand nose

*Inkomst
reghdfe cancer ib0.year_group##ib1.income_qt ib0.year_group##ib80.age_cat, vce(cluster id) absorb(region) 
margins i.income_qt, over(year_group) grand nose


