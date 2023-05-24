preserve
capture rm "estimates/regression_estimates_manymodels.sters"
capture rm "estimates/regression_estimates_onemodel.sters"

vl substitute groups_interacted = ib1.groups##ib0.year_2020
foreach y in $outcomes {
	foreach x in $groups {
		eststo `y'_`x': reghdfe `y' ib1.`x'##ib0.year_2020, vce(cluster id) absorb(region age_cat)
		estwrite . using "estimates/regression_estimates_manymodels.sters", append id(uid)
		eststo clear
	}
	// All in one model
	eststo `y'_full: reghdfe `y' $groups_interacted,  vce(cluster id) absorb(region age_cat)
	estwrite . using "estimates/regression_estimates_onemodel.sters", append id(uid)
	eststo clear
}

restore
exit 0