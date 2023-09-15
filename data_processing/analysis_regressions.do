preserve
capture rm "estimates/regression_estimates_manymodels.sters"
capture rm "estimates/regression_estimates_onemodel.sters"

vl substitute groups_interacted = ib0.year_group##ib1.groups
foreach y in $outcomes {
	foreach x in $groups {
		eststo `y'_`x': reghdfe `y' ib0.year_group##ib1.`x' ib0.year_group##ib80.age_cat, vce(cluster id) absorb(region)
		estwrite . using "estimates/regression_estimates_manymodels.sters", append id(uid)
		eststo clear
	}
	// All in one model
	eststo `y'_full: reghdfe `y' $groups_interacted ib0.year_group##ib80.age_cat,  vce(cluster id) absorb(region)
	estwrite . using "estimates/regression_estimates_onemodel.sters", append id(uid)
	eststo clear
}

restore
exit 0