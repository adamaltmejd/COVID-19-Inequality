preserve
capture rm "estimates/margin_estimates_manymodels.sters"
capture rm "estimates/margin_estimates_onemodel.sters"

// Main model
estimates drop _all
estread using "estimates/regression_estimates_manymodels.sters", id(uid)
foreach y in $outcomes {
	foreach x in $groups {
		estimates restore `y'_`x'
		eststo `y'_`x': margins `x'##year_2020, grand post
		estwrite `y'_`x' using "estimates/margin_estimates_manymodels.sters", append id(uid)
	}
}
eststo clear

// Full model (only one model per outcome)
// Takes a very long time to run.
/*
estimates drop _all
estread using "estimates/regression_estimates_onemodel.sters", id(uid)
foreach y in $outcomes {
	foreach x in $groups {
		estimates restore `y'_full
		eststo `y'_`x': margins `x'##year_2020, grand post
		estwrite `y'_`x' using "estimates/margin_estimates_onemodel.sters", append id(uid)
	}
}
eststo clear
estimates drop _all
*/

restore
exit 0