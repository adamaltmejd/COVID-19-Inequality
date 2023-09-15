preserve

// Regression results
local replace replace
estimates drop _all
estread using estimates/regression_estimates_manymodels.sters
foreach x in $groups {
	foreach y in $outcomes {
		estimates restore `y'_`x'
		regsave using "estimates/regression_results.dta", `replace' addlabel(y, `y', x, `x', est, manymodels) tstat pval ci level(95) detail(all)
		local replace append
	}
}
estimates drop _all
estread using estimates/regression_estimates_onemodel.sters
foreach y in $outcomes {
		estimates restore `y'_full
		regsave using "estimates/regression_results.dta", `replace' addlabel(y, `y', est, onemodel) tstat pval ci level(95) detail(all)
}

// Margins results
//local replace replace
tempfile marginsdata
foreach est in manymodels {
	
	estimates drop _all
	estread using estimates/margin_estimates_`est'.sters
	
	foreach x in $groups {
		foreach y in $outcomes {
			estimates restore `y'_`x'
			
			// Regsave does not work with margins, nose
			// regsave using "estimates/margins_results.dta", `replace' addlabel(y, `y', x, `x', est, `est') detail(all)
			// Instead, save estimates manually
			clear
			matrix et = e(b)'
			quietly svmat2 et,  rnames(var) names( coef ) full
			generate y = "`y'"
			generate x = "`x'"
			generate est = "`est'"
			
			capture save `marginsdata'
			if (_rc != 0) {
				append using `marginsdata'
				save `marginsdata', replace
			}
			//local replace append
		}
	}
}
save estimates/margins_results.dta, replace

use "estimates/regression_results.dta", clear
export delimited "output/regression_results.csv", delimiter(,) quote replace

use "estimates/margins_results.dta", clear
export delimited "output/margins_results.csv", delimiter(,) quote replace

restore
exit 0