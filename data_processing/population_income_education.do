forvalues i=2015/2020 {

	if (inlist(`i', 2015, 2016, 2017, 2018)) {
		local utb="Sun2000niva_old"
		local syss="SyssStat11"
	} 
	else {
		local utb="Sun2020niva_old"
		local syss="SyssStat19"
	}
	
	odbc load, exec("select P1105_LopNr_PersonNr, DispInk04, `utb', `syss' from Individ_`i'")dsn("P1105") clear
		
	rename (P1105_LopNr_PersonNr `utb' `syss') (id edu emp)
	duplicates drop id, force
	
	gen education=real(edu)
	recode education (.=0) (2=1) (3=2) (4=2) (5=3) (6=3) (7=3)
	rename DispInk04 disposable_income
	gen year=`i'+1
	destring disposable_income, replace

	generate employed = 0
	destring emp, replace
	replace employed = 1 if emp == 1

	keep id year education disposable_income employed
	tempfile inut`i'
	save `inut`i'', replace
}

forvalues i=2015/2019 {
	append using `inut`i''
}
	
save "data/temp/population_income_education.dta", replace

exit