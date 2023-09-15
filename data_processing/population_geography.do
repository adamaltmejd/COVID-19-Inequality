* Region + municipality

forval i=2015/2021 {

	odbc load, exec("select P1105_LopNr_PersonNr as id, Kommun as municipality, Lan as region from RTB`i'")dsn("P1105") clear

	gen year=`i'+1
	duplicates drop id, force

	tempfile geo`i'
	save `geo`i'', replace
}

forvalues i=2015/2020 {
	append using `geo`i''
}
	
save "data\temp\population_geography.dta", replace