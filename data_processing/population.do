forvalues i=2015/2020 {
	odbc load, exec("select * from RTB`i'")dsn("P1105") clear
	rename P1105_LopNr_PersonNr id
	gen year=`i'+1
	keep id year
	duplicates drop id, force
	tempfile pop`i'
	save `pop`i''
}

forvalues i=2015/2019 {
	append using `pop`i''
}

// Drop users with bad numbers
preserve
odbc load, exec("select * from Population_PersonNr_20211231")dsn("P1105") clear
keep if FelPersonNr == 1 | AterAnv == 1
rename P1105_LopNr_PersonNr id
keep id FelPersonNr AterAnv LopNrByte
tempfile bad_ids
save `bad_ids'

restore
merge m:1 id using `bad_ids'
keep if _merge == 1
drop _merge

save "data/temp/population.dta", replace