// -
// Population of positive cases

odbc load, exec("select P1105_LopNr_PersonNr as id, Provtagningsdatum, Diagnosdatum, Statistikdatum from FHM_SMINET")dsn("P1105") clear
recast long id
gen date=Provtagningsdatum
replace date=Diagnosdatum if date=="NULL"

// Statistikdatum is date (never missing) need to convert to string
tostring Statistikdatum, generate(Statistikdatum2) format(%tdCCYY-NN-DD) force
replace date=Statistikdatum2 if date=="NULL"

gen year=substr(date,1,4)
destring year, replace

keep id year
gen covid_pos = 1
duplicates drop

save "data\temp\outcome_covid_positive.dta", replace

// -
// Population of COVID-19 hospitalized individuals

odbc load, exec("SELECT lopnr AS id, INDATUMA FROM SoS_R_PAR_SV_KLEV4 WHERE hdia IN ('U071', 'U072') UNION ALL SELECT P1105_LopNr_PersonNr AS id, INDATUMA FROM SoS_R_PAR_SV_TOT_LEV5 WHERE hdia IN ('U071', 'U072')")dsn("P1105") clear
recast long id
gen year=substr(INDATUMA,1,4)
destring year, replace
replace year = 2020 if year < 2020 // 2 observations have wrong dates
drop INDATUMA

gen covid_hosp = 1
duplicates drop
save "data\temp\outcome_covid_hospitalized.dta", replace

// -
// Population of individuals who died from COVID-19
odbc load, exec("SELECT lopnr AS id, DODSDAT FROM SoS_R_DORS_KLEV4 WHERE ULORSAK IN ('U071', 'U072') UNION ALL SELECT P1105_LopNr_PersonNr AS id, DODSDAT FROM SoS_R_DORS_TOT_LEV5 WHERE ULORSAK IN ('U071', 'U072')")dsn("P1105") clear
recast long id
gen year=substr(DODSDAT,1,4)
destring year, replace
drop DODSDAT

gen covid_dead = 1
duplicates drop
save "data\temp\outcome_covid_dead.dta", replace

// -
// Population of individuals who were vaccinated against COVID-19
odbc load, exec("select P1105_Lopnr_PersonNr as id, vaccination_date from FHM_NVR_Covid_20230316")dsn("P1105") clear
collapse (min) vaccination_date, by(id)
generate covid_vaccinated = 1
generate year = year(vaccination_date)
save "data\temp\outcome_covid_vaccinated.dta", replace

exit 0

