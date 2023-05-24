// Deaths 2016-2019
odbc load, exec("select lopnr as id, DODSDAT, ULORSAK as cause_of_death from SoS_R_DORS_ENGANG")dsn("P1105") clear
tostring DODSDAT, replace
tempfile dead_2016_2019
save `dead_2016_2019'
odbc load, exec("select lopnr as id, DODSDAT, ULORSAK as cause_of_death from SoS_R_DORS_KLEV4")dsn("P1105") clear
append using `dead_2016_2019'

gen death_date = date(DODSDAT, "YMD")
format death_date %td
gen year = year(death_date)
keep if year >= 2016
keep id year death_date cause_of_death
generate dead_any_cause = 1
save "data\temp\outcome_dead_any_cause.dta", replace

// -
// ANTIDEPRESSANTS
odbc load, exec("select lopnr as id, ATC, EDATUM from SoS_T_R_LMED_ENGANG WHERE ATC IN ('N06A')")dsn("P1105") clear

tempfile lmed
save `lmed'

odbc load, exec("select lopnr as id, ATC, EDATUM from SoS_R_LMED_KLEV4 WHERE ATC IN ('N06A')")dsn("P1105") clear

append using `lmed'

generate year = year(EDATUM)
drop EDATUM
keep if year >= 2015

generate antidepressants = 1

keep id year antidepressants
duplicates drop

// Ensure we only count those who did not use during the year before
reshape wide antidepressants, i(id) j(year)
gen new_antidep2021=1 if antidepressants2021==1 & missing(antidepressants2020)
gen new_antidep2020=1 if antidepressants2020==1 & missing(antidepressants2019)
gen new_antidep2019=1 if antidepressants2019==1 & missing(antidepressants2018)
gen new_antidep2018=1 if antidepressants2018==1 & missing(antidepressants2017)
gen new_antidep2017=1 if antidepressants2017==1 & missing(antidepressants2016)
gen new_antidep2016=1 if antidepressants2016==1 & missing(antidepressants2015)

drop antidepressants*
reshape long new_antidep, i(id) j(year)
rename new_antidep antidepressants
drop if missing(antidepressants)
duplicates drop
save "data\temp\outcome_antidepressants.dta", replace

// -
// New sedative use
odbc load, exec("select lopnr as id, ATC, EDATUM from SoS_T_R_LMED_ENGANG WHERE ATC IN ('N05B', 'N05C')")dsn("P1105") clear

tempfile lmed_s
save `lmed_s'

odbc load, exec("select lopnr as id, ATC, EDATUM from SoS_R_LMED_KLEV4 WHERE ATC IN ('N05B', 'N05C')")dsn("P1105") clear

append using `lmed_s'

generate year = year(EDATUM)
drop EDATUM
keep if year >= 2015

generate sedatives = 1
keep id year sedatives
duplicates drop

// Ensure we only count those who did not use during the year before
reshape wide sedatives, i(id) j(year)
gen new_sed2021=1 if sedatives2021==1 & missing(sedatives2020)
gen new_sed2020=1 if sedatives2020==1 & missing(sedatives2019)
gen new_sed2019=1 if sedatives2019==1 & missing(sedatives2018)
gen new_sed2018=1 if sedatives2018==1 & missing(sedatives2017)
gen new_sed2017=1 if sedatives2017==1 & missing(sedatives2016)
gen new_sed2016=1 if sedatives2016==1 & missing(sedatives2015)

drop sedatives*
reshape long new_sed, i(id) j(year)
rename new_sed sedatives
drop if missing(sedatives)
duplicates drop
save "data\temp\outcome_sedatives.dta", replace

// -
// Calls to 1177 about mental health problems
odbc load, exec("SELECT p1105_lopnr_personnr AS id, contactreason, documentcreatedtime FROM Inera_VPTU_Coronadata")dsn("P1105") clear lowercase

* Include: Confusion, Worry, Sadness, Sleeping difficulties
keep if (strpos(contactreason,"rvirring")==3|strpos(contactreason,"Oro")==1|strpos(contactreason,"Nedst")==1|strpos(contactreason,"mnbesv")==3)

gen year=substr(documentcreatedtime, 1,4)
destring year, replace
drop if year==2021 // We only have full-year data for 2019 and 2020
keep id year
duplicates drop 
gen psych_1177 = 1
save "data\temp\outcome_psych_1177.dta", replace

// -
// Visits to open care psychiatry
odbc load, exec("SELECT DISTINCT lopnr AS id, AR AS year FROM SoS_TTTT_R_PAR_OV_ENGANG WHERE MVO > 900 AND MVO != 999 UNION ALL SELECT DISTINCT lopnr AS id, AR AS year FROM SoS_R_PAR_OV_KLEV4 WHERE MVO > 900 AND MVO != 999")dsn("P1105") clear
generate psych_outp = 1
save "data\temp\outcome_psych_outp.dta", replace

// -
// Hospitalizations for common psychiatric disorders.
odbc load, exec("SELECT DISTINCT lopnr AS id, AR AS year FROM SoS_TTTT_R_PAR_SV_ENGANG WHERE hdia LIKE 'F1%' OR hdia LIKE 'F2%' OR hdia LIKE 'F3%' OR hdia LIKE 'F6%' UNION ALL SELECT DISTINCT lopnr AS id, AR AS year FROM SoS_R_PAR_SV_KLEV4 WHERE hdia LIKE 'F1%' OR hdia LIKE 'F2%' OR hdia LIKE 'F3%' OR hdia LIKE 'F6%'")dsn("P1105") clear
generate psych_hosp = 1 
save "data\temp\outcome_psych_hosp.dta", replace

// -
// Suicides or deaths from injury or poisoning with unclear intent
odbc load, exec("SELECT lopnr as id, AR as year from SoS_R_DORS_ENGANG WHERE ULORSAK LIKE 'Y1%' OR ULORSAK LIKE 'Y2%' OR ULORSAK LIKE 'X6%' OR ULORSAK LIKE 'X7%' OR ULORSAK IN ('Y30','Y31','Y32','Y33','Y34','X80','X81','X82','X83','X84') UNION ALL SELECT lopnr as id, AR as year from SoS_R_DORS_KLEV4 WHERE ULORSAK LIKE 'Y1%' OR ULORSAK LIKE 'Y2%' OR ULORSAK LIKE 'X6%' OR ULORSAK LIKE 'X7%' OR ULORSAK IN ('Y30','Y31','Y32','Y33','Y34','X80','X81','X82','X83','X84')")dsn("P1105") clear 
generate psych_death = 1
save "data\temp\outcome_psych_death.dta", replace

// -
// Surgeries

// In-patient care
odbc load, exec("select * from SoS_R_PAR_SV_KLEV4")dsn("P1105") clear
keep kva_typ* op* OPD* INDATUMA lopnr
tempfile sv
save `sv'
odbc load, exec("select * from SoS_TTTT_R_PAR_SV_ENGANG")dsn("P1105") clear
keep kva_typ* op* OPD* INDATUMA lopnr
tostring INDATUMA, replace
append using `sv'

rename lopnr id
gen n=_n
reshape long kva_typ op OPD, i(n)
keep if kva_typ=="K" // Keep only surgeries

rename OPD surgery_date
generate year=year(surgery_date)
keep if year >= 2016 & year <= 2021

keep id year surgery_date
duplicates drop

save `sv', replace

// Outpatient care
odbc load, exec("select * from SoS_R_PAR_OV_KLEV4")dsn("P1105") clear
keep kva_typ* AR lopnr INDATUMA
tempfile ov
save `ov'

odbc load, exec("select * from SoS_TTTT_R_PAR_OV_ENGANG")dsn("P1105") clear
keep kva_typ* AR lopnr INDATUMA
tostring INDATUMA, replace
append using `ov'

rename (lopnr AR) (id year)
keep if year >= 2016 & year <= 2021

generate surgery_date = date(INDATUMA, "YMD")
format surgery_date %td
drop INDATUMA

generate double n=_n
reshape long kva_typ, i(n)
keep if kva_typ=="K" // Keep only surgeries

keep id year surgery_date

append using `sv'
generate surgery = 1

//duplicates drop id year, force // keep multiple surgeries for now to calculate survival matched to each surgery
save "data\temp\outcome_surgeries.dta", replace

// Surgery survival rate (non-covid deaths)
use "data\temp\outcome_dead_any_cause.dta", clear
gen died_from_covid = 0
replace died_from_covid = 1 if inlist(cause_of_death, "U071", "U072")
keep id death_date died_from_covid

// Merge deaths to all surgieries
merge 1:m id using "data\temp\outcome_surgeries.dta"
keep if surgery==1
gen survived_days = death_date - surgery_date
replace survived_days = . if survived_days < 0

generate surg_dead_30d = 0
replace surg_dead_30d = 1 if survived_days <= 30 & !missing(survived_days)
replace surg_dead_30d = . if died_from_covid == 1 // Set COVID deaths to missing

collapse (max) surg_dead_30d, by(id year surgery)
save "data\temp\outcome_surgeries.dta", replace

// -
// DISPOSABLE INCOME

forval i=2015/2021 {
	odbc load, exec("select P1105_LopNr_PersonNr as id, DispInk04 as dispinc from Individ_`i'")dsn("P1105") clear
	generate year=`i'
	destring dispinc, replace
	tempfile dispinc`i'
	save `dispinc`i''
}
forval i=2015/2020 {
	append using `dispinc`i''
}

duplicates drop id year, force
reshape wide dispinc, i(id) j(year)

gen R2016=dispinc2016/dispinc2015
gen R2017=dispinc2017/dispinc2016
gen R2018=dispinc2018/dispinc2017
gen R2019=dispinc2019/dispinc2018
gen R2020=dispinc2020/dispinc2019
gen R2021=dispinc2021/dispinc2020

keep R* id

reshape long R, i(id) j(year)

gen dispinc_drop = 0 if !missing(R)
replace dispinc_drop = 1 if R < (1 - (1 / 12))

save "data\temp\outcome_disposable_income.dta", replace

//-
// UNEMPLOYMENT

// Arbetsförmedlingen data is not available for 2021
odbc load, exec("select P1105_LopNr_PersonNr as id, INTR_DAT from AMS_INSPER")dsn("P1105") clear
generate year = year(INTR_DAT)
keep if year > 2015
keep id year
duplicates drop
/*
// LISA Data for unemployment can't identify start of spells. Difficult to classify _new_ unemployment. (but results are similar)
forval i=2015/2021 {
	odbc load, exec("select P1105_LopNr_PersonNr as id, ALKod as unemp_code from Individ_`i'")dsn("P1105") clear
	generate year=`i'
	destring unemp_code, replace
	keep if unemp_code > 0 & !missing(unemp_code)
	drop unemp_code
	tempfile unemp`i'
	save `unemp`i''
}
forval i=2015/2020 {
	append using `unemp`i''
}
duplicates drop id year, force
*/
generate unemployed = 1
save "data\temp\outcome_unemployment.dta", replace

//-
// CANCER
// Cancer data is not available for 2021
odbc load, exec("select lopnr as id, AR as year, ICDO10, DIADAT from SoS_T_R_CAN")dsn("P1105") clear
keep if year>2015
duplicates drop
gen diagnose_date = date(DIADAT, "YMD")
format diagnose_date %td
generate cancer = 1
save "data\temp\outcome_cancer.dta", replace

// 1-year cancer survival
odbc load, exec("select * from SoS_R_DORS_ENGANG")dsn("P1105") clear
keep lopnr DODSDAT ULORSAK MORSAK*
tostring DODSDAT, replace
gen death_date = date(DODSDAT, "YMD" )
format death_date %td

tempfile dead_2016_2019
save `dead_2016_2019'
odbc load, exec("select * from SoS_R_DORS_KLEV4")dsn("P1105") clear
keep lopnr DODSDAT ULORSAK MORSAK*
gen death_date = date(DODSDAT, "YMD" )
format death_date %td
append using `dead_2016_2019'

rename (lopnr) (id)

// Reduce compexity of causes
replace ULORSAK = substr(ULORSAK,1,3)
forval i=1/48 {
	replace MORSAK`i' = substr(MORSAK`i',1,3)
}
// Drop empty variablaes
drop MORSAK26 MORSAK27 MORSAK28 MORSAK29 MORSAK30 MORSAK31 MORSAK32 MORSAK33 MORSAK34 MORSAK35 MORSAK36 MORSAK37 MORSAK38 MORSAK39 MORSAK40 MORSAK41 MORSAK42 MORSAK43 MORSAK44 MORSAK45 MORSAK46 MORSAK47 MORSAK48 

merge 1:m id using "data\temp\outcome_cancer.dta"
keep if cancer==1

gen match = 0
replace match = 1 if ICDO10 == ULORSAK
forval i=1/25 {
	replace match = 1 if ICDO10==MORSAK`i'
}

gen cancer_survival_days = death_date - diagnose_date
replace cancer_survival_days = . if cancer_survival_days < 0

gen cancer_dead_365d = 0
replace cancer_dead_365d = 1 if cancer_survival_days <= 365 & !missing(cancer_survival_days) & match == 1
replace cancer_dead_365d = . if cancer_survival_days <= 365 & !missing(cancer_survival_days) & match == 0

collapse (max) cancer_dead_365d, by(id year cancer)
save "data\temp\outcome_cancer.dta", replace
