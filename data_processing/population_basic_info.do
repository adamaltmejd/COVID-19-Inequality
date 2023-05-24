// REGION OF BIRTH
odbc load, exec("select * from Fodelseuppg_20211231")dsn("P1105") clear

// Fix encoding issues
capture erase "bak.stunicode/countries_and_regions.dta"
capture erase "bak.stunicode/status.stunicode/countries_and_regions.dta.t"
capture erase "countries_and_regions.dta"
save "countries_and_regions.dta"
clear
unicode encoding set latin1
unicode translate "countries_and_regions.dta"
use "countries_and_regions.dta", clear
erase "countries_and_regions.dta"

// Cleanup duplicates
duplicates tag P1105_LopNr_PersonNr, gen(dup)
destring FodelseLan, replace
sort P1105_LopNr_PersonNr Fodelseland FodelseLan
egen hjalp=group(P1105_LopNr_PersonNr Fodelseland)
duplicates tag P1105_LopNr_PersonNr hjalp, gen(dup2)
duplicates drop P1105_LopNr_PersonNr if dup2==1, force
gen hjalp2=1 if dup==1 & Fodelseland!="SVERIGE"
replace Fodelseland="ÖÖ" if Fodelseland=="" & dup==1
sort P1105_LopNr_PersonNr hjalp2 Fodelseland 
duplicates drop P1105_LopNr_PersonNr, force
replace Fodelseland="SVERIGE" if Fodelseland=="ÖÖ"
keep P1105_LopNr_PersonNr Fodelseland
rename (P1105_LopNr_PersonNr Fodelseland) (id RTBLandsNamn)

// Fix incorrect country names
replace RTBLandsNamn = "AZERBAJDJAN" if RTBLandsNamn == "AZERBAJDZJAN" | RTBLandsNamn == "Azerbajdzjan"
replace RTBLandsNamn = "VITRYSSLAND" if RTBLandsNamn == "BELARUS"
replace RTBLandsNamn = "BOSNIEN-HERCEGOVINA" if RTBLandsNamn == "BOSNIEN OCH HERCEGOVINA" | RTBLandsNamn == "Bosnien och Hercegovina"
replace RTBLandsNamn = "BRUNEI DARUSSALAM" if RTBLandsNamn == "BRUNEI"
replace RTBLandsNamn = "KONGO, DEMOKRATISKA REPUBLIKEN" if RTBLandsNamn == "DEMOKRATISKA REPUBLIKEN KONGO" | RTBLandsNamn == "Demokratiska republiken Kongo"
replace RTBLandsNamn = "ARABEMIRATEN, FÖRENADE" if RTBLandsNamn == "FÖRENADE ARABEMIRATEN"
replace RTBLandsNamn = "USA" if RTBLandsNamn == "FÖRENTA STATERNA"
replace RTBLandsNamn = "SWAZILAND" if RTBLandsNamn == "ESWATINI"
replace RTBLandsNamn = "YEMEN" if RTBLandsNamn == "JEMEN"
replace RTBLandsNamn = "COMORERNA" if RTBLandsNamn == "KOMORERNA"
replace RTBLandsNamn = "KOREA, NORD-" if RTBLandsNamn == "NORDKOREA" | RTBLandsNamn == "Nordkorea"
replace RTBLandsNamn = "KOREA, SYD-" if RTBLandsNamn == "SYDKOREA"
replace RTBLandsNamn = "MOCAMBIQUE" if RTBLandsNamn == "MOÇAMBIQUE"
replace RTBLandsNamn = "S:T KITTS OCH NEVIS" if RTBLandsNamn == "SAINT KITTS OCH NEVIS" | RTBLandsNamn == "Saint Kitts och Nevis"
replace RTBLandsNamn = "S:T LUCIA" if RTBLandsNamn == "SAINT LUCIA"
replace RTBLandsNamn = "S:T VINCENT OCH GRENADINERNA" if RTBLandsNamn == "SAINT VINCENT OCH GRENADINERNA"
replace RTBLandsNamn = "SAO TOME OCH PRINCIPE" if RTBLandsNamn == "SÃO TOMÉ OCH PRÍNCIPE"
replace RTBLandsNamn = "TADJIKISTAN" if RTBLandsNamn == "TADZJIKISTAN" | RTBLandsNamn == "Tadzjikistan"
replace RTBLandsNamn = "TAIWAN" if RTBLandsNamn == "TAIWAN, PROVINS AV KINA"

// Unknown
replace RTBLandsNamn = "Missing" if inlist(RTBLandsNamn, "OB", "OKÄNT LAND", "UPPHÖRT LAND", "OKÄNT ÖÖ")
tempfile ind_cob
save `ind_cob'

// Land grouping
odbc load, exec("select * from RTBLand2020_EU27_2020")dsn("P1105") clear

// Fix encoding issues
capture erase "bak.stunicode/world_regions.dta"
capture erase "bak.stunicode/status.stunicode/world_regions.dta.t"
capture erase "world_regions.dta"
save "world_regions.dta"
clear
unicode encoding set latin1
unicode translate "world_regions.dta"
use "world_regions.dta", clear
erase "world_regions.dta"

replace VarldsdelNamn="Missing" if VarldsdelNamn == "Statslös" | VarldsdelNamn == "Okänt" | VarldsdelNamn == "" | missing(VarldsdelNamn)

gen country=0
replace country = 1 if VarldsdelNamn == "Sverige"
replace country = 2 if inlist(VarldsdelNamn, "Norden", "EU utom Norden", "Europa utom EU och Norden", "Norden utom Sverige", "Sovjetunionen")
replace country = 3 if inlist(VarldsdelNamn, "Afrika", "Asien", "Nordamerika", "Oceanien", "Sydamerika")
keep RTBLandsNamn country

// Merge back
merge 1:m RTBLandsNamn using `ind_cob'
drop if _merge==1
drop _merge RTBLandsNamn
replace country = 0 if missing(country)
label define label_country_lab 0 "COB Unknown" 1 "Sweden" 2 "Europe" 3 "Outside of Europe"
label values country label_country

tempfile region_of_birth
save `region_of_birth'

// Get basic info
odbc load, exec("select * from Population_PersonNr_20211231")dsn("P1105") clear

gen birth_year=substr(FodelseArMan,1,4)
destring birth_year, replace

destring Kon, replace
gen male = 0
replace male = 1 if Kon == 1

rename P1105_LopNr_PersonNr id

keep id birth_year male

// Merge with region of birth
merge 1:1 id using `region_of_birth'
drop if _merge==2 // Drop those not in population
drop _merge

// Save
save "data\temp\population_basic_info.dta", replace


