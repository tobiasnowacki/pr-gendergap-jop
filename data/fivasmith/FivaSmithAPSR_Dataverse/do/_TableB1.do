use dta\DynastyPanel.dta, clear

keep if year>1950 & year<1983
keep if districtid!=.

foreach var in cand seat {
rdrobust dynasty_`var'_ever margin if lastRank>100, p(1) kernel(uniform)
}

gen bw_cand=0.040
gen bw_seat=0.050

**********************************************************************************************************************
**********************************************************************************************************************
**********************************************************************************************************************

/* REGRESSION ANALYSIS */

gen yearXdistrict=year*district
gen seatXmargin=seat*margin
gen margin2=margin^2
gen seatXmargin2=seat*margin2

tab rank, gen(d_rank)
tab party, gen(d_party)
tab year, gen(d_year)
tab districtid, gen(d_districtid)

drop d_party1  /* DNA IS REFERENCE CATEGORY */
drop isic6 /* FARMER IS REFERENCE CATEGORY */
drop d_year1 /* 1953 is REF CAT */
drop d_rank1 /* rank1 is ref cat */

/* DISCONTINUITY ANALYSIS */

*foreach i in 0 100 1000 3388 {
foreach i in 100  {
preserve
rename seat RDestimate
keep if lastRank>`i'
foreach var in cand seat {
estimates clear
reg dynasty_`var'_ever RDestimate margin seatXmargin  if abs(margin)<bw_`var', cluster(pid)
eststo
estadd local YearFE "No"
estadd local PartyFE "No"
estadd local DistrictFE "No"
estadd local RankFE "No"
estadd local Controls "No"
reg dynasty_`var'_ever RDestimate margin seatXmargin d_year* if abs(margin)<bw_`var', cluster(pid)
eststo
estadd local YearFE "Yes"
estadd local PartyFE "No"
estadd local DistrictFE "No"
estadd local RankFE "No"
estadd local Controls "No"
reg dynasty_`var'_ever RDestimate margin seatXmargin d_year* d_party* if abs(margin)<bw_`var', cluster(pid)
eststo
estadd local YearFE "Yes"
estadd local PartyFE "Yes"
estadd local DistrictFE "No"
estadd local RankFE "No"
estadd local Controls "No"
reg dynasty_`var'_ever RDestimate margin seatXmargin d_year* d_party* d_district* if abs(margin)<bw_`var', cluster(pid)
eststo
estadd local YearFE "Yes"
estadd local PartyFE "Yes"
estadd local DistrictFE "Yes"
estadd local RankFE "No"
estadd local Controls "No"
reg dynasty_`var'_ever RDestimate margin seatXmargin d_year* d_party* d_district* d_rank* if abs(margin)<bw_`var', cluster(pid)
eststo
estadd local YearFE "Yes"
estadd local PartyFE "Yes"
estadd local DistrictFE "Yes"
estadd local RankFE "Yes"
estadd local Controls "No"
reg dynasty_`var'_ever RDestimate margin seatXmargin d_year* d_party* d_district* d_rank* isic* female  if abs(margin)<bw_`var', cluster(pid)
eststo
estadd local YearFE "Yes"
estadd local PartyFE "Yes"
estadd local DistrictFE "Yes"
estadd local RankFE "Yes"
estadd local Controls "Yes"
esttab using tables/TableB1_`var'.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f) ///
drop(*margin* _cons *year* female*  *isic* *d_party* *district* *rank*) nomtitles nonotes ///
stats(r2 N BW YearFE PartyFE DistrictFE RankFE Controls, labels ("R$^2$" "N" "BW" "Year FE" "Party FE" "District FE" "Rank FE" "Controls") fmt(3 0)) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  
}
}

