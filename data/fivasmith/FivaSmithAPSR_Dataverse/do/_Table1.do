use dta/DynastyPanel, clear

keep if year>1950 & year<1983
keep if districtid!=.

************************************************************************************************************

gen yearXdistrict=year*district
gen seatXmargin=seat*margin
gen margin2=margin^2
gen seatXmargin2=seat*margin2

tab rank, gen(d_rank)
tab party, gen(d_party)
tab year, gen(d_year)
tab districtid, gen(d_districtid)

drop d_party1  /* DNA IS REFERENCE CATEGORY */
drop d_year1 /* 1953 is REF CAT */
drop d_rank1 /* rank1 is ref cat */

*** FINDING OPTIMAL BW***
foreach var in cand_next1 seat_next1 terms_served mpprecede{
rdrobust `var' margin, p(1) kernel(uniform)
}

gen bw_cand_next1=0.038
gen bw_seat_next1=0.049
gen bw_terms_served=0.046
gen bw_mpprecede=0.066

******************************
******************************

rename seat RDestimate

foreach var in seat_next1 cand_next1 terms_served mpprecede {
estimates clear
reg `var' RDestimate margin seatXmargin  if abs(margin)<bw_`var', cluster(pid)
eststo
reg `var' RDestimate margin seatXmargin d_year*  if abs(margin)<bw_`var', cluster(pid)
eststo
reg `var' RDestimate margin seatXmargin d_year* d_party* if abs(margin)<bw_`var', cluster(pid)
eststo
reg `var' RDestimate margin seatXmargin d_year* d_party* d_district* if abs(margin)<bw_`var', cluster(pid)
eststo
reg `var' RDestimate margin seatXmargin d_year* d_party* d_district* d_rank* if abs(margin)<bw_`var', cluster(pid)
eststo
reg `var' RDestimate margin seatXmargin d_year* d_party* d_district* d_rank* female isic*  if abs(margin)<bw_`var', cluster(pid)
eststo
esttab using tables/Table1_`var'.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f) coeflabels(RDestimate "RD estimate") ///
drop(*margin* _cons *year* female* *isic*  *d_party* *district* *rank*) nomtitles nonotes ///
stats(r2 N BW, labels ("R$^2$" "N" "BW" ) fmt(3 0 3) ) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  
}
