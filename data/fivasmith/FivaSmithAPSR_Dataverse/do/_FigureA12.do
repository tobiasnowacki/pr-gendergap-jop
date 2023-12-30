***********************************************************************************************
***********************************************************************************************
***********************************************************************************************
*********************** SPECIFICATION WITH CONTROLS *******************************************
***********************************************************************************************
***********************************************************************************************
***********************************************************************************************

use dta/DynastyPanel, clear

keep if year>1950 & year<1983
keep if districtid!=.
keep if main==1

replace seat_prev4=. if year<1969
replace seat_prev3=. if year<1965
replace seat_prev2=. if year<1961
replace seat_prev1=. if year<1957

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

***********************************************************************************************
***********************************************************************************************
***********************************************************************************************

postfile dataset str10 (depvar) BW pointest lower upper using estimates/bw_controls.dta, replace

foreach depvar in seat_next1 cand_next1 ever_seat terms_served mpprecede{
	forvalues BW=0.01(0.005)0.25 {
	reg `depvar' seat margin seatXmargin d_year* d_party* d_district* d_rank* female isic* if abs(margin)<`BW', vce(cluster pid)
	post dataset ("`depvar'") (`BW') (_b[seat]) (_b[seat]-1.96*_se[seat]) (_b[seat]+1.96*_se[seat])
	}
}
postclose dataset

use estimates/bw_controls.dta, clear
keep if BW<0.155
keep if BW>0.005
label var BW "Bandwidth"

preserve
keep if depvar=="cand_next1"
twoway (line lower BW , lwidth(thin) lpattern(dash)) (line upper BW , lwidth(thin) lpattern(dash)) (line pointest BW, lpattern(solid) lwidth(thick)), ///
ytitle(Estimate) yscale(range(0.6 0.6)) ylabel(-0.6(0.2)0.6)  xline(0.038, lpattern(shortdash) lcolor(gray*0.75)) xline(0.074, lpattern(shortdash) lcolor(gray*0.75)) //////
legend(off) subtitle(Candidate running in election t+1)  plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono)
graph save figures/gph/_cand_next1_controls.gph, replace
restore

preserve
keep if depvar=="seat_next1"
twoway (line lower BW , lwidth(thin) lpattern(dash)) (line upper BW , lwidth(thin) lpattern(dash)) (line pointest BW, lpattern(solid) lwidth(thick)), ///
ytitle(Estimate) yscale(range(0.6 0.6)) ylabel(-0.6(0.2)0.6) xline(0.049, lpattern(shortdash) lcolor(gray*0.75)) xline(0.098, lpattern(shortdash) lcolor(gray*0.75)) //////
legend(off) subtitle(Candidate winning seat in election t+1)  plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono)
graph save figures/gph/_seat_next1_controls.gph, replace
restore

preserve
keep if depvar=="terms_serv"
twoway (line lower BW , lwidth(thin) lpattern(dash)) (line upper BW , lwidth(thin) lpattern(dash)) (line pointest BW, lpattern(solid) lwidth(thick)), ///
ytitle(Estimate) yscale(range(-3 3)) ylabel(-3(1)3)  xline(0.046, lpattern(shortdash) lcolor(gray*0.75)) xline(0.092, lpattern(shortdash) lcolor(gray*0.75)) //////
legend(off) subtitle(Terms served)  plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono)
graph save figures/gph/_terms_served_controls.gph, replace
restore

preserve
keep if depvar=="mpprecede"
twoway (line lower BW , lwidth(thin) lpattern(dash)) (line upper BW , lwidth(thin) lpattern(dash)) (line pointest BW, lpattern(solid) lwidth(thick)), ///
ytitle(Estimate) yscale(range(-0.15 0.15)) ylabel(-0.15(0.05)0.15) xline(0.066, lpattern(shortdash) lcolor(gray*0.75)) xline(0.132, lpattern(shortdash) lcolor(gray*0.75)) ///
legend(off) subtitle(Family member winning future seat)  plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono)
graph save figures/gph/_mpprecede_controls.gph, replace
restore

cd figures/gph

graph combine _cand_next1_controls.gph _seat_next1_controls.gph _terms_served_controls.gph _mpprecede_controls.gph, plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono)
graph export ../FigureA12.eps, replace
cd ../../
