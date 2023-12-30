use dta/DynastyPanel_Early.dta, clear

collapse (sum) seat (mean) parliament, by(year party)

*drop if seat==0
*egen seatmain=sum(seat), by(year)
*replace seat=parliament-seatmain if party=="oth"
gen seatsh=seat/parliament

replace party=upper(party)
replace party="KrF" if party=="KRF"
replace party="FrP" if party=="FRP"

foreach party in NKP SV DNA SP V KrF H FrP {
scatter seatsh year if party=="`party'", connect(l) lwidth(medium) mcolor(black) msymbol(circle) lcolor(black) ytitle("Fraction") xtitle("Election year") title("`party'") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) ylabel(0(0.1)0.6) ymtick(0(0.1)0.6)
graph save figures/gph/_`party'.gph, replace
}

cd figures/gph/
graph combine _NKP.gph _SV.gph _DNA.gph _SP.gph _V.gph _KrF.gph _H.gph _FrP.gph, ycommon xcommon plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono)
cd ../../
graph export figures/FigureA1.eps, replace

