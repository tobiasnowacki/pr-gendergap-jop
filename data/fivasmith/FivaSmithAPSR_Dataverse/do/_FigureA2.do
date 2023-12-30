*********************************************************************
****************   mpsucceed TRENDS *********************************
*********************************************************************
use dta/DynastyPanel_Early.dta, clear

keep if districtid!=.
drop if year<1945
***drop if margin==. & seat==0  /* all hopeless candidates */
keep if seat==1

replace party=upper(party)

cd figures/gph
replace party=upper(party)
replace party="KrF" if party=="KRF"
replace party="FrP" if party=="FRP"

foreach party in NKP SV DNA SP V KrF H FrP {
preserve
keep if party=="`party'"

collapse mpsucceed, by(year)

twoway (connected mpsucceed year, mcolor(black) msymbol(circle) lcolor(black)), ytitle("") ///
title(`party') ytitle("Fraction") xtitle("Election year") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono)
graph save _`party'.gph, replace
restore
}

graph combine _NKP.gph _SV.gph _DNA.gph _SP.gph _V.gph _KrF.gph _H.gph _FrP.gph, xcommon ycommon plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono)
graph export ../FigureA2.eps, replace
cd ../../
