*********************************************************************
****************   mpprecede TRENDS *********************************
*********************************************************************
use dta/DynastyPanel_Early.dta, clear

keep if districtid!=.
drop if year<1945
** drop if margin==. & seat==0  /* drop all hopeless candidates  -- feb 2017 version */
keep if seat==1  /* drop all hopeless candidates  and all marginal candidates -- oct 2017 version */

collapse mpprecede, by(year)

twoway (connected mpprecede year, mcolor(black) msymbol(circle) lcolor(black)), ytitle("") ///
ylabel(0(.02).08) ytitle(Fraction) xtitle("Election year") xlabel(1945(4)2013, angle(forty_five)) plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono)
graph export figures/FigureA4.eps, replace
