use "dta/USNorway.dta", clear

collapse junior, by(country year)
replace junior = junior*100

twoway (connected junior year if country == "Norway", mcolor(black) msymbol(circle) lcolor(black)) ///
(connected junior year if country == "United States", mcolor(white) msymbol(circle) mfcolor(white) mlcolor(black) lcolor(black) lpattern(dash)), ///
ytitle(Percent) ylabel(0(5)15, angle(horizontal) gmin gmax) xtitle(Year) xlabel(1945(5)2015, angle(forty_five)) ///
legend(order(1 "Norway" 2 "United States") cols(1) position(11) ring(0)) plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) 

graph export figures/Figure1.eps, replace 
