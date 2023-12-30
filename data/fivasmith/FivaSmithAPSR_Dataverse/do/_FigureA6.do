use dta/DynastyPanel.dta, clear

keep if districtid!=.
keep if year>1950 & year<1983

****************
** TRUNCATING **
****************
replace margin=-0.25 if margin<-0.25 & margin!=.
replace margin=0.25 if margin>0.25 & margin!=.
***************

twoway (hist margin if margin<0, width(0.005) start(-0.25) freq bfcolor(gs13) blcolor(gs6)) (hist margin if margin>0, width(0.005) start(0) freq bfcolor(gs8) blcolor(gs6)), ///
plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) xlabel(-0.25(0.05)0.25) ///
legend(label(1 "Loser") label(2 "Winner") pos(3) ring(0) col(1)) xtitle(Win margin)
graph export figures/FigureA6.eps, replace
