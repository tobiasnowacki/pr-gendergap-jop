use dta\DynastyPanel.dta, clear

keep if year>1950 & year<1983
keep if districtid!=.

sum if lastRank<3000
binscatter dynasty_cand_ever lastRank if lastRank<3000, xline(100, lcolor(gray) lpattern(dash)) xline(1000, lcolor(gray) lpattern(dash)) ///
plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) ytitle("Fraction") title("") nq(50) xtitle(Last name rank) linetype(none) mcolor(gray)  xlabel(100 1000 2000 3000)
graph export figures/FigureB1.eps, replace
