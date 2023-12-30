use dta\DynastyPanel.dta, clear
keep if year>1950 & year<1983
keep if districtid!=.

egen partyseats=sum(seat), by(districtid party year)
gen rankstar=partyseats-rank

twoway (hist rankstar if rankstar<-1 , discrete width(0.5) start(-21) freq bfcolor(white) blcolor(gs6)) (hist rankstar if rankstar==-1 , discrete width(0.5) start(-1) freq bfcolor(gs13) blcolor(gs6) ) (hist rankstar if rankstar==0 , start(0) freq width(0.5)  bfcolor(gs8) blcolor(gs6) discrete) (hist rankstar if rankstar>0 , discrete width(0.5) start(0) freq bfcolor(black) blcolor(gs6) ) , ///
plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) xlabel(-20(2)6) ///
legend(label(1 "Hopeless") label(2 "Marginal losers") label(3 "Marginal winners") label(4 "Safe") pos(10) ring(0) col(1)) xtitle(Rank from marginally elected)
graph export figures/FigureA5.eps, replace
