use dta/DynastyPanel.dta, clear

keep if districtid!=.
keep if year>1950 & year<1983

******************************************************************
************************** MCCRARY PLOTS**************************
******************************************************************

DCdensity margin, breakpoint(0) generate(Xj Yj r0 fhat se_fhat) 
graph save figures\gph\McCrary_full.gph, replace

drop Xj Yj r0 fhat se_fhat
preserve
drop if margin==.
drop if abs(margin)>0.05
DCdensity margin if abs(margin)<0.05, breakpoint(0) generate(Xj Yj r0 fhat se_fhat)
graph save figures\gph\McCrary_5pp.gph, replace

cd figures\gph\
graph combine McCrary_full.gph McCrary_5pp.gph, cols(1) row(2) plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono)
cd ..\..\
*graph play McCrary
*graph play McCraryAxes
graph export figures\FigureA7.eps, replace
restore
