use dta/DynastyPanel, clear

keep if year>1950 & year<1983
keep if districtid!=.
keep if abs(margin)<0.05

************************************************************************************************************
*rdplot seat_prev1 margin if  abs(margin)<0.05, p(1) nbins(10 10) graph_options(xtitle("Win margin (t)") title(Candidate Winning Seat in Election t + 1) plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) yscale(range(0 1)) ylabel(0(0.1)1) ymtick(0(0.1)1) legend(off)) ci(95)
*rdplot seat_prev1 margin if  abs(margin)<0.05, p(1) graph_options(xtitle("Win margin (t)") title(Candidate Winning Seat in Election t + 1) plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) yscale(range(0 1)) ylabel(0(0.1)1) ymtick(0(0.1)1) legend(off)) ci(95) bandwidth(es)


/* CREATING BINS WITH ROUGHLY EQUAL NUMBER OF OBS */
*xtile bins=margin if margin<0, nq(10)
*xtile bins2=margin if margin>0, nq(10)
*replace bins=bins2+10 if missing(bins)
*drop bins2

gen bins=.
replace bins=1 if margin>-0.050 & margin<-0.045
replace bins=2 if margin>-0.045 & margin<-0.040
replace bins=3 if margin>-0.040 & margin<-0.035
replace bins=4 if margin>-0.035 & margin<-0.030
replace bins=5 if margin>-0.030 & margin<-0.025
replace bins=6 if margin>-0.025 & margin<-0.020
replace bins=7 if margin>-0.020 & margin<-0.015
replace bins=8 if margin>-0.015 & margin<-0.010
replace bins=9 if margin>-0.010 & margin<-0.005
replace bins=10 if margin>-0.005 & margin<-0.000
replace bins=11 if margin>0.000 & margin<0.005
replace bins=12 if margin>0.005 & margin<0.010
replace bins=13 if margin>0.010 & margin<0.015
replace bins=14 if margin>0.015 & margin<0.020
replace bins=15 if margin>0.020 & margin<0.025
replace bins=16 if margin>0.025 & margin<0.030
replace bins=17 if margin>0.030 & margin<0.035
replace bins=18 if margin>0.035 & margin<0.040
replace bins=19 if margin>0.040 & margin<0.045
replace bins=20 if margin>0.045 & margin<0.050
gen midbin=-0.0525+bins*0.005

foreach var in isic1 isic2 isic3 isic4 isic5 isic6 isic7 isic8 isic9 isic10 isic0 {
bys bins: egen mean`var'=mean(`var')

*** 95% CI to the left of the cut-off
reg `var' margin if margin<0, robust
predict fit`var'
predict fitsd`var', stdp
gen upfit`var'=fit`var'+1.96*fitsd`var' if margin<0
gen downfit`var'=fit`var'-1.96*fitsd`var' if margin<0

*** 95% CI to the right of the cut-off
reg `var' margin if margin>0, robust
predict fit_`var'
predict fitsd_`var', stdp
replace upfit`var'=fit_`var'+1.96*fitsd_`var'  if margin>0
replace downfit`var'=fit_`var'-1.96*fitsd_`var'  if margin>0
}

foreach var in isic1 { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  subtitle("Managers") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.4)) ylabel(0(0.1)0.4) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}

foreach var in isic2 { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  subtitle("Professionals") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.4)) ylabel(0(0.1)0.4) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}

foreach var in isic3 { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  subtitle("Technicians") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.4)) ylabel(0(0.1)0.4) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}

foreach var in isic4 { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  subtitle("Clerical support workers") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.4)) ylabel(0(0.1)0.4) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}

foreach var in isic5 { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  subtitle("Service and sales workers") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.4)) ylabel(0(0.1)0.4) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}

foreach var in isic6 { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  subtitle("Agric. and fishery workers") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.4)) ylabel(0(0.1)0.4) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}

foreach var in isic7 { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  subtitle("Craft workers") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.4)) ylabel(0(0.1)0.4) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}

foreach var in isic8 { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  subtitle("Machine oper. and assemblers") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.4)) ylabel(0(0.1)0.4) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}

foreach var in isic9 { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  subtitle("Elementary occupations") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.4)) ylabel(0(0.1)0.4) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}

foreach var in isic10 { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  subtitle("Armed forces occupations") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.4)) ylabel(0(0.1)0.4) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}

foreach var in isic0 { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  subtitle("No occupation") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.4)) ylabel(0(0.1)0.4) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}

cd figures/gph/
graph combine _isic1.gph _isic2.gph _isic3.gph _isic4.gph _isic5.gph _isic6.gph _isic7.gph _isic8.gph _isic9.gph _isic10.gph _isic0.gph, plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) 
graph export ../FigureA15.eps, replace
cd ../../
