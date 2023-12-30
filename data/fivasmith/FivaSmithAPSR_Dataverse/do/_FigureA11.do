use dta/DynastyPanel, clear

keep if year>1950 & year<1983
keep if districtid!=.
keep if abs(margin)<0.05

gen higher_rank=0
replace higher_rank=1 if rank>rank_next1
replace higher_rank=0 if rank_next1==. /*not running*/

gen top_rank=0
replace top_rank=1 if rank==1

gen top_rank_next1=0
replace top_rank_next1=1 if rank_next1==1
replace top_rank_next1=0 if rank_next1==. /*not running*/

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

foreach var in higher_rank top_rank_next1 {
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

foreach var in higher_rank { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  title("Higher rank in Election t+1") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.5)) ylabel(0(0.1)0.5) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}


foreach var in top_rank_next1 { 
*** plot
twoway (rarea upfit`var' downfit`var' margin if margin<0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) (rarea upfit`var' downfit`var' margin if margin>0, sort fcolor(gs12*0.6) lcolor(gs12*0.6)) ///
(line fit`var' margin if margin<0, sort lcolor(black) lwidth(medium)) (line fit_`var' margin if margin>0, sort lcolor(black) lwidth(medium)) (scatter mean`var' midbin, msize(medium) mlwidth(thin) mcolor(black) msymbol(circle_hollow)) , ///
 ytitle("Fraction") xtitle("Win margin (t)") xline(0, lcolor(black) lwidth(thin)) legend(off)  title("Top rank in Election t+1") plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) yscale(range(0 0.5)) ylabel(0(0.1)0.5) xlabel(-0.05(0.02)0.05)
 graph save figures/gph/_`var'.gph, replace
}
 
 cd figures/gph/
 graph combine _higher_rank.gph _top_rank_next1.gph, plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) 
 graph export ../FigureA11.eps, replace
 cd ../../
