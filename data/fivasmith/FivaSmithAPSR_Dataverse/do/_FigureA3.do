use dta/FivaSmith_Jan2018.dta, clear   /* use FivaSmith data set because need to include non-main candidates */
keep if year>1944

gen main=0
replace main=1 if party=="nkp"|party=="sv"|party=="dna"|party=="sp"|party=="v"|party=="krf"|party=="h"|party=="frp"

gen zz=1
egen NoC=sum(zz), by(year)

gen yy=1
replace yy=0 if main==0
egen NoC_main=sum(yy), by(year)

collapse NoC NoC_main, by(year)
twoway (scatter NoC year, connect(l) msize(medium) mlwidth(thin) mcolor(black)) (scatter NoC_main year, connect(l) msize(medium) mlwidth(thin) mcolor(gray)), plotregion(lcolor(white) ilcolor(white)) graphregion(fcolor(white)) scheme(s1mono) ///
xtitle(Election year) ytitle("Number of candidates") yscale(range(0 4000)) ylabel(0(1000)4000)  legend(order(1 "No. of candidates (all lists)" 2 "No. of candidates (main parties)") pos(5) ring(0) col(1)) xlabel(1945(4)2013, angle(forty_five)) 
graph export figures/FigureA3.eps, replace
