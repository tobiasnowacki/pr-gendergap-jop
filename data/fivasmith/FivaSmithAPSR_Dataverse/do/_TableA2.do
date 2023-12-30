use dta\DynastyPanel.dta, replace
keep if year>1944 & year<2000
keep if districtid!=.
keep if seat==1
tab party, gen(d_party)
gen first_districtid=districtid
replace first_district=0 if year<1945
collapse max_mpcabinet last_year mpsucceed female isic* d_party* first_year (max) first_districtid mpprecede mppartyyouth mppartyyouthleader terms_served (min) rank, by(pid)
drop if last_year>2000 /* drop all MPs running after 1997 , new restriction imposed october 2017 */
rename rank min_rank
tab min_rank

*** after collapse we have unique 519 obs. 
bysort max_mpcabinet: tab mpprecede
bysort min_rank: tab mpprecede

tab first_year, gen(d_first_year)
tab first_districtid, gen(d_first_districtid)

gen FirstRanked=0
replace FirstRanked=1 if min_rank==1

gen CabinetExperience=max_mpcabinet
gen TermsServed=terms_served

sutex mpprecede mpsuc FirstRanked TermsServed CabinetExperience first_year d_party* ///
, minmax title(Descriptive statistics for OLS sample \label{DescriptivesOLS}) ///
file(tables/TableA2.tex) replace

