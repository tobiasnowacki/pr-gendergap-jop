*use dta\DynastyPanel_Early.dta, clear
use dta\DynastyPanel.dta, replace
*bysort pid: egen terms_served=sum(seat) 

*egen terms_served2=sum(seat), by(id)

keep if year>1944 & year<2000
*keep if year>1950 & year<1983
keep if districtid!=.
*keep if main==1

keep if seat==1
*** in total we have 1215 obs. (roughly 152 obs per yer)
tab party, gen(d_party)

gen first_districtid=districtid
replace first_district=0 if year<1945

collapse max_mpcabinet last_year mpsucceed female isic* d_party* first_year (max) first_districtid mpprecede mppartyyouth mppartyyouthleader terms_served (min) rank, by(pid)

drop if last_year>2000 /* drop all MPs running after 1997 , new restriction imposed october 2017 */
tab first_year
tab terms_served
tab first_distr
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

estimates clear
reg mpprecede TermsServed, robust
eststo
estadd local YearFE "No"
estadd local PartyFE "No"
estadd local DistrictFE "No"
reg mpprecede FirstRanked, robust
eststo
estadd local YearFE "No"
estadd local PartyFE "No"
estadd local DistrictFE "No"
reg mpprecede CabinetExperience, robust
eststo
estadd local YearFE "No"
estadd local PartyFE "No"
estadd local DistrictFE "No"
reg mpprecede FirstRanked TermsServed CabinetExperience, robust
eststo
estadd local YearFE "No"
estadd local PartyFE "No"
estadd local DistrictFE "No"
reg mpprecede FirstRanked TermsServed CabinetExperience d_first_year* d_party* d_first_districtid* , robust
eststo
estadd local YearFE "Yes"
estadd local PartyFE "Yes"
estadd local DistrictFE "Yes"
esttab using tables/TableA1.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f)  ///
drop(*d_*) nomtitles nonotes stats(N r2 YearFE PartyFE DistrictFE, labels ("N" "r2" "FirstYear FE" "PartyFE" "DistrictFE")) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  
