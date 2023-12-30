use dta/DynastyPanel, clear

keep if year>1950 & year<1983
keep if districtid!=.
keep if abs(margin)<0.5  /* all candidates that are marginal */

gen higher_rank=0
replace higher_rank=1 if rank>rank_next1
replace higher_rank=0 if rank_next1==. /*not running*/

gen top_rank=0
replace top_rank=1 if rank==1

gen top_rank_next1=0
replace top_rank_next1=1 if rank_next1==1
replace top_rank_next1=0 if rank_next1==. /*not running*/

tab party, gen(d_party)

sutex candrun candwin first_year last_year rank top_rank margin ///
seat deputy seat_next1 cand_next1 higher_rank top_rank_next1 terms_served mpprecede mpsucceed female isic1-isic10 isic0 d_party* ///
, minmax title(Descriptive statistics for RD sample \label{DescriptivesRD}) ///
file(tables/TableA3.tex) replace
