/*
Kate Schneider
Replication code for:
	Schneider et al (2022). "Assessing the affordability of nutrient-adequate diets." American Journal of Agricultural Economics. 
Tables and figures Replication Code
Last modified: 8 June 2022
Contact: kschne29@jhu.edu
Created in Stata version 15.1, revised in Stata version 17.0
*/

// Working directory
global myfiles "yourfilepath"
cd "$myfiles"

// Conceptual Framework
use HHNeeds_IndivLevel_r, clear
drop if data_round==1
browse if hhsize==5 & age_sex_grp==16 
sort case_id HHID y2_hhid y3_hhid data_round nutr_no
foreach v in amdr_lower_perweek amdr_upper_perweek EAR_sharing UL_sharing {
	gen `v'pd=`v'/7
	}
ren amdr_lower_perweekpd amdr_lower_pd
ren amdr_upper_perweekpd amdr_upper_pd
egen hhenergy=total(kcal_perday) if nutr_no==2, by(case_id HHID y2_hhid y3_hhid data_round)
sort case_id HHID y2_hhid y3_hhid data_round nutr_no
browse nutr_no age_sex_grp sex age_rptd ageinmonths ///
	lactating ear ul max_ear_perkcal min_ul_perkcal kcal_perday ///
	hhenergy if y3_hhid=="1289-001" & data_round==3

// Figure 1 // Price observations/missing data
	use CPIDataForCoNA, clear
	keep if year>=2013
	drop if inlist(market_no,2,17,22,24)
	keep market_no market region district year month food_no food_item price_lcu ///
		uni_price_mwk uni_price_net rural_urban dateMY
	gen missing=1 if price_lcu==.
	replace missing=0 if price_lcu!=.
	tab food_item, sum(missing)
	labmask food_no, values(food_item)
	labmask market_no, values(market)
	tab food_no, sum(food_no)
		// Generate Season
		gen season=1 if inlist(month,3,4,5,6,7,8)
		replace season=0 if inlist(month,9,10,11,12,1,2)
			lab def seas 1 "Post-Harvest" 0 "Lean"
			lab val season seas
	format dateMY %tmCCYY-NN
	gen date=ym(year,month)
	tab date, nolabel
		format date %tmCCYY-NN
	forval d=636/691 {
		di %tmCCYY-NN `d' " " _c
	}

	collapse (sum) missing, by(food_no dateMY)
	reshape wide missing, i(food_no) j(dateMY)
	ren missing# date#
	export excel using "Figure 1.xlsx", firstrow(varl) ///
		sheet("Sheet1") sheetmodify
	* Tile graph created with Excel conditional formatting

// Supplementary Tables B-1 and B-2 // Relevance of market prices to households 
use MalawiIHPS_DietQualCost_FoodHHExp, clear
tab food_no, nolabel
preserve
	import excel using "MalawiIHPS_Foods+FoodComp", sheet(Market-Household Item Match) firstrow clear
	drop fooditem
	tempfile match
		save `match', replace
restore
keep if inlist(food_no, 101, 103, 104, 106, 111, 112, 113, 201, 203, 205, 301, 302, 303, 304, 308, 401, 402, 403, 404, 405, 408, 409, 410, 411, 501, 504, 505, 506, 508, 601, 602, 603, 605, 606, 607, 701, 702, 801, 803, 810, 827, 5021, 5022, 5023, 5031)
drop if food_no==.
merge m:1 food_no using `match'
drop _merge 
preserve
	use CPIDataForCoNA, clear
	keep market_no year month food_no uni_price_mwk dateMY
	rename food_no CPI_food_no
	tempfile marketprices
		save `marketprices'
restore
merge m:1 market_no CPI_food_no year month using `marketprices'
drop if _merge!=3
drop _merge
tab fooditem, m
encode fooditem, gen(foodno_seq)
tab foodno_seq, nolabel
estpost tabstat unitcost_district if foodno_seq==7, stats(n min max p50 mean sd semean)
tab fooditem, sum(unitcost_district)
forvalues f=1/45 {
	sum unitcost_district if foodno_seq==`f', d
	replace unitcost_district=. if unitcost_district>`r(p99)' & foodno_seq==`f'
}
tab fooditem, sum(unitcost_district)
histogram unitcost_district if foodno_seq==7

putexcel set "Supp Table G", modify
putexcel A1="Food item"
putexcel B1="Mean unit cost"
putexcel C1="Mean unit market price"
putexcel E1="SE (Diff.)"
putexcel F1="p (Diff.)"
putexcel H1="Degrees of freedom"
forvalues f=1/45 {
	ttest unitcost_district=uni_price_mwk if foodno_seq==`f'
    local j = `f' + 1
    putexcel A`j' = `"`:label foodno_seq `f''"'
    putexcel B`j' = (`=r(mu_1)')
    putexcel C`j' = (`=r(mu_2)')
    putexcel E`j' = (`=r(se)') 
    putexcel F`j' = (`=r(p)')
	putexcel H`j' = (`=r(df_t)')
}

// TABLE 1 // Summary Statistics
* Household Characteristics 
	use MalawiIHPS_DietQualCost_HH_r, clear
		svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
		replace reside=0 if reside==1 & inlist(district,107,210,314,315) // Misclassified as rural, living in city
		lab var reside "Rural (%)"
		global sumstats hhsize pctfoodexp			
			eststo sum2: svy, subpop(if data_round==2 & market_no!=. & reside==1): mean $sumstats
			eststo sum3: svy, subpop(if data_round==3 & market_no!=. & reside==1): mean $sumstats
			eststo sum4: svy, subpop(if market_no!=. & reside==1 & data_round!=1): mean $sumstats			
		esttab sum2 sum3 sum4 using "Table 1.rtf", label replace cells("b(fmt(2)) se(par)") ///
			mlabels("2013" "2016/17" "Overall") title("Table 1. Summary Statistics") ///
			nonumbers noobs wide collabels("Mean" "(SE)") 
		lab var case_id "Households"
		eststo sum12: estpost sum case_id if data_round==2 & market_no!=. & reside==1
		eststo sum13: estpost sum case_id if data_round==3 & market_no!=. & reside==1
		eststo sum14: estpost sum case_id if market_no!=. & reside==1 & data_round!=1
	esttab sum12 sum13 sum14 using "Table 1.rtf", label append ///
		cell(count) nonumbers wide noobs
		lab var case_id ""
	use MalawiIHPS_DietQualCost_PID_r, clear
		svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
		lab var reside "Rural (%)"
		replace reside=0 if reside==1 & inlist(district,107,210,314,315) // Misclassified as rural, living in city
		gen adult=1 if inlist(age_sex_grp,10,11,12,13,16,17,18,19,24,25)
		gen child=1 if inlist(age_sex_grp,1,2,3,4,5,6,7,8,9,14,15,23)
		egen totaladult=total(adult), by(case_id HHID y2_hhid y3_hhid data_round)
		egen totalchild=total(child), by(case_id HHID y2_hhid y3_hhid data_round)
		lab var totaladult "N Adults (>18)"
		lab var totalchild "N Children (<=18)"
			eststo sum6: svy, subpop(if data_round==2 & market_no!=. & reside==1): mean totaladult totalchild  
			eststo sum7: svy, subpop(if data_round==3 & market_no!=. & reside==1): mean totaladult totalchild  
			eststo sum8: svy, subpop(if market_no!=. & reside==1 & data_round!=1): mean totaladult totalchild  	
	esttab sum6 sum7 sum8 using "Table 1.rtf", label append ///
		cells("b(fmt(2) label(Mean)) se(par label((SE)))") ///
		nonumbers wide noobs	
		lab var PID "Individuals"
		eststo sum16: estpost sum PID if data_round==2 & age_sex_grp!=1 & market_no!=. & reside==1 & daysate_conv!=0
		eststo sum17: estpost sum PID if data_round==3 & age_sex_grp!=1 & market_no!=. & reside==1 & daysate_conv!=0
		eststo sum18: estpost sum PID if age_sex_grp!=1 & market_no!=. & reside==1 & data_round!=1 & daysate_conv!=0
	esttab sum16 sum17 sum18 using "Table 1.rtf", label append cells(count) ///
		nonumbers wide noobs
		
* Foods per market month with price
	use CPIDataForCoNA, clear
	drop if year<2013
	drop if year==2017 & month==8
	drop if inlist(market_no,2,18,22,24) // markets with no households
	merge m:1 dateMY using PPP_denton
	drop if _merge==2
	drop _merge
	tab food_item, sum(food_no)
	gen priced=1 if uni_price_net!=.
	egen pricedall=total(priced), by(market_no dateMY)
	lab var pricedall "Food items priced per market-month"
		eststo sum16: estpost sum pricedall if year>=2013 & year<=2015 
		eststo sum17: estpost sum pricedall if year>=2016 & year<=2017 
		eststo sum18: estpost sum pricedall 
	esttab sum16 sum17 sum18 using "Table 1.rtf", label append ///
		cells("mean(fmt(2) label(Mean)) sd(par label((SD)))") note("Standard deviation in parentheses")				
	
* Household expenditure
	use MalawiIHPS_DietQualCost_HH_r, clear
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	drop if year<2013 
	lab def year 1 "2010" 2 "2013" 3 "2016/17"
	lab val data_round year
	preserve
		use PPP_denton, clear
		collapse (first) PPP, by(year)
		tempfile ppp
			save `ppp', replace
	restore
	merge m:1 year using `ppp'
	drop if _merge==2
	drop _merge

	foreach v in totalexp_food totalexpenditure {
			gen `v'_ppp=.
			replace `v'_ppp=`v'/PPP
		}

	lab var totalexp_food_ppp "Annual food expenditure"
	lab var totalexpenditure_ppp "Annual total expenditure"

	gen totalexp_food_ppp_pd=(totalexp_food_ppp/365)
	gen totalexpenditure_ppp_pd=(totalexpenditure_ppp/365)
	lab var totalexp_food_ppp_pd "Per day (food)"
	lab var totalexpenditure_ppp_pd "Per day (total)"
	
	gen totalexp_food_ppp_pdpc=(totalexp_food_ppp/365/hhsize)
	gen totalexpenditure_ppp_pdpc=(totalexpenditure_ppp/365/hhsize)
	lab var totalexp_food_ppp_pdpc "Per day per capita (food)"
	lab var totalexpenditure_ppp_pdpc "Per day per capita (total)"

	* Food and total Spending and CoNAs - Medians
	global hhresults totalexp_food_ppp  totalexp_food_ppp_pd totalexp_food_ppp_pdpc ///
		 totalexpenditure_ppp totalexpenditure_ppp_pd totalexpenditure_ppp_pdpc pctfoodexp 

	* Median
	foreach v in $hhresults {
		eststo sum6: epctile `v', svy p(50) subpop(if data_round==2 & reside==1 & market_no!=.)
		eststo sum7: epctile `v', svy p(50) subpop(if data_round==3 & reside==1 & market_no!=.) 
		eststo sum8: epctile `v', svy p(50) subpop(if data_round!=1 & reside==1 & market_no!=.)
	esttab sum6 sum7 sum8 using "Table 1.rtf", ///
		label append cells("b(fmt(2)) se(par)") mlabels("2013" "2016/17" "Overall") ///
		nonumbers wide collabels("Median" "(SE)") 
	}

* Markets included 
	use CPIDataForCoNA, clear
	drop if year<2013
	drop if year==2017 & month==8
	drop if inlist(market_no,2,18,22,24) // markets with no households
	unique market_no
	
* Unique food items
	unique food_no

* N nutrients
	use MalawiIHPS_DietQualCost_Nut_HH_r, clear
	unique nutr_no if nutr_no!=1 // 1 = price for linear programming
	labellist nutr // Note vitamin A and retinol included separately

	// Supplementary information (footnotes)
	* Excluded
		* Number of infants
		use MalawiIHPS_DietQualCost_PID_r, clear
			lab var PID "Infants (rural)"
			eststo sum24: estpost sum PID if data_round==2 &  age_sex_grp==1 & market_no!=. & reside==1
			eststo sum25: estpost sum PID if data_round==3 &  age_sex_grp==1 & market_no!=. & reside==1
			eststo sum26: estpost sum PID if  age_sex_grp==1 & market_no!=. & reside==1 & data_round!=1
			esttab sum24 sum25 sum26 using "Table 1.rtf", label append cells(count) ///
				nonumbers wide noobs 
		lab var PID ""
		* Number of households unmatched to market
		use MalawiIHPS_DietQualCost_HH_r, clear
			replace reside=0 if reside==1 & inlist(district,107,210,314,315) // Misclassified as rural, living in city
			lab var case_id "Rural households unmatched to markets"
			eststo sum12: estpost sum case_id if data_round==2 & market_no==. & reside==1
			sum case_id if data_round==3 & market_no==. & reside==1 // 0 add manually
			eststo sum14: estpost sum case_id if market_no==. & reside==1 & data_round!=1
			esttab sum12  sum14 using "Table 1.rtf", label append ///
				cell(count) nonumbers wide noobs
			lab var case_id "Urban households"
			eststo sum12b: estpost sum case_id if data_round==2 & reside==0
			eststo sum13b: estpost sum case_id if data_round==3 & reside==0
			eststo sum14b: estpost sum case_id if reside==0 & data_round!=1
			esttab sum12b sum13b sum14b using "Table 1.rtf", label append ///
				cell(count) nonumbers wide noobs
				lab var case_id ""
		* Number of individuals unmatched to markets
		use MalawiIHPS_DietQualCost_PID_r, clear
			svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
			lab var reside "Rural (%)"
			replace reside=0 if reside==1 & inlist(district,107,210,314,315) // Misclassified as rural, living in city
			lab var PID "Rural individuals unmatched to markets"
			eststo sum20: estpost sum PID if data_round==2 & age_sex_grp!=1 & market_no==. & reside==1
			sum PID if data_round==3 & age_sex_grp!=1 & market_no==. & reside==1 // 0 add manually
			eststo sum22: estpost sum PID if age_sex_grp!=1 & market_no==. & reside==1 & data_round!=1
		esttab sum20 sum22 using "Table 1.rtf", label append cells(count) ///
			nonumbers wide noobs note("Population statistics, corrected using sampling weights.") 

	* Unique households observed in both rounds:
		use MalawiIHPS_DietQualCost_HH_r, clear
		drop if market_no==.
		lab var reside "Rural (%)"
		drop if reside==0
		drop if data_round==1	
		keep case_id HHID y2_hhid y3_hhid data_round
		sort case_id HHID data_round
		split y2_hhid, p("-") gen(y2hhidPID)
			ren y2hhidPID1 y2_hhid_num
			ren y2hhidPID2 y2_pos_y1
			destring y2_hhid_num y2_pos_y1, replace
		split y3_hhid, p("-") gen(y3hhidPID)
			ren y3hhidPID1 y3_hhid_num
			ren y3hhidPID2 y3_pos_y2
			destring y3_hhid_num y3_pos_y2, replace
		egen lowestPIDy2=min(y2_pos_y1), by(case_id data_round)
		egen lowestPIDy3=min(y3_pos_y2), by(case_id data_round)
		sort HHID data_round y2_pos_y1 y3_pos_y2
		duplicates tag data_round HHID, gen(splits)
		distinct 
		tab splits
		distinct if splits!=0
		bys data_round: distinct
		sort HHID data_round y2_pos_y1 y3_pos_y2
		drop if HHID==HHID[_n-1] & data_round==data_round[_n-1] & splits!=0
		sort HHID data_round y2_pos_y1 y3_pos_y2
		drop if HHID==HHID[_n-1] & data_round==data_round[_n-1] & splits!=0
		bys data_round: distinct
		distinct HHID
		unique HHID data_round
		tab data_round
		egen roundcount=total(data_round), by(HHID)
			// HHs with both rounds will sum to 5
			tab roundcount
		drop if roundcount!=5
		distinct HHID
		unique HHID data_round
				* Check 
				forval i=2/3 {
				unique HHID if data_round==`i'
				} 			
				* Unique & total
				unique HHID 	

// FIGURES 2 AND 3 // COST AND FEASIBILITY OVER FULL TIME SERIES
use MalawiIHPS_DietQualCost_CoNAiCoNAseries_r, clear
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	drop if year<2013 
	drop if reside==0
	
	gen iCoNA_HH_pd_ppp_percap=iCoNA_HH_pd_ppp/hhsize
	gen CoNA_sharing_pd_ppp_percap=CoNA_sharing_pd_ppp/hhsize
	
	gen iCoNA_HH_pd_ppp_perkkcal=(iCoNA_HH_pd_ppp/KCAL_hh_perday)*1000
	gen CoNA_sharing_pd_ppp_perkkcal=(CoNA_sharing_pd_ppp/KCAL_hh_perday)*1000

	replace CoNA_solution=0 if CoNA_sharing==.

	gen avail_share=1 if CoNA_solution==1
		replace avail_share=0 if CoNA_solution==0
	gen avail_indiv=1 if iCoNA_solutionallHH==1
		replace avail_indiv=0 if iCoNA_solutionallHH==0
		
	collapse (median) iCoNA_HH_pd_ppp CoNA_sharing_pd_ppp ///
		iCoNA_HH_pd_ppp_perkkcal CoNA_sharing_pd_ppp_perkkcal ///
		iCoNA_HH_pd_ppp_percap CoNA_sharing_pd_ppp_percap ///
		(mean) avail_share avail_indiv (first) year [pw=pweight], by(dateMY)
	ren iCoNA_HH_pd_ppp_percap indiv_median
	ren CoNA_sharing_pd_ppp_percap share_median
	foreach v in avail_indiv avail_share {
		replace `v'=`v'*100
		}
	lab var avail_share "Household Sharing"
	lab var avail_indiv "Individualized Diets"
	lab var share_median "Household Sharing"
	lab var indiv_median "Individualized Diets"
	
	**Figure 2
	twoway (bar avail_indiv dateMY if  dateMY<2017-07, scheme(plotplain) yscale(axis(1) range(0(10)100)) ylabel(#10, labsize(small)) fcolor(navy) lcolor(navy) barwidth(.8) ) (bar avail_share dateMY if dateMY<2017-07, ytitle("% Households with feasible diet", size(medsmall) margin(1 3 0 0)) xtitle("") xlabel(,nolabel) barwidth(.8) fcolor(emidblue) lcolor(emidblue)  xtick(#27, labsize(vsmall) tstyle(minor) labstyle(angle(vertical)))  note(" ", size(tiny) bmargin(-10 0 -10 0)) legend(col(2) row(1) rowgap(*.01) colgap(*.5) symysize(*.5) symxsize(*3) size(small) pos(11) ring(.5) region(color(none)))) 
* Reference lines added manually
* graph export "Fig 2.png", replace

	**Figure 3
	twoway (line indiv_median dateMY if dateMY<2017-07, lcolor(navy) ytitle("Median daily cost per capita (2011 US$)", size(medsmall) margin(3 1 0 0)) xtitle("") xlabel(,nolabel) xtick(#27, labsize(vsmall) tstyle(minor) labstyle(angle(vertical))) note(" ", size(tiny) bmargin(-10 0 -10 0)) legend(col(2) row(1) rowgap(*.01) colgap(*.5) symysize(*.5) symxsize(*3) size(medsmall) pos(11) ring(0)) name(cost, replace)) (line share_median dateMY if dateMY<2017-07, scheme(plotplain) lcolor(emidblue))
* Reference lines added manually
* graph export "Fig 3.png", replace

	* Supplementary information (in text)
		tabstat avail*
		tabstat *median
		
// SEASONALITY //
use MalawiIHPS_DietQualCost_CoNAiCoNAseries_r, clear 
	keep if year>=2013
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	lab var reside "Rural (%)"
	drop if reside!=1

		// Merge in region and household composition
		merge m:1 case_id HHID y2_hhid y3_hhid data_round using MalawiIHPS_DietQualCost_HH_r, ///
			keepusing(region dependency_ratio fem_male_ratio)
			drop if _merge!=3
			drop _merge
			lab def reg 1 "Central" 2 "Northern" 3 "Southern"
			lab val region reg

		// Generate Season
		gen season=1 if inlist(month,3,4,5,6,7,8)
		replace season=0 if inlist(month,9,10,11,12,1,2)
			lab def seas 1 "Post-Harvest" 0 "Lean"
			lab val season seas
			
		// Generate cost per capita
			* Real
			gen cost_pc_share_real=CoNA_sharing_pd_ppp/hhsize
			gen cost_pc_indiv_real=iCoNA_HH_pd_ppp/hhsize
			lab var cost_pc_share_real "Cost per day, household sharing, per capita (2011 US$)"
			lab var cost_pc_indiv_real "Cost per day, individualized diets, per capita (2011 US$)"
			gen log_cost_pc_share_real=log(cost_pc_share_real)
			gen log_cost_pc_indiv_real=log(cost_pc_indiv_real)
			lab var log_cost_pc_share_real "Log Cost per day, household sharing, per capita"
			lab var log_cost_pc_indiv_real "Cost per day, individualized diets, per capita"
			* Nominal
			gen cost_pc_share_nom=CoNA_sharing_pd/hhsize
			gen cost_pc_indiv_nom=iCoNA_HH_pd/hhsize
			lab var cost_pc_share_nom "Cost per day, household sharing, per capita (MWK)"
			lab var cost_pc_indiv_nom "Cost per day, individualized diets, per capita (MWK)"
			gen log_cost_pc_share_nom=log(cost_pc_share_nom)
			gen log_cost_pc_indiv_nom=log(cost_pc_indiv_nom)
			lab var log_cost_pc_share_nom "Log Cost per day, household sharing, per capita"
			lab var log_cost_pc_indiv_nom "Cost per day, individualized diets, per capita"

		// Generate feasibility outcome variable	
		gen available_indiv=1 if iCoNA_solutionallHH==1
			replace available_indiv=0 if available_indiv==.
		gen available_share=1 if CoNA_solution==1
			replace available_share=0 if available_share==.
		
		lab var region "Region"
		lab var market_no "Market fixed effect"
		lab var hhsize "Total household size"
		
		// Label months
		lab def month ///
			1 "Jan" ///
			2 "Feb"  ///
			3 "Mar" ///
			4 "Apr" ///
			5 "May" ///
			6 "Jun" ///
			7 "Jul" ///
			8 "Aug" ///
			9 "Sep" ///
			10 "Oct" ///
			11 "Nov" ///
			12 "Dec" 
		lab val month month
	
	// Generate differenced log nominal cost
	sort hhxround dateMY
	gen k_i=.
	gen d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-1] if dateMY!=2016-01
		replace k_i=1 if d_cost_indiv!=.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-2] ///
			if !inlist(dateMY,2016-01,2016-02) & d_cost_indiv==.
		replace k_i=2 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-3] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03) & d_cost_indiv==.	
		replace k_i=3 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-4] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04) & d_cost_indiv==.	
		replace k_i=4 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-5] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05) & d_cost_indiv==.	
		replace k_i=5 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-6] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06) & d_cost_indiv==.	
		replace k_i=6 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-7] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07) & d_cost_indiv==.	
		replace k_i=7 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-8] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08) & d_cost_indiv==.	
		replace k_i=8 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-9] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09) & d_cost_indiv==.	
		replace k_i=9 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-10] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10) & d_cost_indiv==.	
		replace k_i=10 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-11] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10,2016-11) & d_cost_indiv==.	
		replace k_i=11 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-12] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10,2016-11,2016-12) & d_cost_indiv==.	
		replace k_i=12 if d_cost_indiv!=. & k_i==.
	gen k_s=.
	gen d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-1] if dateMY!=2016-01
		replace k_s=1 if d_cost_share!=.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-2] ///
			if !inlist(dateMY,2016-01,2016-02) & d_cost_share==.
		replace k_s=2 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-3] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03) & d_cost_share==.	
		replace k_s=3 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-4] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04) & d_cost_share==.	
		replace k_s=4 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-5] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05) & d_cost_share==.	
		replace k_s=5 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-6] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06) & d_cost_share==.	
		replace k_s=6 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-7] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07) & d_cost_share==.	
		replace k_s=7 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-8] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08) & d_cost_share==.	
		replace k_s=8 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-9] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09) & d_cost_share==.	
		replace k_s=9 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-10] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10) & d_cost_share==.	
		replace k_s=10 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-11] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10,2016-11) & d_cost_share==.	
		replace k_s=11 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-12] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10,2016-11,2016-12) & d_cost_share==.	
		replace k_s=12 if d_cost_share!=. & k_s==.

	* Generate cos and sin functions of month
	forval m=1/12 {
		gen cos`m'=cos((`m'*_pi)/6)
		gen sin`m'=sin((`m'*_pi)/6)
		}
	gen d_cos=.
	gen d_sin=.
	forval m=2/12 {
		local d=(`m'-1)
		di `d'
		replace d_cos=cos`m'-cos`d' if month==`m'
		replace d_sin=sin`m'-sin`d' if month==`m'
		}
		replace d_cos=cos1-cos12 if month==1
		replace d_sin=sin1-sin12 if month==1

	* Generate monthly dummies for stochastic trend model - no gaps
		gen jan=1 if month==1
			replace jan=-1 if month==12
			replace jan=0 if jan==.
		gen feb=1 if month==2
			replace feb=-1 if month==1
			replace feb=0 if feb==.
		gen mar=1 if month==3
			replace mar=-1 if month==2
			replace mar=0 if mar==.
		gen apr=1 if month==4
			replace apr=-1 if month==3
			replace apr=0 if apr==.
		gen may=1 if month==5
			replace may=-1 if month==4
			replace may=0 if may==.
		gen jun=1 if month==6
			replace jun=-1 if month==5
			replace jun=0 if jun==.
		gen jul=1 if month==7
			replace jul=-1 if month==6
			replace jul=0 if jul==.
		gen aug=1 if month==8
			replace aug=-1 if month==7
			replace aug=0 if aug==.
		gen sep=1 if month==9
			replace sep=-1 if month==8
			replace sep=0 if sep==.
		gen oct=1 if month==10
			replace oct=-1 if month==9
			replace oct=0 if oct==.
		gen nov=1 if month==11
			replace nov=-1 if month==10
			replace nov=0 if nov==.
		gen dec=1 if month==12
			replace dec=-1 if month==11
			replace dec=0 if dec==.
	* Generate monthly dummies for stochastic trend model - gaps individual
		gen jan_i=1 if month==1
			replace jan_i=-1 if month==12 & k_i==1
			replace jan_i=(-1-k_i) if month==12 & k_i>1
			replace jan_i=0 if jan_i==.
		gen feb_i=1 if month==2
			replace feb_i=-1 if month==1 & k_i==1
			replace feb_i=(-1-k_i) if month==1 & k_i>1
			replace feb_i=0 if feb_i==.
		gen mar_i=1 if month==3
			replace mar_i=-1 if month==2 & k_i==1
			replace mar_i=(-1-k_i) if month==2 & k_i>1
			replace mar_i=0 if mar_i==.
		gen apr_i=1 if month==4
			replace apr_i=-1 if month==3 & k_i==1
			replace apr_i=(-1-k_i) if month==3 & k_i>1
			replace apr_i=0 if apr_i==.
		gen may_i=1 if month==5
			replace may_i=-1 if month==4 & k_i==1
			replace may_i=(-1-k_i) if month==4 & k_i>1
			replace may_i=0 if may_i==.
		gen jun_i=1 if month==6
			replace jun_i=-1 if month==5 & k_i==1
			replace jun_i=(-1-k_i) if month==5 & k_i>1
			replace jun_i=0 if jun_i==.
		gen jul_i=1 if month==7
			replace jul_i=-1 if month==6 & k_i==1
			replace jul_i=(-1-k_i) if month==6 & k_i>1
			replace jul_i=0 if jul_i==.
		gen aug_i=1 if month==8
			replace aug_i=-1 if month==7 & k_i==1
			replace aug_i=(-1-k_i) if month==7 & k_i>1
			replace aug_i=0 if aug_i==.
		gen sep_i=1 if month==9
			replace sep_i=-1 if month==8 & k_i==1
			replace sep_i=(-1-k_i) if month==8 & k_i>1
			replace sep_i=0 if sep_i==.
		gen oct_i=1 if month==10
			replace oct_i=-1 if month==9 & k_i==1
			replace oct_i=(-1-k_i) if month==9 & k_i>1
			replace oct_i=0 if oct_i==.
		gen nov_i=1 if month==11
			replace nov_i=-1 if month==10 & k_i==1
			replace nov_i=(-1-k_i) if month==10 & k_i>1
			replace nov_i=0 if nov_i==.
		gen dec_i=1 if month==12
			replace dec_i=-1 if month==11 & k_i==1
			replace dec_i=(-1-k_i) if month==11 & k_i>1
			replace dec_i=0 if dec_i==.
	* Generate monthly dummies for stochastic trend model - gaps shared
		gen jan_s=1 if month==1
			replace jan_s=-1 if month==12 & k_s==1
			replace jan_s=(-1-k_s) if month==12 & k_s>1
			replace jan_s=0 if jan_s==.
		gen feb_s=1 if month==2
			replace feb_s=-1 if month==1 & k_s==1
			replace feb_s=(-1-k_s) if month==1 & k_s>1
			replace feb_s=0 if feb_s==.
		gen mar_s=1 if month==3
			replace mar_s=-1 if month==2 & k_s==1
			replace mar_s=(-1-k_s) if month==2 & k_s>1
			replace mar_s=0 if mar_s==.
		gen apr_s=1 if month==4
			replace apr_s=-1 if month==3 & k_s==1
			replace apr_s=(-1-k_s) if month==3 & k_s>1
			replace apr_s=0 if apr_s==.
		gen may_s=1 if month==5
			replace may_s=-1 if month==4 & k_s==1
			replace may_s=(-1-k_s) if month==4 & k_s>1
			replace may_s=0 if may_s==.
		gen jun_s=1 if month==6
			replace jun_s=-1 if month==5 & k_s==1
			replace jun_s=(-1-k_s) if month==5 & k_s>1
			replace jun_s=0 if jun_s==.
		gen jul_s=1 if month==7
			replace jul_s=-1 if month==6 & k_s==1
			replace jul_s=(-1-k_s) if month==6 & k_s>1
			replace jul_s=0 if jul_s==.
		gen aug_s=1 if month==8
			replace aug_s=-1 if month==7 & k_s==1
			replace aug_s=(-1-k_s) if month==7 & k_s>1
			replace aug_s=0 if aug_s==.
		gen sep_s=1 if month==9
			replace sep_s=-1 if month==8 & k_s==1
			replace sep_s=(-1-k_s) if month==8 & k_s>1
			replace sep_s=0 if sep_s==.
		gen oct_s=1 if month==10
			replace oct_s=-1 if month==9 & k_s==1
			replace oct_s=(-1-k_s) if month==9 & k_s>1
			replace oct_s=0 if oct_s==.
		gen nov_s=1 if month==11
			replace nov_s=-1 if month==10 & k_s==1
			replace nov_s=(-1-k_s) if month==10 & k_s>1
			replace nov_s=0 if nov_s==.
		gen dec_s=1 if month==12
			replace dec_s=-1 if month==11 & k_s==1
			replace dec_s=(-1-k_s) if month==11 & k_s>1
			replace dec_s=0 if dec_s==.

	// Label months
	global months feb mar apr may jun jul aug sep oct nov dec
	global months_i feb_i mar_i apr_i may_i jun_i jul_i aug_i sep_i oct_i nov_i dec_i
	global months_s feb_s mar_s apr_s may_s jun_s jul_s aug_s sep_s oct_s nov_s dec_s
		
	foreach v of varlist jan* {
			lab var `v' "January" 
			}
	foreach v of varlist feb* {
			lab var `v' "February" 
			}
	foreach v of varlist mar* {
			lab var `v' "March" 
			}
	foreach v of varlist apr* {
			lab var `v' "April" 
			}
	foreach v of varlist may* {
			lab var `v' "May" 
			}
	foreach v of varlist jun* {
			lab var `v' "June" 
			}
	foreach v of varlist jul* {
			lab var `v' "July" 
			}
	foreach v of varlist aug* {
			lab var `v' "August" 
			}
	foreach v of varlist sep* {
			lab var `v' "September" 
			}
	foreach v of varlist oct* {
			lab var `v' "October" 
			}
	foreach v of varlist nov* {
			lab var `v' "November" 
			}
	foreach v of varlist dec* {
			lab var `v' "December" 
			}

	scalar drop _all

	* Model Comparisons - COST
	/// Trignometric model
		*Individualized diets
			* OLS
			eststo olstrig_i: reg d_cost_indiv d_cos d_sin [pweight=pweight], vce(cluster ea_id)
				fitstat, saving(OTI)
			scalar alphai=_b[d_cos]
			scalar betai=_b[d_sin]
			scalar lambdai=sqrt((alphai^2)+(betai^2))
			scalar omegai=atan(alphai/betai)
			gen seas_factori=lambdai*(cos((month*_pi)/6)-omegai)
			tab month, sum(seas_factori)
		
		* Seasonal gap 
		egen smax_i=max(seas_factori)
		egen smin_i=min(seas_factori)
		gen seasgap_i=smax_i-smin_i
		sum(seasgap_i)

	* Shared diets
		eststo olstrig_s: reg d_cost_share d_cos d_sin [pweight=pweight], vce(cluster ea_id)
		scalar alphas=_b[d_cos]
		scalar betas=_b[d_sin]
		scalar lambdas=sqrt((alphas^2)+(betas^2))
		scalar omegas=atan(alphas/betas)
		gen seas_factors=lambdas*(cos((month*_pi)/6)-omegas)
		tab month, sum(seas_factors)
		
		* Seasonal gap 
		egen smax_s=max(seas_factors)
		egen smin_s=min(seas_factors)
		gen seasgap_s=smax_s-smin_s
		sum(seasgap_s)
		
	/// Stochastic Trend Seasonal dummy 
		* OLS
		eststo olsdum_i: reg d_cost_indiv $months_i [pweight=pweight], vce(cluster ea_id)
		eststo olsdum_s: reg d_cost_share $months_s [pweight=pweight], vce(cluster ea_id)
	
	* Model comparison Trig vs Dummy
	estimates stat ols* 

		* Seasonal factors (Stochastic trend ols)
			// Individualized
				gen seasfactordum_i=.
				eststo dum_i: reg d_cost_indiv $months_i [pweight=pweight], vce(cluster ea_id)
					scalar def avgdum_i=(_b[_cons]+_b[feb_i]+_b[mar_i]+_b[apr_i]+_b[may_i]+_b[jun_i]+_b[jul_i] ///
						+_b[aug_i]+_b[sep_i]+_b[oct_i]+_b[nov_i]+_b[dec_i])/12
					foreach v in $months_i {
					scalar def `v'seasfactordum_i=_b[`v']-avgdum_i
					}		
					scalar def jan_iseasfactordum_i=_b[_cons]-avgdum_i
					replace seasfactordum_i=jan_iseasfactordum_i if month==1
					replace seasfactordum_i=feb_iseasfactordum_i if month==2
					replace seasfactordum_i=mar_iseasfactordum_i if month==3
					replace seasfactordum_i=apr_iseasfactordum_i if month==4
					replace seasfactordum_i=may_iseasfactordum_i if month==5
					replace seasfactordum_i=jun_iseasfactordum_i if month==6
					replace seasfactordum_i=jul_iseasfactordum_i if month==7
					replace seasfactordum_i=aug_iseasfactordum_i if month==8
					replace seasfactordum_i=sep_iseasfactordum_i if month==9
					replace seasfactordum_i=oct_iseasfactordum_i if month==10
					replace seasfactordum_i=nov_iseasfactordum_i if month==11
					replace seasfactordum_i=dec_iseasfactordum_i if month==12
				egen smaxdum_i=max(seasfactordum_i)
				egen smindum_i=min(seasfactordum_i)
				gen seasgapdum_i=smaxdum_i-smindum_i
				sum(seasgapdum_i)
				replace seasfactordum_i=seasfactordum_i*100
				tab month, sum(seasfactordum_i)
			
			// Shared
				gen seasfactordum_s=.
				eststo dum_s: reg d_cost_share $months_s [pweight=pweight], vce(cluster ea_id)
					scalar def avgdum_s=(_b[_cons]+_b[feb_s]+_b[mar_s]+_b[apr_s]+_b[may_s]+_b[jun_s]+_b[jul_s] ///
						+_b[aug_s]+_b[sep_s]+_b[oct_s]+_b[nov_s]+_b[dec_s])/12
					foreach v in $months_s {
					scalar def `v'seasfactordum_s=_b[`v']-avgdum_s
					}		
					scalar def jan_sseasfactordum_s=_b[_cons]-avgdum_s
					replace seasfactordum_s=jan_sseasfactordum_s if month==1
					replace seasfactordum_s=feb_sseasfactordum_s if month==2
					replace seasfactordum_s=mar_sseasfactordum_s if month==3
					replace seasfactordum_s=apr_sseasfactordum_s if month==4
					replace seasfactordum_s=may_sseasfactordum_s if month==5
					replace seasfactordum_s=jun_sseasfactordum_s if month==6
					replace seasfactordum_s=jul_sseasfactordum_s if month==7
					replace seasfactordum_s=aug_sseasfactordum_s if month==8
					replace seasfactordum_s=sep_sseasfactordum_s if month==9
					replace seasfactordum_s=oct_sseasfactordum_s if month==10
					replace seasfactordum_s=nov_sseasfactordum_s if month==11
					replace seasfactordum_s=dec_sseasfactordum_s if month==12
				egen smaxdum_s=max(seasfactordum_s)
				egen smindum_s=min(seasfactordum_s)
				gen seasgapdum_s=smaxdum_s-smindum_s
				sum(seasgapdum_s)
				replace seasfactordum_s=seasfactordum_s*100
				tab month, sum(seasfactordum_s)

	* Individualized
	estimates stat olsdum_i olstrig_i
	* Shared
	estimates stat olsdum_s  olstrig_s 

	// Supplementary Materials Table C // Model Fit Statistics
		esttab olsdum_i olstrig_i using "Supp Table C.rtf", label replace stats(ar2 aic bic F N, labels("Adj. R2" "AIC" "BIC" "F-stat" "N") star(F)) title("Individualized Diets, Model Selection Results") nogaps mlabels("Stochastic Trend Dummy Model" "Trigonometric Model") 
		esttab olsdum_s  olstrig_s using "Supp Table C.rtf", label append stats(ar2 aic bic F N, labels("Adj. R2" "AIC" "BIC" "F-stat" "N") star(F)) title("Household Sharing, Model Selection Results") nogaps mlabels("Stochastic Trend Dummy Model" "Trigonometric Model") 
* Note AIC and BIC report the value multiplied by N, table reports per observation

		* R-squared and F degrees of freedom manually:
			eststo olsdum_i: reg d_cost_indiv $months_i [pweight=pweight], vce(cluster ea_id)
				ereturn list r2_a
			eststo olstrig_i: reg d_cost_indiv d_cos d_sin [pweight=pweight], vce(cluster ea_id)
				ereturn list r2_a
			
			eststo olsdum_s: reg d_cost_share $months_s [pweight=pweight], vce(cluster ea_id)
				ereturn list r2_a
			eststo olstrig_s: reg d_cost_share d_cos d_sin [pweight=pweight], vce(cluster ea_id)
				ereturn list r2_a

	* Feasibility seasonal factors
	cap drop availseas_i
	gen availseas_i=.
		eststo dum_i: reg iCoNA_solutionallHH $months_i [pweight=pweight], vce(cluster ea_id)
			scalar def avgdum_i=(_b[_cons]+_b[feb_i]+_b[mar_i]+_b[apr_i]+_b[may_i]+_b[jun_i]+_b[jul_i] ///
				+_b[aug_i]+_b[sep_i]+_b[oct_i]+_b[nov_i]+_b[dec_i])/12
			foreach v in $months_i {
			scalar def `v'availseas_i=_b[`v']-avgdum_i
			}		
			scalar def jan_iavailseas_i=_b[_cons]-avgdum_i
			replace availseas_i=jan_iavailseas_i if month==1
			replace availseas_i=feb_iavailseas_i if month==2
			replace availseas_i=mar_iavailseas_i if month==3
			replace availseas_i=apr_iavailseas_i if month==4
			replace availseas_i=may_iavailseas_i if month==5
			replace availseas_i=jun_iavailseas_i if month==6
			replace availseas_i=jul_iavailseas_i if month==7
			replace availseas_i=aug_iavailseas_i if month==8
			replace availseas_i=sep_iavailseas_i if month==9
			replace availseas_i=oct_iavailseas_i if month==10
			replace availseas_i=nov_iavailseas_i if month==11
			replace availseas_i=dec_iavailseas_i if month==12

	cap drop availseas_s
	gen availseas_s=.
		eststo dum_s: reg CoNA_solution $months_s [pweight=pweight], vce(cluster ea_id)
			scalar def avgdum_s=(_b[_cons]+_b[feb_s]+_b[mar_s]+_b[apr_s]+_b[may_s]+_b[jun_s]+_b[jul_s] ///
				+_b[aug_s]+_b[sep_s]+_b[oct_s]+_b[nov_s]+_b[dec_s])/12
			foreach v in $months_s {
			scalar def `v'availseas_s=_b[`v']-avgdum_s
			}		
			scalar def jan_savailseas_s=_b[_cons]-avgdum_s
			replace availseas_s=jan_savailseas_s if month==1
			replace availseas_s=feb_savailseas_s if month==2
			replace availseas_s=mar_savailseas_s if month==3
			replace availseas_s=apr_savailseas_s if month==4
			replace availseas_s=may_savailseas_s if month==5
			replace availseas_s=jun_savailseas_s if month==6
			replace availseas_s=jul_savailseas_s if month==7
			replace availseas_s=aug_savailseas_s if month==8
			replace availseas_s=sep_savailseas_s if month==9
			replace availseas_s=oct_savailseas_s if month==10
			replace availseas_s=nov_savailseas_s if month==11
			replace availseas_s=dec_savailseas_s if month==12
			
		replace availseas_i=availseas_i*100
		replace availseas_s=availseas_s*100
		tab month, sum(availseas_i)		
		tab month, sum(availseas_s)		
		egen amaxdum_i=max(availseas_i)
		egen amindum_i=min(availseas_i)
		egen amaxdum_s=max(availseas_s)
		egen amindum_s=min(availseas_s)
		gen seasgapavail_i=amaxdum_i-amindum_i
		gen seasgapavail_s=amaxdum_s-amindum_s
		sum(seasgapavail_i)
		sum(seasgapavail_s)
		
 * Percent with available diet - Equation 9
	eststo odds_i: reg iCoNA_solutionallHH ib1.month market_no [pweight=pweight], vce(cluster ea_id)
		ereturn list r2_a
		margins month
		marginsplot
		
		gen probavail_i=.
			replace probavail_i=_b[_cons] if month==1
			replace probavail_i=_b[_cons]+_b[2.month] if month==2
			replace probavail_i=_b[_cons]+_b[3.month] if month==3
			replace probavail_i=_b[_cons]+_b[4.month] if month==4
			replace probavail_i=_b[_cons]+_b[5.month] if month==5
			replace probavail_i=_b[_cons]+_b[6.month] if month==6
			replace probavail_i=_b[_cons]+_b[7.month] if month==7
			replace probavail_i=_b[_cons]+_b[8.month] if month==8
			replace probavail_i=_b[_cons]+_b[9.month] if month==9
			replace probavail_i=_b[_cons]+_b[10.month] if month==10
			replace probavail_i=_b[_cons]+_b[11.month] if month==11
			replace probavail_i=_b[_cons]+_b[12.month] if month==12		
	
	eststo odds_s: reg CoNA_solution ib1.month market_no [pweight=pweight], vce(cluster ea_id)
		ereturn list r2_a
		margins month
		marginsplot
			gen probavail_s=.
			replace probavail_s=_b[_cons] if month==1
			replace probavail_s=_b[_cons]+_b[2.month] if month==2
			replace probavail_s=_b[_cons]+_b[3.month] if month==3
			replace probavail_s=_b[_cons]+_b[4.month] if month==4
			replace probavail_s=_b[_cons]+_b[5.month] if month==5
			replace probavail_s=_b[_cons]+_b[6.month] if month==6
			replace probavail_s=_b[_cons]+_b[7.month] if month==7
			replace probavail_s=_b[_cons]+_b[8.month] if month==8
			replace probavail_s=_b[_cons]+_b[9.month] if month==9
			replace probavail_s=_b[_cons]+_b[10.month] if month==10
			replace probavail_s=_b[_cons]+_b[11.month] if month==11
			replace probavail_s=_b[_cons]+_b[12.month] if month==12
			
replace probavail_i=probavail_i*100
replace probavail_s=probavail_s*100

	tab month, sum(probavail_i)		
	tab month, sum(probavail_s)		
	egen prbavmaxdum_i=max(probavail_i)
	egen prbavmindum_i=min(probavail_i)
	egen prbavmaxdum_s=max(probavail_s)
	egen prbavmindum_s=min(probavail_s)
	gen seasgapprbav_i=prbavmaxdum_i-prbavmindum_i
	gen seasgapprbav_s=prbavmaxdum_s-prbavmindum_s
	sum(seasgapprbav_i)
	sum(seasgapprbav_s)


// Table 2 //
	* Monthly seasonal factor results
		eststo mi: tab month, sum(seasfactordum_i)
		eststo ms: tab month, sum(seasfactordum_s)
		eststo mai: tab month, sum(probavail_i)
		eststo mas: tab month, sum(probavail_s)
	* Seasonal gap
		foreach v in seasfactordum_i seasfactordum_s seasgapdum_i seasgapdum_s {
		replace `v'=`v'*100
		}
		eststo gap: tabstat seasgapdum_i seasgapdum_s
	preserve
		putexcel set "Table 2", modify
		collapse (first) seasfactordum_i seasfactordum_s seasgapdum_i seasgapdum_s, by(month)
		mkmat seas*, matrix(sfcost)
		putexcel B3=matrix(sfcost), names
	restore
		
	preserve
		collapse (mean) probavail_i probavail_s (first) seasgapprbav_i seasgapprbav_s, by(month)
		mkmat probavail* seasgapprbav*, matrix(sfavail)
		putexcel B17=matrix(sfavail), names
	restore
	
save MalawiIHPS_DietQualCost_seasonality, replace
	
	* Cost and feasiblity summary statistics
	use MalawiIHPS_DietQualCost_CoNAiCoNAseries_r, clear
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	lab var reside "Rural (%)"
	drop if reside!=1
	drop if year<2013
		lab def year 1 "2010" 2 "2013" 3 "2016/17"
	lab val data_round year
	
	gen iCoNA_HH_pd_ppp_percap=iCoNA_HH_pd_ppp/hhsize
	gen CoNA_sharing_pd_ppp_percap=CoNA_sharing_pd_ppp/hhsize
		
	lab var CoNA_solution "Available (% months), Shared diet"
	lab var iCoNA_solutionallHH "Available (% months), Individualized diets"
	
	replace CoNA_solution=CoNA_solution*100
	replace iCoNA_solutionallHH=iCoNA_solutionallHH*100
	
		eststo avail: svy, subpop(if data_round!=1 & reside==1 & market_no!=.): mean  iCoNA_solutionallHH CoNA_solution
		esttab avail using "Table 2.rtf", label replace cells("b(fmt(2)) se(par)") mlabels("Overall") nonumbers wide collabels("Mean" "(SE)") 
		foreach v in iCoNA_HH_pd_ppp_percap CoNA_sharing_pd_ppp_percap {
			eststo cost: epctile `v', svy p(50) subpop(if 		data_round!=1 & reside==1 & market_no!=.)
			esttab cost using "Table 2.rtf", label append cells("b(fmt(2)) se(par)") mlabels("Overall") nonumbers wide collabels("Median" "(SE)") 
			}		
	
// Figure 4 //
use MalawiIHPS_DietQualCost_seasonality, clear
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	replace seasfactordum_i=seasfactordum_i/100
	replace seasfactordum_s=seasfactordum_s/100
	
	tw (bar probavail_i month,  yaxis(1) yscale(range(40(10)100)) ytick(#6) ylabel(#6, val) bcolor(emidblue)) ///
	(line seasfactordum_i month, connect(ascending) lcolor(navy) yaxis(2) yscale(range(-5(1)5))), ///
		xtitle("") xscale(range(1(1)12)) xtick(#12, val labsize(vsmall) tstyle(minor) ///
		labstyle(angle(vertical))) xlabel(none) ///
		ytitle("", size(zero) axis(2))   ///
		ytitle("Percent Feasible", margin(1 0 0 0) axis(1) size(small)) ///
		title("Individualized Diets", size(medsmall)) ///
		legend(off) name(indiv, replace) graphregion(color(white)) 	 
	tw (bar probavail_s month, yaxis(1) yscale(range(40(10)100)) ytick(#6) ylabel(#6, val) bcolor(emidblue)) ///
	(line seasfactordum_s month, connect(ascending) lcolor(navy) yaxis(2) yscale(range(-5(1)5))), ///
		xtitle("") xscale(range(1(1)12)) xtick(#12, val labsize(vsmall) tstyle(minor) ///
		labstyle(angle(vertical))) xlabel(none) graphregion(color(gs16)) ///
		ytitle("% Change in Monthly Cost Relative to Average", orientation(rvertical) ///
		margin(0 1 0 0) size(small) axis(2)) ///
		ytitle("", size(zero) axis(1)) graphregion(color(white)) ///
		title("Household Sharing", size(medsmall)) ///
		legend(col(2) row(2) rowgap(*.01) colgap(*.5) symysize(*.5) ///
			symxsize(*3) size(small) pos(11) ring(0) region(lstyle(none)) ///
				label(1 "Feasibility") label(2 "Cost"))	name(share, replace)
graph combine indiv share, ycommon graphregion(color(white))
graph export "Fig 4.png", replace

// Replicate seasonality with infeasible as highest cost 
use MalawiIHPS_DietQualCost_CoNAiCoNAseries_r, clear 
	keep if year>=2013
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	lab var reside "Rural (%)"
	drop if reside!=1

		// Merge in region 
		merge m:1 case_id HHID y2_hhid y3_hhid data_round using MalawiIHPS_DietQualCost_HH_r, ///
			keepusing(region)
			drop if _merge!=3
			drop _merge
			lab def reg 1 "Central" 2 "Northern" 3 "Southern"
			lab val region reg
			
		replace iCoNA_HH_pd_ppp=. if iCoNA_solutionallHH==0
			
		// Generate cost per capita
			* Real
			gen cost_pc_share_real=CoNA_sharing_pd_ppp/hhsize
				egen maxshared=max(cost_pc_share_real), by(dateMY)
				replace cost_pc_share_real=maxshared if cost_pc_share_real==.
			gen cost_pc_indiv_real=iCoNA_HH_pd_ppp/hhsize 
				egen maxindiv=max(cost_pc_indiv_real), by(dateMY)
				replace cost_pc_indiv_real=maxindiv if cost_pc_indiv_real==.
			lab var cost_pc_share_real "Cost per day, household sharing, per capita (2011 US$)"
			lab var cost_pc_indiv_real "Cost per day, individualized diets, per capita (2011 US$)"
			gen log_cost_pc_share_real=log(cost_pc_share_real)
			gen log_cost_pc_indiv_real=log(cost_pc_indiv_real)
			lab var log_cost_pc_share_real "Log Cost per day, household sharing, per capita"
			lab var log_cost_pc_indiv_real "Cost per day, individualized diets, per capita"
			* Nominal
			gen cost_pc_share_nom=CoNA_sharing_pd/hhsize
				egen maxsharednom=max(cost_pc_share_nom)
				replace cost_pc_share_nom=maxsharednom if cost_pc_share_nom==.
			gen cost_pc_indiv_nom=iCoNA_HH_pd/hhsize
				egen maxindivnom=max(cost_pc_indiv_nom)
				replace cost_pc_indiv_nom=maxindivnom if cost_pc_indiv_nom==.
			lab var cost_pc_share_nom "Cost per day, household sharing, per capita (MWK)"
			lab var cost_pc_indiv_nom "Cost per day, individualized diets, per capita (MWK)"
			gen log_cost_pc_share_nom=log(cost_pc_share_nom)
			gen log_cost_pc_indiv_nom=log(cost_pc_indiv_nom)
			lab var log_cost_pc_share_nom "Log Cost per day, household sharing, per capita"
			lab var log_cost_pc_indiv_nom "Cost per day, individualized diets, per capita"

		// Generate feasibility outcome variable	
		gen available_indiv=1 if iCoNA_solutionallHH==1
			replace available_indiv=0 if available_indiv==.
		gen available_share=1 if CoNA_solution==1
			replace available_share=0 if available_share==.
				
		lab var region "Region"
		lab var market_no "Market fixed effect"
		lab var hhsize "Total household size"
		
		// Label months
		lab def month ///
			1 "Jan" ///
			2 "Feb"  ///
			3 "Mar" ///
			4 "Apr" ///
			5 "May" ///
			6 "Jun" ///
			7 "Jul" ///
			8 "Aug" ///
			9 "Sep" ///
			10 "Oct" ///
			11 "Nov" ///
			12 "Dec" 
		lab val month month
	
	// Generate differenced log nominal cost
	sort hhxround dateMY
	gen k_i=.
	gen d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-1] if dateMY!=2016-01
		replace k_i=1 if d_cost_indiv!=.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-2] ///
			if !inlist(dateMY,2016-01,2016-02) & d_cost_indiv==.
		replace k_i=2 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-3] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03) & d_cost_indiv==.	
		replace k_i=3 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-4] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04) & d_cost_indiv==.	
		replace k_i=4 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-5] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05) & d_cost_indiv==.	
		replace k_i=5 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-6] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06) & d_cost_indiv==.	
		replace k_i=6 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-7] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07) & d_cost_indiv==.	
		replace k_i=7 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-8] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08) & d_cost_indiv==.	
		replace k_i=8 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-9] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09) & d_cost_indiv==.	
		replace k_i=9 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-10] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10) & d_cost_indiv==.	
		replace k_i=10 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-11] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10,2016-11) & d_cost_indiv==.	
		replace k_i=11 if d_cost_indiv!=. & k_i==.
		replace d_cost_indiv=(log_cost_pc_indiv_nom)-log_cost_pc_indiv_nom[_n-12] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10,2016-11,2016-12) & d_cost_indiv==.	
		replace k_i=12 if d_cost_indiv!=. & k_i==.
	gen k_s=.
	gen d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-1] if dateMY!=2016-01
		replace k_s=1 if d_cost_share!=.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-2] ///
			if !inlist(dateMY,2016-01,2016-02) & d_cost_share==.
		replace k_s=2 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-3] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03) & d_cost_share==.	
		replace k_s=3 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-4] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04) & d_cost_share==.	
		replace k_s=4 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-5] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05) & d_cost_share==.	
		replace k_s=5 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-6] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06) & d_cost_share==.	
		replace k_s=6 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-7] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07) & d_cost_share==.	
		replace k_s=7 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-8] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08) & d_cost_share==.	
		replace k_s=8 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-9] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09) & d_cost_share==.	
		replace k_s=9 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-10] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10) & d_cost_share==.	
		replace k_s=10 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-11] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10,2016-11) & d_cost_share==.	
		replace k_s=11 if d_cost_share!=. & k_s==.
		replace d_cost_share=(log_cost_pc_share_nom)-log_cost_pc_share_nom[_n-12] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10,2016-11,2016-12) & d_cost_share==.	
		replace k_s=12 if d_cost_share!=. & k_s==.

	* Generate cos and sin functions of month
	forval m=1/12 {
		gen cos`m'=cos((`m'*_pi)/6)
		gen sin`m'=sin((`m'*_pi)/6)
		}
	gen d_cos=.
	gen d_sin=.
	forval m=2/12 {
		local d=(`m'-1)
		di `d'
		replace d_cos=cos`m'-cos`d' if month==`m'
		replace d_sin=sin`m'-sin`d' if month==`m'
		}
		replace d_cos=cos1-cos12 if month==1
		replace d_sin=sin1-sin12 if month==1

	* Generate monthly dummies for stochastic trend model - no gaps
		gen jan=1 if month==1
			replace jan=-1 if month==12
			replace jan=0 if jan==.
		gen feb=1 if month==2
			replace feb=-1 if month==1
			replace feb=0 if feb==.
		gen mar=1 if month==3
			replace mar=-1 if month==2
			replace mar=0 if mar==.
		gen apr=1 if month==4
			replace apr=-1 if month==3
			replace apr=0 if apr==.
		gen may=1 if month==5
			replace may=-1 if month==4
			replace may=0 if may==.
		gen jun=1 if month==6
			replace jun=-1 if month==5
			replace jun=0 if jun==.
		gen jul=1 if month==7
			replace jul=-1 if month==6
			replace jul=0 if jul==.
		gen aug=1 if month==8
			replace aug=-1 if month==7
			replace aug=0 if aug==.
		gen sep=1 if month==9
			replace sep=-1 if month==8
			replace sep=0 if sep==.
		gen oct=1 if month==10
			replace oct=-1 if month==9
			replace oct=0 if oct==.
		gen nov=1 if month==11
			replace nov=-1 if month==10
			replace nov=0 if nov==.
		gen dec=1 if month==12
			replace dec=-1 if month==11
			replace dec=0 if dec==.
	* Generate monthly dummies for stochastic trend model - gaps individual
		gen jan_i=1 if month==1
			replace jan_i=-1 if month==12 & k_i==1
			replace jan_i=(-1-k_i) if month==12 & k_i>1
			replace jan_i=0 if jan_i==.
		gen feb_i=1 if month==2
			replace feb_i=-1 if month==1 & k_i==1
			replace feb_i=(-1-k_i) if month==1 & k_i>1
			replace feb_i=0 if feb_i==.
		gen mar_i=1 if month==3
			replace mar_i=-1 if month==2 & k_i==1
			replace mar_i=(-1-k_i) if month==2 & k_i>1
			replace mar_i=0 if mar_i==.
		gen apr_i=1 if month==4
			replace apr_i=-1 if month==3 & k_i==1
			replace apr_i=(-1-k_i) if month==3 & k_i>1
			replace apr_i=0 if apr_i==.
		gen may_i=1 if month==5
			replace may_i=-1 if month==4 & k_i==1
			replace may_i=(-1-k_i) if month==4 & k_i>1
			replace may_i=0 if may_i==.
		gen jun_i=1 if month==6
			replace jun_i=-1 if month==5 & k_i==1
			replace jun_i=(-1-k_i) if month==5 & k_i>1
			replace jun_i=0 if jun_i==.
		gen jul_i=1 if month==7
			replace jul_i=-1 if month==6 & k_i==1
			replace jul_i=(-1-k_i) if month==6 & k_i>1
			replace jul_i=0 if jul_i==.
		gen aug_i=1 if month==8
			replace aug_i=-1 if month==7 & k_i==1
			replace aug_i=(-1-k_i) if month==7 & k_i>1
			replace aug_i=0 if aug_i==.
		gen sep_i=1 if month==9
			replace sep_i=-1 if month==8 & k_i==1
			replace sep_i=(-1-k_i) if month==8 & k_i>1
			replace sep_i=0 if sep_i==.
		gen oct_i=1 if month==10
			replace oct_i=-1 if month==9 & k_i==1
			replace oct_i=(-1-k_i) if month==9 & k_i>1
			replace oct_i=0 if oct_i==.
		gen nov_i=1 if month==11
			replace nov_i=-1 if month==10 & k_i==1
			replace nov_i=(-1-k_i) if month==10 & k_i>1
			replace nov_i=0 if nov_i==.
		gen dec_i=1 if month==12
			replace dec_i=-1 if month==11 & k_i==1
			replace dec_i=(-1-k_i) if month==11 & k_i>1
			replace dec_i=0 if dec_i==.
	* Generate monthly dummies for stochastic trend model - gaps shared
		gen jan_s=1 if month==1
			replace jan_s=-1 if month==12 & k_s==1
			replace jan_s=(-1-k_s) if month==12 & k_s>1
			replace jan_s=0 if jan_s==.
		gen feb_s=1 if month==2
			replace feb_s=-1 if month==1 & k_s==1
			replace feb_s=(-1-k_s) if month==1 & k_s>1
			replace feb_s=0 if feb_s==.
		gen mar_s=1 if month==3
			replace mar_s=-1 if month==2 & k_s==1
			replace mar_s=(-1-k_s) if month==2 & k_s>1
			replace mar_s=0 if mar_s==.
		gen apr_s=1 if month==4
			replace apr_s=-1 if month==3 & k_s==1
			replace apr_s=(-1-k_s) if month==3 & k_s>1
			replace apr_s=0 if apr_s==.
		gen may_s=1 if month==5
			replace may_s=-1 if month==4 & k_s==1
			replace may_s=(-1-k_s) if month==4 & k_s>1
			replace may_s=0 if may_s==.
		gen jun_s=1 if month==6
			replace jun_s=-1 if month==5 & k_s==1
			replace jun_s=(-1-k_s) if month==5 & k_s>1
			replace jun_s=0 if jun_s==.
		gen jul_s=1 if month==7
			replace jul_s=-1 if month==6 & k_s==1
			replace jul_s=(-1-k_s) if month==6 & k_s>1
			replace jul_s=0 if jul_s==.
		gen aug_s=1 if month==8
			replace aug_s=-1 if month==7 & k_s==1
			replace aug_s=(-1-k_s) if month==7 & k_s>1
			replace aug_s=0 if aug_s==.
		gen sep_s=1 if month==9
			replace sep_s=-1 if month==8 & k_s==1
			replace sep_s=(-1-k_s) if month==8 & k_s>1
			replace sep_s=0 if sep_s==.
		gen oct_s=1 if month==10
			replace oct_s=-1 if month==9 & k_s==1
			replace oct_s=(-1-k_s) if month==9 & k_s>1
			replace oct_s=0 if oct_s==.
		gen nov_s=1 if month==11
			replace nov_s=-1 if month==10 & k_s==1
			replace nov_s=(-1-k_s) if month==10 & k_s>1
			replace nov_s=0 if nov_s==.
		gen dec_s=1 if month==12
			replace dec_s=-1 if month==11 & k_s==1
			replace dec_s=(-1-k_s) if month==11 & k_s>1
			replace dec_s=0 if dec_s==.

	// Label months
	global months feb mar apr may jun jul aug sep oct nov dec
	global months_i feb_i mar_i apr_i may_i jun_i jul_i aug_i sep_i oct_i nov_i dec_i
	global months_s feb_s mar_s apr_s may_s jun_s jul_s aug_s sep_s oct_s nov_s dec_s
		
	foreach v of varlist jan* {
			lab var `v' "January" 
			}
	foreach v of varlist feb* {
			lab var `v' "February" 
			}
	foreach v of varlist mar* {
			lab var `v' "March" 
			}
	foreach v of varlist apr* {
			lab var `v' "April" 
			}
	foreach v of varlist may* {
			lab var `v' "May" 
			}
	foreach v of varlist jun* {
			lab var `v' "June" 
			}
	foreach v of varlist jul* {
			lab var `v' "July" 
			}
	foreach v of varlist aug* {
			lab var `v' "August" 
			}
	foreach v of varlist sep* {
			lab var `v' "September" 
			}
	foreach v of varlist oct* {
			lab var `v' "October" 
			}
	foreach v of varlist nov* {
			lab var `v' "November" 
			}
	foreach v of varlist dec* {
			lab var `v' "December" 
			}

	scalar drop _all

	* Model Comparisons 
	/// Trignometric model
		*Individualized diets
			eststo olstrig_i: reg d_cost_indiv d_cos d_sin [pweight=pweight], vce(cluster ea_id)
				fitstat, saving(OTI)
			scalar alphai=_b[d_cos]
			scalar betai=_b[d_sin]
			scalar lambdai=sqrt((alphai^2)+(betai^2))
			scalar omegai=atan(alphai/betai)
			gen seas_factori=lambdai*(cos((month*_pi)/6)-omegai)
			tab month, sum(seas_factori)
			
		* Seasonal gap 
		egen smax_i=max(seas_factori)
		egen smin_i=min(seas_factori)
		gen seasgap_i=smax_i-smin_i
		sum(seasgap_i)

	* Shared diets
		eststo olstrig_s: reg d_cost_share d_cos d_sin [pweight=pweight], vce(cluster ea_id)
		scalar alphas=_b[d_cos]
		scalar betas=_b[d_sin]
		scalar lambdas=sqrt((alphas^2)+(betas^2))
		scalar omegas=atan(alphas/betas)
		gen seas_factors=lambdas*(cos((month*_pi)/6)-omegas)
		tab month, sum(seas_factors)
		
		* Seasonal gap
		egen smax_s=max(seas_factors)
		egen smin_s=min(seas_factors)
		gen seasgap_s=smax_s-smin_s
		sum(seasgap_s)
		
	/// Stochastic Trend Seasonal dummy 
		eststo olsdum_i: reg d_cost_indiv $months_i [pweight=pweight], vce(cluster ea_id)
		eststo olsdum_s: reg d_cost_share $months_s [pweight=pweight], vce(cluster ea_id)

		* Seasonal factors
			// Individualized
				gen seasfactordum_i=.
				eststo dum_i: reg d_cost_indiv $months_i [pweight=pweight], vce(cluster ea_id)
					scalar def avgdum_i=(_b[_cons]+_b[feb_i]+_b[mar_i]+_b[apr_i]+_b[may_i]+_b[jun_i]+_b[jul_i] ///
						+_b[aug_i]+_b[sep_i]+_b[oct_i]+_b[nov_i]+_b[dec_i])/12
					foreach v in $months_i {
					scalar def `v'seasfactordum_i=_b[`v']-avgdum_i
					}		
					scalar def jan_iseasfactordum_i=_b[_cons]-avgdum_i
					replace seasfactordum_i=jan_iseasfactordum_i if month==1
					replace seasfactordum_i=feb_iseasfactordum_i if month==2
					replace seasfactordum_i=mar_iseasfactordum_i if month==3
					replace seasfactordum_i=apr_iseasfactordum_i if month==4
					replace seasfactordum_i=may_iseasfactordum_i if month==5
					replace seasfactordum_i=jun_iseasfactordum_i if month==6
					replace seasfactordum_i=jul_iseasfactordum_i if month==7
					replace seasfactordum_i=aug_iseasfactordum_i if month==8
					replace seasfactordum_i=sep_iseasfactordum_i if month==9
					replace seasfactordum_i=oct_iseasfactordum_i if month==10
					replace seasfactordum_i=nov_iseasfactordum_i if month==11
					replace seasfactordum_i=dec_iseasfactordum_i if month==12
				egen smaxdum_i=max(seasfactordum_i)
				egen smindum_i=min(seasfactordum_i)
				gen seasgapdum_i=smaxdum_i-smindum_i
				sum(seasgapdum_i)
				tab month, sum(seasfactordum_i)
			
			// Shared
				gen seasfactordum_s=.
				eststo dum_s: reg d_cost_share $months_s [pweight=pweight], vce(cluster ea_id)
					scalar def avgdum_s=(_b[_cons]+_b[feb_s]+_b[mar_s]+_b[apr_s]+_b[may_s]+_b[jun_s]+_b[jul_s] ///
						+_b[aug_s]+_b[sep_s]+_b[oct_s]+_b[nov_s]+_b[dec_s])/12
					foreach v in $months_s {
					scalar def `v'seasfactordum_s=_b[`v']-avgdum_s
					}		
					scalar def jan_sseasfactordum_s=_b[_cons]-avgdum_s
					replace seasfactordum_s=jan_sseasfactordum_s if month==1
					replace seasfactordum_s=feb_sseasfactordum_s if month==2
					replace seasfactordum_s=mar_sseasfactordum_s if month==3
					replace seasfactordum_s=apr_sseasfactordum_s if month==4
					replace seasfactordum_s=may_sseasfactordum_s if month==5
					replace seasfactordum_s=jun_sseasfactordum_s if month==6
					replace seasfactordum_s=jul_sseasfactordum_s if month==7
					replace seasfactordum_s=aug_sseasfactordum_s if month==8
					replace seasfactordum_s=sep_sseasfactordum_s if month==9
					replace seasfactordum_s=oct_sseasfactordum_s if month==10
					replace seasfactordum_s=nov_sseasfactordum_s if month==11
					replace seasfactordum_s=dec_sseasfactordum_s if month==12
				egen smaxdum_s=max(seasfactordum_s)
				egen smindum_s=min(seasfactordum_s)
				gen seasgapdum_s=smaxdum_s-smindum_s
				sum(seasgapdum_s)
				tab month, sum(seasfactordum_s)

	* Individualized
	estimates stat olsdum_i olstrig_i 
	* Shared
	estimates stat olsdum_s olstrig_s  
	**Confirms dummy model is preferred

// Table 2 - Column 3 //
	* Monthly seasonal factor results
		eststo mi: tab month, sum(seasfactordum_i)
		eststo ms: tab month, sum(seasfactordum_s)
	* Seasonal gap
		foreach v in seasfactordum_i seasfactordum_s seasgapdum_i seasgapdum_s {
		replace `v'=`v'*100
		}
		eststo gap: tabstat seasgapdum_i seasgapdum_s
	preserve
		putexcel set "Table 2_col 3", modify
		collapse (first) seasfactordum_i seasfactordum_s seasgapdum_i seasgapdum_s, by(month)
		mkmat seas*, matrix(sfcost)
		putexcel B3=matrix(sfcost), names
	restore

// Supplementary Fig A //
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	tw (line seasfactordum_i month, connect(ascending) yaxis(1)) (line seasfactordum_s month, connect(ascending) yaxis(1)), xtitle("") xscale(range(1(1)12)) xtick(#12, val labsize(vsmall) tstyle(minor) labstyle(angle(vertical))) xlabel(none) ytitle("% Change in Monthly Cost Relative to Average", margin(0 1 0 0) size(small) axis(1)) yline(0, lcolor(black) lpattern(solid) lwidth(vthin)) legend(col(2) row(2) rowgap(*.01) colgap(*.5) symysize(*.5) symxsize(*2) size(small) pos(11) ring(0) label(1 "Individualized Diets") label(2 "Household Sharing"))
graph export "Supplementary Fig A.png", replace

	
// Figure 5 // 
	* Food prices 
	use CPIDataforCoNA, clear
	recode food_group (15=14) (16=15) (17=16)
	merge m:1 month year using PPP_denton
	drop if _merge==2
	drop _merge
	drop if year<2013
	drop if inlist(market_no,2,17,22,24) // urban
	replace uni_price_net=. if market_no==15 & food_no==48 & month==1 & year==2017 // extreme outlier likely data entry error
	tab food_item, sum(uni_price_net)
	lab var uni_price_net "Net Unit Price (nominal MWK)"
	gen unitprice_ppp=uni_price_net/PPP_month_interpolated
	lab var unitprice_ppp "Net unit price per kg, edible portion (2011 US$)"

		* Change nutrient content to per kg (currently nutrients per 100g edible portion)
		foreach v of varlist energy carbohydrate protein lipids vitA retinol vitC vitE ///
			thiamin riboflavin niacin vitB6 folate vitB12 calcium copper iron  ///
			magnesium phosphorus selenium zinc sodium {
			replace `v'=`v'*10
			lab var `v' "`v' per kg"
			}
		* Unit price per 1000 kcal
		gen unitprice_perkkcal=uni_price_net*1000/energy
		gen unitprice_perkkcal_ppp=unitprice_perkkcal/PPP_month_interpolated
			lab var unitprice_perkkcal_ppp "Net unit price per 1,000 kca1 (2011 US$)"
			
	tabstat unitprice_perkkcal_ppp, by(food_item) stats(mean semean p50 min max )

	keep market_no market region district year month food_no food_item uni_price_net ///
		food_group food_group_name MWI_food_group MWI_food_group_name dateMY unitprice_ppp ///
		unitprice_perkkcal_ppp unitprice_perkkcal 

	gen available=1 if unitprice_ppp!=.
	replace available=0 if unitprice_ppp==.

	lab def fg 1 "Alcohol, stimulants, spices & condiment" ///
		2 "Caloric beverages" ///
		3 "Cereals & cereal products" ///
		4 "Dark green leafy vegetables" ///
		5 "Eggs" ///
		6 "Fish & seafood" ///
		7 "Flesh meat" ///
		8 "Legumes" ///
		9 "Milk & milk products" ///
		10 "Oils & fats" ///
		11 "Other fruit" ///
		12 "Other vegetable" ///
		13 "Roots & tubers" ///
		14 "Sweets & confectionary" ///
		15 "Vitamin-A rich fruits" ///
		16 "Vitamin-A rich vegetables & tubers" 
	lab val food_group fg

		// Label months
		lab def month ///
			1 "Jan" ///
			2 "Feb"  ///
			3 "Mar" ///
			4 "Apr" ///
			5 "May" ///
			6 "Jun" ///
			7 "Jul" ///
			8 "Aug" ///
			9 "Sep" ///
			10 "Oct" ///
			11 "Nov" ///
			12 "Dec" 
		lab val month month
		
	sort market_no food_no dateMY
		
		* Gen log change in food item nominal price per month
	gen log_uni_price_net=log(uni_price_net)
	gen k=.
	lab var k "number of gap months"
	gen d_price=(log_uni_price_net)-log_uni_price_net[_n-1] if dateMY!=2016-01 & food_no==food_no[_n-1] & market_no==market_no[_n-1]
		replace k=1 if d_price!=. & k==.
		replace d_price=(log_uni_price_net)-log_uni_price_net[_n-2] ///
			if !inlist(dateMY,2016-01,2016-02) & d_price==. & food_no==food_no[_n-1] & market_no==market_no[_n-1]
			replace k=2 if d_price!=. & k==.
		replace d_price=(log_uni_price_net)-log_uni_price_net[_n-3] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03) & d_price==.	& food_no==food_no[_n-1] & market_no==market_no[_n-1]
			replace k=3 if d_price!=. & k==.
		replace d_price=(log_uni_price_net)-log_uni_price_net[_n-4] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04) & d_price==.	& food_no==food_no[_n-1] & market_no==market_no[_n-1]
			replace k=4 if d_price!=. & k==.
		replace d_price=(log_uni_price_net)-log_uni_price_net[_n-5] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05) & d_price==.	& food_no==food_no[_n-1] & market_no==market_no[_n-1]
			replace k=5 if d_price!=. & k==.
		replace d_price=(log_uni_price_net)-log_uni_price_net[_n-6] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06) & d_price==.	& food_no==food_no[_n-1] & market_no==market_no[_n-1]
			replace k=6 if d_price!=. & k==.
		replace d_price=(log_uni_price_net)-log_uni_price_net[_n-7] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07) & d_price==.	& food_no==food_no[_n-1] & market_no==market_no[_n-1]
			replace k=7 if d_price!=. & k==.
		replace d_price=(log_uni_price_net)-log_uni_price_net[_n-8] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08) & d_price==.	& food_no==food_no[_n-1] & market_no==market_no[_n-1]
			replace k=8 if d_price!=. & k==.
		replace d_price=(log_uni_price_net)-log_uni_price_net[_n-9] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09) & d_price==.	& food_no==food_no[_n-1] & market_no==market_no[_n-1]
			replace k=9 if d_price!=. & k==.
		replace d_price=(log_uni_price_net)-log_uni_price_net[_n-10] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10) & d_price==.	& food_no==food_no[_n-1] & market_no==market_no[_n-1]
			replace k=10 if d_price!=. & k==.
		replace d_price=(log_uni_price_net)-log_uni_price_net[_n-11] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10,2016-11) & d_price==.	& food_no==food_no[_n-1] & market_no==market_no[_n-1]
			replace k=11 if d_price!=. & k==.
		replace d_price=(log_uni_price_net)-log_uni_price_net[_n-12] ///
			if !inlist(dateMY,2016-01,2016-02,2016-03,2016-04,2016-05,2016-06,2016-07,2016-08,2016-09,2016-10,2016-11,2016-12) & d_price==.	& food_no==food_no[_n-1] & market_no==market_no[_n-1]
			replace k=12 if d_price!=. & k==.
	* Generate monthly dummies for stochastic trend model - no gaps
		gen jan=1 if month==1
			replace jan=-1 if month==12
			replace jan=0 if jan==.
		gen feb=1 if month==2
			replace feb=-1 if month==1
			replace feb=0 if feb==.
		gen mar=1 if month==3
			replace mar=-1 if month==2
			replace mar=0 if mar==.
		gen apr=1 if month==4
			replace apr=-1 if month==3
			replace apr=0 if apr==.
		gen may=1 if month==5
			replace may=-1 if month==4
			replace may=0 if may==.
		gen jun=1 if month==6
			replace jun=-1 if month==5
			replace jun=0 if jun==.
		gen jul=1 if month==7
			replace jul=-1 if month==6
			replace jul=0 if jul==.
		gen aug=1 if month==8
			replace aug=-1 if month==7
			replace aug=0 if aug==.
		gen sep=1 if month==9
			replace sep=-1 if month==8
			replace sep=0 if sep==.
		gen oct=1 if month==10
			replace oct=-1 if month==9
			replace oct=0 if oct==.
		gen nov=1 if month==11
			replace nov=-1 if month==10
			replace nov=0 if nov==.
		gen dec=1 if month==12
			replace dec=-1 if month==11
			replace dec=0 if dec==.
	* Generate monthly dummies for stochastic trend model with gaps 
		gen jan_i=1 if month==1
			replace jan_i=-1 if month==12 & k==1
			replace jan_i=(-1-k) if month==12 & k>1
			replace jan_i=0 if jan_i==.
		gen feb_i=1 if month==2
			replace feb_i=-1 if month==1 & k==1
			replace feb_i=(-1-k) if month==1 & k>1
			replace feb_i=0 if feb_i==.
		gen mar_i=1 if month==3
			replace mar_i=-1 if month==2 & k==1
			replace mar_i=(-1-k) if month==2 & k>1
			replace mar_i=0 if mar_i==.
		gen apr_i=1 if month==4
			replace apr_i=-1 if month==3 & k==1
			replace apr_i=(-1-k) if month==3 & k>1
			replace apr_i=0 if apr_i==.
		gen may_i=1 if month==5
			replace may_i=-1 if month==4 & k==1
			replace may_i=(-1-k) if month==4 & k>1
			replace may_i=0 if may_i==.
		gen jun_i=1 if month==6
			replace jun_i=-1 if month==5 & k==1
			replace jun_i=(-1-k) if month==5 & k>1
			replace jun_i=0 if jun_i==.
		gen jul_i=1 if month==7
			replace jul_i=-1 if month==6 & k==1
			replace jul_i=(-1-k) if month==6 & k>1
			replace jul_i=0 if jul_i==.
		gen aug_i=1 if month==8
			replace aug_i=-1 if month==7 & k==1
			replace aug_i=(-1-k) if month==7 & k>1
			replace aug_i=0 if aug_i==.
		gen sep_i=1 if month==9
			replace sep_i=-1 if month==8 & k==1
			replace sep_i=(-1-k) if month==8 & k>1
			replace sep_i=0 if sep_i==.
		gen oct_i=1 if month==10
			replace oct_i=-1 if month==9 & k==1
			replace oct_i=(-1-k) if month==9 & k>1
			replace oct_i=0 if oct_i==.
		gen nov_i=1 if month==11
			replace nov_i=-1 if month==10 & k==1
			replace nov_i=(-1-k) if month==10 & k>1
			replace nov_i=0 if nov_i==.
		gen dec_i=1 if month==12
			replace dec_i=-1 if month==11 & k==1
			replace dec_i=(-1-k) if month==11 & k>1
			replace dec_i=0 if dec_i==.
				foreach v of varlist jan* {
						lab var `v' "January" 
						}
				foreach v of varlist feb* {
						lab var `v' "February" 
						}
				foreach v of varlist mar* {
						lab var `v' "March" 
						}
				foreach v of varlist apr* {
						lab var `v' "April" 
						}
				foreach v of varlist may* {
						lab var `v' "May" 
						}
				foreach v of varlist jun* {
						lab var `v' "June" 
						}
				foreach v of varlist jul* {
						lab var `v' "July" 
						}
				foreach v of varlist aug* {
						lab var `v' "August" 
						}
				foreach v of varlist sep* {
						lab var `v' "September" 
						}
				foreach v of varlist oct* {
						lab var `v' "October" 
						}
				foreach v of varlist nov* {
						lab var `v' "November" 
						}
				foreach v of varlist dec* {
						lab var `v' "December" 
						}

	* Gen cos and sin functions
	forval m=1/12 {
		gen cos`m'=cos((`m'*_pi)/6)
		gen sin`m'=sin((`m'*_pi)/6)
		}
	gen d_cos=.
	gen d_sin=.
	forval m=2/12 {
		local d=(`m'-1)
		di `d'
		replace d_cos=cos`m'-cos`d' if month==`m'
		replace d_sin=sin`m'-sin`d' if month==`m'
		}
		replace d_cos=cos1-cos12 if month==1
		replace d_sin=sin1-sin12 if month==1

	lab val food_group fg
	lab val month month
	global months feb mar apr may jun jul aug sep oct nov dec
	global months_i feb_i mar_i apr_i may_i jun_i jul_i aug_i sep_i oct_i nov_i dec_i

		* Maize grain
			// Model comparisons - maize grain
			eststo maize_olsdum: reg d_price $months_i if food_no==1, vce(cluster market_no)
			eststo maize_olstrig: reg d_price d_cos d_sin if food_no==1, vce(cluster market_no)
			estimates stat maize_olsdum  maize_olstrig 
				** dummy preferred by AIC not BIC
				
			// Seasonal gap 
			eststo maize_trig: reg d_price d_cos d_sin if food_no==1, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factormaize1=lambda*(cos((month*_pi)/6)-omega)
					* Seasonal gap (fixed effects)
					tempvar smax
					egen `smax'=max(seas_factormaize1)
					tempvar smin
					egen `smin'=min(seas_factormaize1)
					gen seasgap_m1=`smax'-`smin'
					sum(seasgap_m1)
		* ADMARC Maize grain
			// Model comparisons - 
			eststo admaize_olsdum: reg d_price $months_i if food_no==2, vce(cluster market_no)
			eststo admaize_olstrig: reg d_price d_cos d_sin if food_no==2, vce(cluster market_no)
			estimates stat admaize_olsdum  admaize_olstrig 
				** dummy preferred by AIC not BIC

			// Seasonal gap
			eststo admaize_trig: reg d_price d_cos d_sin if food_no==2, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factoradmaize2=lambda*(cos((month*_pi)/6)-omega)
					* Seasonal gap (fixed effects)
					tempvar smax
					egen `smax'=max(seas_factoradmaize2)
					tempvar smin
					egen `smin'=min(seas_factoradmaize2)
					gen seasgap_m2=`smax'-`smin'
					sum(seasgap_m2)
			scalar lamdasqrd=lambda*2
			di lamdasqrd // confirm seas gap = lambda*2

		* Cereals
			// Model comparisons - 
			eststo cer_olsdum: reg d_price $months_i if food_group==3, vce(cluster market_no)
			eststo cer_olstrig: reg d_price d_cos d_sin if food_group==3, vce(cluster market_no)
			estimates stat cer_olsdum  cer_olstrig 

			// Seasonal gap 
			eststo cer_trig: reg d_price d_cos d_sin if food_group==3, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factorcereals=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factorcereals)
					tempvar smin
					egen `smin'=min(seas_factorcereals)
					gen seasgap_cer=`smax'-`smin'
					sum(seasgap_cer)
		* Leafy green
			// Model comparisons - 
			eststo olsdum: reg d_price $months_i if food_group==4, vce(cluster market_no)
			eststo olstrig: reg d_price d_cos d_sin if food_group==4, vce(cluster market_no)
			estimates stat olsdum  olstrig 
			
			// Seasonal gap for preferred specification
			reg d_price d_cos d_sin if food_group==4, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factorgreens=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factorgreens)
					tempvar smin
					egen `smin'=min(seas_factorgreens)
					gen seasgap_greens=`smax'-`smin'
					sum(seasgap_greens)
		* Eggs
			// Model comparisons - 
			eststo olsdum: reg d_price $months_i if food_group==5, vce(cluster market_no)
			eststo olstrig: reg d_price d_cos d_sin if food_group==5, vce(cluster market_no)
			estimates stat olsdum olstrig
			
			// Seasonal gap for preferred specification - trig fixed effects
			reg d_price d_cos d_sin if food_group==5, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factoreggs=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factoreggs)
					tempvar smin
					egen `smin'=min(seas_factoreggs)
					gen seasgap_eggs=`smax'-`smin'
					sum(seasgap_eggs)
		* Fish
			// Model comparisons - 
			eststo olsdum: reg d_price $months_i if food_group==6, vce(cluster market_no)
			eststo olstrig: reg d_price d_cos d_sin if food_group==6, vce(cluster market_no)
			estimates stat olsdum  olstrig 

			// Seasonal gap for preferred specification - trig fixed effects
			reg d_price d_cos d_sin if food_group==6, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factorfish=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factorfish)
					tempvar smin
					egen `smin'=min(seas_factorfish)
					gen seasgap_fish=`smax'-`smin'
					sum(seasgap_fish)
		* Meat
			// Model comparisons - 
			eststo olsdum: reg d_price $months_i if food_group==7, vce(cluster market_no)
			eststo olstrig: reg d_price d_cos d_sin if food_group==7, vce(cluster market_no)
			estimates stat olsdum  olstrig 
			
			// Seasonal gap for preferred specification - trig fixed effects
			reg d_price d_cos d_sin if food_group==7,  vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factormeat=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factormeat)
					tempvar smin
					egen `smin'=min(seas_factormeat)
					gen seasgap_meat=`smax'-`smin'
					sum(seasgap_meat)
		* Legumes
			// Model comparisons - 
			eststo olsdum: reg d_price $months_i if food_group==8, vce(cluster market_no)
			eststo olstrig: reg d_price d_cos d_sin if food_group==8, vce(cluster market_no)
			estimates stat olsdum  olstrig 
			**dummy preferred by AIC not BIC
			
			// Seasonal gap 
			reg d_price d_cos d_sin if food_group==8, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factorleg=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factorleg)
					tempvar smin
					egen `smin'=min(seas_factorleg)
					gen seasgap_leg=`smax'-`smin'
					sum(seasgap_leg)
		* Milk
			// Model comparisons - 
			eststo olsdum: reg d_price $months_i if food_group==9, vce(cluster market_no)
			eststo olstrig: reg d_price d_cos d_sin if food_group==9, vce(cluster market_no)
			estimates stat olsdum  olstrig
			**dummy preferred by AIC not BIC
			
			// Seasonal gap 
			reg d_price d_cos d_sin if food_group==9, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factormilk=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factormilk)
					tempvar smin
					egen `smin'=min(seas_factormilk)
					gen seasgap_milk=`smax'-`smin'
					sum(seasgap_milk)
		* Oils
			// Model comparisons - 
			eststo olsdum: reg d_price $months_i if food_group==10, vce(cluster market_no)
			eststo olstrig: reg d_price d_cos d_sin if food_group==10, vce(cluster market_no)
			estimates stat olsdum  olstrig 
			
			// Seasonal gap for preferred specification - trig fixed effects
			reg d_price d_cos d_sin if food_group==10, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factoroil=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factoroil)
					tempvar smin
					egen `smin'=min(seas_factoroil)
					gen seasgap_oil=`smax'-`smin'
					sum(seasgap_oil)
		* Other fruit - 
			// Model comparisons - 
			eststo olsdum: reg d_price $months_i if food_group==11, vce(cluster market_no)
			eststo olstrig: reg d_price d_cos d_sin if food_group==11, vce(cluster market_no)
			estimates stat olsdum  olstrig 
				** dummy preferred by AIC not BIC
				
			// Seasonal gap for preferred specification - trig fixed effects
			reg d_price d_cos d_sin if food_group==11, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factorofru=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factorofru)
					tempvar smin
					egen `smin'=min(seas_factorofru)
					gen seasgap_ofru=`smax'-`smin'
					sum(seasgap_ofru)
		* Other veg
			// Model comparisons - 
			eststo olsdum: reg d_price $months_i if food_group==12, vce(cluster market_no)
			eststo olstrig: reg d_price d_cos d_sin if food_group==12, vce(cluster market_no)
			estimates stat olsdum  olstrig 
			
			// Seasonal gap for preferred specification - trig fixed effects
			reg d_price d_cos d_sin if food_group==12, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factoroveg=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factoroveg)
					tempvar smin
					egen `smin'=min(seas_factoroveg)
					gen seasgap_oveg=`smax'-`smin'
					sum(seasgap_oveg)
		* Roots & tubs
			// Model comparisons - 
			eststo olsdum: reg d_price $months_i if food_group==13, vce(cluster market_no)
			eststo olstrig: reg d_price d_cos d_sin if food_group==13, vce(cluster market_no)
			estimates stat olsdum  olstrig 
			** dummy preferred by AIC not BIC
			
			// Seasonal gap for preferred specification - trig fixed effects
			reg d_price d_cos d_sin if food_group==13, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factorrt=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factorrt)
					tempvar smin
					egen `smin'=min(seas_factorrt)
					gen seasgap_rt=`smax'-`smin'
					sum(seasgap_rt)
		* VitA fruit
			// Model comparisons - 
			eststo olsdum: reg d_price $months_i if food_group==15, vce(cluster market_no)
			eststo olstrig: reg d_price d_cos d_sin if food_group==15, vce(cluster market_no)
			estimates stat olsdum  olstrig 
			
			// Seasonal gap 
			reg d_price d_cos d_sin if food_group==15, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factorvtafru=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factorvtafru)
					tempvar smin
					egen `smin'=min(seas_factorvtafru)
					gen seasgap_vtafru=`smax'-`smin'
					sum(seasgap_vtafru)
		* VitA veg
			// Model comparisons - 
			eststo olsdum: reg d_price $months_i if food_group==16, vce(cluster market_no)
			eststo olstrig: reg d_price d_cos d_sin if food_group==16, vce(cluster market_no)
			estimates stat olsdum  olstrig 
			** dummy preferred by AIC, not by BIC
			
			// Seasonal gap 
			reg d_price d_cos d_sin if food_group==16, vce(cluster market_no)		
			scalar alpha=_b[d_cos]
			scalar beta=_b[d_sin]
			scalar lambda=sqrt((alpha^2)+(beta^2))
			scalar omega=atan(alpha/beta)
			gen seas_factorvtaveg=lambda*(cos((month*_pi)/6)-omega)
					tempvar smax
					egen `smax'=max(seas_factorvtaveg)
					tempvar smin
					egen `smin'=min(seas_factorvtaveg)
					gen seasgap_vtaveg=`smax'-`smin'
					sum(seasgap_vtaveg)

	putexcel set "Fig 5", modify
	tabstat seasgap*
	foreach v of varlist seasgap* {
		replace `v'=`v'*100
		}
	lab var seasgap_m1 "Maize grain"
	lab var seasgap_m2 "Admarc maize"
	lab var seasgap_cer "Cereals"
	lab var seasgap_greens "Green leafy veg" 
	lab var seasgap_eggs "Eggs"
	lab var seasgap_fish "Fish"
	lab var seasgap_meat "Meat"
	lab var seasgap_leg "Legumes"
	lab var seasgap_milk "Milk"
	lab var seasgap_oil "Oils"
	lab var seasgap_ofru "Other fruit"
	lab var seasgap_oveg "Other vegetable"
	lab var seasgap_rt "Roots & tubers"
	lab var seasgap_vtafru "Vit A rich fruits"
	lab var seasgap_vtaveg "Vit A rich veg"
	forval fg=3/13 {
		unique food_no if food_group==`fg'
		scalar n`fg'=`r(unique)'
		local cell=`fg'+2
		putexcel E`cell'=n`fg'
	}
	forval fg=15/16 {
		unique food_no if food_group==`fg'
		scalar n`fg'=`r(unique)'
		local cell=`fg'+2
		putexcel E`cell'=n`fg'
	}

	collapse (mean) seasgap*, by(food_group)

	ren seasgap* seasgap#, addnumber
	drop if food_group==14 // sweets
	recode food_group (15=14) (16=15)
	lab def fg 1 "Maize grain" 2 "Admarc maize" 14 "Vitamin A rich fruits" ///
		15 "Vitamin A rich vegetables & tubers" 16 "", modify
	lab val food_group fg
	forval fg=1/15 {
	replace seasgap`fg'=. if food_group!=`fg'
	}

	gen seasgap=.
	order seasgap, after(food_group)
	forval fg=1/15 {
	replace seasgap=seasgap`fg' if food_group==`fg'
	}
	drop seasgap1-seasgap15
	mkmat food_group seasgap, matrix(seasfoodprice)
	putexcel B2=matrix(seasfoodprice), names
	labellist fg // row labels
	save Malawi_FoodPrice_seasonality, replace

**# Bookmark #1
// Table 3 // Affordability
use MalawiIHPS_DietQualCost_CoNAiCoNAseries_r, clear
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	lab var reside "Rural (%)"
	drop if reside!=1
	drop if year<2013
	tab CoNA_solution
	egen solutions_peryear=total(CoNA_solution), by(case_id HHID y2_hhid y3_hhid year)
	tab solutions_peryear
	unique hhxround if solutions_peryear==0, by(year) gen(n_nosolutions_year) d
	unique hhxround, by(year) gen(n_hhs_year)
	egen isolutions_peryear=total(iCoNA_solutionallHH), by(case_id HHID y2_hhid y3_hhid data_round year)
	tab isolutions_peryear
	lab var isolutions_peryear "Number of solutions to individualized diets per year)
	unique hhxround if isolutions_peryear==0, by(year) gen(in_nosolutions_year) 
	replace iCoNA_HH_pd=. if iCoNA_solutionallHH==0
	gen iCoNA_HH_pd_ppp_percap=iCoNA_HH_pd_ppp/hhsize
	gen CoNA_sharing_pd_ppp_percap=CoNA_sharing_pd_ppp/hhsize
	
	gen iCoNA_HH_pd_ppp_perkkcal=(iCoNA_HH_pd_ppp/KCAL_hh_perday)*1000
	gen CoNA_sharing_pd_ppp_perkkcal=(CoNA_sharing_pd_ppp/KCAL_hh_perday)*1000

	tab date_consump_MY
	cap drop dateconsumpyear
	gen dateconsumpyear1=dofm(date_consump_MY)
	gen dateconsumpyear=yofd(dateconsumpyear1)
	format dateconsumpyear %ty
	tab dateconsumpyear
	keep if year==dateconsumpyear
	keep case_id HHID y2_hhid y3_hhid data_round month CoNA_sharing market_no ///
		date_consump dateMY date_consump_MY hhxround CoNA_solution  ///
		solutions_peryear PPP_month_interpolated CoNA_sharing_pd ///
		CoNA_sharing_month iCoNA_pd iCoNA_month iCoNA_HH_pd  ///
		iCoNA_solutionallHH CoNA_sharing_pd_ppp CoNA_sharing_month_ppp  ///
		iCoNA_pd_ppp iCoNA_month_ppp iCoNA_HH_pd_ppp iCoNA_HH_pd_ppp_percap  ///
		CoNA_sharing_pd_ppp_percap iCoNA_HH_pd_ppp_perkkcal  ///
		CoNA_sharing_pd_ppp_perkkcal KCAL_hh_perday
	egen hhsolperyear_s=count(CoNA_sharing), by(hhxround)
	egen hhsolperyear_i=count(iCoNA_HH_pd), by(hhxround)
	preserve
		collapse (max) hhsolperyear_i hhsolperyear_s (first) KCAL_hh_perday, by(case_id HHID y2_hhid y3_hhid data_round)
		tempfile sols
			save `sols', replace
	restore
	merge m:1 case_id HHID y2_hhid y3_hhid data_round using `sols'
		drop _merge
	keep if dateMY==date_consump_MY
	tempfile cost
		save `cost'
		
**# Bookmark #2
	* Relative ratios
	use MalawiIHPS_DietQualCost_HH_r, clear
	drop *CoNA*
	drop if data_round==1
	drop if reside==0
	lab def year 1 "2010" 2 "2013" 3 "2016/17"
	lab val data_round year
	merge 1:1 case_id HHID y2_hhid y3_hhid data_round using `cost'
		*150 unmatched = urban district coded as rural household and 4 in Neno district
	drop if _merge==1
	drop _merge
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	foreach v in totalexp_food totalexpenditure {
			gen `v'_ppp=.
			replace `v'_ppp=`v'/PPP
		}
		
	lab var totalexp_food_ppp "Annual food expenditure"
	lab var totalexpenditure_ppp "Annual total expenditure"
	gen totalexp_food_ppp_pd=(totalexp_food_ppp/365)
	gen totalexpenditure_ppp_pd=(totalexpenditure_ppp/365)
	lab var totalexp_food_ppp_pd "Per day (food)"
	lab var totalexpenditure_ppp_pd "Per day (total)"

	foreach v in CoNA_solution iCoNA_solutionallHH {
		replace `v'=`v'*100
		}
	lab var CoNA_solution "Households with available diet in month of survey (%)"
	lab var iCoNA_solutionallHH "Households with available diet in month of survey (%)"

	* Feasibility in month of survey
	eststo sum6: svy, subpop(if data_round==2 & reside==1 & market_no!=.): mean CoNA_solution iCoNA_solutionallHH
	eststo sum7: svy, subpop(if data_round==3 & reside==1 & market_no!=.): mean CoNA_solution iCoNA_solutionallHH
	eststo sum8: svy, subpop(if data_round!=1 & reside==1 & market_no!=.): mean CoNA_solution iCoNA_solutionallHH
	esttab sum6 sum7 sum8 using "Table 5.rtf", ///
		label replace cells("b(fmt(2)) se(par)") mlabels("2013" "2016/17" "Overall") ///
		nonumbers wide collabels("Mean" "(SE)") 
	* Cost in month of survey
	foreach v in iCoNA_HH_pd_ppp iCoNA_HH_pd_ppp_percap iCoNA_HH_pd_ppp_perkkcal {
		replace `v'=. if iCoNA_solutionallHH==0
		}
	global costs iCoNA_HH_pd_ppp iCoNA_HH_pd_ppp_percap iCoNA_HH_pd_ppp_perkkcal ///
		CoNA_sharing_pd_ppp CoNA_sharing_pd_ppp_percap CoNA_sharing_pd_ppp_perkkcal
	unique y2_hhid if iCoNA_solutionallHH==100 & data_round==2
	unique y3_hhid if iCoNA_solutionallHH==100 & data_round==3
	unique hhxround if iCoNA_solutionallHH==100
	
	foreach v in $costs {
		eststo sum6: epctile `v', svy p(50) subpop(if data_round==2 & reside==1 & market_no!=.)
		eststo sum7: epctile `v', svy p(50) subpop(if data_round==3 & reside==1 & market_no!=.) 
		eststo sum8: epctile `v', svy p(50) subpop(if data_round!=1 & reside==1 & market_no!=.)
	esttab sum6 sum7 sum8 using "Table 5.rtf", ///
		label append cells("b(fmt(2)) se(par)") mlabels("2013" "2016/17" "Overall") ///
		nonumbers wide collabels("Median" "(SE)") 
	}

	* Generate percent spending less than CoNA
	gen iCoNA_HH_Food=iCoNA_HH_pd/(totalexp_food/365)
	gen iCoNA_HH_TotalExp=iCoNA_HH_pd/(totalexpenditure/365)
	gen CoNA_Food=CoNA_sharing_pd/(totalexp_food/365)
	gen CoNA_TotalExp=CoNA_sharing_pd/(totalexpenditure/365)
	gen i_cantafford_food=100 if iCoNA_HH_Food>1 & iCoNA_HH_Food!=.
	replace i_cantafford_food=0 if iCoNA_HH_Food<=1 & iCoNA_HH_Food!=.
		tab i_cantafford_food, m
	gen i_cantafford_ttlexp=100 if iCoNA_HH_TotalExp>1 & iCoNA_HH_TotalExp!=.
	replace i_cantafford_ttlexp=0 if iCoNA_HH_TotalExp<=1 & iCoNA_HH_TotalExp!=.
		tab i_cantafford_ttlexp, m
	lab var i_cantafford_food "Population spending less on food than adequate diet cost (%)"
	lab var i_cantafford_ttlexp "Population that cannot afford the adequate diet at all (%)"
	gen s_cantafford_food=100 if CoNA_Food>1 & CoNA_Food!=.
	replace s_cantafford_food=0 if CoNA_Food<=1 & CoNA_Food!=.
		tab s_cantafford_food, m
	gen s_cantafford_ttlexp=100 if CoNA_TotalExp>1 & CoNA_TotalExp!=.
	replace s_cantafford_ttlexp=0 if CoNA_TotalExp<=1 & CoNA_TotalExp!=.
		tab s_cantafford_ttlexp, m
	lab var s_cantafford_food "Population spending less on food than adequate diet cost (%)"
	lab var s_cantafford_ttlexp "Population that cannot afford the adequate diet at all (%)"

	gen CoNA_iCoNAHH=CoNA_sharing_pd/iCoNA_HH_pd
		lab var CoNA_iCoNAHH "Ratio of Daily Household Sharing to Individual Diets CoNA"

	* Relative to food spending
	global hhresults iCoNA_HH_Food iCoNA_HH_TotalExp CoNA_Food CoNA_TotalExp CoNA_iCoNAHH

	* Median
	foreach v in $hhresults {
		eststo sum6: epctile `v', svy p(50) subpop(if data_round==2 & reside==1 & market_no!=.)
		eststo sum7: epctile `v', svy p(50) subpop(if data_round==3 & reside==1 & market_no!=.) 
		eststo sum8: epctile `v', svy p(50) subpop(if data_round!=1 & reside==1 & market_no!=.)
	esttab sum6 sum7 sum8 using "Table 5.rtf", ///
		label append cells("b(fmt(2)) se(par)") mlabels("2013" "2016/17" "Overall") ///
		nonumbers wide collabels("Median" "(SE)") 
	}
	* N households with at least one solution in year of survey
	unique hhxround if hhsolperyear_i==0 & data_round==2
	unique hhxround if hhsolperyear_i==0 & data_round==3
	unique hhxround if hhsolperyear_s==0 & data_round==2
	unique hhxround if hhsolperyear_s==0 & data_round==3

// Affordability support for narrative
	foreach v in i_cantafford_food i_cantafford_ttlexp {
		replace `v'=100 if iCoNA_solutionallHH==0
		}	
	foreach v in s_cantafford_food s_cantafford_ttlexp {
		replace `v'=100 if CoNA_solution==0
		}	
	
	keep case_id HHID y2_hhid y3_hhid data_round pweight ea_id year ///
	reside stratum region i_cantafford_food i_cantafford_ttlexp s_cantafford_food s_cantafford_ttlexp		

	ren i_* *1
	ren s_* *2

	reshape long cantafford_food cantafford_ttlexp, i(case_id HHID y2_hhid y3_hhid data_round) j(scenario)
	lab var scenario ""
	lab drop nutr
	lab def sc 1 "Individualized Diets" 2 "Household Sharing"
	lab val scenario sc

	lab var cantafford_food "Without spending more on food"
	lab var cantafford_ttlexp "At all"

	graph bar cantafford_food cantafford_ttlexp [pw=pweight], over(scenario, label(labsize(medsmall))) ///
		scheme(plotplain) ytitle("% Household who do not have access to a nutrient-adequate diet", size(medsmall)) ///
		yscale(range(0 85)) ytick(#9) ylabel(#8, val) yline(50) yline(25) yline(75) ///
		legend(label(1 "Without spending more on food")  ///
		label(2 "At all") col(2) row(1) ///
		rowgap(*.01) colgap(*.5) size(medsmall) pos(11) ring(0) region(color(none)))
	graph export "Fig 5.png", replace

	* Support for narrative
	svy: mean cantafford_food cantafford_ttlexp, over(scenario)
	
// Supp Table B // Individual diet cost results
use MalawiIHPS_DietQualCost_iCoNA, clear
replace iCoNA_solution=iCoNA_solution*100
drop if inlist(market_no,2,18,22,24) // markets with no households observed in survey
eststo i1: estpost tabstat iCoNA_solution, stats(mean sd) by(age_sex_grp)

eststo i2: estpost tabstat iCoNA_pd_ppp, ///
	stats(p50 sd) by(age_sex_grp)
tempvar popsharepct
	gen `popsharepct'=popshare*100
eststo i5: estpost tabstat `popsharepct' if ///
	inlist(year,2013,2016,2017), stats(mean) by(age_sex_grp)

esttab i5 using "Suppl Table B.rtf", label replace ///
	wide cells("mean(fmt(2))") collab("%") nonumbers noobs mlabel("Population Share") ///
	title("Suppl Table B. Individual Daily Cost of Nutrient Adequacy over 25 markets January 2013-July 2017")
esttab i1 using "Suppl Table B.rtf", label append  ///
	wide cells("mean(fmt(2)) sd(par fmt(2))") ///
	nonumbers noobs mlabel("Months with Solution (%)") ///
	collab("Mean" "(SD)") noconstant ///
	title("Suppl Table B. Individual Daily Cost of Nutrient Adequacy over 25 markets January 2013-July 2017")
esttab i2 using "Suppl Table B.rtf", label append  ///
	wide cells("p50(fmt(2)) sd(par fmt(2))") ///
	nonumbers noobs mlabel("CoNA/day/person (2011 US$)") ///
	collab("Median" "(SD)") noconstant ///
	note("* Reports results when upper bound of protein AMDR is relaxed (increased) by 50% for children 6-36 months." ///
	"Population shares calculated with survey weights from household data. Age-sex groups based on DRI categories, disaggregating 3 year old to combine energy and micronutrient groups.")
* Population weighted average
asgen weightedAvail=iCoNA_solution, w(popshare)
sum weightedAvail
asgen weightedCost=iCoNA_pd_ppp, w(popshare)
sum weightedCost

// Supp Table D // % nutrient expenditure on items in price list
		* Percent of nutrients from items in price list
		use MalawiIHPS_DietQualCost_FoodNUT_HHExp, clear
		drop if year<2013
		svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
		lab var reside "Rural (%)"
		drop if reside!=1
		gen inpricelist=.
			replace inpricelist=1 if inlist(food_no,101,102,103,104,105, ///
				820,106,205,203,831,832,201,202,822,838,301,302,303,308,304, ///
				305,311,312,313,834,833,401,404,408,409,410,402,405,411,403, ///
				602,603,605,606,607,601,508,522,824,503,502,826,5021,5022, ///
				5023,5031,5032,5033,810,111,112,113,827,504,505,825,506,701, ///
				702,501,823,803,801,901,907,910)
			replace inpricelist=0 if inpricelist==.
		gen nutcovered=tot_nutr_byfood_total if inpricelist==1
			replace nutcovered=0 if inpricelist==0
		gen nutNOTcovered=tot_nutr_byfood_total if inpricelist==0
			replace nutNOTcovered=0 if inpricelist==1
			
		collapse (sum) nutcovered nutNOTcovered tot_nutr_byfood_total (first) region year month ///
			ea_id reside market market_no ta_code pweight, ///
			by(case_id HHID y2_hhid y3_hhid date_consump data_round nutr_no)
		ren tot_nutr_byfood_total totalnut
		gen pctnutcovered=(nutcovered/totalnut)*100
		gen pctnutNOTcovered=(nutNOTcovered/totalnut)*100
		svy: mean pctnutcovered pctnutNOTcovered // check
		eststo n22: svy: mean pctnutcovered if nutr_no==2 & data_round==2
		eststo n23: svy: mean pctnutcovered if nutr_no==2 & data_round==3
		eststo n2all: svy: mean pctnutcovered if nutr_no==2
		esttab n22 n23 n2all using "Supp Table D.rtf", replace ///
			label nogaps wide star noobs cells("b(fmt(2)) se(par fmt(3))") ///
			collabels("Coef." "(SE)") ///
			title("Table D. Percent of nutrients supplied by food items included in the retail market food price list") ///
			note("Population statistics corrected using sampling weights." ///
			"Heteroskedasticity robust standard errors clustered at the enumeration area level.")

		forval n=3/23 {
		eststo n`n'2: svy: mean pctnutcovered if nutr_no==`n' & data_round==2
		eststo n`n'3: svy: mean pctnutcovered if nutr_no==`n' & data_round==3
		eststo n`n'all: svy: mean pctnutcovered if nutr_no==`n'
		esttab n`n'2 n`n'3 n`n'all using "Supp Table D.rtf", append ///
			label nogaps wide star noobs cells("b(fmt(2)) se(par fmt(3))") ///
			collabels("Mean" "(SE)") mlabels("2013" "2016/17" "Overall")
		}
		
	* Percent of expenditure from items in price list
		use MalawiIHPS_DietQualCost_FoodHHExp, clear
		drop if year<2013
		svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
		lab var reside "Rural (%)"
		drop if reside!=1
		gen inpricelist=.
			replace inpricelist=1 if inlist(food_no,101,102,103,104,105, ///
				820,106,205,203,831,832,201,202,822,838,301,302,303,308,304, ///
				305,311,312,313,834,833,401,404,408,409,410,402,405,411,403, ///
				602,603,605,606,607,601,508,522,824,503,502,826,5021,5022, ///
				5023,5031,5032,5033,810,111,112,113,827,504,505,825,506,701, ///
				702,501,823,803,801,901,907,910)
			replace inpricelist=0 if inpricelist==.
		tab inpricelist, m
		tab food_no, sum(food_no)
		tab food_no inpricelist
		* percent observations in food list
		di 48276/57450 // 84%

		gen spendingcovered=item_expenditure if inpricelist==1
			replace spendingcovered=0 if inpricelist==0
		gen spendingNOTcovered=item_expenditure if inpricelist==0
			replace spendingNOTcovered=0 if inpricelist==1
		preserve

		collapse (sum) spendingcovered  spendingNOTcovered (first) totalexp_food_weekly region year month ///
			ea_id reside market market_no ta_code pweight, ///
			by(case_id HHID y2_hhid y3_hhid date_consump data_round)
		gen pctexpcovered=(spendingcovered/totalexp_food_weekly)*100
		gen pctexpNOTcovered=(spendingNOTcovered/totalexp_food_weekly)*100
		svy: mean pctexpcovered pctexpNOTcovered // check
		eststo exp2: svy: mean pctexpcovered if data_round==2
		eststo exp3: svy: mean pctexpcovered if data_round==3
		eststo expall: svy: mean pctexpcovered
		esttab exp2 exp3 expall using "Supp Table D.rtf", append ///
			label nogaps wide star noobs cells("b(fmt(2)) se(par fmt(3))") ///
			collabels("Mean" "(SE)") mlabels("2013" "2016/17" "Overall")
		restore
		* % Spending on items not covered in food list
		gen shareexponuncovered=spendingNOTcovered/totalexp_food_weekly
		collapse (median) shareexponuncovered [pw=pweight], ///
			by(case_id HHID y2_hhid y3_hhid date_consump data_round food_no)
		tab food_no, sum(shareexponuncovered)
		unique case_id data_round
			di 2137*0.05
			* foods purchased by more than 5% of households: 
			* 	sorghum, orange sp, soyabean flour, other wild greens, smoked fish, 
			*	wild fruit, yeast, chips, samosa, alcohol

