capture log close
clear all
set more off

* ==== SET DIRECTORY & GLOBAL ==== *
cd 			"C:\Users\\`c(username)'\OneDrive - Australian National University\School\ANU\3a Master Thesis"
gl iraw 	"2 Data\3 IFLS\2 Raw"
gl ipro 	"2 Data\3 IFLS\3 Processed\2 Child Labor"
gl ifin 	"2 Data\3 IFLS\4 Final\2 Child Labor"
gl ouw 		"2 Data\5 Our World in Data"
gl dsc 		"2 Data\6 Descriptive Statistics"
gl lg 		"4 Log\2 Child Labor"
gl tb 		"5 Tables\2 Child Labor"
gl gp 		"6 Graph\2 Child Labor"

forval i = 1/5 {
	gl ifls`i' "$iraw\IFLS`i'"
}

gl cov male2 islam2 javanese2 bwin_formalwork electric filtered_water owntoilet hhsize_ch
gl eduhealth atdshs2 gradshs2 missedact_poorhealth depsymptoms
gl labovar employedpw wformal wcontract earningspy

*********************************
***** PART I: DATA CLEANING *****
*********************************

* ==== ROSTER ==== *
loc w4 "07"
loc w5 "14"

forval i=4/5 {
	* prepare ptrack data
	use "${ifls`i'}\ptrack.dta", clear
	if `i'==5 {
		la val bth_month . 		
	}
	else if `i'==4 {
		ren bth_mnth bth_month
	}
	duplicates drop pidlink, force
	loc var hhid`w`i'' pid`w`i'' pidlink age_`w`i'' sex 
	keep `var' bth* 
	save "$ipro\ifls`i'_ptrack.dta", replace 
	
	* prepare ar data
	use "${ifls`i'}\bk_ar1.dta", clear
	drop if inlist(ar01a,0,3) // dead or no longer in household
	
	* randomization to remove duplicate pidlink
	duplicates tag pidlink, gen(dup)
	set seed 821280345
	isid hhid`w`i'' pid`w`i''
	sort hhid`w`i'' pid`w`i''
	g rand=runiform()
	sort rand dup
	drop if rand<rand[_n+1] & dup>0
	duplicates report pidlink
	
	* merging ar with ptrack data
	di as res _dup(59) "-" _n ///
	"merging roster IFLS`i' with ptrack" _n ///
	_dup(59) "-"
	merge 1:1 pidlink using "$ipro\ifls`i'_ptrack.dta"
	drop if _merge==2

	* constructing age and sex variable
	clonevar age=age_`w`i''
	replace age=ar09 if age==.
	replace age=. if inlist(age,998,999) 
	replace sex=ar07 if sex==.
	
	* constructing birth date
	qui ds bth_month ar08mth bth_day ar08day
	foreach x of varlist `r(varlist)' {
		replace `x'=. if inlist(`x',98,99)
	}
	foreach x of varlist bth_year ar08yr {
		replace `x'=. if inlist(`x',9998,9999)
	}
	replace bth_month=ar08mth if bth_month==.
	replace bth_year=ar08yr if bth_year==. 
	replace bth_day=ar08day if bth_day==.
	
	preserve
	drop if bth_year<. & bth_month<.
	tempfile temp
	save `temp', replace 
	restore 
	
	keep if bth_year<. & bth_month<.

	foreach x in month day {
		g sd`x'=bth_`x'<10
		tostring bth_`x', replace
		replace bth_`x'="" if bth_`x'=="."
		forval j=1/9 {
			replace bth_`x'=subinstr(bth_`x',"`j'","0`j'",.) if sd`x'==1
		}
	}

	g slash1="/"
	g slash2="/"

	egen bd_YM=concat(bth_year slash1 bth_month) 
	egen bd_YMD=concat(bth_year slash1 bth_month slash2 bth_day) if bth_day~=""	

	g daysto_pol03=date("25/03/2003","DMY")
	g daysto_pol98=date("01/10/1998","DMY")
	g daysto_pol02=date("13/08/2002","DMY")
	
	loc lYM 	"(year and month)"
	loc lYMD 	"(full date)"
	loc l98 	15
	loc l03 	18
	loc l02 	15
	
	* constructing running and itt variable 
	foreach x in YM YMD {
		g daysto_bd_`x'=date(bd_`x',"`x'")	
		clonevar bd_`x'_num=bd_`x'
		replace bd_`x'_num=subinstr(bd_`x'_num,"/","",.)
		destring bd_`x'_num, replace	
		foreach y of varlist bd_`x' bd_`x'_num {
			la var `y' "Birth date `l`x''" 			
		}

		foreach j in 98 03 02 {
			g agebypol`j'_`x' = (daysto_pol`j' - daysto_bd_`x')/365 if daysto_bd_`x'>=0
			replace agebypol`j'_`x' = (daysto_pol`j' + (daysto_bd_`x'*-1))/365 if daysto_bd_`x'<0
			g runvar_abp`j'_`x' = (agebypol`j'_`x' - `l`j'')*-1
			g itt_abp`j'_`x'=1 if runvar_abp`j'_`x'>=0
			replace itt_abp`j'_`x'=0 if runvar_abp`j'_`x'<0

			if "`x'"=="YM" {
				g bd`j'_`x' = bd_`x'_num+`l`j''00
				if `j'==98 {
					g runvar_bd`j'_`x' = bd`j'_`x'-199810
				}
				else if "`j'"=="03" {
					g runvar_bd`j'_`x' = bd`j'_`x'-200303					
				}
				else if "`j'"=="02" {
					g runvar_bd`j'_`x' = bd`j'_`x'-200208	
				}
			}
			else if "`x'"=="YMD" {
				g bd`j'_`x' = bd_`x'_num+`l`j''0000
				if `j'==98 {
					g runvar_bd`j'_`x' = bd`j'_`x'-19981001
				}
				else if "`j'"=="03" {
					g runvar_bd`j'_`x' = bd`j'_`x'-20030325					
				}
				else if "`j'"=="02" {
					g runvar_bd`j'_`x' = bd`j'_`x'-20020813	
				}
			}	
		
			g itt_bd`j'_`x'=1 if runvar_bd`j'_`x'>=0
			replace itt_bd`j'_`x'=0 if runvar_bd`j'_`x'<0

			labvars runvar_abp`j'_`x' 			"Age by `j' policy normalized `l`x''" ///
					itt_abp`j'_`x' 				"Intent-to-treat `j' based on age by policy `l`x''" ///
					agebypol`j'_`x' 			"Age by `j' policy `l`x''" ///
					bd`j'_`x'					"Birthday in `j' `l`x''" ///
					runvar_bd`j'_`x' 			"Birthday in `j' normalized `l`x''" ///
					itt_bd`j'_`x' 				"Intent-to-treat `j' based on birthday `l`x''" ///
					, alternate
		}
	}
	
	* 15th and 18th birthday
	forval j =12(3)18 {
		g bth`j'yr=bth_year+`j'		
	} 
	
	append using `temp', force
	
	keep `var' age* runvar* itt* bth*yr bth_year bd* ar02b ar15d ar15 ar16 ar17
	drop age_`w`i''

	save "$ipro\ifls`i'_ar1.dta", replace
}

* ==== EMPLOYMENT HISTORY ==== *

/* 
IFLS1 
- work status --> tk35 --> label as IFLS5

IFLS2
- work status --> tk33
emptyp:
           1 01. Self-emp
           4 04. Govt wkr
           5 05. Pvt wkr
           6 06. Fam wkr
          98 98. DK
          99 99. Missing

- sector --> tk32ind
indus:
           0 0.Insuffic information
           1 1.Agriculture
           2 2.Mining,quarrying
           3 3.Manufacturing
           4 4.Electricity,gas,water
           5 5.Construction
           6 6.Wholesale,retail,hote
           7 7.Transport,communicati
           8 8.Finance/insur,real es
           9 9.Community,psnl servic

IFLS3
- work status --> tk33 --> label as IFLS5         
- sector --> tk31aa0 --> label as IFLS2

IFLS4
- work status --> tk33 --> label as IFLS5         
- sector --> tk31a --> label as IFLS2

IFLS5
- work status --> tk33
- sector --> tk31a

*/

loc w1 "93"
loc w2 "97"
loc w3 "00"
loc w4 "07"
loc w5 "14"

* data per wave
forval i=1/5 {
	loc tk1 tk28
	if `i'==1 {
		use "${ifls`i'}\buk3tk3.dta", clear
		g informal=inlist(tk35,1,2,7,8)
		g formal=inlist(tk35,3,4,5)
		g unpaidwk=tk35==6
		g paidwk=inlist(tk35,1,2,3,4,5,7,8)
		foreach x of varlist informal formal unpaidwk paidwk {
			replace `x'=. if inlist(tk35,98,.)
		}
		loc tk2 `tk1' informal formal unpaidwk paidwk
		keep hhid`w`i'' pid`w`i'' pidlink `tk2' year
		reshape wide `tk2', i(hhid`w`i'' pid`w`i'') j(year)			
	}
	else {
		use "${ifls`i'}\b3a_tk3.dta", clear
		g informal=inlist(tk33,1,2,7,8)
		g formal=inlist(tk33,3,4,5)
		g unpaidwk=tk33==6
		g paidwk=inlist(tk33,1,2,3,4,5,7,8)
		foreach x of varlist informal formal unpaidwk paidwk {
			replace `x'=. if inlist(tk33,9,98,.)
		}
		if inlist(`i',2,3) {
			if `i'==2 {
				clonevar wsec=tk32ind
				recode wsec (7 8 9 = 4) (0 = .)
			}
			else if `i'==3 {
				clonevar wsec=tk31aa0
				replace wsec="" if wsec=="X"
				destring wsec, replace
				recode wsec (7 8 9 = 4) (0 = .)
			}
			loc tk2 `tk1' formal informal unpaidwk paidwk wsec 				
			keep hhid`w`i'' pid`w`i'' pidlink `tk2' tk28yr
			reshape wide `tk2', i(hhid`w`i'' pid`w`i'') j(tk28yr)		
		}
		else if inlist(`i',4,5) {
			if `i'==4 {
				destring tk31a, replace
			}
			clonevar wsec=tk31a
			recode wsec (7 8 9 10 = 4) (95 0 = .)
			loc tk2 `tk1' formal informal unpaidwk paidwk wsec 	
			keep hhid`w`i'' pid`w`i'' pidlink `tk2' tk28year
			reshape wide `tk2', i(hhid`w`i'' pid`w`i'') j(tk28year)
			qui ds
			loc var `r(varlist)'
			merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b3a_tk1.dta"
			drop if _merge==2
			replace tk2820`w`i''=1 if tk01a==1
			keep `var' tk05
		}	
		la def sector 	1 "1 Agriculture, forestry, fishing and hunting" ///
						2 "2 Mining and quarrying" ///
						3 "3 Manufacturing" ///
						4 "4 Services" ///
						5 "5 Construction" ///
						6 "6 Trading, restaurants, hotels" 
		la val wsec* sector	
	}
	save "$ipro\ifls`i'_tk3.dta", replace 
}

* ==== CHILD WORKING HISTORY ==== *
loc w4 "07"
loc w5 "14"

forval i=4/5 {
	use "${ifls`i'}\b5_dla6.dta", clear
	drop if dla2type==4
	recode dla56a (9=.)
	keep dla56a hhid`w`i'' pid`w`i'' dla2type
	reshape wide dla56a, i(hhid`w`i'' pid`w`i'') j(dla2type)
	egen dla56a=rowmin(dla56a*)
	g clpaid=dla56a1==1
	replace clpaid=. if dla56a1==.
	g clunpaid=inlist(1,dla56a2,dla56a3)
	replace clunpaid=. if dla56a2==. & dla56a3==.
	keep hhid`w`i'' pid`w`i'' dla56a clpaid clunpaid
	save "$ipro\ifls`i'_dla6.dta", replace 
}

* ==== CRISIS & RECESSION DATA ==== *
import delimited "$ouw\gdp-maddison-project-database.csv", clear 
keep if entity=="Indonesia"
g growth=(gdp[_n]-gdp[_n-1])/gdp[_n-1]
g crisisyr=growth<0
keep year crisisyr
save "$ouw\indonesia-crisis-years.dta", replace

* ==== EDUCATION HISTORY ==== *
loc w4 "07"
loc w5 "14"

forval i=4/5 {
	use "${ifls`i'}\b3a_dl4.dta", clear
	drop if dl4type==4
	loc dl dl11a dl11c dl11d dl13 dl14a
	keep `dl' hhid`w`i'' pid`w`i'' dl4type
	reshape wide `dl', i(hhid`w`i'' pid`w`i'') j(dl4type)
	loc dl dl*
	
	foreach x of varlist dl11a* {
		recode `x' (9999=.)
	}
	g nvrinsch=dl11a1==. & dl11a3==. & dl11a3==.
	
	g dropout=1 if inlist(3,dl11d1,dl11d2,dl11d3)
	replace dropout=0 if !inlist(3,dl11d1,dl11d2,dl11d3) & nvrinsch==0
	
	g stop_schooling=.
	replace stop_schooling=1 if dl11a2==. & (dl11c1==7|dl11d1==1)
	replace stop_schooling=1 if dl11a3==. & (dl11c2==7|dl11d2==1)
	replace stop_schooling=0 if !inlist(.,dl11a1,dl11a2,dl11a3)
	
	g tempabsent=1 if inlist(1,dl14a1,dl14a2,dl14a3)
	replace tempabsent=0 if !inlist(1,dl14a1,dl14a2,dl14a3) & nvrinsch==0
	
	g failgrade=1 if inlist(1,dl131,dl132,dl133)
	replace failgrade=0 if !inlist(1,dl131,dl132,dl133) & nvrinsch==0
	
	drop `dl' nvrinsch
	save "$ipro\ifls`i'_dl4.dta", replace
}

* ==== CHILD EDUCATION HISTORY ==== *
loc w4 "07"
loc w5 "14"

forval i=4/5 {
	use "${ifls`i'}\b5_dla2.dta", clear
	loc dla dla71a dla71c dla71d dla73 dla74a
	keep `dla' hhid`w`i'' pid`w`i'' dlatype
	if `i'==4 {
		reshape wide `dla', i(hhid`w`i'' pid`w`i'') j(dlatype) string		
	}
	else {
		reshape wide `dla', i(hhid`w`i'' pid`w`i'') j(dlatype)		
	}
	loc dla dla*

	foreach x of varlist dla71a* {
		recode `x' (9998=.)
	}
	g nvrinsch=dla71a1==. & dla71a2==. & dla71a3==.

	g dropout=1 if inlist(3,dla71d1,dla71d2,dla71d3)
	replace dropout=0 if !inlist(3,dla71d1,dla71d2,dla71d3) & nvrinsch==0
	
	g stop_schooling=.
	replace stop_schooling=1 if dla71a2==. & (dla71c1==7|dla71d1==1)
	replace stop_schooling=1 if dla71a3==. & (dla71c2==7|dla71d2==1)
	replace stop_schooling=0 if !inlist(.,dla71a1,dla71a2,dla71a3)
	
	g tempabsent=1 if inlist(1,dla74a1,dla74a2,dla74a3)
	replace tempabsent=0 if !inlist(1,dla74a1,dla74a2,dla74a3) & nvrinsch==0
	
	g failgrade=1 if inlist(1,dla731,dla732,dla733)
	replace failgrade=0 if !inlist(1,dla731,dla732,dla733) & nvrinsch==0
	
	drop `dla' nvrinsch
	save "$ipro\ifls`i'_dla2.dta", replace
}

* ==== CHRONIC CONDITIONS & ILLNESS ==== *
loc w4 "07"
loc w5 "14"

forval i=4/5 {
	use "${ifls`i'}\b3b_cd2.dta", clear
	bys hhid`w`i'' pid`w`i'': egen chroniccond=min(cd01)
	duplicates drop hhid`w`i'' pid`w`i'', force
	keep  hhid`w`i'' pid`w`i'' chroniccond
	recode chroniccond (3=0) (8=.)
	save "$ipro\ifls`i'_cd2.dta", replace 

	use "${ifls`i'}\b3b_cd3.dta", clear
	bys hhid`w`i'' pid`w`i'': egen chronicill=min(cd05)
	duplicates drop hhid`w`i'' pid`w`i'', force
	keep  hhid`w`i'' pid`w`i'' chronicill
	recode chronicill (3=0) (8=.) (9=.)
	save "$ipro\ifls`i'_cd3.dta", replace 
}

* ==== MENTAL HEALTH ==== *
loc w4 "07"
loc w5 "14"

forval i=4/5 {
	use "${ifls`i'}\b3b_kp.dta", clear
	loc kp kp02
	recode kp02 (1=0) (2=1) (3=2) (4=3)
	keep `kp' hhid`w`i'' pid`w`i'' kptype
	reshape wide `kp', i(hhid`w`i'' pid`w`i'') j(kptype) string
	foreach x of varlist kp02E kp02H {
		recode `x' (3=0) (2=1) (1=2) (0=3)
	}
	egen depscore=rowtotal(kp02*)
	g depsymptoms=depscore>=10
	drop kp02*
	save "$ipro\ifls`i'_kp.dta", replace 
}

* ==== MARRIAGE HISTORY ==== *
loc w4 "07"
loc w5 "14"

forval i=4/5 {
	use "${ifls`i'}\b3a_kw3.dta", clear
	if `i'==4 {
		destring pid`w`i'', replace
	}
	g childmarriage=kw11<18
	replace childmarriage=. if inlist(kw11,.,98,99)
	bys hhid`w`i'' pid`w`i'': egen firstmy=min(kw10yr)
	keep if kw10yr==firstmy | childmarriage<.
	duplicates drop hhid`w`i'' pid`w`i'', force
	keep hhid`w`i'' pid`w`i'' childmarriage firstmy
	save "$ipro\ifls`i'_kw3.dta", replace
}

* ==== MERGE ALL ==== *
loc w4 "07"
loc w5 "14"

forval i=4/5 {
	use "$ipro\ifls`i'_ar1.dta", clear
	qui ds
	loc var01 `r(varlist)'

	forval j=2/`i' {
		di as res _dup(59) "-" _n ///
		"merging roster IFLS`i' with tk3 IFLS`j'" _n ///
		_dup(59) "-"
		if `j'==`i' {
			merge 1:1 hhid`w`i'' pid`w`i'' using "$ipro\ifls`j'_tk3.dta"			
		}
		else {
			merge 1:1 pidlink using "$ipro\ifls`j'_tk3.dta"			
		}
		drop if _merge==2
		loc var02 `var01' tk28* formal* informal* unpaidwk* paidwk* wsec* 
		keep `var02' 
	}

	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with dla6" _n ///
	_dup(59) "-"
	* some children were not born at the time of the policy introduction
	* hence the unmatched from using
	merge 1:1 hhid`w`i'' pid`w`i'' using "$ipro\ifls`i'_dla6.dta"
	drop if _merge==2
	drop _merge 
	qui ds 
	loc var03 `r(varlist)'

	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with tk2" _n ///
	_dup(59) "-"
	merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b3a_tk2.dta"
	drop if _merge==2
	loc var04 `var03' tk24a tk25a1 tk25a2 tk25a2b tk26a1 tk26a3 ///
	tk24b tk25b1 tk25b2 tk25b2b tk26b1 tk26b3 tk24a5 tk24b5 tk23a4 tk23b4 ///
	tk21a tk22a tk21b tk22b tk23a tk23b  
	keep `var04'

	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with kr" _n ///
	_dup(59) "-"
	merge m:1 hhid`w`i'' using "${ifls`i'}\b2_kr.dta"
	drop if _merge==2
	loc var05 `var04' kr11 kr13 kr20 
	keep `var05'
	
	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with b3a cov" _n ///
	_dup(59) "-"
	foreach x of varlist sex age {
		clonevar `x'1=`x'
	}
	merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b3a_cov.dta", update replace
	drop if _merge==2
	loc var06 `var05' dob_day dob_mth dob_yr sex1 age1
	keep `var06'
	
	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with b5 cov" _n ///
	_dup(59) "-"
	merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b5_cov.dta", update replace
	drop if _merge==2
	keep `var06'

	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with tk1" _n ///
	_dup(59) "-"
	merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b3a_tk1.dta"
	drop if _merge==2
	loc var07 `var06' tk01a tk02
	keep `var07'

	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with tr" _n ///
	_dup(59) "-"
	merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b3a_tr.dta"
	drop if _merge==2
	loc var08 `var07' tr12 
	keep `var08'

	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with dl1" _n ///
	_dup(59) "-"
	merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b3a_dl1.dta"
	drop if _merge==2
	loc var09 `var08' dl01f dl06 dl07
	keep `var09'

	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with dl1" _n ///
	_dup(59) "-"
	merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b5_dla1.dta"
	drop if _merge==2
	loc var10 `var09' dla01 dla08 dla09
	keep `var10'

	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with kk1" _n ///
	_dup(59) "-"
	merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b3b_kk1.dta"
	drop if _merge==2
	loc var11 `var10' kk01 kk02a kk02b kk02k
	keep `var11'

	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with maa" _n ///
	_dup(59) "-"
	merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b5_maa1.dta"
	drop if _merge==2
	loc var12 `var11' maa0a maa0b maa0c
	keep `var12'
	
	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with crisis data" _n ///
	_dup(59) "-"
	clonevar year=bth_year
	merge m:1 year using "$ouw\indonesia-crisis-years.dta"
	drop if _merge==2
	drop _merge year
	ren crisisyr bth0_crisis
	replace bth0_crisis=1 if inrange(bth_year,1942,1948)

	clonevar year=bth12yr
	merge m:1 year using "$ouw\indonesia-crisis-years.dta"
	drop if _merge==2
	drop _merge year
	ren crisisyr bth12_crisis
	replace bth12_crisis=1 if inrange(bth12yr,1942,1948)
	
	loc ddl4 "education history"
	loc ddla2 "child education history"
	loc dcd2 "chronic condition"
	loc dcd3 "chronic illness"
	loc dkp "mental health"
	loc dkw3 "marital history"
	
	foreach x in dl4 dla2 cd2 cd3 kp kw3 {
		di as res _dup(59) "-" _n ///
		"merging main dataset IFLS`i' with `d`x''" _n ///
		_dup(59) "-"
		merge 1:1 hhid`w`i'' pid`w`i'' using "$ipro\ifls`i'_`x'.dta"
		drop if _merge==2
		drop _merge	
	}

	if `i'==5 {
		di as res _dup(59) "-" _n ///
		"merging main dataset IFLS`i' with sa" _n ///
		_dup(59) "-"
		qui ds 
		loc vara `r(varlist)'
		merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b3b_sa.dta"
		drop if _merge==2		
		loc varb `vara' sa02 sa03 sa04 sa05 sa06 sa09 sa10 sa11 sa13
		keep `varb'

		di as res _dup(59) "-" _n ///
		"merging main dataset IFLS`i' with eh" _n ///
		_dup(59) "-"
		merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b3b_eh.dta"
		drop if _merge==2
		loc varc `varb' eh01 eh03 eh04 eh05
		keep `varc'
		
		di as res _dup(59) "-" _n ///
		"merging main dataset IFLS`i' with cob" _n ///
		_dup(59) "-"
		merge 1:1 hhid`w`i'' pid`w`i'' using "${ifls`i'}\b3b_cob.dta"
		drop if _merge==2
		loc vard `varc' w_abil
		keep `vard'
	}	
	
	ren (sex age) (sex2 age2)
	recode age2 (998=.)

	foreach x of varlist age1 age2 {
		g `x'sq=`x'^2
	}

	** merged data
	save "$ipro\ifls`i'_merged.dta", replace 

	** finding timing of start working
	keep hhid`w`i'' pid`w`i'' tk28* formal* informal* unpaidwk* paidwk* wsec*
	loc tk tk28 formal informal unpaidwk paidwk wsec
	reshape long `tk', i(hhid`w`i'' pid`w`i'') j(year)
	bys hhid`w`i'' pid`w`i'': egen startwork=min(year) if tk28==1
	foreach x of varlist formal informal unpaidwk paidwk {
		bys hhid`w`i'' pid`w`i'': egen startwork_`x'=min(year) if `x'==1		
	}
	qui levelsof wsec
	foreach x in `r(levels)' {
		bys hhid`w`i'' pid`w`i'': egen startwork_sec`x'=min(year) if wsec==`x'	
	}
	save "$ipro\ifls`i'_tk3_mergedindv", replace 

	keep if startwork<.
	duplicates drop hhid`w`i'' pid`w`i'', force 
	keep hhid`w`i'' pid`w`i'' startwork*
	merge 1:1 hhid`w`i'' pid`w`i'' using "$ipro\ifls`i'_merged.dta", nogen
	drop tk28* formal* informal* unpaidwk* paidwk* wsec*

	** final merged data
	save "$ipro\ifls`i'_prefin.dta", replace

	** creating new variables	
	g compschooling=bth15yr>1993
	g islam1=ar15==1
	replace islam1=. if inlist(ar15,99,.)
	clonevar islam2=islam1
	replace islam2=1 if tr12==1
	replace islam2=0 if inrange(tr12,2,6)
	g javanese1=ar15d==1
	replace javanese1=. if inlist(javanese1,98,99,.)
	clonevar javanese2=javanese1
	replace javanese2=1 if regexm(dl01f,"A")
	replace javanese2=0 if !regexm(dl01f,"A")
	replace dl01f=strtrim(dl01f)
	replace javanese2=. if dl01f==""
	forval j=1/2 {
		g male`j'=sex`j'==1
		replace male`j'=. if sex`j'==.	
	}

	g healthy1=inrange(maa0a,1,2)|inrange(kk01,1,2)
	replace healthy1=. if maa0a==. & kk01==.
	g healthy2=inrange(kk02k,1,2)
	replace healthy2=. if kk02k==.
	clonevar daysmissed_poorhealth=kk02a
	replace daysmissed_poorhealth=maa0b if daysmissed_poorhealth==.
	clonevar daysinbed_poorhealth=kk02b
	replace daysinbed_poorhealth=maa0c if daysinbed_poorhealth==. 
	foreach x of varlist days* {
		replace `x'=. if `x'>31		
	}

	g missedact_poorhealth=daysmissed_poorhealth>0
	replace missedact_poorhealth=. if daysmissed_poorhealth==.
	g inbed_poorhealth=daysinbed_poorhealth>0
	replace inbed_poorhealth=. if daysinbed_poorhealth==.

	loc luni "university"
	loc lshs "senior high school"
	// loc ljhs "junior high school"
	// loc lprim "primary school"

	g atduni1=inlist(ar16,60,61,62,63,13)
	g atdshs1=inlist(ar16,5,6,15,74) | atduni==1
	g gradshs1=(inlist(ar16,5,6,15,74) & ar17==7) | atduni1==1
	// g atdjhs=inlist(ar16,3,4,12,73) | inlist(1,atdshs,atduni)
	// g atdprim=inlist(ar16,2,11,72) | inlist(1,atdjhs,atdshs,atduni)

	g atduni2=inlist(dl06,60,61,62,63,13)
	g atdshs2=inlist(dl06,5,6,15,74) | inlist(dla08,5,6,15,74) | atduni2==1
	g gradshs2=((inlist(dl06,5,6,15,74) & dl07==7)|(inlist(dla08,5,6,15,74) & dla09==7)) | atduni2==1

	foreach x in uni shs {
		forval j=1/2 {
			la var atd`x'`j' "Attended `l`x''"
			if "`x'"=="shs" {
				la var grad`x'`j' "Graduated `l`x''"		
			}
		}
	}

	g employedpw=inlist(1,tk01a,tk02)
	replace employedpw=. if tk01a==.
	g laborunion=inlist(1,tk23a4,tk23b4)
	g wformal=inrange(tk24a,3,5)|inrange(tk24b,3,5)
	g wcontract=inrange(tk24a5,1,2)|inrange(tk24b5,1,2)
	egen earningspm=rowtotal(tk25a1 tk26a1 tk25b1 tk26b1)
	egen earningspy=rowtotal(tk25a2 tk25a2b tk26a3 tk25b2 tk25b2b tk26b3)
	foreach x of varlist earnings* {
		g ln`x'=log(`x')
// 		replace ln`x'=0.00001 if `x'==0
	}
	
	forval j=1/2 {
		egen whpw`j'=rowmean(tk2`j'a tk2`j'b)
		recode whpw`j' (.=0)
		g earningsph`j'=earningspm/(whpw`j'*4)
		recode earningsph`j' (.=0)
		g lnearningsph`j'=log(earningsph`j')
// 		replace lnearningsph`j'=0.00001 if earningsph`j'==0
	}
	
	foreach x of varlist laborunion wformal wcontract {
		replace `x'=. if employedpw==.		
	}

	*************************************
	*** length of schooling (from ar) ***
	*************************************

	* no schooling (highest level attended kindergarten = no schooling)
	g length_schooling1=0 if inlist(ar16,1,90)  

	* primary school (did not finish first grade = 1 year)
	* assume pesantren and slb is primary level
	replace length_schooling1=1 if inlist(ar16,2,11,72,14,17) & inrange(ar17,0,1)
	forval j =2/5 {
		replace length_schooling1=`j' if inlist(ar16,2,11,72,14,17) & ar17==`j'
	}
	replace length_schooling1=6 if inlist(ar16,2,11,72,14,17) & inrange(ar17,6,7)

	* junior high (did not finish first grade = 7 year)
	replace length_schooling1=7 if inlist(ar16,3,4,12,73) & inrange(ar17,0,1)
	replace length_schooling1=8 if inlist(ar16,3,4,12,73) & ar17==2
	replace length_schooling1=9 if inlist(ar16,3,4,12,73) & inlist(ar17,3,7)

	* senior high (did not finish first grade = 10 year)
	replace length_schooling1=10 if inlist(ar16,5,6,15,74) & inrange(ar17,0,1)
	replace length_schooling1=11 if inlist(ar16,5,6,15,74) & ar17==2
	replace length_schooling1=12 if inlist(ar16,5,6,15,74) & inlist(ar17,3,7)

	* university 
	* did not finish first grade = 13 year
	* bachelor degree assumed to never take diploma previously
	* grade>4 = grade=4 for uni
	* grade>3 = grade=3 for uni
	replace length_schooling1=13 if inlist(ar16,13,60,61) & inrange(ar17,0,1)
	forval j =2/4 {
		replace length_schooling1=`j'+12 if inlist(ar16,13,61) & ar17==`j'
	}
	forval j =2/3 {
		replace length_schooling1=`j'+12 if ar16==60 & ar17==`j'
	}
	replace length_schooling1=16 if inlist(ar16,13,61) & inrange(ar17,5,7)
	replace length_schooling1=15 if ar16==60 & inrange(ar17,4,7)

	* master (grade>3 = grade=2)
	replace length_schooling1=17 if ar16==62 & inrange(ar17,0,1)
	replace length_schooling1=18 if ar16==62 & inrange(ar17,2,7)

	* doctoral 
	* all assumed to have completed master first
	* grade>4 = grade=4
	replace length_schooling1=19 if ar16==63 & inrange(ar17,0,1)
	forval j = 2/3 {
		replace length_schooling1=`j'+18 if ar16==63 & ar17==`j'
	}
	replace length_schooling1=22 if ar16==63 & inrange(ar17,4,7)	
		
	* for the remaining missing values:
	* year of schooling = grade for grade 1-6 (for those with uncommon school type)
	* year of schooling = 1 if grade==0 (did not finish first year)
	* year of schooling = mean if grade==7 (graduated) 
	replace length_schooling1=ar17 if length_schooling1==. & inrange(ar17,1,6)  
	replace length_schooling1=1 if length_schooling1==. & ar17==0  
	qui sum length_schooling1
	replace length_schooling1=floor(`r(mean)') if length_schooling1==. & ar17==7   

	* if there are still remaining missing values (for those who doesn't know the grade)
	* year of schooling = mean year of schooling of the education level
	* primary
	qui sum length_schooling1 if inlist(ar16,2,11,72,14,17) 
	replace length_schooling1=floor(`r(mean)') if length_schooling1==. & inlist(ar16,2,11,72,14,17)

	* junior
	qui sum length_schooling1 if inlist(ar16,3,4,12,73) 
	replace length_schooling1=floor(`r(mean)') if length_schooling1==. & inlist(ar16,3,4,12,73)

	* senior
	qui sum length_schooling1 if inlist(ar16,5,6,15,74) 
	replace length_schooling1=floor(`r(mean)') if length_schooling1==. & inlist(ar16,5,6,15,74)
	
	* uni
	qui sum length_schooling1 if inlist(ar16,13,61) 
	replace length_schooling1=floor(`r(mean)') if length_schooling1==. & inlist(ar16,13,61)

	* diploma
	qui sum length_schooling1 if ar16==60 
	replace length_schooling1=floor(`r(mean)') if length_schooling1==. & ar16==60

	* master
	qui sum length_schooling1 if ar16==62 
	replace length_schooling1=floor(`r(mean)') if length_schooling1==. & ar16==62

	* doctoral
	qui sum length_schooling1 if ar16==63 
	replace length_schooling1=floor(`r(mean)') if length_schooling1==. & ar16==63

	* don't know the grade and level
	qui sum length_schooling1
	replace length_schooling1=floor(`r(mean)') if length_schooling1==. & ar17==98

	*************************************
	*** length of schooling (from dl) ***
	*************************************

	* no schooling (highest level attended kindergarten = no schooling)
	g length_schooling2=0 if inlist(dl06,1,90) | dla01==3 

	* primary school (did not finish first grade = 1 year)
	* assume pesantren and slb is primary level
	replace length_schooling2=1 if (inlist(dl06,2,11,72,14,17) & inrange(dl07,0,1))|(inlist(dla08,2,11,72,14,17) & inrange(dla09,0,1))
	forval j =2/5 {
		replace length_schooling2=`j' if (inlist(dl06,2,11,72,14,17) & dl07==`j')|(inlist(dla08,2,11,72,14,17) & dla09==`j')
	}
	replace length_schooling2=6 if (inlist(dl06,2,11,72,14,17) & inrange(dl07,6,7))|(inlist(dla08,2,11,72,14,17) & inrange(dla09,6,7))

	* junior high (did not finish first grade = 7 year)
	replace length_schooling2=7 if (inlist(dl06,3,4,12,73) & inrange(dl07,0,1))|(inlist(dla08,3,4,12,73) & inrange(dla09,0,1))
	replace length_schooling2=8 if (inlist(dl06,3,4,12,73) & dl07==2)|(inlist(dla08,3,4,12,73) & dla09==2)
	replace length_schooling2=9 if (inlist(dl06,3,4,12,73) & inlist(dl07,3,7))|(inlist(dla08,3,4,12,73) & inlist(dla09,3,7))

	* senior high (did not finish first grade = 10 year)
	replace length_schooling2=10 if (inlist(dl06,5,6,15,74) & inrange(dl07,0,1))|(inlist(dla08,5,6,15,74) & inrange(dla09,0,1))
	replace length_schooling2=11 if (inlist(dl06,5,6,15,74) & dl07==2)|(inlist(dla08,5,6,15,74) & dla09==2)
	replace length_schooling2=12 if (inlist(dl06,5,6,15,74) & inlist(dl07,3,7))|(inlist(dla08,5,6,15,74) & inlist(dla09,3,7))

	* university 
	* did not finish first grade = 13 year
	* bachelor degree assumed to never take diploma previously
	* grade>4 = grade=4 for uni
	* grade>3 = grade=3 for uni
	replace length_schooling2=13 if inlist(dl06,13,60,61) & inrange(dl07,0,1)
	forval j =2/4 {
		replace length_schooling2=`j'+12 if inlist(dl06,13,61) & dl07==`j'
	}
	forval j =2/3 {
		replace length_schooling2=`j'+12 if dl06==60 & dl07==`j'
	}
	replace length_schooling2=16 if inlist(dl06,13,61) & inrange(dl07,5,7)
	replace length_schooling2=15 if dl06==60 & inrange(dl07,4,7)

	* master (grade>3 = grade=2)
	replace length_schooling2=17 if dl06==62 & inrange(dl07,0,1)
	replace length_schooling2=18 if dl06==62 & inrange(dl07,2,7)

	* doctoral 
	* all assumed to have completed master first
	* grade>4 = grade=4
	replace length_schooling2=19 if dl06==63 & inrange(dl07,0,1)
	forval j = 2/3 {
		replace length_schooling2=`j'+18 if dl06==63 & dl07==`j'
	}
	replace length_schooling2=22 if dl06==63 & inrange(dl07,4,7)	
		
	* for the remaining missing values:
	* year of schooling = grade for grade 1-6 (for those with uncommon school type)
	* year of schooling = 1 if grade==0 (did not finish first year)
	* year of schooling = mean if grade==7 (graduated) 
	replace length_schooling2=dl07 if length_schooling2==. & inrange(dl07,1,6)  
	replace length_schooling2=1 if length_schooling2==. & dl07==0  
	qui sum length_schooling2
	replace length_schooling2=floor(`r(mean)') if length_schooling2==. & dl07==7   

	* if there are still remaining missing values (for those who doesn't know the grade)
	* year of schooling = mean year of schooling of the education level
	* primary
	qui sum length_schooling2 if inlist(dl06,2,11,72,14,17) 
	replace length_schooling2=floor(`r(mean)') if length_schooling2==. & inlist(dl06,2,11,72,14,17)

	* junior
	qui sum length_schooling2 if inlist(dl06,3,4,12,73) 
	replace length_schooling2=floor(`r(mean)') if length_schooling2==. & inlist(dl06,3,4,12,73)

	* senior
	qui sum length_schooling2 if inlist(dl06,5,6,15,74) 
	replace length_schooling2=floor(`r(mean)') if length_schooling2==. & inlist(dl06,5,6,15,74)
	
	* uni
	qui sum length_schooling2 if inlist(dl06,13,61) 
	replace length_schooling2=floor(`r(mean)') if length_schooling2==. & inlist(dl06,13,61)

	* diploma
	qui sum length_schooling2 if dl06==60 
	replace length_schooling2=floor(`r(mean)') if length_schooling2==. & dl06==60

	* master
	qui sum length_schooling2 if dl06==62 
	replace length_schooling2=floor(`r(mean)') if length_schooling2==. & dl06==62

	* doctoral
	qui sum length_schooling2 if dl06==63 
	replace length_schooling2=floor(`r(mean)') if length_schooling2==. & dl06==63

	* don't know the grade and level
	qui sum length_schooling2
	replace length_schooling2=floor(`r(mean)') if length_schooling2==. & dl07==98

	***************
	* child labor *
	***************

	forval j=15(3)18 {
		clonevar childlabor`j'=dla56a
		recode childlabor`j' (3=0)
		replace childlabor`j'=1 if startwork<bth`j'yr & childlabor`j'==.
		replace childlabor`j'=0 if startwork>=bth`j'yr & childlabor`j'==.	
		la var childlabor`j' "Ever worked before `j'"

		foreach x in paid unpaid {
			clonevar cl`x'`j'=cl`x'
			replace cl`x'`j'=1 if startwork_`x'wk<bth`j'yr & cl`x'`j'==.
			replace cl`x'`j'=0 if startwork_`x'wk>=bth`j'yr & cl`x'`j'==.	
			la var cl`x'`j' "Ever do `x' work before `j'"
		}

		foreach x in formal informal {
			g cl`x'`j'=1 if startwork_`x'<bth`j'yr
			replace cl`x'`j'=0 if startwork_`x'>=bth`j'yr	
			la var cl`x'`j' "Ever do `x' work before `j'"
		}

		loc l1 "Agriculture" 
		loc l2 "Mining" 
		loc	l3 "Manufacturing" 
		loc	l4 "Services" 
		loc	l5 "Construction" 
		loc	l6 "Trading, restaurants" 

		forval k=1/6 {
			g clsec`k'`j'=1 if startwork_sec`k'<bth`j'yr
			replace clsec`k'`j'=0 if startwork_sec`k'>=bth`j'yr	
			la var clsec`k'`j' "`l`k''"
		}
	}
																	
	if `i'==5 {
		clonevar bwin_wtype=sa13
		missings report bwin_wtype
		egen earnings_pm=rowtotal(tk25a1 tk26a1 tk25b1 tk26b1)
		egen earnings_py=rowtotal(tk25a2 tk26a3 tk25b2 tk26b3 tk25a2b tk25b2b)

		foreach x in pm py {
			bys hhid`w`i'': egen max_earnings_`x'=max(earnings_`x')
			g breadwinner_`x'=earnings_`x'==max_earnings_`x'		
		}
		tempfile tempdata
		save `tempdata', replace 

		di as res _dup(59) "-" _n ///
		"merging breadwinner data" _n ///
		_dup(59) "-"
		keep if breadwinner_py==1
		duplicates tag hhid`w`i'', gen(dup)
		drop if dup>0 & ar02b>1
		keep hhid`w`i'' tk24a
		ren tk24a tk24a_bwin  
		merge 1:m hhid`w`i'' using `tempdata', nogen
		replace bwin_wtype=tk24a_bwin if bwin_wtype==. & age1<15
		missings report bwin_wtype

		recode bwin_wtype (3 4 5 = 1) (1 2 6/10 = 0) (99=.), gen(bwin_formalwork)

		clonevar electric=sa09
		replace electric=kr11 if electric==. & age1<15
		recode electric (3=0)	
		
		clonevar drinkwater1=sa10
		recode drinkwater1 (1/3 95=0) (4=1)
		clonevar drinkwater2=kr13
		recode drinkwater2 (1/8 95=0) (10=1)
		clonevar filtered_water=drinkwater1
		replace filtered_water=drinkwater2 if filtered_water==. & age1<15

		clonevar toilettype1=sa11
		recode toilettype1 (3/95=0) (1/2=1) (99=.)
		clonevar toilettype2=kr20
		recode toilettype2 (3/11 95=0) (1/2=1)
		clonevar owntoilet=toilettype1
		replace owntoilet=toilettype2 if owntoilet==. & age1<15

		clonevar hhsize_ch=sa02
		bys hhid`w`i'': g hhsize_now=_N
		replace hhsize_ch=hhsize_now if hhsize_ch==. & age1<15

		missings report electric owntoilet filtered_water hhsize_ch

		clonevar chpoor=eh01
		recode chpoor (5=1) (1/4 = 0)
		clonevar chbedrest=eh03
		clonevar chhospital=eh04
		foreach x of varlist chbedrest chhospital {
			recode `x' (3=0)			
		}
		g chmorbidity=0 if regexm(eh05,"W")
		foreach x in A B C D E F G H I J K L M V {
			replace chmorbidity=1 if regexm(eh05,"`x'")
		}

		labvars chpoor "Poor health" ///
				chbedrest "Ever confined to bed for at least a month" ///
				chhospital "Ever hospitalized for at least a month" ///
				chmorbidity "Have morbidity" ///
				, alternate

		drop bwin_wtype *earnings_* breadwinner* drinkwater* toilet* ///
		hhsize_now tk* sa* ar* kr* eh* tr* dl0* maa* kk*
	}

	labvars dropout "Dropped out of school" ///
			stop_schooling "Discontinue schooling" ///
			tempabsent "Ever temporarily left school" ///
			failgrade "Ever failed grade" ///
			healthy1 "Feeling healthy" /// 
			healthy2 "Feeling healthy" ///
			chroniccond "Has chronic conditions" ///
			chronicill "Has chronic illness" ///
			daysmissed_poorhealth "Number of days missed in past month" ///
			daysinbed_poorhealth "Number of days in bed in past month" ///
			missedact_poorhealth "Ever missed activities in the past month" ///
			inbed_poorhealth "Ever bedridden in the past month" ///
			employedpw "Employed in paid work in the past week" ///
			laborunion "Join labor union" ///
			wformal "Working in formal job" ///
			wcontract "Working with a contract" ///
			earningspm "Earnings in the past month" ///
			earningspy "Earnings in the past year" ///
			lnearningspm "Earnings in the past month" ///
			lnearningspy "Earnings in the past year" ///
			, alternate
			
	forval j=1/2 {
		labvars earningsph`j' "Earnings per hour" ///
				lnearningsph`j' "Earnings per hour" ///
				, alternate
	}		

	** final data for regression
	save "$ifin\ifls`i'_final_main.dta", replace 
}

* ==== ALTERNATIVE RUNNING VARIABLE ==== *
forval i=4/5 {
	use "$ifin\ifls`i'_final_main.dta", clear
	drop runvar* itt* bth*yr bth_year bd* agebypol* childlabor* cl*1* *crisis compschooling
	
	foreach x of varlist dob* {
		recode `x' (98=.) (9998=.) (99=.) (9999=.)
		la val `x' . 
	}
	
	keep if dob_yr<. & dob_mth<.
	
	foreach x in day mth {
		g sd`x'=dob_`x'<10
		tostring dob_`x', replace
		replace dob_`x'="" if dob_`x'=="."
		forval j=1/9 {
			replace dob_`x'=subinstr(dob_`x',"`j'","0`j'",.) if sd`x'==1
		}
		drop sd`x'
	}
	
	g slash1="/"
	g slash2="/"

	egen dob_YM=concat(dob_yr slash1 dob_mth) 
	egen dob_YMD=concat(dob_yr slash1 dob_mth slash2 dob_day) if dob_day~=""	

	g daysto_pol03=date("25/03/2003","DMY")
	g daysto_pol98=date("01/10/1998","DMY")
	g daysto_pol02=date("13/08/2002","DMY")
	
	loc lYM 	"(year and month)"
	loc lYMD 	"(full date)"
	loc l98 	15
	loc l03 	18
	loc l02 	15
	
	* constructing running and itt variable 
	foreach x in YM YMD {
		g daysto_dob_`x'=date(dob_`x',"`x'")	
		clonevar dob_`x'_num=dob_`x'
		replace dob_`x'_num=subinstr(dob_`x'_num,"/","",.)
		destring dob_`x'_num, replace	
		foreach y of varlist dob_`x' dob_`x'_num {
			la var `y' "Birth date `l`x''" 			
		}

		foreach j in 98 03 02 {
			g agebypol`j'_`x' = (daysto_pol`j' - daysto_dob_`x')/365 if daysto_dob_`x'>=0
			replace agebypol`j'_`x' = (daysto_pol`j' + (daysto_dob_`x'*-1))/365 if daysto_dob_`x'<0
			g runvar_abp`j'_`x' = (agebypol`j'_`x' - `l`j'')*-1
			g itt_abp`j'_`x'=1 if runvar_abp`j'_`x'>=0
			replace itt_abp`j'_`x'=0 if runvar_abp`j'_`x'<0

			if "`x'"=="YM" {
				g dob`j'_`x' = dob_`x'_num+`l`j''00
				if `j'==98 {
					g runvar_dob`j'_`x' = dob`j'_`x'-199810
				}
				else if "`j'"=="03" {
					g runvar_dob`j'_`x' = dob`j'_`x'-200303					
				}
				else if "`j'"=="02" {
					g runvar_dob`j'_`x' = dob`j'_`x'-200208	
				}
			}
			else if "`x'"=="YMD" {
				g dob`j'_`x' = dob_`x'_num+`l`j''0000
				if `j'==98 {
					g runvar_dob`j'_`x' = dob`j'_`x'-19981001
				}
				else if "`j'"=="03" {
					g runvar_dob`j'_`x' = dob`j'_`x'-20030325					
				}
				else if "`j'"=="02" {
					g runvar_dob`j'_`x' = dob`j'_`x'-20020813	
				}
			}	
		
			g itt_dob`j'_`x'=1 if runvar_dob`j'_`x'>=0
			replace itt_dob`j'_`x'=0 if runvar_dob`j'_`x'<0

			labvars runvar_abp`j'_`x' 			"Age by `j' policy normalized `l`x''" ///
					itt_abp`j'_`x' 				"Intent-to-treat `j' based on age by policy `l`x''" ///
					agebypol`j'_`x' 			"Age by `j' policy `l`x''" ///
					dob`j'_`x'					"Birthday in `j' `l`x''" ///
					runvar_dob`j'_`x' 			"Birthday in `j' normalized `l`x''" ///
					itt_dob`j'_`x' 				"Intent-to-treat `j' based on birthday `l`x''" ///
					, alternate
		}
	}
	
	* 15th and 18th birthday
	forval j =12(3)18 {
		g dob`j'yr=dob_yr+`j'		
	} 

	forval j=15(3)18 {
		clonevar childlabor`j'=dla56a
		recode childlabor`j' (3=0)
		replace childlabor`j'=1 if startwork<dob`j'yr & childlabor`j'==.
		replace childlabor`j'=0 if startwork>=dob`j'yr & childlabor`j'==.	
		la var childlabor`j' "Ever worked before `j'"

		foreach x in paid unpaid {
			clonevar cl`x'`j'=cl`x'
			replace cl`x'`j'=1 if startwork_`x'wk<dob`j'yr & cl`x'`j'==.
			replace cl`x'`j'=0 if startwork_`x'wk>=dob`j'yr & cl`x'`j'==.	
			la var cl`x'`j' "Ever do `x' work before `j'"
		}

		foreach x in formal informal {
			g cl`x'`j'=1 if startwork_`x'<dob`j'yr
			replace cl`x'`j'=0 if startwork_`x'>=dob`j'yr	
			la var cl`x'`j' "Ever do `x' work before `j'"
		}

		loc l1 "Agriculture" 
		loc l2 "Mining" 
		loc	l3 "Manufacturing" 
		loc	l4 "Services" 
		loc	l5 "Construction" 
		loc	l6 "Trading, restaurants" 

		forval k=1/6 {
			g clsec`k'`j'=1 if startwork_sec`k'<dob`j'yr
			replace clsec`k'`j'=0 if startwork_sec`k'>=dob`j'yr	
			la var clsec`k'`j' "`l`k''"
		}
	}
	
	di as res _dup(59) "-" _n ///
	"merging main dataset IFLS`i' with crisis data" _n ///
	_dup(59) "-"
	clonevar year=dob_yr
	merge m:1 year using "$ouw\indonesia-crisis-years.dta"
	drop if _merge==2
	drop _merge year
	ren crisisyr bth0_crisis
	replace bth0_crisis=1 if inrange(dob_yr,1942,1948)

	clonevar year=dob_yr
	merge m:1 year using "$ouw\indonesia-crisis-years.dta"
	drop if _merge==2
	drop _merge year
	ren crisisyr bth12_crisis
	replace bth12_crisis=1 if inrange(dob_yr,1942,1948)
	
	g compschooling=dob15yr>1993
	replace childmarriage=1 if firstmy<dob18yr
	replace childmarriage=0 if firstmy>=dob18yr
	replace childmarriage=. if inlist(firstmy,9998,9999)

	drop slash* daysto*	
	save "$ifin\ifls`i'_final_alt.dta", replace 
}

***************************************
***** PART II: SUMMARY STATISTICS *****
***************************************

* ==== SET THEME FOR GRAPH ==== *
set scheme plotplainblind

* ==== DESCRIPTIVE STATISTICS ==== *

* cl incidence 15-17 in 2019 
use "$dsc\IDN_A-filtered-2024-03-11.dta", clear
di obs_value[4]/obs_value[1]

* cl incidence 10-14 since 1970
clear all
mat cl = (1970, 13 \ 1997, 7 \ 1998, 8 \ 2000, 7.8 \ 2010, 3.7)
matrix coln cl = year clrate
svmat cl, names(col)
graph bar clrate, over(year) ytitle("")
graph export "$gp\clrate_10-14.png", replace

* school enrollment
import excel "$dsc\P_Data_Extract_From_World_Development_Indicators.xlsx", clear first 
keep SeriesName YR1975 YR1987 YR1995 YR2005 YR2015 YR2016
foreach x of varlist YR* {
	destring `x',replace force
}
keep if regexm(SeriesName,"enrollment")
replace YR2015=YR2016 if SeriesName=="School enrollment, primary (% net)" 
drop YR2016
reshape long YR, i(SeriesName) j(year)
graph bar YR, over(SeriesName) over(year) asyvar legend(lab(1 "Primary") lab(2 "Secondary")) ytitle("")
graph export "$gp\erate_1975-2015.png", replace
/* add footnote:
- the 2015 primary enrolment rate is extrapolated from the 2016 number
- primary school enrolment reached its highest in mid 1980 and then fell until early 2000s, 
possibly due to oil bust of 1980s and then AFC. It picks up again until now  
*/

* ==== SUMMARY STATISTICS ==== *
use "$ifin\ifls5_final_alt.dta", clear
format earningspy %12.0fc
recode earningspy (0=.)

** just outcomes
est clear 
forval i=0/1 {
	estpost sum childlabor* $eduhealth $labovar if itt_abp03_YM==`i' & inrange(age2,18,55)
	eststo sumstat`i'	
}

esttab sumstat0 sumstat1 using "$tb\summary statistics-outcome.tex", main(mean) ///
label replace nonum nostar nonotes noobs mtitle("Control" "Treatment") b(%14.2fc) nogaps ///
coeflab(childlabor15 "Ever worked before the age of 15" ///
		childlabor18 "Ever worked before the age of 18" ///
		atdshs2 "Attended senior secondary" ///
		gradshs2 "Graduated senior secondary" ///
		missedact_poorhealth "Ever missed activities in the past month due to poor health" ///
		depsymptoms "Exhibiting depressive symptoms" ///
		employedpw "Working in the past week" ///
		wformal "Working in a formal employment" ///
		wcontract "Working with a contract" ///
		earningspy "Earnings in the past year (Rp.)") 
		
** just covariates
est clear 
forval i=0/1 {
	estpost sum $cov if itt_abp03_YM==`i' & inrange(age2,18,55)
	eststo sumstat`i'	
}

esttab sumstat0 sumstat1 using "$tb\summary statistics-covariates.tex", main(mean) ///
label replace nonum nostar nonotes noobs mtitle("Control" "Treatment") b(%14.2fc) nogaps ///
coeflab(male2 "Is a male" /// 
		islam2 "Is a Moslem" /// 
		javanese2 "Is a Javanese" ///
		bwin_formalwork "Main breadwinner during childhood was in formal work" ///
		electric "House during childhood had access to electricity" ///
		filtered_water "House during childhood drank filtered water" ///
		owntoilet "House during childhood had their own toilet" /// 
		hhsize_ch "Household size during childhood")   

****************************
***** PART III: RD PLOT ****
****************************

* ==== CHILD LABOR ==== *
use "$ifin\ifls5_final_alt.dta", clear
loc p0 "Randomization"
loc p1 "Linear Fit"
loc p2 "Quadratic Fit"
forval x=15(3)18 {
		forval i=0/2 {
		rdplot childlabor`x' runvar_abp03_YM if inrange(age2,18,55) & inrange(runvar_abp03_YM,-6,6), ///
		p(`i') kernel(tri) covs($cov) graph_options(legend(off) subtitle("`p`i''") saving(`x'`i', replace)) 
		graph export "$gp\rdplot_childlabor`x'_p`i'.png", replace 
	}
	graph combine `x'1.gph `x'2.gph `x'0.gph , row(1) ///
	b1title("Age by policy reform relative to cutoff (reversed), in year and month", size(small)) 
	graph export "$gp\rdplot_childlabor`x'.png", replace 
}
		
********************************
***** PART IV: ESTIMATIONS *****
********************************
		
* ==== WRITE PROGRAMS ==== *
pro randtomat
matrix list res
clear
svmat res, names(col)
g v="diff_means" if _n==1
replace v="p-value" if _n==2 
replace v="eff_obs_left" if _n==3
replace v="eff_obs_right" if _n==4
end

* ==== INSTALL PACKAGES ==== *
// ssc install estout, all replace 
// ssc install texdoc, all replace
// net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
// net install rdlocrand, from(https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata) replace
// net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
// net install rdpower, from(https://raw.githubusercontent.com/rdpackages/rdpower/master/stata) replace

* ==== RUNNING VARIABLE MANIPULATION TEST ==== *
use "$ifin\ifls5_final_alt.dta", clear
rddensity runvar_abp03_YM if inrange(age2,18,55)

* ==== MASSPOINTS ==== *
use "$ifin\ifls5_final_alt.dta", clear
forval i=0/1 {
	distinct runvar_abp03_YM if itt_abp03_YM==`i' & inrange(age2,18,55)
	loc u`i'=`r(ndistinct)'
	loc n`i'=`r(N)'
	loc un`i'=`n`i''/`u`i''
	loc fn`i': 	di %12.0fc `n`i''
	loc fun`i': di %12.0fc `un`i''
}

matrix A = (`u0',`n0', `un0' \ `u1',`n1', `un1')
matrix rown A = Control Treatment
matrix coln A = Unique Total Eff_Obs
matrix list A
clear 
svmat A, names(col)
g Group="Control" if _n==1
replace Group="Treatment" if _n==2
order Group Unique Total Eff_Obs
export excel "$tb\masspoints.xlsx", replace first(var)

texdoc init "$tb\masspoints.tex", replace force
tex \begin{tabular}{lll}
tex \hline\hline 
tex      						& Control 		& Treatment  \\
tex \hline
tex Number of observations    	& `fn0'    		& `fn1'        \\
tex Mass points 				& `u0'    		& `u1'    \\
tex Observation per mass point 	& `fun0'		& `fun1' \\     
tex \hline\hline 
tex \end{tabular}
texdoc close

* ==== COVARIATE BALANCE ==== * 
use "$ifin\ifls5_final_alt.dta", clear
est clear 
foreach x of global cov {
	eststo `x': rdrobust `x' runvar_abp03_YM if inrange(age2,18,55), p(1) vce(cluster runvar_abp03_YM) all
} 

esttab male2 islam2 javanese2 bwin_formalwork ///
using "$tb\covbal1.tex", replace label se star(* 0.10 ** 0.05 *** 0.01) ///
noobs mtitle("Male" "Islam" "Javanese" "Breadwinner formal work")

esttab electric filtered_water owntoilet hhsize_ch ///
using "$tb\covbal2.tex", replace label se star(* 0.10 ** 0.05 *** 0.01) ///
noobs mtitle("Electricity access" "Drank filtered water" "Own toilet" "Household size")		

* ==== CHILD LABOR ==== *
est clear
capture log close
log using "$lg\res-childlabor-main.smcl", replace
use "$ifin\ifls5_final_alt.dta", clear
foreach x of varlist childlabor* {
	forval i=1/2 {
		di as text _dup(59) "=" ///
		_n as res "`x', p(`i')" ///
		_n as text _dup(59) "=" 
		rdrobust `x' runvar_abp03_YM if inrange(age2,18,55), p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
		loc `x'tau`i': di %12.4fc `e(tau_bc)'
		loc `x'se`i': di %12.4fc `e(se_tau_rb)'
		loc `x'tstat`i'=``x'tau`i''/``x'se`i''
		loc `x'star`i'=cond(abs(``x'tstat`i'')>2.58,"***",cond(abs(``x'tstat`i'')>1.96,"**",cond(abs(``x'tstat`i'')>1.645,"*","")))
		loc `x'nl`i': di %12.0fc `e(N_l)'
		loc `x'nr`i': di %12.0fc `e(N_r)'
		loc `x'efnl`i': di %12.0fc `e(N_h_l)'
		loc `x'efnr`i': di %12.0fc `e(N_h_r)'
		eststo `x'`i': rdpow `x' runvar_abp03_YM if inrange(age2,18,55), p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
		loc `x'pw`i': di %12.4fc `r(power_rbc)'
		qui estadd scalar nl=`e(N_l)' 
		qui estadd scalar nr=`e(N_r)'
		qui estadd scalar efnl=`e(N_h_l)'
		qui estadd scalar efnr=`e(N_h_r)'
		qui estadd scalar pw=`r(power_rbc)'	
	}
}

esttab childlabor151 childlabor181 childlabor152 childlabor182 ///
using "$tb/res-rd-childlabor-main.tex", replace label se ///
star(* 0.10 ** 0.05 *** 0.01) stats(nl nr efnl efnr pw, labels("Control" "Treatment" "Eff. Control" "Eff. Treatment" "Power") fmt(%12.0fc)) ///
prehead(`"\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"' ///
		`"\begin{tabular}{l*{4}{c}}"' ///
		`"\hline\hline"' ///
		`"&\multicolumn{2}{c}{Linear} &\multicolumn{2}{c}{Quadratic} \\"'  ///
		`"\cmidrule(rl){2-5}"')  
		
foreach x of varlist childlabor* {
	di as text _dup(59) "=" ///
	_n as res "`x', p(0)" ///
	_n as text _dup(59) "=" 
	cap noisily: rdrandinf `x' runvar_abp03_YM if inrange(age2,18,55), kernel(triangular) cov($cov) wmasspoints level(0.1)
	loc `x'nl: di %12.0fc `r(N_left)'
	loc `x'nr: di %12.0fc `r(N_right)'
	loc `x'tau: di %12.4fc `r(obs_stat)'
	loc `x'tstat=invnormal((`r(asy_pval)'/2))
	loc `x'se: di %12.4fc (``x'tau' / ``x'tstat') 
	loc `x'star=cond(abs(``x'tstat') >2.58,"***",cond(abs(``x'tstat') >1.96,"**",cond(abs(``x'tstat') >1.645,"*","")))
}
matrix res= (`childlabor15tau', `childlabor18tau' \ `childlabor15se', `childlabor18se'  \ `childlabor15nl', `childlabor18nl' \ `childlabor15nr', `childlabor18nr') 
matrix coln res = childlabor15 childlabor18
randtomat
order v childlabor15 childlabor18
export excel "$tb\res-rand-childlabor-main.xlsx", replace first(var)

texdoc init "$tb\res-childlabor-main.tex", replace force
tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
tex \begin{tabular}{l*{2}{c}} 
tex \hline\hline 
tex                    &\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}\\
tex                    &\multicolumn{1}{c}{Ever work before 15}&\multicolumn{1}{c}{Ever work before 18}\\
tex \hline
tex &\multicolumn{2}{c}{Panel A: Linear} \\
tex Treatment Effect	&    `childlabor15tau1'\sym{`childlabor15star1'}    &    `childlabor18tau1'\sym{`childlabor18star1'} \\
tex                    	&    (`childlabor15se1')         					&    (`childlabor18se1')          \\
tex Eff. Control        &      `childlabor15efnl1'       &      `childlabor18efnl1'       \\
tex Eff. Treatment      &      `childlabor15efnr1'       &      `childlabor18efnr1'       \\
tex Power               &      `childlabor15pw1'         &      `childlabor18pw1'         \\
tex \hline
tex &\multicolumn{2}{c}{Panel B: Quadratic} \\
tex Treatment Effect	&    `childlabor15tau2'\sym{`childlabor15star2'}    &    `childlabor18tau2'\sym{`childlabor18star2'} \\
tex                    	&    (`childlabor15se2')         					&    (`childlabor18se2')          \\
tex Eff. Control        &      `childlabor15efnl2'       &      `childlabor18efnl2'       \\
tex Eff. Treatment      &      `childlabor15efnr2'       &      `childlabor18efnr2'       \\
tex Power               &      `childlabor15pw2'         &      `childlabor18pw2'         \\
tex \hline
tex &\multicolumn{2}{c}{Panel C: Mean Difference} \\
tex Treatment Effect	&    `childlabor15tau'\sym{`childlabor15star'}  &    `childlabor18tau'\sym{`childlabor18star'} \\
tex                    	&    (`childlabor15se')         				&    (`childlabor18se')          \\
tex Eff. Control        &      `childlabor15nl'       	&      `childlabor18nl'       \\
tex Eff. Treatment      &      `childlabor15nr'       	&      `childlabor18nr'       \\
tex Power               &      `childlabor15pw1'        &      `childlabor18pw1'         \\
tex \hline
tex Control             &      `childlabor15nl1'        &      `childlabor18nl1'         \\
tex Treatment           &      `childlabor15nr1'        &      `childlabor18nr1'         \\
tex \hline\hline
tex \multicolumn{3}{l}{\footnotesize Standard errors in parentheses}\\
tex \multicolumn{3}{l}{\footnotesize \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\
tex \end{tabular}
texdoc close
log close 

* ==== TYPES OF WORK ==== *
est clear
capture log close
log using "$lg\res-cltypes.smcl", replace
use "$ifin\ifls5_final_alt.dta", clear
forval i=1/2 {
	foreach x of varlist cl*15 cl*18 {
		di as text _dup(59) "=" ///
		_n as res "`x', p(`i')" ///
		_n as text _dup(59) "=" 
		rdrobust `x' runvar_abp03_YM if inrange(age2,18,55), p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
		loc `x'tau`i': di %12.4fc `e(tau_bc)'
		loc `x'se`i': di %12.4fc `e(se_tau_rb)'
		loc `x'tstat`i'=``x'tau`i''/``x'se`i''
		loc `x'star`i'=cond(abs(``x'tstat`i'')>2.58,"***",cond(abs(``x'tstat`i'')>1.96,"**",cond(abs(``x'tstat`i'')>1.645,"*","")))
		loc `x'nl`i': di %12.0fc `e(N_l)'
		loc `x'nr`i': di %12.0fc `e(N_r)'
		loc `x'efnl`i': di %12.0fc `e(N_h_l)'
		loc `x'efnr`i': di %12.0fc `e(N_h_r)'
		eststo `x'`i': rdpow `x' runvar_abp03_YM if inrange(age2,18,55), p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
		loc `x'pw`i': di %12.4fc `r(power_rbc)'
		qui estadd scalar nl=`e(N_l)' 
		qui estadd scalar nr=`e(N_r)'
		qui estadd scalar efnl=`e(N_h_l)' 
		qui estadd scalar efnr=`e(N_h_r)'
		qui estadd scalar pw=`r(power_rbc)'	
	}
	esttab clpaid15`i' clpaid18`i' clunpaid15`i' clunpaid18`i'  ///
	using "$tb/res-rd-(un)paid-p`i'.tex", replace label se ///
	star(* 0.10 ** 0.05 *** 0.01) stats(nl nr efnl efnr pw, labels("Control" "Treatment" "Eff. Control" "Eff. Treatment" "Power") fmt(%12.0fc)) 

	esttab clformal15`i' clformal18`i' clinformal15`i' clinformal18`i'  ///
	using "$tb/res-rd-(in)formal-p`i'.tex", replace label se ///
	star(* 0.10 ** 0.05 *** 0.01) stats(nl nr efnl efnr pw, labels("Control" "Treatment" "Eff. Control" "Eff. Treatment" "Power") fmt(%12.0fc)) 

	forval k=15(3)18 {
		esttab clsec1`k'`i' clsec2`k'`i' clsec3`k'`i' clsec4`k'`i' clsec5`k'`i' clsec6`k'`i'   ///
		using "$tb/res-rd-cl`k'-p`i'.tex", replace se ///
		star(* 0.10 ** 0.05 *** 0.01) stats(nl nr efnl efnr pw, labels("Control" "Treatment" "Eff. Control" "Eff. Treatment" "Power") fmt(%12.0fc)) ///
		prehead(`"\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"' ///
				`"\begin{tabular}{l*{6}{c}}"' ///
				`"\hline\hline"' ///
                `"&\multicolumn{6}{c}{Ever do work in ____ before `k'} \\"'  ///
                `"\cmidrule(rl){2-7}"')  
	}	
}

foreach x of varlist cl*15 cl*18 {
	di as text _dup(59) "=" ///
	_n as res "`x', p(0)" ///
	_n as text _dup(59) "=" 
	cap noisily: rdrandinf `x' runvar_abp03_YM if inrange(age2,18,55), kernel(triangular) cov($cov) wmasspoints level(0.1) 
	loc `x'nl: di %12.0fc `r(N_left)'
	loc `x'nr: di %12.0fc `r(N_right)'
	loc `x'tau: di %12.4fc `r(obs_stat)'
	loc `x'tstat=invnormal((`r(asy_pval)'/2))
	loc `x'se: di %12.4fc (``x'tau' / ``x'tstat') 
	loc `x'star=cond(abs(``x'tstat') >2.58,"***",cond(abs(``x'tstat') >1.96,"**",cond(abs(``x'tstat') >1.645,"*","")))
}

foreach x in paid formal {
	if "`x'"=="paid" {
		loc y "un"
	}
	if "`x'"=="formal" {
		loc y "in"
	}
	matrix res= (`cl`x'15tau', `cl`x'18tau', `cl`y'`x'15tau', `cl`y'`x'18tau' ///
	\ `cl`x'15se', `cl`x'18se', `cl`y'`x'15se', `cl`y'`x'18se' ///
	\ `cl`x'15nl', `cl`x'18nl', `cl`y'`x'15nl', `cl`y'`x'18nl' ///
	\ `cl`x'15nr', `cl`x'18nr', `cl`y'`x'15nr', `cl`y'`x'18nr') 
	matrix coln res = cl`x'15 cl`x'18 cl`y'`x'15 cl`y'`x'18 
	randtomat
	order v cl`x'15 cl`x'18 cl`y'`x'15 cl`y'`x'18
	export excel "$tb\res-rand-(`y')`x'.xlsx", replace first(var)
	
	texdoc init "$tb\res-(`y')`x'.tex", replace force
	tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
	tex \begin{tabular}{l*{4}{c}} 
	tex \hline\hline 
	tex                    &\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}&\multicolumn{1}{c}{(3)}&\multicolumn{1}{c}{(4)}\\
	tex 				   &\multicolumn{2}{c}{Did `x' work}&\multicolumn{2}{c}{Did `y'`x' work}\\	
	tex                    &\multicolumn{1}{c}{Before 15}&\multicolumn{1}{c}{Before 18}&\multicolumn{1}{c}{Before 15}&\multicolumn{1}{c}{Before 18}\\
	tex \hline
	tex &\multicolumn{4}{c}{Panel A: Linear} \\
	tex Treatment Effect	&    `cl`x'15tau1'\sym{`cl`x'15star1'}    	&    `cl`x'18tau1'\sym{`cl`x'18star1'} 	&    `cl`y'`x'15tau1'\sym{`cl`y'`x'15star1'}    	&    `cl`y'`x'18tau1'\sym{`cl`y'`x'18star1'} \\
	tex                    	&    (`cl`x'15se1')         				&    (`cl`x'18se1')          			&    (`cl`y'`x'15se1')         						&    (`cl`y'`x'18se1')           \\
	tex Eff. Control        &      `cl`x'15efnl1'       				&      `cl`x'18efnl1'       			&      `cl`y'`x'15efnl1'       						&      `cl`y'`x'18efnl1'       \\
	tex Eff. Treatment      &      `cl`x'15efnr1'       				&      `cl`x'18efnr1'       			&      `cl`y'`x'15efnr1'       						&      `cl`y'`x'18efnr1'       \\
	tex Power               &      `cl`x'15pw1'         				&      `cl`x'18pw1'         			&      `cl`y'`x'15pw1'         						&      `cl`y'`x'18pw1'         \\
	tex \hline
	tex &\multicolumn{4}{c}{Panel B: Quadratic} \\
	tex Treatment Effect	&    `cl`x'15tau2'\sym{`cl`x'15star2'}    	&    `cl`x'18tau2'\sym{`cl`x'18star2'} 	&    `cl`y'`x'15tau2'\sym{`cl`y'`x'15star2'}    	&    `cl`y'`x'18tau2'\sym{`cl`y'`x'18star2'} \\
	tex                    	&    (`cl`x'15se2')         				&    (`cl`x'18se2')          			&    (`cl`y'`x'15se2')         						&    (`cl`y'`x'18se2')           \\
	tex Eff. Control        &      `cl`x'15efnl2'       				&      `cl`x'18efnl2'      				&      `cl`y'`x'15efnl2'       						&      `cl`y'`x'18efnl2'        \\
	tex Eff. Treatment      &      `cl`x'15efnr2'       				&      `cl`x'18efnr2'       			&      `cl`y'`x'15efnr2'       						&      `cl`y'`x'18efnr2'        \\
	tex Power               &      `cl`x'15pw2'         				&      `cl`x'18pw2'         			&      `cl`y'`x'15pw2'         						&      `cl`y'`x'18pw2'          \\
	tex \hline
	tex &\multicolumn{4}{c}{Panel C: Mean Difference} \\
	tex Treatment Effect	&    `cl`x'15tau'\sym{`cl`x'15star'}  	&    `cl`x'18tau'\sym{`cl`x'18star'} 	&    `cl`y'`x'15tau'\sym{`cl`y'`x'15star'}  &    `cl`y'`x'18tau'\sym{`cl`y'`x'18star'}  \\
	tex                    	&    (`cl`x'15se')         				&    (`cl`x'18se')          			&    (`cl`y'`x'15se')         				&    (`cl`y'`x'18se')           \\
	tex Eff. Control        &      `cl`x'15nl'       				&      `cl`x'18nl'       				&      `cl`y'`x'15nl'       				&      `cl`y'`x'18nl'        \\
	tex Eff. Treatment      &      `cl`x'15nr'       				&      `cl`x'18nr'       				&      `cl`y'`x'15nr'       				&      `cl`y'`x'18nr'        \\
	tex Power               &      `cl`x'15pw1'        				&      `cl`x'18pw1'         			&      `cl`y'`x'15pw1'        				&      `cl`y'`x'18pw1'         \\
	tex \hline
	tex Control             &      `cl`x'15nl1'        				&      `cl`x'18nl1'         			&      `cl`y'`x'15nl1'        				&      `cl`y'`x'18nl1'          \\
	tex Treatment           &      `cl`x'15nr1'        				&      `cl`x'18nr1'         			&      `cl`y'`x'15nr1'        				&      `cl`y'`x'18nr1'         \\
	tex \hline\hline
	tex \multicolumn{5}{l}{\footnotesize Standard errors in parentheses}\\
	tex \multicolumn{5}{l}{\footnotesize \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\
	tex \end{tabular}
	texdoc close	
}

forval i=15(3)18 {
	matrix res= (`clsec1`i'tau', `clsec2`i'tau', `clsec3`i'tau', `clsec4`i'tau', `clsec5`i'tau', `clsec6`i'tau' ///
	\ `clsec1`i'se', `clsec2`i'se', `clsec3`i'se', `clsec4`i'se', `clsec5`i'se', `clsec6`i'se' ///
	\ `clsec1`i'nl', `clsec2`i'nl', `clsec3`i'nl', `clsec4`i'nl', `clsec5`i'nl', `clsec6`i'nl' ///
	\ `clsec1`i'nr', `clsec2`i'nr', `clsec3`i'nr', `clsec4`i'nr', `clsec5`i'nr', `clsec6`i'nr')
	matrix coln res = clsec1`i' clsec2`i' clsec3`i' clsec4`i' clsec5`i' clsec6`i'
	randtomat
	order v clsec1`i' clsec2`i' clsec3`i' clsec4`i' clsec5`i' clsec6`i'
	export excel "$tb\res-rand-clsec-`i'.xlsx", replace first(var)
	
	texdoc init "$tb\res-clsec-`i'.tex", replace force
	tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
	tex \begin{tabular}{l*{6}{c}} 
	tex \hline\hline 
	tex                    &\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}&\multicolumn{1}{c}{(3)}&\multicolumn{1}{c}{(4)}&\multicolumn{1}{c}{(5)}&\multicolumn{1}{c}{(6)}\\
	tex 				   &\multicolumn{6}{c}{Ever do work in __ before `i'}\\	
	tex                    &\multicolumn{1}{c}{Agriculture}&\multicolumn{1}{c}{Mining}&\multicolumn{1}{c}{Manufacturing}&\multicolumn{1}{c}{Services}&\multicolumn{1}{c}{Construction}&\multicolumn{1}{c}{Retail}\\
	tex \hline
	tex &\multicolumn{6}{c}{Panel A: Linear} \\
	tex Treatment Effect	&    `clsec1`i'tau1'\sym{`clsec1`i'star1'}    	&    `clsec2`i'tau1'\sym{`clsec2`i'star1'} 	&    `clsec3`i'tau1'\sym{`clsec3`i'star1'}    	&    `clsec4`i'tau1'\sym{`clsec4`i'star1'} 	&    `clsec5`i'tau1'\sym{`clsec5`i'star1'}    	&    `clsec6`i'tau1'\sym{`clsec6`i'star1'} \\
	tex                    	&    (`clsec1`i'se1')         					&    (`clsec2`i'se1')          				&    (`clsec3`i'se1')         					&    (`clsec4`i'se1') 						&    (`clsec5`i'se1')         					&    (`clsec6`i'se1')           \\
	tex Eff. Control        &      `clsec1`i'efnl1'       					&      `clsec2`i'efnl1'       				&      `clsec3`i'efnl1'       					&      `clsec4`i'efnl1' 					&      `clsec5`i'efnl1'       					&      `clsec6`i'efnl1'       \\
	tex Eff. Treatment      &      `clsec1`i'efnr1'       					&      `clsec2`i'efnr1'       				&      `clsec3`i'efnr1'       					&      `clsec4`i'efnr1' 					&      `clsec5`i'efnr1'       					&      `clsec6`i'efnr1'       \\
	tex Power               &      `clsec1`i'pw1'         					&      `clsec2`i'pw1'         				&      `clsec3`i'pw1'         					&      `clsec4`i'pw1'  						&      `clsec5`i'pw1'         					&      `clsec6`i'pw1'        \\
	tex \hline
	tex &\multicolumn{6}{c}{Panel B: Quadratic} \\
	tex Treatment Effect	&    `clsec1`i'tau2'\sym{`clsec1`i'star2'}    	&    `clsec2`i'tau2'\sym{`clsec2`i'star2'} 	&    `clsec3`i'tau2'\sym{`clsec3`i'star2'}    	&    `clsec4`i'tau2'\sym{`clsec4`i'star2'} 	&    `clsec5`i'tau2'\sym{`clsec5`i'star2'}    	&    `clsec6`i'tau2'\sym{`clsec6`i'star2'} \\
	tex                    	&    (`clsec1`i'se2')         					&    (`clsec2`i'se2')          				&    (`clsec3`i'se2')         					&    (`clsec4`i'se2') 						&    (`clsec5`i'se2')         					&    (`clsec6`i'se2')           \\
	tex Eff. Control        &      `clsec1`i'efnl2'       					&      `clsec2`i'efnl2'      				&      `clsec3`i'efnl2'       					&      `clsec4`i'efnl2' 					&      `clsec5`i'efnl2'       					&      `clsec6`i'efnl2'        \\
	tex Eff. Treatment      &      `clsec1`i'efnr2'       					&      `clsec2`i'efnr2'       				&      `clsec3`i'efnr2'       					&      `clsec4`i'efnr2' 					&      `clsec5`i'efnr2'       					&      `clsec6`i'efnr2'        \\
	tex Power               &      `clsec1`i'pw2'         					&      `clsec2`i'pw2'         				&      `clsec3`i'pw2'         					&      `clsec4`i'pw2' 						&      `clsec5`i'pw2'         					&      `clsec6`i'pw2'          \\
	tex \hline
	tex &\multicolumn{6}{c}{Panel C: Mean Difference} \\
	tex Treatment Effect	&    `clsec1`i'tau'\sym{`clsec1`i'star'}  	&    `clsec2`i'tau'\sym{`clsec2`i'star'} 	&    `clsec3`i'tau'\sym{`clsec3`i'star'}  	&    `clsec4`i'tau'\sym{`clsec4`i'star'} 	&    `clsec5`i'tau'\sym{`clsec5`i'star'}  	&    `clsec6`i'tau'\sym{`clsec4`i'star'}  \\
	tex                    	&    (`clsec1`i'se')         				&    (`clsec2`i'se')          				&    (`clsec3`i'se')         				&    (`clsec4`i'se')  						&    (`clsec5`i'se')         				&    (`clsec6`i'se')          \\
	tex Eff. Control        &      `clsec1`i'nl'       					&      `clsec2`i'nl'       					&      `clsec3`i'nl'       					&      `clsec4`i'nl'  						&      `clsec5`i'nl'       					&      `clsec6`i'nl'       \\
	tex Eff. Treatment      &      `clsec1`i'nr'       					&	   `clsec2`i'nr'       					&      `clsec3`i'nr'       					&      `clsec4`i'nr' 						&      `clsec5`i'nr'       					&      `clsec6`i'nr'        \\
	tex Power               &      `clsec1`i'pw1'        				&      `clsec2`i'pw1'         				&      `clsec3`i'pw1'        				&      `clsec4`i'pw1'   					&      `clsec5`i'pw1'        				&      `clsec6`i'pw1'      \\
	tex \hline
	tex Control             &      `clsec1`i'nl1'        				&      `clsec2`i'nl1'         				&      `clsec3`i'nl1'        				&      `clsec4`i'nl1'  						&      `clsec5`i'nl1'        				&      `clsec6`i'nl1'         \\
	tex Treatment           &      `clsec1`i'nr1'        				&      `clsec2`i'nr1'         				&      `clsec3`i'nr1'        				&      `clsec4`i'nr1'   					&      `clsec5`i'nr1'        				&      `clsec6`i'nr1'      \\
	tex \hline\hline
	tex \multicolumn{7}{l}{\footnotesize Standard errors in parentheses}\\
	tex \multicolumn{7}{l}{\footnotesize \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\
	tex \end{tabular}
	texdoc close	
}
log close 

* ==== EDUCATION & HEALTH ==== *
est clear
capture log close
log using "$lg\res-eduhealth.smcl", replace
use "$ifin\ifls5_final_alt.dta", clear
forval i=1/2 {
	foreach x of global eduhealth {
		di as text _dup(59) "=" ///
		_n as res "`x', p(`i')" ///
		_n as text _dup(59) "=" 
		rdrobust `x' runvar_abp03_YM if inrange(age2,18,55), p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
		loc `x'tau`i': di %12.4fc `e(tau_bc)'
		loc `x'se`i': di %12.4fc `e(se_tau_rb)'
		loc `x'tstat`i'=``x'tau`i''/``x'se`i''
		loc `x'star`i'=cond(abs(``x'tstat`i'')>2.58,"***",cond(abs(``x'tstat`i'')>1.96,"**",cond(abs(``x'tstat`i'')>1.645,"*","")))
		loc `x'nl`i': di %12.0fc `e(N_l)'
		loc `x'nr`i': di %12.0fc `e(N_r)'
		loc `x'efnl`i': di %12.0fc `e(N_h_l)'
		loc `x'efnr`i': di %12.0fc `e(N_h_r)'
		eststo `x'`i': rdpow `x' runvar_abp03_YM if inrange(age2,18,55), p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
		loc `x'pw`i': di %12.4fc `r(power_rbc)'
		qui estadd scalar nl=`e(N_l)' 
		qui estadd scalar nr=`e(N_r)'
		qui estadd scalar efnl=`e(N_h_l)' 
		qui estadd scalar efnr=`e(N_h_r)'
		qui estadd scalar pw=`r(power_rbc)'	
	}
	esttab atdshs2`i' gradshs2`i' missedact_poorhealth`i' depsymptoms`i' ///
	using "$tb/res-rd-eduhealth-p`i'.tex", replace label se ///
	star(* 0.10 ** 0.05 *** 0.01) stats(nl nr efnl efnr pw, labels("Control" "Treatment" "Eff. Control" "Eff. Treatment" "Power") fmt(%12.0fc)) 
}

foreach x of global eduhealth {
	di as text _dup(59) "=" ///
	_n as res "`x', p(0)" ///
	_n as text _dup(59) "=" 
	cap noisily: rdrandinf `x' runvar_abp03_YM if inrange(age2,18,55), kernel(triangular) cov($cov) wmasspoints level(0.1)
	loc `x'nl: di %12.0fc `r(N_left)'
	loc `x'nr: di %12.0fc `r(N_right)'
	loc `x'tau: di %12.4fc `r(obs_stat)'
	loc `x'tstat=invnormal((`r(asy_pval)'/2))
	loc `x'se: di %12.4fc (``x'tau' / ``x'tstat') 
	loc `x'star=cond(abs(``x'tstat') >2.58,"***",cond(abs(``x'tstat') >1.96,"**",cond(abs(``x'tstat') >1.645,"*","")))
}
matrix res= (`atdshs2tau', `gradshs2tau', `missedact_poorhealthtau', `depsymptomstau' ///
\ `atdshs2se', `gradshs2se', `missedact_poorhealthse', `depsymptomsse' ///
\ `atdshs2nl', `gradshs2nl', `missedact_poorhealthnl', `depsymptomsnl' /// 
\ `atdshs2nr', `gradshs2nr', `missedact_poorhealthnr', `depsymptomsnr') 
matrix coln res = $eduhealth
randtomat
order v $eduhealth
export excel "$tb\res-rand-eduhealth.xlsx", replace first(var)

texdoc init "$tb\res-eduhealth.tex", replace force
tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
tex \begin{tabular}{l*{4}{c}} 
tex \hline\hline 
tex                    &\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}&\multicolumn{1}{c}{(3)}&\multicolumn{1}{c}{(4)}\\
tex                    &\multicolumn{1}{c}{Attended High School}&\multicolumn{1}{c}{Graduated High School}&\multicolumn{1}{c}{Poor health}&\multicolumn{1}{c}{Depressive symptoms}\\
tex \hline
tex &\multicolumn{4}{c}{Panel A: Linear} \\
tex Treatment Effect	&    `atdshs2tau1'\sym{`atdshs2star1'}    	&    `gradshs2tau1'\sym{`gradshs2star1'} 	&    `missedact_poorhealthtau1'\sym{`missedact_poorhealthstar1'}    	&    `depsymptomstau1'\sym{`depsymptomsstar1'} \\
tex                    	&    (`atdshs2se1')         				&    (`gradshs2se1')          			&    (`missedact_poorhealthse1')         						&    (`depsymptomsse1')           \\
tex Eff. Control        &      `atdshs2efnl1'       				&      `gradshs2efnl1'       			&      `missedact_poorhealthefnl1'       						&      `depsymptomsefnl1'       \\
tex Eff. Treatment      &      `atdshs2efnr1'       				&      `gradshs2efnr1'       			&      `missedact_poorhealthefnr1'       						&      `depsymptomsefnr1'       \\
tex Power               &      `atdshs2pw1'         				&      `gradshs2pw1'         			&      `missedact_poorhealthpw1'         						&      `depsymptomspw1'         \\
tex \hline
tex &\multicolumn{4}{c}{Panel B: Quadratic} \\
tex Treatment Effect	&    `atdshs2tau2'\sym{`atdshs2star2'}    	&    `gradshs2tau2'\sym{`gradshs2star2'} 	&    `missedact_poorhealthtau2'\sym{`missedact_poorhealthstar2'}    	&    `depsymptomstau2'\sym{`depsymptomsstar2'} \\
tex                    	&    (`atdshs2se2')         				&    (`gradshs2se2')          			&    (`missedact_poorhealthse2')         						&    (`depsymptomsse2')           \\
tex Eff. Control        &      `atdshs2efnl2'       				&      `gradshs2efnl2'      				&      `missedact_poorhealthefnl2'       						&      `depsymptomsefnl2'        \\
tex Eff. Treatment      &      `atdshs2efnr2'       				&      `gradshs2efnr2'       			&      `missedact_poorhealthefnr2'       						&      `depsymptomsefnr2'        \\
tex Power               &      `atdshs2pw2'         				&      `gradshs2pw2'         			&      `missedact_poorhealthpw2'         						&      `depsymptomspw2'          \\
tex \hline
tex &\multicolumn{4}{c}{Panel C: Mean Difference} \\
tex Treatment Effect	&    `atdshs2tau'\sym{`atdshs2star'}  	&    `gradshs2tau'\sym{`gradshs2star'} 	&    `missedact_poorhealthtau'\sym{`missedact_poorhealthstar'}  &    `depsymptomstau'\sym{`depsymptomsstar'}  \\
tex                    	&    (`atdshs2se')         				&    (`gradshs2se')          			&    (`missedact_poorhealthse')         				&    (`depsymptomsse')           \\
tex Eff. Control        &      `atdshs2nl'       				&      `gradshs2nl'       				&      `missedact_poorhealthnl'       				&      `depsymptomsnl'        \\
tex Eff. Treatment      &      `atdshs2nr'       				&      `gradshs2nr'       				&      `missedact_poorhealthnr'       				&      `depsymptomsnr'        \\
tex Power               &      `atdshs2pw1'        				&      `gradshs2pw1'         			&      `missedact_poorhealthpw1'        				&      `depsymptomspw1'         \\
tex \hline
tex Control             &      `atdshs2nl1'        				&      `gradshs2nl1'         			&      `missedact_poorhealthnl1'        				&      `depsymptomsnl1'          \\
tex Treatment           &      `atdshs2nr1'        				&      `gradshs2nr1'         			&      `missedact_poorhealthnr1'        				&      `depsymptomsnr1'         \\
tex \hline\hline
tex \multicolumn{5}{l}{\footnotesize Standard errors in parentheses}\\
tex \multicolumn{5}{l}{\footnotesize \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\
tex \end{tabular}
texdoc close	
log close 

* ==== ADULT LABOR OUTCOMES ==== *
est clear
capture log close
log using "$lg\res-adultlabor.smcl", replace
use "$ifin\ifls5_final_alt.dta", clear
forval i=1/2 {
	foreach x of global labovar {
		di as text _dup(59) "=" ///
		_n as res "`x', p(`i')" ///
		_n as text _dup(59) "=" 
		rdrobust `x' runvar_abp03_YM if inrange(age2,18,55), p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
		loc `x'tau`i': di %12.4fc `e(tau_bc)'
		loc `x'se`i': di %12.4fc `e(se_tau_rb)'
		loc `x'tstat`i'=``x'tau`i''/``x'se`i''
		loc `x'star`i'=cond(abs(``x'tstat`i'')>2.58,"***",cond(abs(``x'tstat`i'')>1.96,"**",cond(abs(``x'tstat`i'')>1.645,"*","")))
		loc `x'nl`i': di %12.0fc `e(N_l)'
		loc `x'nr`i': di %12.0fc `e(N_r)'
		loc `x'efnl`i': di %12.0fc `e(N_h_l)'
		loc `x'efnr`i': di %12.0fc `e(N_h_r)'
		eststo `x'`i': rdpow `x' runvar_abp03_YM if inrange(age2,18,55), p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
		loc `x'pw`i': di %12.4fc `r(power_rbc)'
		qui estadd scalar nl=`e(N_l)' 
		qui estadd scalar nr=`e(N_r)'
		qui estadd scalar efnl=`e(N_h_l)' 
		qui estadd scalar efnr=`e(N_h_r)'
		qui estadd scalar pw=`r(power_rbc)'	
	}
	esttab employedpw`i' wformal`i' wcontract`i' ///
	using "$tb/res-rd-adultlabor-p`i'.tex", replace label se ///
	star(* 0.10 ** 0.05 *** 0.01) stats(nl nr efnl efnr pw, labels("Control" "Treatment" "Eff. Control" "Eff. Treatment" "Power") fmt(%12.0fc)) 
}

foreach x of global labovar {
	di as text _dup(59) "=" ///
	_n as res "`x', p(0)" ///
	_n as text _dup(59) "=" 
	cap noisily: rdrandinf `x' runvar_abp03_YM if inrange(age2,18,55), kernel(triangular) cov($cov) wmasspoints level(0.1)
	loc `x'nl: di %12.0fc `r(N_left)'
	loc `x'nr: di %12.0fc `r(N_right)'
	loc `x'tau: di %12.4fc `r(obs_stat)'
	loc `x'tstat=invnormal((`r(asy_pval)'/2))
	loc `x'se: di %12.4fc (``x'tau' / ``x'tstat') 
	loc `x'star=cond(abs(``x'tstat') >2.58,"***",cond(abs(``x'tstat') >1.96,"**",cond(abs(``x'tstat') >1.645,"*","")))
}
matrix res= (`employedpwtau', `wformaltau', `wcontracttau', `lnearningspytau' ///
\ `employedpwse', `wformalse', `wcontractse', `lnearningspyse' ///
\ `employedpwnl', `wformalnl', `wcontractnl', `lnearningspynl' /// 
\ `employedpwnr', `wformalnr', `wcontractnr', `lnearningspynr') 
matrix coln res = $labovar
randtomat
order v $labovar
export excel "$tb\res-rand-adultlabor.xlsx", replace first(var)

texdoc init "$tb\res-adultlabor.tex", replace force
tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
tex \begin{tabular}{l*{4}{c}} 
tex \hline\hline 
tex                    &\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}&\multicolumn{1}{c}{(3)}&\multicolumn{1}{c}{(4)}\\
tex                    &\multicolumn{1}{c}{Employed past week}&\multicolumn{1}{c}{Work formal jobs}&\multicolumn{1}{c}{Work with a contract}&\multicolumn{1}{c}{Earnings past month}\\
tex \hline
tex &\multicolumn{4}{c}{Panel A: Linear} \\
tex Treatment Effect	&    `employedpwtau1'\sym{`employedpwstar1'}    	&    `wformaltau1'\sym{`wformalstar1'} 	&    `wcontracttau1'\sym{`wcontractstar1'}    	&    `lnearningspytau1'\sym{`lnearningspystar1'} \\
tex                    	&    (`employedpwse1')         				&    (`wformalse1')          			&    (`wcontractse1')         						&    (`lnearningspyse1')           \\
tex Eff. Control        &      `employedpwefnl1'       				&      `wformalefnl1'       			&      `wcontractefnl1'       						&      `lnearningspyefnl1'       \\
tex Eff. Treatment      &      `employedpwefnr1'       				&      `wformalefnr1'       			&      `wcontractefnr1'       						&      `lnearningspyefnr1'       \\
tex Power               &      `employedpwpw1'         				&      `wformalpw1'         			&      `wcontractpw1'         						&      `lnearningspypw1'         \\
tex \hline
tex &\multicolumn{4}{c}{Panel B: Quadratic} \\
tex Treatment Effect	&    `employedpwtau2'\sym{`employedpwstar2'}    	&    `wformaltau2'\sym{`wformalstar2'} 	&    `wcontracttau2'\sym{`wcontractstar2'}    	&    `lnearningspytau2'\sym{`lnearningspystar2'} \\
tex                    	&    (`employedpwse2')         				&    (`wformalse2')          			&    (`wcontractse2')         						&    (`lnearningspyse2')           \\
tex Eff. Control        &      `employedpwefnl2'       				&      `wformalefnl2'      				&      `wcontractefnl2'       						&      `lnearningspyefnl2'        \\
tex Eff. Treatment      &      `employedpwefnr2'       				&      `wformalefnr2'       			&      `wcontractefnr2'       						&      `lnearningspyefnr2'        \\
tex Power               &      `employedpwpw2'         				&      `wformalpw2'         			&      `wcontractpw2'         						&      `lnearningspypw2'          \\
tex \hline
tex &\multicolumn{4}{c}{Panel C: Mean Difference} \\
tex Treatment Effect	&    `employedpwtau'\sym{`employedpwstar'}  	&    `wformaltau'\sym{`wformalstar'} 	&    `wcontracttau'\sym{`wcontractstar'}  &    `lnearningspytau'\sym{`lnearningspystar'}  \\
tex                    	&    (`employedpwse')         				&    (`wformalse')          			&    (`wcontractse')         				&    (`lnearningspyse')           \\
tex Eff. Control        &      `employedpwnl'       				&      `wformalnl'       				&      `wcontractnl'       				&      `lnearningspynl'        \\
tex Eff. Treatment      &      `employedpwnr'       				&      `wformalnr'       				&      `wcontractnr'       				&      `lnearningspynr'        \\
tex Power               &      `employedpwpw1'        				&      `wformalpw1'         			&      `wcontractpw1'        				&      `lnearningspypw1'         \\
tex \hline
tex Control             &      `employedpwnl1'        				&      `wformalnl1'         			&      `wcontractnl1'        				&      `lnearningspynl1'          \\
tex Treatment           &      `employedpwnr1'        				&      `wformalnr1'         			&      `wcontractnr1'        				&      `lnearningspynr1'         \\
tex \hline\hline
tex \multicolumn{5}{l}{\footnotesize Standard errors in parentheses}\\
tex \multicolumn{5}{l}{\footnotesize \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\
tex \end{tabular}
texdoc close	
log close 

* ==== HETEROGENEITY ==== *
est clear
capture log close
log using "$lg\res-heterogeneity.smcl", replace
foreach y in male2 javanese2 bwin_formalwork {
	use "$ifin\ifls5_final_alt.dta", clear
	loc h=substr("`y'",1,4)
	g runvarsq_abp03_YM=runvar_abp03_YM^2
	forval i=1/2 {
		foreach x of varlist childlabor* {
			forval j=0/1 {
				di as text _dup(59) "=" ///
				_n as res "nonparam, `x', p(`i'), `h'==`j'" ///
				_n as text _dup(59) "=" 
				rdrobust `x' runvar_abp03_YM if inrange(age2,18,55) & `y'==`j', p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
				loc `x'tau`i'`h'`j': di %12.4fc `e(tau_bc)'
				loc `x'se`i'`h'`j': di %12.4fc `e(se_tau_rb)'
				loc `x'tstat`i'`h'`j'=``x'tau`i'`h'`j''/``x'se`i'`h'`j''
				loc `x'star`i'`h'`j'=cond(abs(``x'tstat`i'`h'`j'')>2.58,"***",cond(abs(``x'tstat`i'`h'`j'')>1.96,"**",cond(abs(``x'tstat`i'`h'`j'')>1.645,"*","")))
				loc `x'nl`i'`h'`j': di %12.0fc `e(N_l)'
				loc `x'nr`i'`h'`j': di %12.0fc `e(N_r)'
				loc `x'efnl`i'`h'`j': di %12.0fc `e(N_h_l)'
				loc `x'efnr`i'`h'`j': di %12.0fc `e(N_h_r)'
				loc `x'lbw`i'`h'`j'=-`e(h_l)'
				loc `x'rbw`i'`h'`j'=`e(h_r)'
				eststo `x'`i'`h'`j': rdpow `x' runvar_abp03_YM if inrange(age2,18,55) & `y'==`j', p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
				loc `x'pw`i'`h'`j': di %12.4fc `r(power_rbc)'
				qui estadd scalar nl=`e(N_l)' 
				qui estadd scalar nr=`e(N_r)'
				qui estadd scalar efnl=`e(N_h_l)' 
				qui estadd scalar efnr=`e(N_h_r)'
				qui estadd scalar pw=`r(power_rbc)'	

				di as text _dup(59) "=" ///
				_n as res "param, `x', p(`i'), `h'==`j'" ///
				_n as text _dup(59) "="
				if `i'==1 {
					reg `x' itt_abp03_YM##c.runvar_abp03_YM $cov if inrange(age2,18,55) & `y'==`j' & inrange(runvar_abp03_YM,``x'lbw`i'`h'`j'',``x'rbw`i'`h'`j''), cluster (runvar_abp03_YM) 
				}
				else if `i'==2 {
					reg `x' itt_abp03_YM##c.runvar_abp03_YM itt_abp03_YM##c.runvarsq_abp03_YM $cov if inrange(age2,18,55) & `y'==`j' & inrange(runvar_abp03_YM,``x'lbw`i'`h'`j'',``x'rbw`i'`h'`j''), vce(cluster runvar_abp03_YM) 					
				}
				loc p`x'tau`i'`h'`j': di %12.4fc r(table)[1,2]
				loc p`x'se`i'`h'`j': di %12.4fc r(table)[2,2]		
				loc p`x'tstat`i'`h'`j'=r(table)[3,2]
				loc p`x'star`i'`h'`j'=cond(abs(`p`x'tstat`i'`h'`j'')>2.58,"***",cond(abs(`p`x'tstat`i'`h'`j'')>1.96,"**",cond(abs(`p`x'tstat`i'`h'`j'')>1.645,"*","")))			
				qui count if inrange(age2,18,55) & runvar_abp03_YM<0 & `y'==`j' & runvar_abp03_YM>=``x'lbw`i'`h'`j'' 
				loc p`x'efnl`i'`h'`j': di %12.0fc `r(N)'
				qui count if inrange(age2,18,55) & `y'==`j' & inrange(runvar_abp03_YM,0,``x'rbw`i'`h'`j'')
				loc p`x'efnr`i'`h'`j': di %12.0fc `r(N)'

				loc np`x'tau`i'`h'`j'=``x'tau`i'`h'`j''
				loc np`x'se`i'`h'`j'=``x'se`i'`h'`j''
				loc np`x'star`i'`h'`j'="``x'star`i'`h'`j''"
				loc np`x'efnl`i'`h'`j'="``x'efnl`i'`h'`j''"
				loc np`x'efnr`i'`h'`j'="``x'efnr`i'`h'`j''"
			}	
		}
		esttab childlabor15`i'`h'0 childlabor15`i'`h'1 childlabor18`i'`h'0 childlabor18`i'`h'1   ///
		using "$tb/res-rd-`h'-p`i'.tex", replace label se ///
		star(* 0.10 ** 0.05 *** 0.01) stats(nl nr efnl efnr pw, labels("Control" "Treatment" "Eff. Control" "Eff. Treatment" "Power") fmt(%12.0fc)) 
	}
	
	// if "`h'"!="java" {
	// 	foreach x of varlist childlabor* {
	// 		forval j=0/1 {
	// 			di as text _dup(59) "=" ///
	// 			_n as res "`x', p(0), `h'==`j'" ///
	// 			_n as text _dup(59) "=" 
	// 			cap noisily: rdrandinf `x' runvar_abp03_YM if inrange(age2,18,55) & `y'==`j', kernel(triangular) cov($cov) wmasspoints level(0.1) 
	// 			loc `x'nl: di %12.0fc `r(N_left)'
	// 			loc `x'nr: di %12.0fc `r(N_right)'
	// 			loc `x'tau: di %12.4fc `r(obs_stat)'
	// 			loc `x'tstat=invnormal((`r(asy_pval)'/2))
	// 			loc `x'se: di %12.4fc (``x'tau' / ``x'tstat') 
	// 			loc `x'star=cond(abs(``x'tstat') >2.58,"***",cond(abs(``x'tstat') >1.96,"**",cond(abs(``x'tstat') >1.645,"*","")))	
	// 		}
	// 	}
	// 	matrix res= (`childlabor15tau`h'0', `childlabor15tau`h'1', `childlabor18tau`h'0', `childlabor18tau`h'1' ///
	// 	\ `childlabor15se`h'0', `childlabor15se`h'1', `childlabor18se`h'0', `childlabor18se`h'1' ///
	// 	\ `childlabor15nl`h'0', `childlabor15nl`h'1', `childlabor18nl`h'0', `childlabor18nl`h'1' ///
	// 	\ `childlabor15nr`h'0', `childlabor15nr`h'1', `childlabor18nr`h'0', `childlabor18nr`h'1') 
	// 	matrix coln res = childlabor15_`h'0 childlabor15_`h'1 childlabor18_`h'0 childlabor18_`h'1
	// 	randtomat
	// 	order v childlabor15_`h'0 childlabor15_`h'1 childlabor18_`h'0 childlabor18_`h'1
	// 	export excel "$tb\res-rand-`h'.xlsx", replace first(var)	
	// }

	loc amale "Male"
	loc bmale "Female"
	loc ajava "Javanese"
	loc bjava "Non-Javanese"
	loc abwin "Breadwinner in formal work"
	loc bbwin "Breadwinner not in formal work"

	texdoc init "$tb\res-`h'.tex", replace force
	tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
	tex \begin{tabular}{l*{4}{c}} 
	tex \hline\hline 
	tex                    &\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}&\multicolumn{1}{c}{(3)}&\multicolumn{1}{c}{(4)}\\
	tex                    &\multicolumn{2}{c}{a`h'}&\multicolumn{2}{c}{b`h'}
	tex                    &\multicolumn{1}{c}{Ever work before 15}&\multicolumn{1}{c}{Ever work before 18}&\multicolumn{1}{c}{Ever work before 15}&\multicolumn{1}{c}{Ever work before 18}\\
	tex \hline
	tex &\multicolumn{4}{c}{Panel A: Nonparametric Linear} \\
	tex Treatment Effect	&    `pchildlabor15tau1`h'1'\sym{`pchildlabor15star1`h'1'}    	&    `pchildlabor18tau1`h'1'\sym{`pchildlabor18star1`h'1'} 	&    `pchildlabor15tau1`h'0'\sym{`pchildlabor15star1`h'0'}  &    `pchildlabor18tau1`h'0'\sym{`pchildlabor18star1`h'0'} \\
	tex                    	&    (`pchildlabor15se1`h'1')         							&    (`pchildlabor18se1`h'1')          						&    (`pchildlabor15se1`h'0')         						&    (`pchildlabor18se1`h'0')           \\
	tex Eff. Control        &      `pchildlabor15efnl1`h'1'       							&      `pchildlabor18efnl1`h'1'       						&      `pchildlabor15efnl1`h'0'       						&      `pchildlabor18efnl1`h'0'       \\
	tex Eff. Treatment      &      `pchildlabor15efnr1`h'1'       							&      `pchildlabor18efnr1`h'1'       						&      `pchildlabor15efnr1`h'0'       						&      `pchildlabor18efnr1`h'0'       \\
	tex Power               &      `npchildlabor15pw1`h'1'         							&      `npchildlabor18pw1`h'1'         						&      `npchildlabor15pw1`h'0'         						&      `npchildlabor18pw1`h'0'         \\
	tex \hline
	tex &\multicolumn{4}{c}{Panel B: Nonparametric Quadratic} \\
	tex Treatment Effect	&    `npchildlabor15tau2`h'1'\sym{`npchildlabor15star2`h'1'}    &    `npchildlabor18tau2`h'1'\sym{`npchildlabor18star2`h'1'} 	&    `npchildlabor15tau2`h'0'\sym{`npchildlabor15star2`h'0'}	&    `npchildlabor18tau2`h'0'\sym{`npchildlabor18star2`h'0'} \\
	tex                    	&    (`npchildlabor15se2`h'1')         							&    (`npchildlabor18se2`h'1')          						&    (`npchildlabor15se2`h'0')         							&    (`npchildlabor18se2`h'0')           \\
	tex Eff. Control        &      `npchildlabor15efnl2`h'1'       							&      `npchildlabor18efnl2`h'1'      							&      `npchildlabor15efnl2`h'0'       							&      `npchildlabor18efnl2`h'0'        \\
	tex Eff. Treatment      &      `npchildlabor15efnr2`h'1'       							&      `npchildlabor18efnr2`h'1'       							&      `npchildlabor15efnr2`h'0'       							&      `npchildlabor18efnr2`h'0'        \\
	tex Power               &      `npchildlabor15pw2`h'1'         							&      `npchildlabor18pw2`h'1'         							&      `npchildlabor15pw2`h'0'         							&      `npchildlabor18pw2`h'0'          \\
	tex \hline
	tex &\multicolumn{4}{c}{Panel C: Parametric Linear} \\
	tex Treatment Effect	&    `pchildlabor15tau`h'1'\sym{`pchildlabor15star`h'1'}  		&    `pchildlabor18tau`h'1'\sym{`pchildlabor18star`h'1'} 	&    `pchildlabor15tau`h'0'\sym{`pchildlabor15star`h'0'}  	&    `pchildlabor18tau`h'0'\sym{`pchildlabor18star`h'0'}  \\
	tex                    	&    (`pchildlabor15se`h'1')         							&    (`pchildlabor18se`h'1')          						&    (`pchildlabor15se`h'0')         						&    (`pchildlabor18se`h'0')           \\
	tex Eff. Control        &      `pchildlabor15nl`h'1'       								&      `pchildlabor18nl`h'1'       							&      `pchildlabor15nl`h'0'       							&      `pchildlabor18nl`h'0'        \\
	tex Eff. Treatment      &      `pchildlabor15nr`h'1'       								&      `pchildlabor18nr`h'1'       							&      `pchildlabor15nr`h'0'       							&      `pchildlabor18nr`h'0'        \\
	tex \hline
	tex &\multicolumn{4}{c}{Panel D: Parametric Quadratic} \\
	tex Treatment Effect	&    `pchildlabor15tau2`h'1'\sym{`pchildlabor15star2`h'1'}    	&    `pchildlabor18tau2`h'1'\sym{`pchildlabor18star2`h'1'} 	&    `pchildlabor15tau2`h'0'\sym{`pchildlabor15star2`h'0'}  &    `pchildlabor18tau2`h'0'\sym{`pchildlabor18star2`h'0'} \\
	tex                    	&    (`pchildlabor15se2`h'1')         							&    (`pchildlabor18se2`h'1')          						&    (`pchildlabor15se2`h'0')         						&    (`pchildlabor18se2`h'0')           \\
	tex Eff. Control        &      `pchildlabor15efnl2`h'1'       							&      `pchildlabor18efnl2`h'1'      						&      `pchildlabor15efnl2`h'0'       						&      `pchildlabor18efnl2`h'0'        \\
	tex Eff. Treatment      &      `pchildlabor15efnr2`h'1'       							&      `pchildlabor18efnr2`h'1'       						&      `pchildlabor15efnr2`h'0'       						&      `pchildlabor18efnr2`h'0'        \\
	tex \hline
	tex Control             &      `childlabor15nl1`h'1'        							&      `childlabor18nl1`h'1'         						&      `childlabor15nl1`h'0'        						&      `childlabor18nl1`h'0'          \\
	tex Treatment           &      `childlabor15nr1`h'1'        							&      `childlabor18nr1`h'1'         						&      `childlabor15nr1`h'0'        						&      `childlabor18nr1`h'0'         \\
	tex \hline\hline
	tex \multicolumn{5}{l}{\footnotesize Standard errors in parentheses}\\
	tex \multicolumn{5}{l}{\footnotesize \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\
	tex \end{tabular}
	texdoc close	
}
log close

* ==== CUTOFF AT 2002 ==== *
est clear
capture log close
log using "$lg\res-childlabor-2002.smcl", replace
use "$ifin\ifls5_final_alt.dta", clear
foreach x of varlist childlabor* {
	forval i=1/2 {
		di as text _dup(59) "=" ///
		_n as res "`x', p(`i')" ///
		_n as text _dup(59) "=" 
		rdrobust `x' runvar_abp02_YM if inrange(age2,18,55), p(`i') covs($cov) vce(cluster runvar_abp02_YM) all
		loc `x'tau`i': di %12.4fc `e(tau_bc)'
		loc `x'se`i': di %12.4fc `e(se_tau_rb)'
		loc `x'tstat`i'=``x'tau`i''/``x'se`i''
		loc `x'star`i'=cond(abs(``x'tstat`i'')>2.58,"***",cond(abs(``x'tstat`i'')>1.96,"**",cond(abs(``x'tstat`i'')>1.645,"*","")))
		loc `x'nl`i': di %12.0fc `e(N_l)'
		loc `x'nr`i': di %12.0fc `e(N_r)'
		loc `x'efnl`i': di %12.0fc `e(N_h_l)'
		loc `x'efnr`i': di %12.0fc `e(N_h_r)'
		eststo `x'`i': rdpow `x' runvar_abp02_YM if inrange(age2,18,55), p(`i') covs($cov) vce(cluster runvar_abp02_YM) all
		loc `x'pw`i': di %12.4fc `r(power_rbc)'
		qui estadd scalar nl=`e(N_l)' 
		qui estadd scalar nr=`e(N_r)'
		qui estadd scalar efnl=`e(N_h_l)' 
		qui estadd scalar efnr=`e(N_h_r)'
		qui estadd scalar pw=`r(power_rbc)'	
	}
}
esttab childlabor151 childlabor181 childlabor152 childlabor182 ///
using "$tb/res-rd-childlabor-2002.tex", replace label se ///
star(* 0.10 ** 0.05 *** 0.01) stats(nl nr efnl efnr pw, labels("Control" "Treatment" "Eff. Control" "Eff. Treatment" "Power") fmt(%12.0fc)) ///
prehead(`"\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"' ///
		`"\begin{tabular}{l*{4}{c}}"' ///
		`"\hline\hline"' ///
		`"&\multicolumn{2}{c}{Linear} &\multicolumn{2}{c}{Quadratic} \\"'  ///
		`"\cmidrule(rl){2-5}"')
				
// foreach x of varlist childlabor* {
// 	di as text _dup(59) "=" ///
// 	_n as res "`x', p(0)" ///
// 	_n as text _dup(59) "=" 
// 	cap noisily: rdrandinf `x' runvar_abp02_YM if inrange(age2,18,55), kernel(triangular) cov($cov) wmasspoints level(0.1) 
// 	loc `x'nl: di %12.0fc `r(N_left)'
// 	loc `x'nr: di %12.0fc `r(N_right)'
// 	loc `x'tau: di %12.4fc `r(obs_stat)'
// 	loc `x'se: di %12.4fc (``x'tau' / ``x'tstat')
// }

// matrix res= (`childlabor15tau', `childlabor18tau' \ `childlabor15se', `childlabor18se'  \ `childlabor15nl', `childlabor18nl' \ `childlabor15nr', `childlabor18nr') 
// matrix coln res = childlabor15 childlabor18
// randtomat
// order v childlabor15 childlabor18
// export excel "$tb\res-rand-childlabor-2002.xlsx", replace first(var)

texdoc init "$tb\res-childlabor-2002.tex", replace force
tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
tex \begin{tabular}{l*{4}{c}} 
tex \hline\hline 
tex                    &\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}&\multicolumn{1}{c}{(3)}&\multicolumn{1}{c}{(4)}\\
tex 				   &\multicolumn{2}{c}{Linear}&\multicolumn{2}{c}{Quadratic}\\
tex                    &\multicolumn{1}{c}{Ever work before 15}&\multicolumn{1}{c}{Ever work before 18}&\multicolumn{1}{c}{Ever work before 15}&\multicolumn{1}{c}{Ever work before 18}\\
tex \hline
tex Treatment Effect	&    `childlabor15tau1'\sym{`childlabor15star1'}    &    `childlabor18tau1'\sym{`childlabor18star1'} 	&    `childlabor15tau2'\sym{`childlabor15star2'}    &    `childlabor18tau2'\sym{`childlabor18star2'} \\
tex                    	&    (`childlabor15se1')         					&    (`childlabor18se1') 							&    (`childlabor15se2')         					&    (`childlabor18se2')                    \\
tex Eff. Control        &      `childlabor15efnl1'       					&      `childlabor18efnl1' 							&      `childlabor15efnl2'       					&      `childlabor18efnl2'       \\
tex Eff. Treatment      &      `childlabor15efnr1'       					&      `childlabor18efnr1' 							&      `childlabor15efnr2'       					&      `childlabor18efnr2'       \\
tex Power               &      `childlabor15pw1'         					&      `childlabor18pw1' 							&      `childlabor15pw2'         					&      `childlabor18pw2'         \\
tex \hline
tex Control             &      `childlabor15nl1'        					&      `childlabor18nl1' 							&      `childlabor15nl2'        					&      `childlabor18nl2'         \\
tex Treatment           &      `childlabor15nr1'        					&      `childlabor18nr1' 							&      `childlabor15nr2'        					&      `childlabor18nr2'         \\
tex \hline\hline
tex \multicolumn{5}{l}{\footnotesize Standard errors in parentheses}\\
tex \multicolumn{5}{l}{\footnotesize \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\
tex \end{tabular}
texdoc close
log close

* ==== CHILD LABOR - 1997 ==== *
est clear
capture log close
log using "$lg\res-childlabor-1998.smcl", replace
use "$ifin\ifls5_final_alt.dta", clear
foreach x of varlist childlabor* {
	forval i=1/2 {
		di as text _dup(59) "=" ///
		_n as res "`x', p(`i')" ///
		_n as text _dup(59) "=" 
		rdrobust `x' runvar_abp98_YM if inrange(age2,18,55), p(`i') covs($cov) vce(cluster runvar_abp98_YM) all
		loc `x'tau`i': di %12.4fc `e(tau_bc)'
		loc `x'se`i': di %12.4fc `e(se_tau_rb)'
		loc `x'tstat`i'=``x'tau`i''/``x'se`i''
		loc `x'star`i'=cond(abs(``x'tstat`i'')>2.58,"***",cond(abs(``x'tstat`i'')>1.96,"**",cond(abs(``x'tstat`i'')>1.645,"*","")))
		loc `x'nl`i': di %12.0fc `e(N_l)'
		loc `x'nr`i': di %12.0fc `e(N_r)'
		loc `x'efnl`i': di %12.0fc `e(N_h_l)'
		loc `x'efnr`i': di %12.0fc `e(N_h_r)'
		eststo `x'`i': rdpow `x' runvar_abp98_YM if inrange(age2,18,55), p(`i') covs($cov) vce(cluster runvar_abp98_YM) all
		loc `x'pw`i': di %12.4fc `r(power_rbc)'
		qui estadd scalar nl=`e(N_l)' 
		qui estadd scalar nr=`e(N_r)'
		qui estadd scalar efnl=`e(N_h_l)' 
		qui estadd scalar efnr=`e(N_h_r)'
		qui estadd scalar pw=`r(power_rbc)'	
	}
}

esttab childlabor151 childlabor181 childlabor152 childlabor182 ///
using "$tb/res-rd-childlabor-1998.tex", replace label se ///
star(* 0.10 ** 0.05 *** 0.01) stats(nl nr efnl efnr pw, labels("Control" "Treatment" "Eff. Control" "Eff. Treatment" "Power") fmt(%12.0fc)) ///
prehead(`"\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"' ///
		`"\begin{tabular}{l*{4}{c}}"' ///
		`"\hline\hline"' ///
		`"&\multicolumn{2}{c}{Linear} &\multicolumn{2}{c}{Quadratic} \\"'  ///
		`"\cmidrule(rl){2-5}"')  
		
foreach x of varlist childlabor* {
	di as text _dup(59) "=" ///
	_n as res "`x', p(0)" ///
	_n as text _dup(59) "=" 
	cap noisily: rdrandinf `x' runvar_abp98_YM if inrange(age2,18,55), kernel(triangular) cov($cov) wmasspoints level(0.1) 
	loc `x'nl: di %12.0fc `r(N_left)'
	loc `x'nr: di %12.0fc `r(N_right)'
	loc `x'tau: di %12.4fc `r(obs_stat)'
	loc `x'tstat=invnormal((`r(asy_pval)'/2))
	loc `x'se: di %12.4fc (``x'tau' / ``x'tstat') 
	loc `x'star=cond(abs(``x'tstat') >2.58,"***",cond(abs(``x'tstat') >1.96,"**",cond(abs(``x'tstat') >1.645,"*","")))
}
matrix res= (`childlabor15tau', `childlabor18tau' \ `childlabor15se', `childlabor18se'  \ `childlabor15nl', `childlabor18nl' \ `childlabor15nr', `childlabor18nr') 
matrix coln res = childlabor15 childlabor18
randtomat
order v childlabor15 childlabor18
export excel "$tb\res-rand-childlabor-1998.xlsx", replace first(var)

texdoc init "$tb\res-childlabor-1998.tex", replace force
tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
tex \begin{tabular}{l*{3}{c}} 
tex \hline\hline 
tex                    &\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}&\multicolumn{1}{c}{(3)}\\
tex                    &\multicolumn{1}{c}{Linear}&\multicolumn{1}{c}{Quadratic}&\multicolumn{1}{c}{Mean Difference}\\
tex \hline
tex Treatment Effect	&    `childlabor15tau1'\sym{`childlabor15star1'}    &    `childlabor15tau2'\sym{`childlabor15star2'} 	&    `childlabor15tau'\sym{`childlabor15star'} \\
tex                    	&    (`childlabor15se1')         					&    (`childlabor15se2') 							&    (`childlabor15se')          \\
tex Eff. Control        &      `childlabor15efnl1'       					&      `childlabor15efnl2' 							&      `childlabor15nl'       \\
tex Eff. Treatment      &      `childlabor15efnr1'       					&      `childlabor15efnr2' 							&      `childlabor15nr'       \\
tex Power               &      `childlabor15pw1'         					&      `childlabor15pw2' 							&      `childlabor15pw1'         \\
tex \hline
tex Control             &      `childlabor15nl1'        					&      `childlabor15nl2' 							&      `childlabor15nl1'         \\
tex Treatment           &      `childlabor15nr1'        					&      `childlabor15nr2' 							&      `childlabor15nr1'         \\
tex \hline\hline
tex \multicolumn{4}{l}{\footnotesize Standard errors in parentheses}\\
tex \multicolumn{4}{l}{\footnotesize \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\
tex \end{tabular}
texdoc close
log close 

* ==== PLACEBO CUT-OFF ==== *
capture log close 
est clear
log using "$lg\placebo.smcl", replace
use "$ifin\ifls5_final_alt.dta", clear
forval j=1/2 {
	foreach x of varlist childlabor* {
		forval i=1/2 {
			di as text _dup(59) "-" ///
			_n as res "`x'" ///
			_n as text _dup(59) "-"

			di as text _dup(59) "-" ///
			_n as res "CUTOFF: `j', p(`i')" ///
			_n as text _dup(59) "-"
			rdrobust `x' runvar_abp03_YM if inrange(age2,18,55), c(`j') p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
			loc `x'atau`i'`j': di %12.4fc `e(tau_bc)'
			loc `x'ase`i'`j': di %12.4fc `e(se_tau_rb)'
			loc `x'atstat`i'`j'=``x'atau`i'`j''/``x'ase`i'`j''
			loc `x'astar`i'`j'=cond(abs(``x'atstat`i'`j'')>2.58,"***",cond(abs(``x'atstat`i'`j'')>1.96,"**",cond(abs(``x'atstat`i'`j'')>1.645,"*","")))
			loc `x'anl`i'`j': di %12.0fc `e(N_l)'
			loc `x'anr`i'`j': di %12.0fc `e(N_r)'
			loc `x'aefnl`i'`j': di %12.0fc `e(N_h_l)'
			loc `x'aefnr`i'`j': di %12.0fc `e(N_h_r)'
			rdpow `x' runvar_abp03_YM if inrange(age2,18,55), c(`j') p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
			loc `x'apw`i'`j': di %12.4fc `r(power_rbc)'

			di as text _dup(59) "-" ///
			_n as res "CUTOFF: -`j', p(`i')" ///
			_n as text _dup(59) "-"
			rdrobust `x' runvar_abp03_YM if inrange(age2,18,55), c(-`j') p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
			loc `x'btau`i'`j': di %12.4fc `e(tau_bc)'
			loc `x'bse`i'`j': di %12.4fc `e(se_tau_rb)'
			loc `x'btstat`i'`j'=``x'btau`i'`j''/``x'bse`i'`j''
			loc `x'bstar`i'`j'=cond(abs(``x'btstat`i'`j'')>2.58,"***",cond(abs(``x'btstat`i'`j'')>1.96,"**",cond(abs(``x'btstat`i'`j'')>1.645,"*","")))
			loc `x'bnl`i'`j': di %12.0fc `e(N_l)'
			loc `x'bnr`i'`j': di %12.0fc `e(N_r)'
			loc `x'befnl`i'`j': di %12.0fc `e(N_h_l)'
			loc `x'befnr`i'`j': di %12.0fc `e(N_h_r)'
			rdpow `x' runvar_abp03_YM if inrange(age2,18,55), c(-`j') p(`i') covs($cov) vce(cluster runvar_abp03_YM) all
			loc `x'bpw`i'`j': di %12.4fc `r(power_rbc)'
		}
		di as text _dup(59) "-" ///
		_n as res "CUTOFF: `j', p(0)" ///
		_n as text _dup(59) "-"
		cap noisily: rdrandinf `x' runvar_abp03_YM if inrange(age2,18,55), c(`j') kernel(triangular) cov($cov) wmasspoints level(0.1)	
		if `j'==1 {
			loc `x'`j'anl: di %12.0fc `r(N_left)'
			loc `x'`j'anr: di %12.0fc `r(N_right)'
			loc `x'`j'atau: di %12.4fc `r(obs_stat)'
			loc `x'`j'atstat=invnormal((`r(asy_pval)'/2))
			loc `x'`j'ase: di %12.4fc (``x'`j'atau' / ``x'`j'atstat') 
			loc `x'`j'astar=cond(abs(``x'`j'atstat') >2.58,"***",cond(abs(``x'`j'atstat') >1.96,"**",cond(abs(``x'`j'atstat') >1.645,"*","")))			
		}

		di as text _dup(59) "-" ///
		_n as res "CUTOFF: -`j', p(0)" ///
		_n as text _dup(59) "-"
		cap noisily: rdrandinf `x' runvar_abp03_YM if inrange(age2,18,55), c(-`j') kernel(triangular) cov($cov) wmasspoints level(0.1)	
		if `j'==1 {
			loc `x'`j'bnl: di %12.0fc `r(N_left)'
			loc `x'`j'bnr: di %12.0fc `r(N_right)'
			loc `x'`j'btau: di %12.4fc `r(obs_stat)'
			loc `x'`j'btstat=invnormal((`r(asy_pval)'/2))
			loc `x'`j'bse: di %12.4fc (``x'`j'btau' / ``x'`j'btstat') 
			loc `x'`j'bstar=cond(abs(``x'`j'btstat') >2.58,"***",cond(abs(``x'`j'btstat') >1.96,"**",cond(abs(``x'`j'btstat') >1.645,"*","")))
		}
	}
}

texdoc init "$tb\res-placebo.tex", replace force
tex \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} 
tex \begin{tabular}{l*{4}{c}} 
tex \hline\hline 
tex                    &\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}&\multicolumn{1}{c}{(3)}&\multicolumn{1}{c}{(4)}\\
tex 				   &\multicolumn{2}{c}{Cutoff: 1}&\multicolumn{2}{c}{Cutoff: -1}\\
tex                    &\multicolumn{1}{c}{Ever work before 15}&\multicolumn{1}{c}{Ever work before 18}&\multicolumn{1}{c}{Ever work before 15}&\multicolumn{1}{c}{Ever work before 18}\\
tex \hline
tex &\multicolumn{4}{c}{Panel A: Linear} \\
tex Treatment Effect	&    `childlabor151atau11'\sym{`childlabor151astar11'}    	&    `childlabor181atau11'\sym{`childlabor181astar11'} 	&    `childlabor151btau11'\sym{`childlabor151bstar11'}    	&    `childlabor181btau11'\sym{`childlabor181bstar11'} \\
tex                    	&    (`childlabor151ase11')         				&    (`childlabor181ase11')          			&    (`childlabor151bse11')         						&    (`childlabor181bse11')           \\
tex Eff. Control        &      `childlabor151aefnl11'       				&      `childlabor181aefnl11'       			&      `childlabor151befnl11'       						&      `childlabor181befnl11'       \\
tex Eff. Treatment      &      `childlabor151aefnr11'       				&      `childlabor181aefnr11'       			&      `childlabor151befnr11'       						&      `childlabor181befnr11'       \\
tex Power               &      `childlabor151apw11'         				&      `childlabor181apw11'         			&      `childlabor151bpw11'         						&      `childlabor181bpw11'         \\
tex \hline
tex &\multicolumn{4}{c}{Panel B: Quadratic} \\
tex Treatment Effect	&    `childlabor151atau21'\sym{`childlabor151astar21'}    	&    `childlabor181atau21'\sym{`childlabor181astar21'} 	&    `childlabor151btau21'\sym{`childlabor151bstar21'}    	&    `childlabor181btau21'\sym{`childlabor181bstar21'} \\
tex                    	&    (`childlabor151ase21')         				&    (`childlabor181ase21')          			&    (`childlabor151bse21')         						&    (`childlabor181bse21')           \\
tex Eff. Control        &      `childlabor151aefnl21'       				&      `childlabor181aefnl21'      				&      `childlabor151befnl21'       						&      `childlabor181befnl21'        \\
tex Eff. Treatment      &      `childlabor151aefnr21'       				&      `childlabor181aefnr21'       			&      `childlabor151befnr21'       						&      `childlabor181befnr21'        \\
tex Power               &      `childlabor151apw21'         				&      `childlabor181apw21'         			&      `childlabor151bpw21'         						&      `childlabor181bpw21'          \\
tex \hline
tex &\multicolumn{4}{c}{Panel C: Mean Difference} \\
tex Treatment Effect	&    `childlabor151atau'\sym{`childlabor151astar'}  	&    `childlabor181atau'\sym{`childlabor181astar'} 	&    `childlabor151btau'\sym{`childlabor151bstar'}  &    `childlabor181btau'\sym{`childlabor181bstar'}  \\
tex                    	&    (`childlabor151ase')         				&    (`childlabor181ase')          			&    (`childlabor151bse')         				&    (`childlabor181bse')           \\
tex Eff. Control        &      `childlabor151anl'       				&      `childlabor181anl'       				&      `childlabor151bnl'       				&      `childlabor181bnl'        \\
tex Eff. Treatment      &      `childlabor151anr'       				&      `childlabor181anr'       				&      `childlabor151bnr'       				&      `childlabor181bnr'        \\
tex Power               &      `childlabor151apw11'        				&      `childlabor181apw11'         			&      `childlabor151bpw11'        				&      `childlabor181bpw11'         \\
tex \hline
tex Control             &      `childlabor151anl11'        				&      `childlabor181anl11'         			&      `childlabor151bnl11'        				&      `childlabor181bnl11'          \\
tex Treatment           &      `childlabor151anr11'        				&      `childlabor181anr11'         			&      `childlabor151bnr11'        				&      `childlabor181bnr11'         \\
tex \hline\hline
tex \multicolumn{5}{l}{\footnotesize Standard errors in parentheses}\\
tex \multicolumn{5}{l}{\footnotesize \sym{*} \(p<0.10\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\
tex \end{tabular}
texdoc close	
log close
