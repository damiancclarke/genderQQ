/* analysisDHS.do                   DCC/KTS                yyyy-mm-dd:2017-07-01
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

     Tests to examine the Quantity-Quality trade-off in DHS countries by gender. 
 Initial data generation can be found in makeDHS.do, as well as the analysis fi-
 les of Bhalotra and Clarke (2016).

*/

 
clear all
version 11.2
set more off
cap log close

*******************************************************************************
*** (0) Globals and Required ados from SSC
*******************************************************************************
global DAT "~/investigacion/2017/genderQQ/data"
global OUT "~/investigacion/2017/genderQQ/results/DHS"
global LOG "~/investigacion/2017/genderQQ/log"

foreach ado in ivreg2 estout plausexog {
    cap which `ado'
    if _rc!=0 ssc install `ado'
}

log using "$LOG/analysisDHS.txt", replace text


*******************************************************************************
*** (1) Setup (+ discretionary choices)
*******************************************************************************
use "$DAT/DHS_QQ_full.dta"

replace bmi    = . if bmi>50
replace height = . if height>240
replace height = . if height<80
replace educ   = . if age<6
replace educ   = . if educ>25
replace educf  = . if educf>25
replace wealth = 6 if wealth==.

drop if twinfamily>2
gen  ALL = 1    

tab country   , gen(_country)
tab year_birth, gen(_yb)
tab age       , gen(_age)
tab contracep , gen(_contracep)
tab educf     , gen(_educf)
tab wealth    , gen(_wealth)


local base malec _country* _yb* _age* _contracep*
local age  motherage motheragesq motheragecub agefirstbirth
local S    _educf* _wealth*
local H    height bmi preClustD preClustN preClustZ

local c1 `base' `age'
local c2 `base' `age' `H'
local c3 `base' `age' `H' `S'

local conditions ALL==1 gender=="F" gender=="M"
local fnames     All Girls Boys
local se cluster(id)
local wt [pw=sweight]
local y  school_zscore
local x  fert
exit
********************************************************************************
**** (2) OLS n+ Regressions 
********************************************************************************    
tokenize `fnames'
foreach condition of local conditions {
    local ests
    local ecnt 1
    local OUT "$OUT/OLS/`1'"

    foreach n in two three four {
        preserve
        keep if `condition'&`n'_plus==1
        egen keeper = rowmiss(`y' `base' `age' `H' educf fert)
        keep if keeper == 0
        
        foreach e of numlist 1(1)3 {
            eststo: reg school_zscore fert `c`e'' `wt', `se'
            local ests `ests' est`ecnt'
            local ++ecnt
        }
        restore
    }

    #delimit ;
    estout `ests' using "`OUT'.txt", replace
    cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par))
    stats (N r2, fmt(%9.0g %9.2f))
    starlevel ("*" 0.10 "**" 0.05 "***" 0.01);
    #delimit cr
    estimates clear
    macro shift
}


********************************************************************************
**** (3) IV (using twin at order n), subsequent inclusion of twin predictors
********************************************************************************
tokenize `fnames'
foreach condition of local conditions {

    local ests1
    local ests2
    local ecnt 1
    local OUT "$OUT/IV/`1'"
    
    foreach n in two three four {
        preserve
        keep if `condition'&`n'_plus==1
        egen keeper = rowmiss(`y' `base' `age' `H' educf fert)
        keep if keeper == 0
        
        local p partial(`base') savefirst 
        local z twin_`n'_fam

        foreach e of numlist 1(1)3 {
            eststo: ivreg2 `y' `c`e'' (`x'=`z') `wt', `se' `p' savefp(fst`ecnt')
            local ests2 `ests2' est`ecnt'
            local ests1 `ests1' fst`ecnt'fert
            mat first=e(first)
            estadd scalar KPF=first[8,1]: fst`ecnt'fert
            estadd scalar KPp=first[7,1]: fst`ecnt'fert
            local ++ecnt
        }
        restore
    }

    #delimit ;
    estout `ests2' using "`OUT'.txt", replace
    cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par))
    stats(r2 N, fmt(%9.2f %9.0g)) starlevel("*" 0.10 "**" 0.05 "***" 0.01);

    estout `ests1' using "`OUT'_first.txt", replace
    cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par))
    stats (N KPF KPp, fmt(%9.0g %9.2f %9.3f))
    starlevel ("*" 0.10 "**" 0.05 "***" 0.01);
    #delimit cr
    estimates clear
    
    macro shift
}


********************************************************************************
**** (X) Clean up
********************************************************************************
log close
