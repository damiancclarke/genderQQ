/* analysisDHS.do                   DCC/KTS                yyyy-mm-dd:2017-07-01
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8


*/

 
clear all
version 11.2
set more off
cap log close

*******************************************************************************
*** (0) Globals and Required ados from SSC
*******************************************************************************
if c(username)=="damian" {
    global DAT "~/investigacion/2017/genderQQ/data"
    global OUT "~/investigacion/2017/genderQQ/results/DHS"
    global LOG "~/investigacion/2017/genderQQ/log"
}
else {
    global DAT "C:\Users\Ignacio\Desktop\Gusy\PROF. CLARKE\data"
    global OUT "C:\Users\Ignacio\Desktop\Gusy\PROF. CLARKE\results\DHS"
    global LOG "C:\Users\Ignacio\Desktop\Gusy\PROF. CLARKE\log"
}

foreach ado in ivreg2 estout plausexog {
    cap which `ado'
    if _rc!=0 ssc install `ado'
}

log using "$LOG/analysisDHS_k8.txt", replace text


*******************************************************************************
*** (1) Setup (+ discretionary choices)
*******************************************************************************
use "$DAT/DHS_QQ_full_sonsdaughters.dta"
*sample 5

replace bmi    = . if bmi>50
replace height = . if height>240
replace height = . if height<80
replace educ   = . if age<6
replace educ   = . if educ>25
replace educf  = . if educf>25
replace educp  = . if educp>25
replace wealth = 6 if wealth==.

drop if twinfamily>2
gen  ALL = 1    

tab country   , gen(_country)
tab year_birth, gen(_yb)
tab age       , gen(_age)
tab contracep , gen(_contracep)
tab educf     , gen(_educf)
tab educp     , gen(_educp)
tab wealth    , gen(_wealth)


local base malec _country* _yb* _age* _contracep*
local age  motherage motheragesq motheragecub agefirstbirth
local S    _educf* _wealth* _educp*
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
local brother sons
local sister  daughters 

egen keeper = rowmiss(`y' `base' `age' `H' educf fert)
keep if keeper == 0


*Estadística Descriptiva
#delimit ;
estpost sum school_zscore fert daughters sons malec;
estout using "$OUT/Summary.tex", replace label style(tex)
cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))")
collabels(, none) mlabels(, none);
#delimit cr
/*
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
*/
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
        
        local p partial(`base') savefirst 
        local z twin_`n'_fam

        foreach e of numlist 1(1)3 {
            eststo: ivreg2 `y' `c`e'' (`x'=`z') `wt', `se' `p' savefp(fst`ecnt')
            local ests2 `ests2' est`ecnt'
            local ests1 `ests1' fst`ecnt'fert
            mat first=e(first)
            estadd scalar KPF=first[8,1]: fst`ecnt'fert
            estadd scalar KPp=first[7,1]: fst`ecnt'fert
            estadd scalar KPFf=first[8,1]
            local ++ecnt
        }
        restore
    }

    #delimit ;
    estout `ests2' using "`OUT'.xls", replace
    cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par))
    stats(r2 N, fmt(%9.2f %9.0g)) starlevel("*" 0.10 "**" 0.05 "***" 0.01);

    estout `ests1' using "`OUT'_first.xls", replace
    cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par))
    stats (N KPF KPp, fmt(%9.0g %9.2f %9.3f))
    starlevel ("*" 0.10 "**" 0.05 "***" 0.01);
    #delimit cr
			   
    macro shift
}
*** Add test of gamma_2^G=gamma_2^B
*** (note uses Frisch Waugh Lowell to concentrate out variables)
local jj=12
foreach n in two three four {
    preserve
    keep if `n'_plus==1
    local z twin_`n'_fam

    foreach var in x y z {
        qui reg ``var'' `c3' `wt' if gender=="F"
        qui predict `var'res if e(sample)==1 if gender=="F", resid

        qui reg ``var'' `c3' `wt' if gender=="M"
        qui predict `var'res2 if e(sample)==1 if gender=="M", resid
        replace `var'res = `var'res2 if gender=="M"
        drop `var'res2
    }
    gen xmaleres = xres*malec
    gen zmaleres = zres*malec

    ivreg2 yres (xres xmaleres = zres zmaleres) `wt', `se' noconstant
    test xres=xmaleres
    local pval = r(p)
    estadd scalar gammatest=`pval': est`jj'
    local jj=`jj'+3
    
    restore
}
***PANEL A, TABLE 3
lab var fert "Total Fertility"
#delimit ;
esttab est3 est6 est9 using "$OUT/IV/nogender.tex", replace keep(fert)
nogaps b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N KPFf, fmt(%12.0gc %9.3f)
      label("\\ Observations" "Kleibergen-Paap rk statistic"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
#delimit cr


***TOP OF PANEL A, TABLE 4 (could remove observations to only have in joint)
#delimit ;
esttab est12 est15 est18 est21 est24 est27 using "$OUT/IV/boygirl.tex",
replace keep(fert) nogaps b(%-9.3f) se(%-9.3f)
starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N KPFf gammatest, fmt(%12.0gc %9.3f %5.3f)
      label("Observations" "Kleibergen-Paap rk statistic"
            "Test of $\gamma\sb{2}^G=\gamma\sb{2}^B$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
#delimit cr
estimates clear

/*
********************************************************************************
**** (4) OLS n+ Regressions effect of sisters and brothers
********************************************************************************    
tokenize `fnames'

foreach condition of local conditions {
    local ests
    local ecnt 1
    local OUT "$OUT/OLS/`1'"

    foreach n in two three four {
        preserve
        keep if `condition'&`n'_plus==1
        egen keeper = rowmiss(`y' `base' `age' `H' educf sons daughters)
        keep if keeper == 0
        
        foreach e of numlist 1(1)3 {
            eststo: reg school_zscore sons daughters `c`e'' `wt', `se'
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
*/

********************************************************************************
**** (5) IV effect of sisters and brothers (using girl twin and boy twins at
****     order n), subsequent inclusion of twin predictors
********************************************************************************
tokenize `fnames'
local ecnt 1
foreach condition of local conditions {
    local ests1sons
    local ests1daughter
    local ests2
    local OUT "$OUT/IV_Mixto/`1'"
    
    foreach n in two three four {
        preserve
        keep if `condition'&`n'_plus==1
        
        local p partial(`base') savefirst 
        local z1 twinBoy_`n'_fam
        local z2 twinGirl_`n'_fam

        foreach e of numlist 1(1)3 {
            #delimit ;
            eststo: ivreg2 `y' `c`e'' (daughters sons=`z1' `z2') `wt',
            `se' `p' savefp(fst`ecnt');
            #delimit cr
            test daughters = sons
            local pval = r(p)
            estadd scalar betatest=`pval'
            
            local ests2 `ests2' est`ecnt'
            local ests1sons `ests1sons' fst`ecnt'sons
            local ests1daughters `ests1daughters' fst`ecnt'daughters
            mat first=e(first)
            estadd scalar KPF_s=first[8,1]: fst`ecnt'sons
            estadd scalar KPFfs=first[8,1]: est`ecnt'
            estadd scalar KPF_d=first[8,2]: fst`ecnt'daughters
            estadd scalar KPFfd=first[8,2]: est`ecnt'
            estadd scalar KPp_s=first[7,1]: fst`ecnt'sons
            estadd scalar KPp_d=first[7,2]: fst`ecnt'daughters
            local ++ecnt
        }
        restore
    }

    /*
    #delimit ;
    estout `ests2' using "`OUT'.xls", replace
    cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par))
    stats(r2 N, fmt(%9.2f %9.0g)) starlevel("*" 0.10 "**" 0.05 "***" 0.01);
    
    estout `ests1sons' using "`OUT'_firstsons.xls", replace
    cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par))
    stats (N KPF_s KPp_s, fmt(%9.0g %9.2f %9.3f))
    starlevel ("*" 0.10 "**" 0.05 "***" 0.01);

    estout `ests1daughters' using "`OUT'_firstdaughters.xls", replace
    cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par))
    stats (N KPF_d KPp_d, fmt(%9.0g %9.2f %9.3f))
    starlevel ("*" 0.10 "**" 0.05 "***" 0.01);	
    #delimit cr
    */
    macro shift
}

*** Add test of gamma_2^G=gamma_2^B
*** (note uses Frisch Waugh Lowell to concentrate out variables)
local jj=12
foreach n in two three four {
    preserve
    keep if `n'_plus==1
    local z1 twinBoy_`n'_fam
    local z2 twinGirl_`n'_fam
    local x1 sons
    local x2 daughters 

    
    foreach var in x1 x2 y z1 z2 {
        qui reg ``var'' `c3' `wt' if gender=="F"
        qui predict `var'res if gender=="F", resid

        qui reg ``var'' `c3' `wt' if gender=="M"
        qui predict `var'res2 if gender=="M", resid
        replace `var'res = `var'res2 if gender=="M"
        drop `var'res2
    }
    gen x1maleres = x1res*malec
    gen x2maleres = x2res*malec
    gen z1maleres = z1res*malec
    gen z2maleres = z2res*malec
    local xs x1res x2res x1maleres x2maleres
    local zs z1res z2res z1maleres z2maleres
    
    ivreg2 yres (`xs' = `zs') `wt', `se' 
    test x1res=x1maleres
    local pval = r(p)
    estadd scalar gamma2test=`pval': est`jj'
    test x2res=x2maleres
    local pval = r(p)
    estadd scalar gamma3test=`pval': est`jj'
    local jj=`jj'+3
    
    restore
}


***PANEL A, TABLE 3
lab var sons "Total Sons"
lab var daughters "Total Daughters"
#delimit ;
esttab est3 est6 est9 using "$OUT/IV/sondaughter.tex", replace
keep(daughters sons)
nogaps b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N KPFfs KPFfd betatest, fmt(%12.0gc %9.3f %9.3f %5.2f)
      label("\\ Observations" "First Stage F-Statistic (Sons)"
            "First Stage F-Statistic (Daughters)"
            "Test of $\beta\sb{Daughter}=\beta\sb{Son}$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
#delimit cr


***TOP OF PANEL A, TABLE 4 (could remove observations to only have in joint)
#delimit ;
esttab est12 est15 est18 est21 est24 est27 using "$OUT/IV/boygirl_sondaughter.tex",
replace keep(daughters sons) nogaps b(%-9.3f) se(%-9.3f)
starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N KPFfs KPFfd betatest gamma2test gamma3test,
      fmt(%12.0gc %9.3f %9.3f %5.2f %5.2f %5.2f)
      label("\\ Observations" "First Stage F-Statistic (Sons)"
            "First Stage F-Statistic (Daughters)"
            "Test of $\delta\sb{2}=\delta\sb{3}$ (p-value)"
            "Test of $\gamma\sb{2}^G=\delta\sb{2}^B$ (p-value)"
            "Test of $\gamma\sb{3}^G=\delta\sb{3}^B$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
#delimit cr
estimates clear

********************************************************************************
**** (6) Gender bias
********************************************************************************    
replace idealGirl = . if idealGirl>20
replace idealBoys = . if idealBoys>20


bys country: egen aveGirls = mean(daughters)
bys country: egen aveBoys  = mean(sons)
bys country: egen aveGirlsIdeal = mean(idealG)
bys country: egen aveBoysIdeal  = mean(idealB)

gen BGratio = aveBoys/aveGirls
gen BGratioIdeal = aveBoysIdeal/aveGirlsIdeal
xtile BGquintile = BGratio, nq(5)
xtile BGquintileIdeal = BGratioIdeal, nq(5)

gen estimate   = .
gen UB         = .
gen LB         = .
gen estnum     = .
gen estimateIV = .
gen UBIV       = .
gen LBIV       = .

preserve


local j = 1
local k = -1
foreach n in two three four {
    local k=`k'+2
    local z twin_`n'_fam    
    foreach quint of numlist 1(1)5 {
        local cond if BGquintileIdeal==`quint'&`n'_plus==1

        foreach var in x y z {
            qui reg ``var'' `c3' `wt' `cond'&gender=="F"
            qui predict `var'res `cond'&gender=="F", resid
            
            qui reg ``var'' `c3' `wt' `cond'&gender=="M"
            qui predict `var'res2 `cond'&gender=="F", resid
            replace `var'res = `var'res2 `cond'&gender=="F"
            drop `var'res2
        }        
        gen fertBoy = xres*malec
        gen zBoy    = zres*malec
        
        reg yres xres fertBoy `wt' `cond', `se'
        replace estimate = _b[fertBoy] in `j'
        replace LB = _b[fertBoy]+invnorm(0.025)*_se[fertBoy] in `j'
        replace UB = _b[fertBoy]+invnorm(0.975)*_se[fertBoy] in `j'
        replace estnum = `k' in `j'

        ivreg2 yres (xres fertBoy=zres zBoy) `wt' `cond', `se'
        replace estimateIV = _b[fertBoy] in `j'
        replace LBIV = _b[fertBoy]+invnorm(0.025)*_se[fertBoy] in `j'
        replace UBIV = _b[fertBoy]+invnorm(0.975)*_se[fertBoy] in `j'
        drop xres yres zres ferBoy zBoy
        
        local ++j
        local ++k
    }
}

format estimate %5.2f
#delimit ;
twoway scatter estimate estnum in 1/`k', msymbol(S) mcolor(black) ||
       rcap UB LB estnum, lcolor(black) scheme(s1mono)
ytitle("QQ Trade-off (Girl Penalty)") xtitle(" ") ylabel(0(0.02)0.12)
xlabel(1  "Low" 2  "Q2"  3 "Q3"  4 "Q4"  5 "High"  6 " "  7 " "
       8  "Low" 9  "Q2" 10 "Q3" 11 "Q4" 12 "High" 13 " " 14 " "
       15 "Low" 16 "Q2" 17 "Q3" 18 "Q4" 19 "High")
yline(0, lcolor(red) lpattern(dash))
text(0.1 3 "Two-Plus") text(0.1 10 "Three-Plus") text(0.1 17 "Four-Plus")
legend(label(1 "Point Estimate (OLS)") label(2 "95% Confidence Interval"));
graph export "$OUT/OLS/genderBiasQQ.eps", replace;


twoway scatter estimateIV estnum in 1/`k', msymbol(S) mcolor(black) ||
       rcap UBIV LBIV estnum, lcolor(black) scheme(s1mono)
ytitle("QQ Trade-off (Girl Penalty)") xtitle(" ")
xlabel(1  "Low" 2  "Q2"  3 "Q3"  4 "Q4"  5 "High"  6 " "  7 " "
       8  "Low" 9  "Q2" 10 "Q3" 11 "Q4" 12 "High" 13 " " 14 " "
       15 "Low" 16 "Q2" 17 "Q3" 18 "Q4" 19 "High")
yline(0, lcolor(red) lpattern(dash))
text(0.1 3 "Two-Plus") text(0.1 10 "Three-Plus") text(0.1 17 "Four-Plus")
legend(label(1 "Point Estimate (IV)") label(2 "95% Confidence Interval"));
graph export "$OUT/IV/genderBiasQQ.eps", replace;
#delimit cr
restore

collapse daughters sons idealGirls idealBoys, by(country WBcountry)
gen BoyGirlRatio = sons/daughters
gen desiredBoyGirl = idealBoys/idealGirls

rename WBcountry NAME
replace NAME = "Central African Rep." if NAME=="Central African Republic"
replace NAME = "Congo" if NAME=="Congo Brazzaville"
replace NAME = "Dem. Rep. Congo" if NAME=="Congo, Dem. Rep."
replace NAME = "Dominican Rep." if NAME=="Dominican Republic"
replace NAME = "Egypt" if NAME=="Egypt, Arab Rep."
replace NAME = "Kyrgyzstan" if NAME=="Kyrgyz Republic"
replace NAME = "Yemen" if NAME=="Yemen, Rep."


merge 1:1 NAME using "$DAT/shape/world"
format BoyGirlRatio   %5.3f
format desiredBoyGirl %5.3f

#delimit ;
spmap BoyGirlRatio if NAME!="Antarctica" using "$DAT/shape/world_coords",
id(_ID) osize(vvthin vvthin vvthin vvthin vvthin) fcolor(Rainbow) clnumber(5)
ndfcolor(gs15) legend(symy(*1.2) symx(*1.2) size(*1.8) position(9))
legend(title("Boy/Girl Ratio", size(*1.2) bexpand justification(left)))
legend(label(1 "No DHS Data")) ndsize(vvthin);
graph export "$OUT/BoyGirlRatio.eps", replace;

spmap desiredBoyGirl if NAME!="Antarctica" using "$DAT/shape/world_coords",
id(_ID) osize(vvthin vvthin vvthin vvthin vvthin) fcolor(Rainbow) clnumber(5)
ndfcolor(gs15) legend(symy(*1.2) symx(*1.2) size(*1.8) position(8))
legend(title("Desired Boy/Girl Ratio", size(*1.2) bexpand justification(left)))
legend(label(1 "No DHS Data")) ndsize(vvthin);
graph export "$OUT/BoyGirlDesired.eps", replace;
#delimit cr

#delimit ;
twoway scatter desiredBoyGirl BoyGirlRatio [w=POP_EST], msymbol(circle_hollow)
||     scatter desiredBoyGirl BoyGirlRatio, mlabel(NAME) scheme(s1mono) m(i);
graph export "$OUT/genderBiasMeasures.eps", replace;
#delimit cr


********************************************************************************
**** (X) Clean up
********************************************************************************
log close
