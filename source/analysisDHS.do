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

log using "$LOG/analysisDHS.txt", replace text


*******************************************************************************
*** (1) Setup (+ discretionary choices)
*******************************************************************************
use "$DAT/DHS_QQ_full_sonsdaughters-all.dta"
*sample 2

replace bmi    = . if bmi>50
replace height = . if height>240
replace height = . if height<80
replace educ   = . if age<6
replace educ   = . if educ>25
**NEW CATEGORICAL EDUC FOR MISSING
replace educf  = 26 if educf>25
replace educp  = 26 if educp>25
replace wealth = 6 if wealth==.
replace educ   = 14 if educ>14&educ!=.&age<=18


gen _bmi1 = bmi<16
gen _bmi2 = bmi>=16  &bmi<18.5
gen _bmi3 = bmi>=18.5&bmi<25
gen _bmi4 = bmi>=25  &bmi<30
gen _bmi5 = bmi>=30  &bmi<35
gen _bmi6 = bmi>=35  &bmi<=50
gen _bmi7 = bmi==.
drop _bmi1

gen _height1 = height<=140
gen _height2 = height> 140 & height<=150
gen _height3 = height> 150 & height<=160
gen _height4 = height> 160 & height<=170
gen _height5 = height> 170 & height<=180
gen _height6 = height> 180 & height!=.
gen _height7 = height==.
drop _height1

        

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
local H    _height* _bmi* preClustD preClustN preClustZ

local c1 `base' `age'
local c2 `base' `age' `H' `S'

local conditions ALL==1 gender=="F" gender=="M"
local fnames     All Girls Boys
local se cluster(id)
local wt [pw=sweight]
local y  school_zscore
local x  fert

*Estadística Descriptiva
lab var school_zscore "Education Z-Score"
lab var educ          "Years of Education"
lab var age           "Child's Age"
lab var fert          "Total Fertility"
lab var daughters     "Total Girl Children"
lab var sons          "Total Boy Children"
lab var malec         "Child is a Boy"
lab var twind         "Twin Birth"
lab var two_plus      "Two-Plus Sample"
lab var three_plus    "Three-Plus Sample"
lab var four_plus     "Four-Plus Sample"

egen keeper = rowmiss(`y' `base' `age' `S' `H' educf fert)
drop school_zscore
bys _cou age: egen sd_educ=sd(educ) if keeper==0
bys _cou age: egen mean_educ=mean(educ) if keeper==0
gen school_zscore=(educ-mean_educ)/sd_educ if keeper==0
replace school_zscore=. if age<6
lab var school_zscore "Education Z-Score"
count if abs(school_zscore)>5 & keeper==0
***
*BELOW IS FOR APPENDIX RESULTS
*replace keeper=1 if abs(school_zscore)>5 & keeper==0
***
/*
#delimit ;
estpost sum school_zscore educ age fert twind daughters sons malec
            two_plus three_plus four_plus if keeper==0, listwise;
estout using "$OUT/Summary.tex", replace label style(tex)
cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))")
collabels(, none) mlabels(, none);

hist school_zscore if keeper==0, scheme(burd) ytitle("Density") xtitle("Z-Score");
graph export "$OUT/Zscore-distribution.eps", replace;
#delimit cr


levelsof _cou,  local(cnames)
levelsof _year, local(ynames)
foreach num of numlist 1(1)5 {
    gen s`num'year=.
}
local cnum=0
local snum=0
foreach country of local cnames {
    local j = 1
    foreach year of local ynames {
        count if _year=="`year'"&_cou==`country'&keeper==1
        if r(N)>0 {
            dis `j'
            if `j'==1 local ++cnum
            replace s`j'=`year' if _cou==`country'
            local ++j            
            local ++snum
        }
    }
}
preserve
collapse s1year s2year s3year s4year s5year, by(country)
drop if s1year==.
gen slash="\\"
egen s6year = concat(s5year slash)
replace s6year=subinstr(s6year,".", "", .)
drop s5year slash
outsheet using "$OUT/surveyYrs.tex", delim("&") noquote nonames replace
restore
dis "There are `snum' surveys in `cnum' countries"
*/

********************************************************************************
**** (1b) Fertility effects
********************************************************************************    
lab var twinGirl_two_fam   "Female Twin at Second Birth"
lab var twinGirl_three_fam "Female Twin at Third Birth"
lab var twinGirl_four_fam  "Female Twin at Fourth Birth"    
lab var twinBoy_two_fam    "Male Twin at Second Birth"
lab var twinBoy_three_fam  "Male Twin at Third Birth"
lab var twinBoy_four_fam   "Male Twin at Fourth Birth"    

foreach num of numlist 3(1)11 {
    gen fert`num'=fert>=`num'&fert!=.
}
local b = 2
local fertEsts
local twinEsts
foreach n in two three four {
    local nlst 3(1)11
    local xlabs 3 "3+" 4 "4+" 5 "5+" 6 "6+" 7 "7+" 8 "8+" 9 "9+" 10 "10+" 11 "11+"
    if `"`n'"'=="three" {
        local nlst 4(1)11
        local xlabs 4 "4+" 5 "5+" 6 "6+" 7 "7+" 8 "8+" 9 "9+" 10 "10+" 11 "11+"
    }
    if `"`n'"'=="four" {
        local nlst 5(1)11
        local xlabs 5 "5+" 6 "6+" 7 "7+" 8 "8+" 9 "9+" 10 "10+" 11 "11+"
    }
    
    preserve
    keep if keeper == 0&`n'_plus==1

    gen point = .
    gen UB    = .
    gen LB    = .
    gen k     = .
    local j = 1
    foreach num of numlist `nlst' {
        reg fert`num' twin_`n'_fam `c2' `wt', `se'
        replace point = _b[twin_`n'_fam] in `j'
        replace LB = _b[twin_`n'_fam]+invnormal(0.025)*_se[twin_`n'_fam] in `j'
        replace UB = _b[twin_`n'_fam]+invnormal(0.975)*_se[twin_`n'_fam] in `j'
        replace k = `num' in `j'
        local ++j
    }
    format point %5.2f
    #delimit ;
    twoway scatter point k in 1/`j', scheme(s1mono) m(S)
    ||     line    point k in 1/`j', lcolor(black) lpattern(dash)
    ||     rcap    UB LB k in 1/`j', yline(0, lcolor(red))
    xlabel(`xlabs') xtitle("Number of Children") ylabel(,angle(0))
    legend(order(1 "Point Estimate" 3 "95% CI"));
    graph export "$OUT/IV/fertilityAll_`n'plus.eps", replace;
    #delimit cr

    gen Girl_`n' = malec==0&bord==`b'
    bys id: egen Girl_`n'_fam = max(Girl_`n')
    gen Boy_`n'  = malec==1&bord==`b'
    bys id: egen Boy_`n'_fam = max(Boy_`n')
    local ++b
    local fertEsts `fertEsts' Girl_`n'_fam Boy_`n'_fam
    local twinEsts `twinEsts' twinGirl_`n'_fam twinBoy_`n'_fam    
    
    local c2a _country* _yb* _age* _contracep* `S' `H' `age'
    eststo: reg fert Girl_`n'_fam Boy_`n'_fam `c2a' `wt', `se'
    test Girl_`n'_fam = Boy_`n'_fam
    local pval = r(p)
    estadd scalar BGtestF=`pval'

    eststo: reg fert twinGirl_`n'_fam twinBoy_`n'_fam `c2' `wt', `se'
    test twinGirl_`n'_fam = twinBoy_`n'_fam
    local pval = r(p)
    estadd scalar BGtestT=`pval'

    foreach gend in Girl Boy {
        gen point`gend' = .
        gen UB`gend'    = .
        gen LB`gend'    = .
        local j = 1
        foreach num of numlist `nlst' {
            reg fert`num' twin`gend'_`n'_fam `c2' `wt', `se'
            replace point`gend' = _b[twin`gend'_`n'_fam] in `j'
            replace LB`gend' = _b[twin`gend'_`n'_fam]-1.96*_se[twin`gend'_`n'_fam] in `j'
            replace UB`gend' = _b[twin`gend'_`n'_fam]+1.96*_se[twin`gend'_`n'_fam] in `j'
            replace k = `num' in `j'
            local ++j
        }
        format point`gend' %5.2f
        #delimit ;
        twoway scatter point`gend' k in 1/`j', scheme(s1mono) m(S)
        ||     line    point`gend' k in 1/`j', lcolor(black) lpattern(dash)
        ||     rcap    UB`gend' LB`gend' k in 1/`j', yline(0, lcolor(red))
        xlabel(`xlabs') xtitle("Number of Children") ylabel(0(0.05)0.25, angle(0))
        legend(order(1 "Point Estimate" 3 "95% CI"));
        graph export "$OUT/IV/fertility`gend'_`n'plus.eps", replace;
        #delimit cr        
    }
    gen k2 = k+0.2
    #delimit ;
    twoway scatter pointGirl k2 in 1/`j', scheme(s1mono) m(S) mcolor(blue)
    ||     line    pointGirl k2 in 1/`j', lcolor(blue) lpattern(dash)
    ||     rcap    UBGirl LBGirl k2 in 1/`j', lcolor(blue)
    ||     scatter pointBoy k in 1/`j', m(Oh) mcolor(red)
    ||     line    pointBoy k in 1/`j', lcolor(red) lpattern(dash)
    ||     rcap    UBBoy LBBoy k in 1/`j', lcolor(red)
    xlabel(`xlabs') xtitle("Number of Children") ylabel(,angle(0))
    yline(0, lcolor(gs12))
    legend(order(1 "Point Estimate (Girls)" 3 "95% CI (Girls)"
                 4 "Point Estimate (Boys)"  6 "95% CI (Boys)"));
    graph export "$OUT/IV/fertility_`n'plus.eps", replace;
    #delimit cr
    restore
}
drop fert3-fert10
#delimit ;
esttab est1 est3 est5 using "$OUT/OLS/IndirectEffect.tex", replace
keep(`fertEsts') nogaps b(%-9.3f) se(%-9.3f) nonotes mlabels(, none)
starlevel("*" 0.10 "**" 0.05 "***" 0.01) nonumbers style(tex) fragment noline
stats(N BGtestF, fmt(%12.0gc %5.3f)
      label("\\ Observations" "Test of Equality of Fertility Impact")) label;

esttab est2 est4 est6 using "$OUT/IV/IndirectEffect.tex", replace
keep(`twinEsts') nogaps b(%-9.3f) se(%-9.3f)  mlabels(, none)
starlevel("*" 0.10 "**" 0.05 "***" 0.01) nonumbers style(tex) fragment noline
stats(N BGtestT, fmt(%12.0gc %5.3f)
      label("\\ Observations" "Test of Equality of Twin Impact")) label nonotes;
#delimit cr
estimates clear


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
        keep if keeper == 0

        keep if `condition'&`n'_plus==1
        
        foreach e of numlist 1 2 {
            eststo: reg school_zscore fert `c`e'' `wt', `se'
            local ests `ests' est`ecnt'
            local ++ecnt
        }
        restore
    }
    macro shift
}

*** Add test of gamma_2^G=gamma_2^B
*** (note uses Frisch Waugh Lovell to concentrate out variables)
local jj=7
foreach n in two three four {
    preserve
    keep if keeper == 0
    keep if `n'_plus==1

    foreach cntrl in 1 2 {
        foreach var in x y {
            qui reg ``var'' `c`cntrl'' `wt' if gender=="F"
            qui predict `var'res            if gender=="F", resid
            
            qui reg ``var'' `c`cntrl'' `wt' if gender=="M"
            qui predict `var'res2           if gender=="M", resid
            replace `var'res = `var'res2    if gender=="M"
            drop `var'res2
        }
        gen xmaleres = xres*malec

        reg yres xres xmaleres `wt', `se' noconstant
        test xmaleres
        local pval = r(p)
        estadd scalar gammatest=`pval': est`jj'
        local jj=`jj'+1
        drop xres xmaleres yres
    }    
    restore
}

***TOP OF PANEL A, TABLE 2
#delimit ;
esttab est2 est4 est6 est8 est10 est12 est14 est16 est18 using
"$OUT/OLS/fertility.tex", replace keep(fert) nogaps b(%-9.3f) se(%-9.3f)
starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N gammatest, fmt(%12.0gc %5.3f)
      label("Observations" "Test of $\gamma\sb{2}^G=\gamma\sb{2}^B$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;

esttab est1 est3 est5 est7 est9 est11 est13 est15 est17 using
"$OUT/OLS/fertility_c1.tex", replace keep(fert) nogaps b(%-9.3f) se(%-9.3f)
starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N gammatest, fmt(%12.0gc %5.3f)
      label("Observations" "Test of $\gamma\sb{2}^G=\gamma\sb{2}^B$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;

#delimit cr
estimates clear


********************************************************************************
**** (3) OLS n+ Regressions effect of sisters and brothers
********************************************************************************    
tokenize `fnames'
foreach condition of local conditions {
    local ests
    local ecnt 1
    local OUT "$OUT/OLS/`1'"

    foreach n in two three four {
        preserve
        keep if `condition'&`n'_plus==1
        keep if keeper == 0
        
        foreach e of numlist 1 2 {
            eststo: reg school_zscore sons daughters `c`e'' `wt', `se'
            local ests `ests' est`ecnt'
            local ++ecnt
            test daughters = sons
            local pval = r(p)
            estadd scalar betatest=`pval'
        }
        restore
    }
    macro shift	
}

*** Add test of gamma_2^G=gamma_2^B
*** (note uses Frisch Waugh Lovell to concentrate out variables)
local jj=7
local kk=13
foreach n in two three four {
    preserve
    keep if keeper == 0
    keep if `n'_plus==1
    local x1 sons
    local x2 daughters
    
    foreach cntrl in 1 2 {
        foreach var in x1 x2 y {
            qui reg ``var'' `c`cntrl'' `wt' if gender=="F"
            qui predict `var'res            if gender=="F", resid
            
            qui reg ``var'' `c`cntrl'' `wt' if gender=="M"
            qui predict `var'res2           if gender=="M", resid
            replace `var'res = `var'res2    if gender=="M"
            drop `var'res2
        }
        gen x1maleres = x1res*malec
        gen x2maleres = x2res*malec
    
        reg yres x1res x2res x1maleres x2maleres `wt', `se' 
        test x2maleres = 0
        local pval = r(p)
        estadd scalar gamma2test=`pval': est`jj'
        test x1maleres = 0
        local pval = r(p)
        estadd scalar gamma3test=`pval': est`kk'
        local jj=`jj'+1
        local kk=`kk'+1
        if `cntrl'==2 {
            keep yres x1res x2res x1maleres x2maleres sweight id
            gen d_2_G = .
            gen d_2_B = .
            gen d_3_G = .
            gen d_3_B = .
            tempfile baseF
            save `baseF'

            set seed 2727
            local s = 50 
            forvalues i = 1/`s' {
                dis "Bootstrap sample `i' for bootstrap test"
                bsample, cluster(id) idcluster(nid)
                reg yres x1res x2res x1maleres x2maleres `wt', cluster(nid)
                use `baseF', clear
                replace d_2_G = _b[x2res] in `i'
                replace d_2_B = _b[x2res]+_b[x2maleres] in `i'
                replace d_3_G = _b[x1res] in `i'
                replace d_3_B = _b[x1res]+_b[x1maleres] in `i'
                sum d_*
                save `baseF', replace
            }
            gen girlBias = d_3_G < d_2_G & d_2_G < d_3_B & d_3_B < d_2_B in 1/`s'
            gen boyBias  = d_3_G > d_2_G & d_2_G > d_3_B & d_3_B > d_2_B in 1/`s'
            sum girlBias
            local gpval = 1-r(mean)
            dis "P-value on Girl Bias is `gpval'"
            sum boyBias
            local bpval = 1-r(mean)
            dis "P-value on Boy Bias is `bpval'"
        }
        if `cntrl'==1 drop x1res x2res yres x1maleres x2maleres
    }    
    restore
}

#delimit ;
esttab est2 est4 est6 est8 est10 est12 est14 est16 est18 using
"$OUT/OLS/sondaughter.tex", replace keep(daughters sons) nogaps
b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N betatest gamma2test gamma3test,
      fmt(%12.0gc %5.3f %5.3f %5.3f)
      label("\\ Observations"
            "Test of $\beta\sb{2}=\beta\sb{3}$ or $\delta\sb{2}=\delta\sb{3}$"
            "Test of $\delta\sb{2}^G=\delta\sb{2}^B$ (p-value)"
            "Test of $\delta\sb{3}^G=\delta\sb{3}^B$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;

esttab est1 est3 est5 est7 est9 est11 est13 est15 est17 using
"$OUT/OLS/sondaughter_c1.tex", replace keep(daughters sons) nogaps
b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N betatest gamma2test gamma3test,
      fmt(%12.0gc %5.3f %5.3f %5.3f)
      label("\\ Observations"
            "Test of $\beta\sb{2}=\beta\sb{3}$ or $\delta\sb{2}=\delta\sb{3}$"
            "Test of $\delta\sb{2}^G=\delta\sb{2}^B$ (p-value)"
            "Test of $\delta\sb{3}^G=\delta\sb{3}^B$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
#delimit cr
estimates clear


********************************************************************************
**** (4) IV (using twin at order n), subsequent inclusion of twin predictors
********************************************************************************
tokenize `fnames'
local ecnt 1
foreach condition of local conditions {    
    foreach n in two three four {
        preserve
        keep if keeper == 0
        keep if `condition'&`n'_plus==1
        
        local p partial(`base') savefirst
        gen Twin = twin_`n'_fam
        local z Twin
        
        foreach e of numlist 1 2 {
            eststo: ivreg2 `y' `c`e'' (`x'=`z') `wt', `se' `p' savefp(f`ecnt')
            mat first=e(first)
            estadd scalar KPF=first[8,1]: f`ecnt'fert
            estadd scalar KPp=first[7,1]: f`ecnt'fert
            local ++ecnt
        }
        restore
    }
    macro shift
}

*** Add test of gamma_2^G=gamma_2^B
*** (note uses Frisch Waugh Lovell to concentrate out variables)
local jj=7
foreach n in two three four {
    preserve
    keep if keeper == 0
    keep if `n'_plus==1
    local z twin_`n'_fam

    foreach cntrl in 1 2 {
        foreach var in x y z {
            qui reg ``var'' `c`cntrl'' `wt'     if gender=="F"
            qui predict `var'res                if gender=="F", resid
            
            qui reg ``var'' `c`cntrl'' `wt'     if gender=="M"
            qui predict `var'res2               if gender=="M", resid
            replace `var'res = `var'res2        if gender=="M"
            drop `var'res2
        }
        gen xmaleres = xres*malec
        gen zmaleres = zres*malec

        ivreg2 yres (xres xmaleres = zres zmaleres) `wt', `se' noconstant
        test xmaleres
        local pval = r(p)
        estadd scalar gammatest=`pval': est`jj'
        local jj=`jj'+1
        drop yres xres xmaleres zres zmaleres
    }
    restore
}
***PANEL A, TABLE 3
lab var fert "Total Fertility"
#delimit ;
esttab est2 est4 est6 using "$OUT/IV/nogender.tex", replace keep(fert)
nogaps b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N, fmt(%12.0gc) label("\\ Observations"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;

esttab est1 est3 est5 using "$OUT/IV/nogender_c1.tex", replace keep(fert)
nogaps b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N, fmt(%12.0gc) label("\\ Observations"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
#delimit cr

***TOP OF PANEL A, TABLE 4 (could remove observations to only have in joint)
#delimit ;
esttab est8 est10 est12 est14 est16 est18 using "$OUT/IV/boygirl.tex",
replace keep(fert) nogaps b(%-9.3f) se(%-9.3f)
starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N gammatest, fmt(%12.0gc %5.3f)
      label("Observations" "Test of $\gamma\sb{2}^G=\gamma\sb{2}^B$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;

esttab est7 est9 est11 est13 est15 est17 using "$OUT/IV/boygirl_c1.tex",
replace keep(fert) nogaps b(%-9.3f) se(%-9.3f)
starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N gammatest, fmt(%12.0gc %5.3f)
      label("Observations" "Test of $\gamma\sb{2}^G=\gamma\sb{2}^B$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
#delimit cr

gen Twin = .
lab var Twin "Twin Birth"
***PANEL A, TABLE 5
#delimit ;
esttab f2fert f4fert f6fert f8fert f10fert f12fert f14fert f16fert f18fert
using "$OUT/IV/firstStage.tex", replace keep(Twin) nogaps b(%-9.3f) se(%-9.3f)
starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N KPF KPp, fmt(%12.0gc %9.1f %5.3f)
      label("\\ Observations" "Kleibergen-Paap rk statistic"
            "p-value rk Test Statistic"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;

esttab f1fert f3fert f5fert f7fert f9fert f11fert f13fert f15fert f17fert
using "$OUT/IV/firstStage_c1.tex", replace keep(Twin) nogaps b(%-9.3f) se(%-9.3f)
starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N KPF KPp, fmt(%12.0gc %9.1f %5.3f)
      label("\\ Observations" "Kleibergen-Paap rk statistic"
            "p-value rk Test Statistic"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
#delimit cr
estimates clear
drop Twin

/*
exit


*-------------------------------------------------------------------------------
*--- (9) Pooled IV estimates using Angrist et al. (JoLE 2012)
*-------------------------------------------------------------------------------
preserve
keep if two_plus==1|three_plus==1|four_plus==1

local zs
local zGirl
local zBoy
foreach n in two three four {
    reg twin_`n'_fam `c2' `wt' if `n'_plus==1
    *Mogstad Wiswall Economics Letters
    predict twin_`n'_star                              if `n'_plus==1
    replace twin_`n'_star = twin_`n'_fam-twin_`n'_star if `n'_plus==1
    replace twin_`n'_star = 0                          if `n'_plus==0
    
    local zs `zs' twin_`n'_star

    foreach S in Girl Boy {
        reg twin`S'_`n'_fam `c2' `wt' if `n'_plus==1
        predict twin`S'_`n'_star                                    if `n'_plus==1
        replace twin`S'_`n'_star = twin`S'_`n'_fam-twin`S'_`n'_star if `n'_plus==1
        replace twin`S'_`n'_star = 0                                if `n'_plus==0

        local z`S' `z`S'' twin`S'_`n'_star
    }
    
}
sum `zs'
sum `zGirl'
sum `zBoy'

local p partial(`base') savefirst

ivreg2 `y' `c2' (`x'=`zs')   `wt', `se' `p'
ivreg2 `y' `c2' (`x'=`zs')   `wt' if gender=="F", `se' `p'
ivreg2 `y' `c2' (`x'=`zs')   `wt' if gender=="M", `se' `p'

ivreg2 `y' `c2' (daughters sons=`zGirl' `zBoy') `wt', `se' `p'
ivreg2 `y' `c2' (daughters sons=`zGirl' `zBoy') `wt' if gender=="F", `se' `p'
ivreg2 `y' `c2' (daughters sons=`zGirl' `zBoy') `wt' if gender=="M", `se' `p'



* Testing for child school ages
gen age1 = age if bord==1
gen age2 = age if bord==2
gen age3 = age if bord==3
gen age4 = age if bord==4
bys id: egen age1m = mean(age1)
bys id: egen age2m = mean(age2)
bys id: egen age3m = mean(age3)
bys id: egen age4m = mean(age4)
gen diff2_1=age1m-age2m
gen diff3_1=age1m-age3m
gen diff4_1=age1m-age4m
gen diff3_2=age2m-age3m
gen diff4_2=age2m-age4m
gen diff4_3=age3m-age4m
gen girl = gender=="F"
gen fertGirl = fert*girl

generat diff = diff2_1 if bord==1&two_plus==1
replace diff = diff3_1 if bord==1&three_plus==1&diff3_1!=.
replace diff = diff3_2 if bord==2&three_plus==1&diff3_2!=.
replace diff = diff4_1 if bord==1&four_plus==1&diff4_1!=.
replace diff = diff4_2 if bord==2&four_plus==1&diff4_2!=.
replace diff = diff4_3 if bord==3&four_plus==1&diff4_3!=.
tab diff

generat diffL6 = 1 if diff<6 & diff>0
replace diffL6 = 0 if diff>=6& diff!=.
foreach l of numlist 0 1 {
    ivreg2 `y' `c2' (`x'=`zs')   `wt' if diffL6==`l', `se' `p'
    ivreg2 `y' `c2' (`x'=`zs')   `wt' if diffL6==`l'&gender=="F", `se' `p'
    ivreg2 `y' `c2' (`x'=`zs')   `wt' if diffL6==`l'&gender=="M", `se' `p'

    ivreg2 `y' `c2' (daughters sons=`zGirl' `zBoy') `wt' if diffL6==`l', `se' `p'
    ivreg2 `y' `c2' (daughters sons=`zGirl' `zBoy') `wt' if diffL6==`l'&gender=="F", `se' `p'
    ivreg2 `y' `c2' (daughters sons=`zGirl' `zBoy') `wt' if diffL6==`l'&gender=="M", `se' `p'

}
exit



foreach k in two three four {
    preserve
    keep if `k'_plus==1
    if `"`k'"'=="two" {
        gen diff     = diff2_1 if bord==1
    }
    if `"`k'"'=="three" {
        gen diff     = diff3_1 if bord==1
        replace diff = diff3_2 if bord==2
    }
    if `"`k'"'=="four" {
        gen diff     = diff4_1 if bord==1
        replace diff = diff4_2 if bord==2
        replace diff = diff4_3 if bord==3        
    }

    gen Twin = twin_`k'_fam
    gen TwinGirl = twin_`k'_fam*girl
    local xs fert fertGirl
    local z  Twin TwinGirl
    dis "Age 12 and over, all, `k' plus"
    ivreg2 `y' `c2' (`xs'=`z') `wt' if diff>=6&diff!=.&age>=12, `se' `p' 
    ivreg2 `y' `c2' (`xs'=`z') `wt' if diff<6 &diff>=0&age>=12, `se' `p'

    dis "Aged under 12, all, `k' plus"
    ivreg2 `y' `c2' (`xs'=`z') `wt' if diff>=6&diff!=.&age<12, `se' `p' 
    ivreg2 `y' `c2' (`xs'=`z') `wt' if diff<6 &diff>=0&age<12, `se' `p'

    restore
}
exit
preserve
local p partial(`base') 
keep if four_plus==1&age>=12

local z twin_four_fam
ivreg2 `y' `c2' (`x'=`z') `wt' if diff>=6&diff!=., `se' `p' 
ivreg2 `y' `c2' (`x'=`z') `wt' if diff<6 &diff>=0, `se' `p'

ivreg2 `y' `c2' (`x'=`z') `wt' if diff>=6&diff!=.&gender=="F", `se' `p' 
ivreg2 `y' `c2' (`x'=`z') `wt' if diff<6&diff>=0 &gender=="F", `se' `p'

ivreg2 `y' `c2' (`x'=`z') `wt' if diff>=6&diff!=.&gender=="M", `se' `p' 
ivreg2 `y' `c2' (`x'=`z') `wt' if diff<6&diff>=0 &gender=="M", `se' `p'

restore

exit
*/
********************************************************************************
**** (5) IV effect of sisters and brothers (using girl twin and boy twins at
****     order n), subsequent inclusion of twin predictors
********************************************************************************
tokenize `fnames'
local ecnt 1
foreach condition of local conditions {
    dis "A"
    local ests1sons
    local ests1daughter
    local ests2
    local OUT "$OUT/IV_Mixto/`1'"
    
    foreach n in two three four {
        preserve
        keep if keeper == 0
        keep if `condition'&`n'_plus==1
        dis "B"
        
        local p partial(`base') savefirst 
        gen BoyTwin  = twinBoy_`n'_fam
        gen GirlTwin = twinGirl_`n'_fam
        local z1 BoyTwin
        local z2 GirlTwin
        dis "C"

        foreach e of numlist 1 2 {
            #delimit ;
            eststo: ivreg2 `y' `c`e'' (daughters sons=`z1' `z2') `wt',
            `se' `p' savefp(f`ecnt');
            #delimit cr
            dis "D"

            test daughters = sons
            local pval = r(p)
            estadd scalar betatest=`pval'
            
            local ests2 `ests2' est`ecnt'
            local ests1sons `ests1sons' f`ecnt'sons
            local ests1daughters `ests1daughters' f`ecnt'daughters
            dis "E"
            mat first=e(first)
            estadd scalar KPF_s=first[15,1]: f`ecnt'sons
            estadd scalar KPF_d=first[15,2]: f`ecnt'daughters
            estadd scalar KPp_s=first[18,1]: f`ecnt'sons
            estadd scalar KPp_d=first[18,2]: f`ecnt'daughters
            local ++ecnt
        }
        restore
    }
    macro shift
}

*** Add test of gamma_2^G=gamma_2^B
*** (note uses Frisch Waugh Lovell to concentrate out variables)
local jj=7
local kk=13
foreach n in two three four {
    preserve
    keep if keeper == 0
    keep if `n'_plus==1
    local z1 twinBoy_`n'_fam
    local z2 twinGirl_`n'_fam
    local x1 sons
    local x2 daughters 

    foreach cntrl in 1 2 {
        foreach var in x1 x2 y z1 z2 {
            qui reg ``var'' `c`cntrl'' `wt' if gender=="F"
            qui predict `var'res            if gender=="F", resid
            
            qui reg ``var'' `c`cntrl'' `wt' if gender=="M"
            qui predict `var'res2           if gender=="M", resid
            replace `var'res = `var'res2    if gender=="M"
            drop `var'res2
        }
        gen x1maleres = x1res*malec
        gen x2maleres = x2res*malec
        gen z1maleres = z1res*malec
        gen z2maleres = z2res*malec
        local xs x1res x2res x1maleres x2maleres
        local zs z1res z2res z1maleres z2maleres
        
        ivreg2 yres (`xs' = `zs') `wt', `se' 
        test x2res=x2maleres
        test x2maleres
        local pval = r(p)
        estadd scalar gamma2test=`pval': est`jj'
        test x1res=x1maleres
        test x1maleres
        local pval = r(p)
        estadd scalar gamma3test=`pval': est`kk'
        local jj=`jj'+1
        local kk=`kk'+1
        if `cntrl'==2 {
            keep yres `xs' `zs' sweight id
            gen d_2_G = .
            gen d_2_B = .
            gen d_3_G = .
            gen d_3_B = .
            tempfile baseF
            save `baseF'

            set seed 2727
            local s = 50 
            forvalues i = 1/`s' {
                dis "Bootstrap sample `i' for bootstrap test"
                bsample, cluster(id) idcluster(nid)
                ivreg2 yres (`xs' = `zs') `wt', cluster(nid)
                use `baseF', clear
                replace d_2_G = _b[x2res] in `i'
                replace d_2_B = _b[x2res]+_b[x2maleres] in `i'
                replace d_3_G = _b[x1res] in `i'
                replace d_3_B = _b[x1res]+_b[x1maleres] in `i'
                sum d_*
                save `baseF', replace
            }
            gen girlBias = d_3_G < d_2_G & d_2_G < d_3_B & d_3_B < d_2_B in 1/`s'
            gen boyBias  = d_3_G > d_2_G & d_2_G > d_3_B & d_3_B > d_2_B in 1/`s'
            sum girlBias
            local gpval = 1-r(mean)
            dis "P-value on Girl Bias is `gpval'"
            sum boyBias
            local bpval = 1-r(mean)
            dis "P-value on Boy Bias is `bpval'"
        }
        if `cntrl'==1 drop `xs' yres z1res z2res z1maleres z2maleres
    }
    restore
}


***PANEL A, TABLE 3
lab var sons "Total Sons"
lab var daughters "Total Daughters"
#delimit ;
esttab est2 est4 est6 using "$OUT/IV/sondaughter.tex", replace
keep(daughters sons)
nogaps b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N betatest, fmt(%12.0gc %5.3f)
      label("\\ Observations"
            "Test of $\beta\sb{Daughter}=\beta\sb{Son}$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;

esttab est1 est3 est5 using "$OUT/IV/sondaughter_c1.tex", replace
keep(daughters sons)
nogaps b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N betatest, fmt(%12.0gc %5.3f)
      label("\\ Observations"
            "Test of $\beta\sb{Daughter}=\beta\sb{Son}$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
#delimit cr

***TOP OF PANEL A, TABLE 4 (could remove observations to only have in joint)
#delimit ;
esttab est8 est10 est12 est14 est16 est18 using "$OUT/IV/boygirl_sondaughter.tex",
replace keep(daughters sons) nogaps b(%-9.3f) se(%-9.3f)
starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N betatest gamma2test gamma3test,
      fmt(%12.0gc %5.3f %5.3f %5.3f)
      label("\\ Observations" "Test of $\delta\sb{2}=\delta\sb{3}$ (p-value)"
            "Test of $\delta\sb{2}^G=\delta\sb{2}^B$ (p-value)"
            "Test of $\delta\sb{3}^G=\delta\sb{3}^B$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;

esttab est7 est9 est11 est13 est15 est17 using "$OUT/IV/boygirl_sondaughter_c1.tex",
replace keep(daughters sons) nogaps b(%-9.3f) se(%-9.3f)
starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N betatest gamma2test gamma3test,
      fmt(%12.0gc %5.3f %5.3f %5.3f)
      label("\\ Observations" "Test of $\delta\sb{2}=\delta\sb{3}$ (p-value)"
            "Test of $\delta\sb{2}^G=\delta\sb{2}^B$ (p-value)"
            "Test of $\delta\sb{3}^G=\delta\sb{3}^B$ (p-value)"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
#delimit cr

***Appendix First Stage Tables
gen BoyTwin = .
gen GirlTwin = .
lab var BoyTwin "Boy Twin Birth"
lab var GirlTwin "Girl Twin Birth"
***PANEL A, TABLE 5
#delimit ;
esttab f2daughters  f4daughters  f6daughters  f8daughters f10daughters
       f12daughters f14daughters f16daughters f18daughters using
"$OUT/IV/firstStage_daughters.tex", replace keep(BoyTwin GirlTwin) nogaps
b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N KPF_d KPp_d, fmt(%12.0gc %9.1f %5.3f)
      label("\\ Observations" "Angrist-Pischke First Stage F-statistic"
            "p-value AP Test Statistic"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;

esttab f1daughters  f3daughters  f5daughters  f7daughters f9daughters
       f11daughters f13daughters f15daughters f17daughters using
"$OUT/IV/firstStage_daughters_c1.tex", replace keep(BoyTwin GirlTwin) nogaps
b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N KPF_d KPp_d, fmt(%12.0gc %9.1f %5.3f)
      label("\\ Observations" "Angrist-Pischke First Stage F-statistic"
            "p-value AP Test Statistic"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;

esttab f2sons f4sons f6sons f8sons f10sons f12sons f14sons f16sons f18sons
using "$OUT/IV/firstStage_sons.tex", replace keep(BoyTwin GirlTwin) nogaps
b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N KPF_s KPp_s, fmt(%12.0gc %9.1f %5.3f)
      label("\\ Observations" "Angrist-Pischke First Stage F-statistic"
            "p-value AP Test Statistic"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;

esttab f1sons f3sons f5sons f7sons f9sons f11sons f13sons f15sons f17sons
using "$OUT/IV/firstStage_sons_c1.tex", replace keep(BoyTwin GirlTwin) nogaps
b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N KPF_s KPp_s, fmt(%12.0gc %9.1f %5.3f)
      label("\\ Observations" "Angrist-Pischke First Stage F-statistic"
            "p-value AP Test Statistic"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
#delimit cr
estimates clear
drop BoyTwin GirlTwin

********************************************************************************
**** (6) IV bounds
********************************************************************************    
foreach n in two three four {
    preserve
    keep if keeper == 0
    gen UB    = .
    gen LB    = .
    gen gendb = ""
    gen gval  = .
    local j = 1
    
    keep if `n'_plus==1
    local z twin_`n'_fam
    foreach gend in F M {
        foreach var in x y z {
            qui reg ``var'' `c2' `wt' if gender=="`gend'"
            qui predict `var'res      if gender=="`gend'", resid
        }

        local cc if gender=="`gend'"
        local mval = 0.05 
        foreach num of numlist 0(1)10 {
            local mu = (`num'/10)*`mval'/2
            local om = ((`num'/10)*`mval'/sqrt(12))^2
            plausexog ltz yres (xres = zres) `wt' `cc', mu(`mu') omega(`om')
            replace UB = e(ub_xres) in `j'
            replace LB = e(lb_xres) in `j'
            replace gendb = "`gend'" in `j'
            replace gval = `mval'*`num' in `j'
            local ++j
        }
        drop xres yres zres
    }
    replace gval = gval*0.1
    format gval %5.2f
    format UB   %5.2f
    format LB   %5.2f
    #delimit ;
    twoway line LB gval if gendb=="F", lpattern(solid) lcolor(red) lwidth(medthick) 
        || line UB gval if gendb=="F", lpattern(solid) lcolor(red) lwidth(medthick) 
        || line LB gval if gendb=="M", lpattern(dash) lcolor(blue) lwidth(medthick) 
        || line UB gval if gendb=="M", lpattern(dash) lcolor(blue) lwidth(medthick) 
    scheme(s1mono) legend(order(1 "95% CI: Girl Children" 3 "95% CI: Boy Children"))
    yline(0, lcolor(gs14) lpattern(solid))
    ytitle("Bounds Estimates of QQ Trade-off") xtitle({&delta});
    graph export "$OUT/boundsEstimates_`n'.eps", replace;
    #delimit cr
    restore
}
exit
*/
********************************************************************************
**** (7) Gender bias
********************************************************************************    
replace idealGirl = . if idealGirl>20
replace idealBoys = . if idealBoys>20


gen idealb_ratio = idealBoys/(idealBoys+idealGirls)
bys country: egen BGratioIndividual = mean(idealb_ratio)

bys country: egen aveGirls = mean(daughters)
bys country: egen aveBoys  = mean(sons)
bys country: egen aveGirlsIdeal = mean(idealG)
bys country: egen aveBoysIdeal  = mean(idealB)

gen BGratio = aveBoys/aveGirls
gen BGratioIdeal = aveBoysIdeal/aveGirlsIdeal
xtile BGquintile = BGratio, nq(5)
xtile BGquintileIdeal = BGratioIdeal, nq(5)
**below is too concentrated to have five quintiles.  Lots of weight on 0.5
xtile BGquintileIndiv = idealb_ratio, nq(5)
replace BGquintileIndiv=2 if BGquintileIndiv==4
replace BGquintileIndiv=3 if BGquintileIndiv==5


********************************************************************************
**** (8) Descriptive Plots
********************************************************************************
preserve
collapse daughters sons idealGirls idealBoys BGratioInd, by(country WBcountry)
gen BoyGirlRatio = sons/daughters
gen desiredBoyGirl = idealBoys/idealGirls
xtile BGquintileIdealW = desiredBoyGirl, nq(5)
gen DHS=1

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

spmap BGratioIndividual if NAME!="Antarctica" using "$DAT/shape/world_coords",
id(_ID) osize(vvthin vvthin vvthin vvthin vvthin) fcolor(Rainbow) clnumber(5)
ndfcolor(gs15) legend(symy(*1.2) symx(*1.2) size(*1.8) position(9))
legend(title("Boy/Girl Ratio", size(*1.2) bexpand justification(left)))
legend(label(1 "No DHS Data")) ndsize(vvthin);
graph export "$OUT/BoyGirlRatioIndividual.eps", replace;

spmap desiredBoyGirl if NAME!="Antarctica" using "$DAT/shape/world_coords",
id(_ID) osize(vvthin vvthin vvthin vvthin vvthin) fcolor(Rainbow) clnumber(5)
ndfcolor(gs15) legend(symy(*1.2) symx(*1.2) size(*1.8) position(8))
legend(title("Desired Boy/Girl Ratio", size(*1.2) bexpand justification(left)))
legend(label(1 "No DHS Data")) ndsize(vvthin);
graph export "$OUT/BoyGirlDesired.eps", replace;
#delimit cr

format BGratioIndiv   %5.2f
format desiredBoyGirl %5.2f
#delimit ;
twoway scatter BGratioIndiv desiredBoyGirl [w=POP_EST], msymbol(circle_hollow)
||     scatter BGratioIndiv desiredBoyGirl, mlabel(NAME) scheme(s1mono) m(i)
mlabposition(1) legend(off)
||     lfit    BGratioIndiv desiredBoyGirl [aw=POP_EST], lcolor(red) lpattern(dash)
lwidth(medthick) ytitle("Individual Level") xtitle("Country Level");
graph export "$OUT/genderBiasMeasures.eps", replace;
#delimit cr

keep if DHS==1
keep country BGquintileIdealW desiredBoyGirl
tempfile countrylevel
save `countrylevel'
exit
restore

preserve
gen fertBoy    = fert*malec
gen estimate   = .
gen UB         = .
gen LB         = .
gen estnum     = .
gen estimateIV = .
gen UBIV       = .
gen LBIV       = .

merge m:1 country using `countrylevel'

keep if keeper == 0
set matsize 1000

local j = 1
local k = -1
local l = 2
foreach n in two three four {
    local k=`k'+2
    local z twin_`n'_fam    
    gen zBoy = twin_`n'_fam*malec
    foreach quint of numlist 1(1)5 {
        local cond if BGquintileIdealW==`quint'&`n'_plus==1

        eststo: reg `y' `x' fertBoy `c2' `wt' `cond', `se'
        replace estimate = _b[fertBoy] in `j'
        replace LB = _b[fertBoy]+invnorm(0.025)*_se[fertBoy] in `j'
        replace UB = _b[fertBoy]+invnorm(0.975)*_se[fertBoy] in `j'
        replace estnum = `k' in `j'

        eststo: ivreg2 `y' (`x' fertBoy=`z' zBoy) `c2' `wt' `cond', `se' savefirst
        replace estimateIV = _b[fertBoy] in `j'
        replace LBIV = _b[fertBoy]+invnorm(0.025)*_se[fertBoy] in `j'
        replace UBIV = _b[fertBoy]+invnorm(0.975)*_se[fertBoy] in `j'

        mat first=e(first)
        estadd scalar KPp_f=first[18,1]: est`l'
        estadd scalar KPp_b=first[18,2]: est`l'
        
        local ++j
        local ++k
        local l=`l'+2
    }
    drop zBoy
}

format estimate %5.2f
#delimit ;
twoway scatter estimate estnum in 1/`k', msymbol(S) mcolor(black) ||
       rcap UB LB estnum, lcolor(black) scheme(s1mono)
ytitle("QQ Trade-off (Girl Penalty)") xtitle(" ") ylabel(0(0.02)0.1)
yline(0, lcolor(red) lpattern(dash))
xlabel(1  "Low" 2  "Q2"  3 "Q3"  4 "Q4"  5 "High"  6 " "  7 " "
       8  "Low" 9  "Q2" 10 "Q3" 11 "Q4" 12 "High" 13 " " 14 " "
       15 "Low" 16 "Q2" 17 "Q3" 18 "Q4" 19 "High") 
text(0.08 3 "Two-Plus") text(0.08 10 "Three-Plus") text(0.08 17 "Four-Plus")
legend(label(1 "Point Estimate (OLS)") label(2 "95% Confidence Interval"));
graph export "$OUT/OLS/genderBiasQQ.eps", replace;


twoway scatter estimateIV estnum in 1/`k', msymbol(S) mcolor(black) ||
       rcap UBIV LBIV estnum, lcolor(black) scheme(s1mono)
ytitle("QQ Trade-off (Girl Penalty)") xtitle(" ") ylabel(-0.4(0.2)0.6)
yline(0, lcolor(red) lpattern(dash))
xlabel(1  "Low" 2  "Q2"  3 "Q3"  4 "Q4"  5 "High"  6 " "  7 " "
       8  "Low" 9  "Q2" 10 "Q3" 11 "Q4" 12 "High" 13 " " 14 " "
       15 "Low" 16 "Q2" 17 "Q3" 18 "Q4" 19 "High") 
text(0.5 3 "Two-Plus") text(0.5 10 "Three-Plus") text(0.5 17 "Four-Plus")
legend(label(1 "Point Estimate (IV)") label(2 "95% Confidence Interval"));
graph export "$OUT/IV/genderBiasQQ.eps", replace;
#delimit cr
exit
lab var fert    "Number of Children"
lab var fertBoy "N Children $\times$ Boy Child"
foreach num of numlist 1(1)5 {
    if `num'==1 local qests est1  est3  est5  est2  est4  est6
    if `num'==2 local qests est7  est9  est11 est8  est10 est12
    if `num'==3 local qests est13 est15 est17 est14 est16 est18
    if `num'==4 local qests est19 est21 est23 est20 est22 est24
    if `num'==5 local qests est25 est27 est29 est26 est28 est30
    
    #delimit ;
    esttab `qests' using "$OUT/IV/quintilesBias_`num'.tex", replace
    keep(fert fertBoy) nogaps b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
    stats(N KPp_f KPp_b, fmt(%12.0gc %5.3f %5.3f)
          label("Observations" "A-P First Stage p-value (N Children)"
                "A-P p-value (N Children $\times$ Boy)"))
    label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
    #delimit cr
}
estimates clear

gen fertxBias     = fert*desiredBoyGirl
gen fertxBiasxBoy = fertBoy*desiredBoyGirl
foreach n in two three four {
    local k=`k'+2
    local z twin_`n'_fam    
    local cond if `n'_plus==1
    gen twinxBias     = twin_`n'_fam*desiredBoyGirl
    gen zBoy = twin_`n'_fam*malec
    gen twinxBiasxBoy = zBoy*desiredBoyGirl

    local xx `x' fertBoy fertxBias fertxBiasxBoy
    local zz `z' zBoy twinxBias twinxBiasxBoy
    eststo: ivreg2 `y' (`xx'=`zz') `c2' `wt' `cond', `se'
    eststo: reg    `y'  `xx'       `c2' `wt' `cond', `se'

    drop zBoy twinxBias twinxBiasxBoy
}
lab var fert          "Number of Children"
lab var fertBoy       "N Children $\times$ Boy Child"
lab var fertxBias     "N Children $\times$ Son Preference"
lab var fertxBiasxBoy "N Children $\times$ Son Pref $\times$ Boy Child"
#delimit ;
esttab est2 est4 est6 est1 est3 est5 using "$OUT/IV/genderBiasInteract.tex",
replace keep(fert fertBoy fertxBias fertxBiasxBoy) nogaps
b(%-9.3f) se(%-9.3f) starlevel("*" 0.10 "**" 0.05 "***" 0.01)
stats(N, fmt(%12.0gc) label("\\ Observations"))
label nonotes mlabels(, none) nonumbers style(tex) fragment noline;
#delimit cr
estimates clear
restore
exit



********************************************************************************
**** (9) Non-linear IV estimates of Mogstad and Wiswall
********************************************************************************
keep if fert<=6
local nbstrap 10
***SAMPLE OF FIRST-BORN CHILDREN (two_plus==1)
* (A)
*Want to use fert 2->3
*Want to use fert 3->4
*Want to use fert 4->5
*Want to use fert 5->6
* (B)
*Include up to fertility 6
*(C)
* First stage I is twin-predicted or 0 if fert is less than N
*(D)
* First stage II is predicted fert of 1 if twinning at N

cap program drop nonlinearIV
program nonlinearIV, eclass
version 11
syntax varlist(fv), fnum(string) [efficient]
*GENERATE TWIN STAR

if `"`fnum'"'=="two" {
    local current two
    local nlist three four five
    local jj=3

    *(A) GENERATE FERT VARIABLES
    foreach num of numlist 3(1)6 {
        gen fert`num'=fert>=`num'
    }
    *GENERATE TWIN STAR
    local ints i.educf#c.motherage i.educf#c.motheragesq i.educp#c.motherage i.educp#c.motheragesq
    foreach num in `nlist' {
        reg twin_`num'_fam `varlist' `ints' i.agemay i.educp if fert>=`jj'
        predict twinStar_`num' if fert>=`jj'
        replace twinStar_`num' = twin_`num'_fam - twinStar_`num'
        replace twinStar_`num' = 0 if fert<`jj'
        local ++jj
    }

    
    *GENERATE EFFICIENT INSTRUMENTS
    if length(`"`efficient'"')!=0 {
        probit fert3 `varlist' i.educf i.agemay
        predict p2hat
        replace p2hat = 1 if twin_two_fam==1
        local l1 = 3
        foreach g in three four five {
            local fcontrol i.educf i.agemay
            if `"`g'"'=="five" local fcontrol i.educf twin_two_fam i.agemay
            local l2 = `l1'+1
            probit fert`l2' `varlist' twinStar_`g' `fcontrol'
            predict p`l1'hat
            local ++l1
        }
    }
    local z  twin_two_fam
    local XF fert3 fert4 fert5 fert6
    local Z1 twin_two_fam twinStar_three twinStar_four twinStar_five
    local Z2 p2hat p3hat p4hat p5hat
}
else if `"`fnum'"'=="three" {
    local current three
    local nlist   four five
    local jj=4
    
    *(A) GENERATE FERT VARIABLES
    foreach num of numlist 4(1)6 {
        gen fert`num'=fert>=`num'
    }
    *GENERATE TWIN STAR
    local ints i.educf#c.motherage i.educf#c.motheragesq i.educp#c.motherage i.educp#c.motheragesq
    foreach num in `nlist' {
        reg twin_`num'_fam `varlist' `ints' i.agemay i.educp if fert>=`jj'
        predict twinStar_`num' if fert>=`jj'
        replace twinStar_`num' = twin_`num'_fam - twinStar_`num'
        replace twinStar_`num' = 0 if fert<`jj'
        local ++jj
    }
    *GENERATE EFFICIENT INSTRUMENTS
    if length(`"`efficient'"')!=0 {
        probit fert4 `varlist' i.educf i.agemay
        predict p3hat
        replace p3hat = 1 if twin_three_fam==1
        local l1 = 4
        foreach g in four five {
            local fcontrol i.educf i.agemay
            if `"`g'"'=="five" local fcontrol i.educf twin_two_fam i.agemay
            local l2 = `l1'+1
            probit fert`l2' `varlist' twinStar_`g' `fcontrol'
            predict p`l1'hat
            local ++l1
        }
    }

    local z  twin_three_fam
    local XF fert4 fert5 fert6
    local Z1 twin_three_fam twinStar_four twinStar_five
    local Z2 p3hat p4hat p5hat
}
else if `"`fnum'"'=="four" {
    local current four
    local nlist   five
    local jj=5

    *(A) GENERATE FERT VARIABLES
    foreach num of numlist 5(1)6 {
        gen fert`num'=fert>=`num'
    }
    *GENERATE TWIN STAR
    local ints i.educf#c.motherage i.educf#c.motheragesq i.educp#c.motherage i.educp#c.motheragesq
    foreach num in `nlist' {
        reg twin_`num'_fam `varlist' `ints' i.agemay i.educp if fert>=`jj'
        predict twinStar_`num' if fert>=`jj'
        replace twinStar_`num' = twin_`num'_fam - twinStar_`num'
        replace twinStar_`num' = 0 if fert<`jj'
        local ++jj
    }

    *GENERATE EFFICIENT INSTRUMENTS
    if length(`"`efficient'"')!=0 {
        probit fert5 `varlist' i.educf i.agemay
        predict p4hat
        replace p4hat = 1 if twin_three_fam==1
        local l1 = 5
        foreach g in five {
            local fcontrol i.educf i.agemay
            if `"`g'"'=="five" local fcontrol i.educf twin_two_fam i.agemay
            local l2 = `l1'+1
            probit fert`l2' `varlist' twinStar_`g' `fcontrol'
            predict p`l1'hat
            local ++l1
        }
    }

    local z  twin_four_fam
    local XF fert5 fert6
    local Z1 twin_four_fam twinStar_five
    local Z2 p4hat p5hat
}
else {
    dis "Error"
    exit
}
local y  school_zscore
local wt [pw=sweight]
*eststo: ivreg2 `y' `varlist' (fert = `z' ) `wt', savefirst savefp(FM)

if length(`"`efficient'"')==0 {
    eststo: ivreg2 `y' `varlist' (`XF' = `Z1') `wt', savefirst savefp(FN)
}
else {
    eststo: ivreg2 `y' `varlist' (`XF' = `Z2') `wt', savefirst savefp(FO)
}
mat beta = e(b)
ereturn post beta
*clean up
cap drop twinSt*
foreach var in fert3 fert4 fert5 fert6 p2hat p3hat p4hat p5hat {
     cap drop `var'
}
end

set matsize 5000
local j = 1
foreach tab in two three four {
    ****GIRLS
    set seed 1934
    preserve
    keep if keeper==0&`tab'_plus==1&malec==0
    ivreg2 `y' `c2' (fert = twin_`tab'_fam) `wt', savefirst savefp(FM)
    estimates store nl`j'
    local ++j
    
    keep if e(sample)==1
    *nonlinearIV `c2', fnum(`tab')
    bstrap _b, rep(`nbstrap') cluster(id): nonlinearIV `c2', fnum(`tab') 
    estimates store nl`j'
    local ++j

    *nonlinearIV `c2', fnum(`tab') efficient
    *bstrap _b, rep(`nbstrap') cluster(id): nonlinearIV `c2', fnum(`tab') efficient
    *estimates store estEF

    if `"`tab'"'=="two" {
        local fst   FNfert3 FNfert4 FNfert5 FNfert6
        local kvars twin_two_fam twinStar_three twinStar_four twinStar_five
        foreach var in `kvars' {
            cap gen `var'=.
        }
        lab var twin_two_fam   "Twin Second"
        lab var twinStar_three "Twin$^{*}$ Third"
        lab var twinStar_four  "Twin$^{*}$ Fourth"
        lab var twinStar_five  "Twin$^{*}$ Fifth"
    }
    if `"`tab'"'=="three" {
        local fst   est1 FNfert4 FNfert5 FNfert6
        local kvars twin_three_fam twinStar_four twinStar_five
        foreach var in `kvars' {
            cap gen `var'=.
        }
        lab var twin_three_fam "Twin Third"
        lab var twinStar_four  "Twin$^{*}$ Fourth"
        lab var twinStar_five  "Twin$^{*}$ Fifth"
    }
    if `"`tab'"'=="four" {
        local fst   est1 est1 FNfert5 FNfert6
        local kvars twin_four_fam twinStar_five
        foreach var in `kvars' {
            cap gen `var'=.
        }
        lab var twin_four_fam "Twin Fourth"
        lab var twinStar_five  "Twin$^{*}$ Fifth"
    }
    #delimit ;
    esttab `fst' using "$OUT/IV/NonLinearIVfirst-`tab'-girls.tex", replace label
    keep(`kvars') nogaps b(%-9.3f) se(%-9.3f) nonotes style(tex) mlabels(, none)
    starlevel("*" 0.10 "**" 0.05 "***" 0.01) nonumbers  fragment noline noobs;
    #delimit cr
    restore

    ****BOYS
    preserve
    keep if keeper==0&`tab'_plus==1&malec==1
    ivreg2 `y' `c2' (fert = twin_`tab'_fam) `wt', savefirst savefp(FM)
    estimates store nl`j'
    local ++j

    keep if e(sample)==1
    *nonlinearIV `c2', fnum(`tab')
    bstrap _b, rep(`nbstrap') cluster(id): nonlinearIV `c2', fnum(`tab') 
    estimates store nl`j'
    local ++j

    *nonlinearIV `c2', fnum(`tab') efficient
    *bstrap _b, rep(`nbstrap') cluster(id): nonlinearIV `c2', fnum(`tab') efficient
    *estimates store estEF
    
    if `"`tab'"'=="two" {
        local kvars twin_two_fam twinStar_three twinStar_four twinStar_five
        foreach var in `kvars' {
            cap gen `var'=.
        }
        lab var twin_two_fam   "Twin Second"
        lab var twinStar_three "Twin$^{*}$ Third"
        lab var twinStar_four  "Twin$^{*}$ Fourth"
        lab var twinStar_five  "Twin$^{*}$ Fifth"
    }
    if `"`tab'"'=="three" {
        local kvars twin_three_fam twinStar_four twinStar_five
        foreach var in `kvars' {
            cap gen `var'=.
        }
        lab var twin_three_fam "Twin Third"
        lab var twinStar_four  "Twin$^{*}$ Fourth"
        lab var twinStar_five  "Twin$^{*}$ Fifth"
    }
    if `"`tab'"'=="four" {
        local kvars twin_four_fam twinStar_five
        foreach var in `kvars' {
            cap gen `var'=.
        }
        lab var twin_four_fam "Twin Fourth"
        lab var twinStar_five  "Twin$^{*}$ Fifth"
    }
    #delimit ;
    esttab `fst' using "$OUT/IV/NonLinearIVfirst-`tab'-boys.tex", replace label
    keep(`kvars') nogaps b(%-9.3f) se(%-9.3f) nonotes style(tex) mlabels(, none)
    starlevel("*" 0.10 "**" 0.05 "***" 0.01) noobs nonumbers  fragment noline;
    #delimit cr
    restore
}
foreach num of numlist 3 4 5 6 {
    local k = `num'-1
    cap gen fert`num'=.
    lab var fert`num' "Siblings $\geq`k'$"
}

lab var fert "Number of Children"
#delimit ;
esttab nl1 nl3 nl5 nl7 nl9 nl11 using "$OUT/IV/NonLinear-PanelA.tex", label
replace keep(fert) nogaps b(%-9.3f) se(%-9.3f) nonotes style(tex) mlabels(, none)
starlevel("*" 0.10 "**" 0.05 "***" 0.01) noobs nonumbers fragment noline;

esttab nl2 nl4 nl6 nl8 nl10 nl12 using "$OUT/IV/NonLinear-PanelB.tex", label
replace keep(fert3 fert4 fert5 fert6) nogaps b(%-9.3f) se(%-9.3f) nonotes noline
style(tex) mlabels(, none) starlevel("*" 0.10 "**" 0.05 "***" 0.01) nonumbers 
stats(N, fmt(%12.0gc) label("\\ Observations")) fragment
order(fert3 fert4 fert5 fert6);
#delimit cr

*estimates clear
    


********************************************************************************
**** (X) Clean up
********************************************************************************
log close
