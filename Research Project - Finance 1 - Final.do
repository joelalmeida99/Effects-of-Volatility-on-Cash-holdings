cls, clear
cd "C:\Users\Lenovo\Documents\Research Project\Stata Files"

use data_firm_vol
use data_firm_fin

merge 1:1 gvkey year using data_firm_vol //merges datasets, adding volatility to main file

gen inv = (capex/at) * 100 //capital expenditures as a percentage of book value of equity
gen cash = (che/at) * 100 //cash as a percentage of book value of assets

preserve
	collapse cash inv (semean) c_se = cash i_se = inv (count) n=cash, by(year)
	twoway (line cash year) (line inv year), name(gr1)
	twoway (scatter cash year [aweight=1/c_se], ms(Oh) mc(blue)) ///
	(scatter inv year [aweight=1/i_se], ms(Oh) mc(black)) ///
	(lfit cash year, lc(blue)) (lfit inv year, lc(black)), name(gr2)
restore

replace year = year-2000

reg inv year , r //assume 5% significance 
//dependant variable is capex, independent variables are bkequity and year 
//equation is : capex = (-0.123*year) + 6.8
//for year : 
//H0 : year = 0
//H1 : year != 0
//t = -17.14 and p < 0.01 , this means that we reject h0 and accept h1 year = -0.15 (as t is not in +-1.96 range and p < 0.05, also since 0 is not in the confidence interval)
//for constant : 
//H0 : cons = 0
//H1 : cons != 0
//t = 96.07 and p < 0.01 , this means that we reject h0 and accept h1 (t not within +-1.96, p < 0.05 , 0 not in C.I.) 

reg cash year , r //assume 5% significance
//dependant variable is che, independant variables are at and year
//equation is : che = (0.169*year) + 10.644
//for year : 
//H0 : year = 0
//H1 : year != 0
//t = 10.56 and p < 0.01, this means that we reject h0 and accept h1 (t is outside +-1.96 range, p < 0.05, 0 does not appear in confidence interval)
//for constant : 
//H0 : cons = 0
//H1 : cons != 0
//t = 57.16 and p < 0.01, this means that we reject h0 and accept h1 (t is outside +-1.96 range, p < 0.05, 0 does not appear in confidence interval)

gen cash_vol_missing = mi(cash_vol)


reg cash year , r , if cash_vol_missing == 0 //assume 5% significance
//dependent variable is che, independent variables are at and year 
//equation is che = (0.176*year) + 10.471 
//for year : 
//H0 : year = 0
//H1 : year != 0
//t = 9.86 and p < 0.01, this means that we reject h0 and accept h1 (t is outside +-1.96 range, p < 0.05, 0 does not appear in confidence interval)
//for constant : 
//H0 : cons = 0
//H1 : cons != 0
//t = 55.57 and p < 0.01, this means that we reject h0 and accept h1 (t is outside +-1.96 range, p < 0.05, 0 does not appear in confidence interval)



reg cash year cash_vol , r , if cash_vol_missing == 0 //assume 5% significance
//dependent variable is che, independent variables are at year and cash_vol 
//equation is che = (0.12*year) + (1.449*cash_vol) + 6.417
//for year : 
//H0 : year = 0
//H1 : year != 0
//t = 6.85 and p < 0.01, this means that we reject h0 and accept h1 (t is outside +-1.96 range, p < 0.05, 0 does not appear in confidence interval)
//for cash_vol : 
//H0 : cash_vol = 0
//H1 : cash_vol != 0
//t = 8.14 and p < 0.01, this means that we reject h0 and accept h1 (t is outside +-1.96 range, p < 0.05, 0 does not appear in confidence interval)
//for constant : 
//H0 : cons = 0
//H1 : cons != 0
//t = 13.79 and p < 0.01, this means that we reject h0 and accept h1 (t is outside +-1.96 range, p < 0.05, 0 does not appear in confidence interval)


gen insmpl = (cash<67.28042) * (cash_vol<25.53325)
reg cash year cash_vol , r , if cash_vol_missing == 0 & insmpl == 1 //assume 5% significance
//equation is che = (0.1044*year) + (1.847*cash_vol) + 5.344
//as we can see, the effect that the year has on cash holdings is still positive and still significant when controlled for the highest 5 values of cash holdings and volatility 
//dependent variable is che, independent variables are at year and cash_vol 
//for year : 
//H0 : year = 0
//H1 : year != 0
//t = 6.74 and p < 0.01, this means that we reject h0 and accept h1 (t is outside +-1.96 range, p < 0.05, 0 does not appear in confidence interval)
//for cash_vol : 
//H0 : cash_vol = 0
//H1 : cash_vol != 0
//t = 16.62 and p < 0.01, this means that we reject h0 and accept h1 (t is outside +-1.96 range, p < 0.05, 0 does not appear in confidence interval)
//for constant : 
//H0 : cons = 0
//H1 : cons != 0
//t = 19.34 and p = 0, this means that we reject h0 and accept h1 (t is outside +-1.96 range, p < 0.05, 0 does not appear in confidence interval)


//percentage of trend due to volatility = (.1759798 - .1044437) / 0.1759798 = 40.65%  

eststo a: reg cash year , r 
eststo b: reg cash year , r , if cash_vol_missing == 0
eststo c: reg cash year cash_vol , r , if cash_vol_missing == 0
eststo d: reg cash year cash_vol , r , if cash_vol_missing == 0 & insmpl == 1
eststo e: reg cash year cash_vol bkequity , r , if cash_vol_missing == 0 & insmpl == 1 //include bkequity as control variable to test for robustness
esttab a b c d e, r2 b(%6.3f) se nogaps

