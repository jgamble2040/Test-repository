cap log close
pause on
log using means_example.log,replace

************************************************
*PART 1: ESTIMATING THE MEAN LEVEL OF EDUCATION*
************************************************

*don't do this in your problem sets:
*use "/Users/tvogl/Dropbox/wws508c/problem sets/nlsy - ps 2/nlsy79.dta" if educ<.,clear
*
*do this instead:
use nlsy79.dta if educ<.,clear

*show mean and sd of education
sum educ
pause
/*type q to continue*/

*95% CI for mean education
mean educ
pause

*bootstrap the mean. we treat our sample as the population, 
*we draw 1000 random samples from it, and we estimate the
*mean and variance for each of these samples
bootstrap r(mean) r(Var) r(N),reps(1000) saving(means,replace): summarize educ
pause

*open the dataset containing the results from each of these 
*1000 random samples
use means,clear

*rename the generated variables
ren _bs_1 meanX
ren _bs_2 varianceX
ren _bs_3 N

*create a variable indexing the observation numbers
gen sample_num = _n

*what are the mean and sd of the estimated means?
*how do they relate to the statistics we calculated
*for the whole population?
sum meanX
pause

*estimate the histogram for the estimated means.
*does it look normal?


*calculate 90% and 95% confidence intervals for 
*each of the samples
gen seXbar = sqrt(varianceX/N)
gen ci95_lb = meanX-1.96*seXbar
gen ci95_ub = meanX+1.96*seXbar
gen ci90_lb = meanX-1.64*seXbar
gen ci90_ub = meanX+1.64*seXbar

*generate dummy variables indicating whether the
*true mean lies within the confidence interval
gen inside95 = (ci95_lb<13.68354&ci95_ub>13.68354)
gen inside90 = (ci90_lb<13.68354&ci90_ub>13.68354)

*do 95% of the 95% CIs contain the mean?
tab inside95
pause

*do 90% of the 90% CIs contain the mean?
tab inside90
pause

*visualize the confidence intervals
twoway (rcap ci95_lb ci95_ub sample_num if inside95==1,lcolor(blue)) ///
       (rcap ci95_lb ci95_ub sample_num if inside95==0,lcolor(red)) ///
       (pci 13.68354 1 13.68354 1000,lcolor(red)) ///
       ,title("1000 95% confidence intervals") ylabel(13.4(0.1)13.9) ///
       legend(off) saving(ci95_means,replace)
pause

twoway (rcap ci90_lb ci90_ub sample_num if inside90==1,lcolor(blue)) ///
       (rcap ci90_lb ci90_ub sample_num if inside90==0,lcolor(red)) ///
       (pci 13.68354 1 13.68354 1000,lcolor(red)) ///
       ,title("1000 90% confidence intervals") ylabel(13.4(0.1)13.9) ///
       legend(off) saving(ci90_means,replace)
pause

graph combine ci95_means.gph ci90_means.gph,rows(2) cols(1) saving(ci_combined_means,replace)
pause

*********************************************************************************************
*PART 2: ESTIMATING THE DIFFERENCE IN MEAN EDUCATION BET. NON-HISPANIC NON-BLACKS AND BLACKS*
*********************************************************************************************

use nlsy79.dta if educ<.&hisp==0,clear

*show mean and sd of education for blacks and non-blacks
tab black,sum(educ)
pause

*test the null hypothesis that difference in mean education is zero
ttest educ,by(black) unequal
pause

*bootstrap the difference in means. we treat our sample as 
*the population, we draw 1000 random samples from it, and 
*we estimate the
*mean and sd for each of these samples
bootstrap r(mu_1) r(mu_2) r(se) ,reps(1000) saving(meandiffs,replace): ttest educ,by(black) unequal
pause

*open the dataset containing the results from each of these 
*1000 random samples
use meandiffs,clear

*rename the generated variables
ren _bs_1 mean_nonblack
ren _bs_2 mean_black
gen mean_diff = mean_nonblack-mean_black
ren _bs_3 se_diff

*create a variable indexing the observation numbers
gen sample_num = _n

*what are the mean and sd of the estimated mean differences?
*how do they relate to the statistics we calculated for the 
*whole population?
sum mean_diff
pause

*estimate the histogram for the estimated mean differences.
*does it look normal?
histogram mean_diff
pause

*calculate 90% and 95% confidence intervals for 
*each of the samples
gen ci95_lb = mean_diff-1.96*se_diff
gen ci95_ub = mean_diff +1.96*se_diff
gen ci90_lb = mean_diff-1.64*se_diff
gen ci90_ub = mean_diff +1.64*se_diff

*generate dummy variables indicating whether the
*true mean lies within the confidence interval
gen inside95 = (ci95_lb<.8436395&ci95_ub>.8436395)
gen inside90 = (ci90_lb<.8436395&ci90_ub>.8436395)

*do 95% of the 95% CIs contain the mean?
tab inside95
pause

*do 90% of the 90% CIs contain the mean?
tab inside90
pause

*visualize the confidence intervals
twoway (rcap ci95_lb ci95_ub sample_num if inside95==1,lcolor(blue)) ///
       (rcap ci95_lb ci95_ub sample_num if inside95==0,lcolor(red)) ///
       (pci .8436395 1 .8436395 1000,lcolor(red)) ///
       ,title("1000 95% confidence intervals") ///
       legend(off) saving(ci95_diffs,replace)
pause

twoway (rcap ci90_lb ci90_ub sample_num if inside90==1,lcolor(blue)) ///
       (rcap ci90_lb ci90_ub sample_num if inside90==0,lcolor(red)) ///
       (pci .8436395 1 .8436395 1000,lcolor(red)) ///
       ,title("1000 90% confidence intervals") ///
       legend(off) saving(ci90_diffs,replace)
pause

graph combine ci95_diffs.gph ci90_diffs.gph,rows(2) cols(1) saving(ci_combined_diffs,replace)
pause

erase means.dta
erase meandiffs.dta

log close

