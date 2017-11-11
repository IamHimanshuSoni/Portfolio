rm(list=ls(all=TRUE))
library(data.table)

##question 1

context1    <- fread('attend.csv')

# Contains data from C:\Users\Jason\Desktop\data\attend.dta
# obs:           680                          
# vars:            11                          29 Jul 2005 21:00
# size:        17,680                          (_dta has notes)
# ---------------------------------------------------------------------------------------------------
#   storage   display    value
# variable name   type    format     label      variable label
# ---------------------------------------------------------------------------------------------------
#   attend          byte    %8.0g                 classes attended out of 32
# termGPA         float   %9.0g                 GPA for term
# priGPA          float   %9.0g                 cumulative GPA prior to term
# ACT             byte    %8.0g                 ACT score
# final           byte    %8.0g                 final exam score
# frosh           byte    %8.0g                 =1 if freshman
# soph            byte    %8.0g                 =1 if sophomore
# hw              byte    %8.0g                 number of homeworks turned in out of 8
# ---------------------------------------------------------------------------------------------------



summary(context1)

# attend         termGPA          priGPA           ACT            final           frosh             soph              hw       
# Min.   : 2.00   Min.   :0.000   Min.   :0.857   Min.   :13.00   Min.   :10.00   Min.   :0.0000   Min.   :0.0000   Min.   :0.000  
# 1st Qu.:24.00   1st Qu.:2.138   1st Qu.:2.190   1st Qu.:20.00   1st Qu.:22.00   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:7.000  
# Median :28.00   Median :2.670   Median :2.560   Median :22.00   Median :26.00   Median :0.0000   Median :1.0000   Median :8.000  
# Mean   :26.15   Mean   :2.601   Mean   :2.587   Mean   :22.51   Mean   :25.89   Mean   :0.2324   Mean   :0.5765   Mean   :6.971  
# 3rd Qu.:30.00   3rd Qu.:3.120   3rd Qu.:2.942   3rd Qu.:25.00   3rd Qu.:29.00   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:8.000  
# Max.   :32.00   Max.   :4.000   Max.   :3.930   Max.   :32.00   Max.   :39.00   Max.   :1.0000   Max.   :1.0000   Max.   :8.000  

## Generate new variables: attendrt for attendence rate and hwrt for homewwork rate

attendrt    <- (context1$attend)/32
hwrt        <- (context1$hw)/8

## Run Models
model1      <- lm(termGPA~priGPA+ACT+attendrt+hwrt, data = context1)

## Summarize
  summary(model1)

  # Call:
#   lm(formula = termGPA ~ priGPA + ACT + attendrt + hwrt, data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.87210 -0.28100  0.00001  0.30164  1.49711 
# 
# Coefficients:
#                Estimate    Std. Error  t value    Pr(>|t|)    
# (Intercept)    -1.286983  0.164169   -7.839 1.77e-14 ***
#   priGPA       0.548962   0.042418   12.942  < 2e-16 ***
#   ACT          0.036099   0.006051   5.966 3.92e-09 ***
#   attendrt     1.052246   0.155436   6.770 2.81e-11 ***
#   hwrt         0.913031   0.116932   7.808 2.22e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4788 on 675 degrees of freedom
# Multiple R-squared:   0.58,	Adjusted R-squared:  0.5775 
# F-statistic:   233 on 4 and 675 DF,  p-value: < 2.2e-16
  
##Interpretations
  # 1a> A unit incease in attendence rate is associated with a 1.052246 unit increase in termGPA
  # 1b> A unit incease in homework rate is associated with a 0.913031 unit increase in termGPA
  # 1c> Substituting coefficient and intercept values in linear model we get the following equation:
  #   termGPAi = -1.286983 + 0.548962*(priGPA) + 0.036099*(ACT) + 1.052246*(attendrt) + 0.913031*(hwrt) + 0.4788
  #   Now substituting the values for above variables that are given in the problem set we get the value for termGPA is
  #   = -1.286983 + 0.548962*(2.2) + 0.036099*(32) + 1.052246*(28/32) + 0.913031*(8/8)  
  #   = 2.909648
  # 1d> Similarly as above predicting termGPA when values of attendence and homework same as previous values but substituting ACT=20
  #     and priGPA=3.9 we get termGPA is  
  #     = -1.286983 + 0.548962*(3.9) + 0.036099*(20) + 1.052246*(28/32) + 0.913031*(8/8)  
  #     = 3.409695
  # 1e> Infering from the above two predictions(1c and 1d), priGPA is more important than ACT as increasing the priGPA has resulted in
  #     an increased termGPA even when the ACT value was decreased in the last prediction.
  # 1f> Again predicting termGPA when values of preGPA, ACT, attendence and homework rate as per given in the problem set2, termGPA is  
  #     = -1.286983 + 0.548962*(3.0) + 0.036099*(25) + 1.052246*(32/32) + 0.913031*(4/8)  
  #     = 2.77114
  # 1g> Again predicting termGPA when values of preGPA, ACT, attendence and homework rate as per given in the problem set2, termGPA is
  #     = -1.286983 + 0.548962*(3.0) + 0.036099*(25) + 1.052246*(16/32) + 0.913031*(8/8)  
  #     = 2.701532
  # 1h> Infering from the above two predictions, attendence rate is more important than homework rate as 
  #     decreasing the attendence has resulted in an decreased termGPA even when the homework value was increased in the last prediction.
  # 1i> It is easier to compare hwrt and attendrt. From the above two comparisions (1f and 1g), we can see that in 1f hwrt is equals to 0.5 while
  #     attendrt is equals to 1 and in  1g hwrt is 1 and attendrt is equlas to 0.5. It is easier to compare the effect of one factor alternately
  #     keeping the same value for another factor as we did i the above two predictions.
  
  
##question 2
  
  rm(list=ls(all=TRUE))
  
  context2    <- fread('CEOSAL2.csv')
  
  summary(context2)
  
  # salary            age           college            grad            comten         ceoten           sales          profits      
  # Min.   : 100.0   Min.   :33.00   Min.   :0.0000   Min.   :0.0000   Min.   : 2.0   Min.   : 0.000   Min.   :   29   Min.   :-463.0  
  # 1st Qu.: 471.0   1st Qu.:52.00   1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:12.0   1st Qu.: 3.000   1st Qu.:  561   1st Qu.:  34.0  
  # Median : 707.0   Median :57.00   Median :1.0000   Median :1.0000   Median :23.0   Median : 6.000   Median : 1400   Median :  63.0  
  # Mean   : 865.9   Mean   :56.43   Mean   :0.9718   Mean   :0.5311   Mean   :22.5   Mean   : 7.955   Mean   : 3529   Mean   : 207.8  
  # 3rd Qu.:1119.0   3rd Qu.:62.00   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:33.0   3rd Qu.:11.000   3rd Qu.: 3500   3rd Qu.: 208.0  
  # Max.   :5299.0   Max.   :86.00   Max.   :1.0000   Max.   :1.0000   Max.   :58.0   Max.   :37.000   Max.   :51300   Max.   :2700.0  
  # mktval     
  # Min.   :  387  
  # 1st Qu.:  644  
  # Median : 1200  
  # Mean   : 3600  
  # 3rd Qu.: 3500  
  # Max.   :45400 
  
  
  # Contains data from C:\Users\Jason\Desktop\data\CEOSAL2.DTA
  # obs:           177                          
  # vars:            15                          17 Aug 1999 23:14
  # size:         6,549                          
  # ---------------------------------------------------------------------------------------------------
  #   storage   display    value
  # variable name   type    format     label      variable label
  # ---------------------------------------------------------------------------------------------------
  #   salary          int     %9.0g                 1990 compensation, $1000s
  # age             byte    %9.0g                 in years
  # college         byte    %9.0g                 =1 if attended college
  # grad            byte    %9.0g                 =1 if attended graduate school
  # comten          byte    %9.0g                 years with company
  # ceoten          byte    %9.0g                 years as ceo with company
  # sales           float   %9.0g                 1990 firm sales, millions
  # profits         int     %9.0g                 1990 profits, millions
  # mktval          float   %9.0g                 market value, end 1990, mills.
  # ---------------------------------------------------------------------------------------------------
  
  ## Run Models

  model2    <- lm(log(salary)~log(mktval)+profits+ceoten, data = context2)
  model3    <- lm(log(salary)~log(mktval)+profits+ceoten+log(sales), data = context2)
  
  ## Summarize
  
  summary(model2)
  
  # Call:
  #   lm(formula = log(salary) ~ log(mktval) + profits + ceoten, data = context2)
  # 
  # Residuals:
  #   Min       1Q     Median       3Q      Max 
  # -2.63382 -0.34660  0.00627  0.35059  1.96220 
  # 
  # Coefficients:
  #                 Estimate  Std. Error  t value  Pr(>|t|)    
  #   (Intercept)  4.7095052  0.3954502   11.909   < 2e-16 ***
  #   log(mktval)  0.2386220  0.0559166   4.267    3.25e-05 ***
  #   profits      0.0000793  0.0001566   0.506    0.6132    
  #   ceoten       0.0114646  0.0055816   2.054    0.0415 *  
  #   ---
  #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  # 
  # Residual standard error: 0.5289 on 173 degrees of freedom
  # Multiple R-squared:  0.2514,	Adjusted R-squared:  0.2384 
  # F-statistic: 19.36 on 3 and 173 DF,  p-value: 7.141e-11
  
  summary(model3)
  
  # Call:
  #   lm(formula = log(salary) ~ log(mktval) + profits + ceoten + log(sales), 
  #      data = context2)
  # 
  # Residuals:
  #   Min       1Q     Median       3Q      Max 
  # -2.48792 -0.29369  0.00827  0.29951  1.85524 
  # 
  # Coefficients:
  #               Estimate   Std.Error  t value    Pr(>|t|)    
  # (Intercept)   4.558e+00  3.803e-01   11.986    < 2e-16 ***
  # log(mktval)   1.018e-01  6.303e-02   1.614     0.1083    
  # profits       2.905e-05  1.503e-04   0.193     0.8470    
  # ceoten        1.168e-02  5.342e-03   2.187     0.0301 *  
  # log(sales)    1.622e-01  3.948e-02   4.109     6.14e-05 ***
  #   ---
  #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  # 
  # Residual standard error: 0.5062 on 172 degrees of freedom
  # Multiple R-squared:  0.3183,	Adjusted R-squared:  0.3024 
  # F-statistic: 20.08 on 4 and 172 DF,  p-value: 1.387e-13
  
  
##Interpretations from model2 and model3 are following:
  # 1j> We didn't take log on the profit as we can see from summary of context2, there are negative values for profit in the data set 
  #     whichis not the case with other dollar valued quantities and log of a negative number is undefined. Therefore, we didn't take log on profit. 
  # 1k> 0.2386220 percent increase in salary is associate with 1 percent increase in market value.
  # 1l> 0.1018 percent increase in salary is associate with 1 percent increase in market value.
  # 1m> As we have not considered sales factor in model2 but as we can infer from model2  that sales is an important factor, model2 can be said to be 
  #     biased and is a case of endogeneity.
  # 1n> Using the summary of model3 as we can infer from the signif. codes that coefficient of profite variable is not significant.
  # 1o> 0.1622 percent increase in salary is associated with 1 percent increase in sales
  

##question 3
  
  rm(list=ls(all=TRUE))
  
  context3    <- fread('hprice1.csv')
  
  summary(context3)
  
  # price           assess          bdrms          lotsize          sqrft         colonial     
  # Min.   :111.0   Min.   :198.7   Min.   :2.000   Min.   : 1000   Min.   :1171   Min.   :0.0000  
  # 1st Qu.:230.0   1st Qu.:253.9   1st Qu.:3.000   1st Qu.: 5733   1st Qu.:1660   1st Qu.:0.0000  
  # Median :265.5   Median :290.2   Median :3.000   Median : 6430   Median :1845   Median :1.0000  
  # Mean   :293.5   Mean   :315.7   Mean   :3.568   Mean   : 9020   Mean   :2014   Mean   :0.6932  
  # 3rd Qu.:326.2   3rd Qu.:352.1   3rd Qu.:4.000   3rd Qu.: 8583   3rd Qu.:2227   3rd Qu.:1.0000  
  # Max.   :725.0   Max.   :708.6   Max.   :7.000   Max.   :92681   Max.   :3880   Max.   :1.0000  
  
  ## Run Models
  
  model4    <- lm(price~bdrms+log(lotsize)+log(sqrft)+colonial, data = context3)
  model5    <- lm(log(price)~bdrms+log(lotsize)+log(sqrft)+colonial, data = context3)
  
  ## Summarize
  
  summary(model4)
  
  # Call:
  #   lm(formula = price ~ bdrms + log(lotsize) + log(sqrft) + colonial, 
  #      data = context3)
  # 
  # Residuals:
  #   Min       1Q       Median   3Q      Max 
  # -109.603  -38.258   -4.325   22.984  220.766 
  # 
  # Coefficients:
  #                 Estimate Std. Error      t value   Pr(>|t|)    
  # (Intercept)    -2030.455     210.967     -9.625     3.68e-15 ***
  #   bdrms           18.572       9.308      1.995     0.0493 *  
  #   log(lotsize)    61.446      12.372      4.966     3.60e-06 ***
  #   log(sqrft)     225.508      30.072      7.499     6.41e-11 ***
  #   colonial         4.134      14.509      0.285     0.7764    
  # ---
  #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  # 
  # Residual standard error: 59.66 on 83 degrees of freedom
  # Multiple R-squared:  0.6781,	Adjusted R-squared:  0.6626 
  # F-statistic: 43.71 on 4 and 83 DF,  p-value: < 2.2e-16
  
  summary(model5)
  
  # Call:
  #   lm(formula = log(price) ~ bdrms + log(lotsize) + log(sqrft) + 
  #        colonial, data = context3)
  # 
  # Residuals:
  #   Min       1Q   Median       3Q      Max 
  # -0.69479 -0.09750 -0.01619  0.09151  0.70228 
  # 
  # Coefficients:
  #                Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)    -1.34959    0.65104  -2.073   0.0413 *  
  #   bdrms         0.02683    0.02872   0.934   0.3530    
  #   log(lotsize)  0.16782    0.03818   4.395 3.25e-05 ***
  #   log(sqrft)    0.70719    0.09280   7.620 3.69e-11 ***
  #   colonial      0.05380    0.04477   1.202   0.2330    
  # ---
  #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  # 
  # Residual standard error: 0.1841 on 83 degrees of freedom
  # Multiple R-squared:  0.6491,	Adjusted R-squared:  0.6322 
  # F-statistic: 38.38 on 4 and 83 DF,  p-value: < 2.2e-16
  
  ##Interpretations for model4 and model5:
  
  # 1p> 1 percent increase in lotsize is associated with $614.46 increase in price of U.S. houses as per model4.
  # 1q> 1 percent increase in lotsize is associated with 0.16782 percent increase in price of U.S. houses as per model4.
  # 1r> If home type is colonial, then the home will cost $4134 more than the other the types of homes.
  # 1s> Comparing the values of Multiple R-squared, as we have a greater value of it for model4 wecan infer that model4 is better for our data set. 
  # 1t> Using model4 
  #     for our data set, value gained increase in 10% sqft is
  #     = (225.508)*(1000)*(10)/(100)= $22550.8
  #     value gained from from enjoyment as given = $20,000
  #     value gain from increasing when a bedroom is added = (18.572)*1000 = $18572
  #     Total value gained 
  #     = (22550.8+20000+18572) = $61122.8
  #     Whereas total expense given is $50000
  #     taking difference of we get a profit = (61122.8-50000) = $11122.8
  #     We see that we are gaining a worth of $11122.8 on expansion, so we should pursue the expansion.

  
  ##question 4
  
  rm(list=ls(all=TRUE))
  
  # Contains data from C:\Users\Jason\Desktop\data\JTRAIN2.DTA
  # obs:           445                          
  # vars:            19                          17 Jan 2000 17:20
  # size:        16,910                          
  # ---------------------------------------------------------------------------------------------------
  #   storage   display    value
  # variable name   type    format     label      variable label
  # ---------------------------------------------------------------------------------------------------
  #   train           byte    %9.0g                 =1 if assigned to job training
  # age             byte    %9.0g                 age in 1977
  # educ            byte    %9.0g                 years of education
  # black           byte    %9.0g                 =1 if black
  # hisp            byte    %9.0g                 =1 if Hispanic
  # married         byte    %9.0g                 =1 if married
  # nodegree        byte    %9.0g                 =1 if no high school degree
  # mosinex         byte    %9.0g                 # mnths prior to 1/78 in expmnt
  # re74            float   %9.0g                 real earns., 1974, $1000s
  # re75            float   %9.0g                 real earns., 1975, $1000s
  # re78            float   %9.0g                 real earns., 1978, $1000s
  # unem74          byte    %9.0g                 =1 if unem. all of 1974
  # unem75          byte    %9.0g                 =1 if unem. all of 1975
  # unem78          byte    %9.0g                 =1 if unem. all of 1978
  # ---------------------------------------------------------------------------------------------------
  
 
  context4    <- fread('JTRAIN2.csv')
  
  summary(context4)
  
  # train             age             educ          black             hisp            married          nodegree        mosinex     
  # Min.   :0.0000   Min.   :17.00   Min.   : 3.0   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.000   Min.   : 5.00  
  # 1st Qu.:0.0000   1st Qu.:20.00   1st Qu.: 9.0   1st Qu.:1.0000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:1.000   1st Qu.:14.00  
  # Median :0.0000   Median :24.00   Median :10.0   Median :1.0000   Median :0.00000   Median :0.0000   Median :1.000   Median :21.00  
  # Mean   :0.4157   Mean   :25.37   Mean   :10.2   Mean   :0.8337   Mean   :0.08764   Mean   :0.1685   Mean   :0.782   Mean   :18.12  
  # 3rd Qu.:1.0000   3rd Qu.:28.00   3rd Qu.:11.0   3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:1.000   3rd Qu.:23.00  
  # Max.   :1.0000   Max.   :55.00   Max.   :16.0   Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.000   Max.   :24.00  
  # re74              re75             re78            unem74           unem75           unem78      
  # Min.   : 0.0000   Min.   : 0.000   Min.   : 0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
  # 1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
  # Median : 0.0000   Median : 0.000   Median : 3.702   Median :1.0000   Median :1.0000   Median :0.0000  
  # Mean   : 2.1023   Mean   : 1.377   Mean   : 5.301   Mean   :0.7326   Mean   :0.6494   Mean   :0.3079  
  # 3rd Qu.: 0.8244   3rd Qu.: 1.221   3rd Qu.: 8.125   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
  # Max.   :39.5707   Max.   :25.142   Max.   :60.308   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
  
  ## Run Models
  
  model6    <- lm(re78~re75+train+educ+black, data = context4)

  
  ## Summarize
  
  summary(model6)
  
  # Call:
  #   lm(formula = re78 ~ re75 + train + educ + black, data = context4)
  # 
  # Residuals:
  #   Min     1Q Median     3Q    Max 
  # -9.120 -4.377 -1.756  3.353 54.058 
  # 
  # Coefficients:
  #              Estimate Std. Error t value Pr(>|t|)   
  # (Intercept)  1.97686    1.89028   1.046   0.2962   
  # re75         0.14697    0.09811   1.498   0.1349   
  # train        1.68422    0.62700   2.686   0.0075 **
  # educ         0.41026    0.17267   2.376   0.0179 * 
  # black       -2.11277    0.82941  -2.547   0.0112 * 
  #   ---
  #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  # 
  # Residual standard error: 6.496 on 440 degrees of freedom
  # Multiple R-squared:  0.04917,	Adjusted R-squared:  0.04053 
  # F-statistic: 5.688 on 4 and 440 DF,  p-value: 0.00018
  
  ##Interpretations for model6
  # 1u> Every $1000 increase in re75 is associated with $146.97 increase in real earninins in 1978
  # 1v> Every single assignment to job training is associated with $1684.22  increase in real earninins in 1978. On the basis of
  # summary values from signif. codes we can infer that this coefficient is significant.
  # 1w> If a person is black, the earning was decreased by $2112.77 for the period of 1976-1977  