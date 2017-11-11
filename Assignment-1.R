install.packages("data.table")
library(data.table)
context1    <- fread('WAGE1.csv')
summary(context1)
#      wage             educ           exper           tenure          nonwhite          female          married           numdep     
#Min.   : 0.530   Min.   : 0.00   Min.   : 1.00   Min.   : 0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000  
#1st Qu.: 3.300   1st Qu.:12.00   1st Qu.: 5.00   1st Qu.: 0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000  
#Median : 4.700   Median :12.00   Median :13.50   Median : 2.000   Median :0.0000   Median :0.0000   Median :1.0000   Median :1.000  
#Mean   : 5.909   Mean   :12.56   Mean   :17.02   Mean   : 5.105   Mean   :0.1027   Mean   :0.4791   Mean   :0.6084   Mean   :1.044  
#3rd Qu.: 6.900   3rd Qu.:14.00   3rd Qu.:26.00   3rd Qu.: 7.000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:2.000  
#Max.   :25.000   Max.   :18.00   Max.   :51.00   Max.   :44.000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :6.000  
#smsa           northcen         south             west           construc          ndurman          trcommpu      
#Min.   :0.0000   Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.00000  
#1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.00000  
#Median :1.0000   Median :0.000   Median :0.0000   Median :0.0000   Median :0.00000   Median :0.0000   Median :0.00000  
#Mean   :0.7224   Mean   :0.251   Mean   :0.3555   Mean   :0.1692   Mean   :0.04563   Mean   :0.1141   Mean   :0.04373  
#3rd Qu.:1.0000   3rd Qu.:0.750   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:0.00000  
#Max.   :1.0000   Max.   :1.000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.00000  
#trade           services         profserv         profocc          clerocc          servocc      
#Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
#Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
#Mean   :0.2871   Mean   :0.1008   Mean   :0.2586   Mean   :0.3669   Mean   :0.1673   Mean   :0.1407  
#3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
#Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  

## Generate new variables
lwage  <- log(context1$wage)

## Run Models
model1      <- lm(wage~educ, data=context1)
model2      <- lm(wage~educ+exper+tenure, data=context1)
model3      <- lm(lwage~educ+exper+tenure, data=context1)

## Summarize
summary(model1)
#Call:
#  lm(formula = wage ~ educ, data = context1)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-5.3707 -2.1578 -0.9854  1.1864 16.3975 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.93389    0.68769  -1.358    0.175    
#educ         0.54470    0.05346  10.189   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 3.392 on 524 degrees of freedom
#Multiple R-squared:  0.1654,	Adjusted R-squared:  0.1638 
#F-statistic: 103.8 on 1 and 524 DF,  p-value: < 2.2e-16

summary(model2)
#Call:
#  lm(formula = wage ~ educ + exper + tenure, data = context1)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.6498 -1.7708 -0.6407  1.2051 14.7201 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -2.91354    0.73172  -3.982 7.81e-05 ***
#  educ         0.60268    0.05148  11.708  < 2e-16 ***
#  exper        0.02252    0.01210   1.861   0.0633 .  
#tenure       0.17002    0.02173   7.825 2.83e-14 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 3.096 on 522 degrees of freedom
#Multiple R-squared:  0.3072,	Adjusted R-squared:  0.3032 
#F-statistic: 77.15 on 3 and 522 DF,  p-value: < 2.2e-16

summary(model3)
#Call:
#  lm(formula = lwage ~ educ + exper + tenure, data = context1)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.05911 -0.29563 -0.03302  0.28590  1.42657 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.282635   0.104331   2.709  0.00697 ** 
#  educ        0.092256   0.007340  12.569  < 2e-16 ***
#  exper       0.004137   0.001726   2.397  0.01687 *  
#  tenure      0.022112   0.003098   7.138 3.19e-12 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.4415 on 522 degrees of freedom
#Multiple R-squared:  0.3165,	Adjusted R-squared:  0.3125 
#F-statistic: 80.56 on 3 and 522 DF,  p-value: < 2.2e-16

## Interpretations
#a> model1 predicts there is a 0.54470 percentage increase in hourly wage when associating it with education
#b> model2 predicts there is a 0.60268 percentage increase in hourly wage controlling for experience and tenure
#c> model2 predicts there is a 0.02252 percentage point increase in hourly wage controlling for education and tenure
#d> model2 predicts there is a 0.17002 percentage point increase in hourly wage controlling for education and experience
#e> model2 predicts there is a 2.91354 percentage point decrease in hourly wage when a person has no education, no experience and no tenure.
#f> model3 predicts there is a 0.092256 percentage increase in hourly wage controlling for experience and tenure
#g> model3 predicts there is a 0.004137 percentage point increase in hourly wage controlling for education and tenure
#h> model3 predicts there is a 0.17002 percentage point increase in hourly wage controlling for education and experience
