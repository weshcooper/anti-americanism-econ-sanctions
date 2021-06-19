install.packages("lessR")
library("lessR")

##############-H1-Conducting logistic regression with success as dependent variable. This performs listwise deletion to get rid of NAs
Logit(success ~ AntiAmericanismScore, data = AAES)

#Box-Tidwell Test (Checks that association between predictor and logit transformation of the outcome is linear)
Logit(success ~ AntiAmericanismScore + AntiAmericanismScore:log(AntiAmericanismScore), data = AAES, brief = TRUE)
#Box-Tidwell test showed that assumption is untrue.
#In other words, association between predictor and logit transformation of the outcome is nonlinear

#Polynomial Logistic Regression
#with squared predictor variable
Logit(success ~ AntiAmericanismScore + I(AntiAmericanismScore^2), data = AAES)

#####Outlier Analysis
#Possible outliers in data frame according to cooks include:
#                 AntiAmericanismScore success fitted residual rstudent  dffits    cooks
#208 (caseid = 1999101601)  83.15       1   0.1346   0.8654   2.0734  0.3783 0.150613
#210 (caseid = 1998051002)  83.15       1   0.1346   0.8654   2.0734  0.3783 0.150613
#135 (caseid = 1995033001)  81.73       1   0.1411   0.8589   2.0454  0.3693 0.139238

#Logistic regression without outliers
Logit(success ~ AntiAmericanismScore, data = AAES, rows = (!caseid %in% c("1999101601", "1998051002", "1995033001")))

#Box-Tidwell Test without outliers (Checks that association between predictor and logit transformation of the outcome is linear)
Logit(success ~ AntiAmericanismScore + AntiAmericanismScore:log(AntiAmericanismScore), data = AAES, rows = (!caseid %in% c("1999101601", "1998051002", "1995033001")))
#Box-Tidwell Test without outliers has p-value equal to or greater than .05
#In other words, association between predictor and logit transformation of the outcome is linear






##############-H2-Conducting logistic regression with imposition as dependent variable. This performs listwise deletion to get rid of NAs
Logit(imposition ~ AntiAmericanismScore, data = AAES)
#not statistically significant

##############-H3-Conducting logistic regression with threat as dependent variable. This performs listwise deletion to get rid of NAs
Logit(threat ~ AntiAmericanismScore, data = AAES)
#not statistically significant

##############-H4-Conducting linear regression with lengthofcase as dependent variable.
H4 <- lm(formula = lengthofcase ~ AntiAmericanismScore, data = AAES)
summary(H4)

##############-H5-Conducting linear regression with success as independent variable and AntiAmericanismScore as dependent variable.
H5 <- lm(formula = AntiAmericanismScore ~ success, data = AAES)
summary(H5)

##############-H6-Conducting linear regression with imposition as independent variable and AntiAmericanismScore as dependent variable.
H6 <- lm(formula = AntiAmericanismScore ~ imposition, data = AAES)
summary(H6)

##############-H7-Conducting linear regression with threat as independent variable and AntiAmericanismScore as dependent variable.
H7 <- lm(formula = AntiAmericanismScore ~ threat, data = AAES)
summary(H7)

##############-H8-Conducting linear regression with lengthofcase as independent variable and AntiAmericanismScore as dependent variable.
H8 <- lm(formula = AntiAmericanismScore ~ lengthofcase, data = AAES)
summary(H8)

##############-H9-Conducting linear regression with AntiAmericanismScore as dependent variable.
H9 <- lm(formula = AntiAmericanismScore ~ success + imposition + threat + lengthofcase, data = AAES)
summary(H9)


