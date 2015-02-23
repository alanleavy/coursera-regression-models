## Question 1
## Consider the mtcars data set. 
## Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight as confounder. 
## Give the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.
data(mtcars)
mtcarsMod <-mtcars
mtcarsMod$cylFactor <- factor(mtcars$cyl)
fitQ1 <- lm(mpg~cylFactor+wt, data=mtcarsMod)
summary(fitQ1)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  33.9908     1.8878  18.006  < 2e-16 ***
#   cylFactor6   -4.2556     1.3861  -3.070 0.004718 ** 
#   cylFactor8   -6.0709     1.6523  -3.674 0.000999 ***
#   wt           -3.2056     0.7539  -4.252 0.000213 ***
# ANSWER: -6.0709


## Question 2
## Consider the mtcars data set. 
## Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight as a possible confounding variable. 
## Compare the effect of 8 versus 4 cylinders on mpg for the adjusted and unadjusted by weight models. 
## Here, adjusted means including the weight variable as a term in the regression model and unadjusted means the model without weight included. 
## What can be said about the effect comparing 8 and 4 cylinders after looking at models with and without weight included?.
data(mtcars)
mtcarsMod <-mtcars
mtcarsMod$cylFactor <- factor(mtcars$cyl)
fitQ2a <- lm(mpg~cylFactor, data=mtcarsMod)
fitQ2b <- lm(mpg~cylFactor+wt, data=mtcarsMod)
summary(fitQ2a)
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  26.6636     0.9718  27.437  < 2e-16 ***
#  cylFactor6   -6.9208     1.5583  -4.441 0.000119 ***
#  cylFactor8  -11.5636     1.2986  -8.905 8.57e-10 **
summary(fitQ2b)
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  33.9908     1.8878  18.006  < 2e-16 ***
#  cylFactor6   -4.2556     1.3861  -3.070 0.004718 ** 
#  cylFactor8   -6.0709     1.6523  -3.674 0.000999 ***
#  wt           -3.2056     0.7539  -4.252 0.000213 ***
# "Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded."


## Question 3
## Consider the mtcars data set. 
## Fit a model with mpg as the outcome that considers number of cylinders as a factor variable and weight as confounder. 
## Now fit a second model with mpg as the outcome model that considers the interaction between number of cylinders (as a factor variable) and weight. 
## Give the P-value for the likelihood ratio test comparing the two models and suggest a model using 0.05 as a type I error rate significance benchmark.
require(lmtest)
data(mtcars)
mtcarsMod <-mtcars
mtcarsMod$cylFactor <- factor(mtcars$cyl)
fitQ3a <- lm(mpg~cylFactor+wt, data=mtcarsMod)
fitQ3b <- lm(mpg~cylFactor*wt, data=mtcarsMod)
summary(fitQ3a)
summary(fitQ3b)
lrtest(fitQ3a, fitQ3b)
# 0.07649 - >0.5  - reject
# "The P-value is larger than 0.05. 
#  So, according to our criterion, we would fail to reject, which suggests that the interaction terms may not be necessary."


## Question 4
## Consider the mtcars data set. 
## Fit a model with mpg as the outcome that includes number of cylinders as a factor variable 
##  and weight inlcuded in the model as
## lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
## How is the wt coefficient interpretted?
require(lmtest)
data(mtcars)
mtcarsMod <-mtcars
mtcarsMod$cylFactor <- factor(mtcars$cyl)
fitQ4 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(fitQ4)
fitQ4b <- lm(mpg ~ wt  + factor(cyl), data = mtcars)
summary(fitQ4b)
# Holding the number of cylinders fixed, an increase of 1000lbs in a car's weight will reduce mpg by 0.5 * 6.411 = 3.2055
# "The estimated expected change in MPG per half ton increase in weight for for a specific number of cylinders (4, 6, 8)." XXX
# "The estimated expected change in MPG per one ton increase in weight for a specific number of cylinders (4, 6, 8)"

 
## Question 5
## Consider the following data set
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
## Give the hat diagonal for the most influential point
fitQ6 <- lm(y~x)
influence.measures(fitQ6)
# 0.995 ??

## Question 6
## Consider the following data set
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
## Give the slope dfbeta for the point with the highest hat value.
fitQ6 <- lm(y~x)
plot(fitQ6)
influence.measures(fitQ6)
0.2043/-1.34e+02
# = -0.001524627 ??
1.344/11.72
# = 0.1146758 ??
# dfb.x = -1.34e+02 = 134

## Question 7
## Consider a regression relationship between Y and X with and without adjustment for a third variable Z. 
## Which of the following is true about comparing the regression coefficient between Y and X 
##  with and without adjustment for Z.

# "It is possible for the coefficient to reverse sign after adjustment. 
#   For example, it can be strongly significant and positive before adjustment and strongly significant and negative after adjustment."

