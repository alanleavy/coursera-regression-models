## Context
## You work for Motor Trend, a magazine about the automobile industry. 
## Looking at a data set of a collection of cars, they are interested in exploring the relationship
##  between a set of variables and miles per gallon (MPG) (outcome). 
## They are particularly interested in the following two questions:
## - “Is an automatic or manual transmission better for MPG”
## - "Quantify the MPG difference between automatic and manual transmissions"


## Question
## Take the mtcars data set and write up an analysis to answer their question
##  using regression models and exploratory data analyses.
## Your report must be:
## - Written as a PDF printout of a compiled (using knitr) R markdown document.
## - Brief. Roughly the equivalent of 2 pages or less for the main text. 
##   Supporting figures in an appendix can be included up to 5 total pages including the 2 for the main report. 
##   The appendix can only include figures.
## - Include a first paragraph executive summary.
## - Upload your PDF by clicking the Upload button below the text box.


require(car)
require(lmtest)

## Data Preparation
## - factorise discrete predectors
## - centre continuous ones, to make the derrived models more interpretable 
data(mtcars) 
mtcars1<-mtcars
mtcars1$vs<-factor(mtcars$vs)
mtcars1$am<-factor(mtcars$am)
mtcars1$disp<-mtcars$disp-mean(mtcars$disp)
mtcars1$hp<-mtcars$hp-mean(mtcars$hp)
mtcars1$drat<-mtcars$drat-mean(mtcars$drat)
mtcars1$wt<-mtcars$wt-mean(mtcars$wt)
mtcars1$qsec<-mtcars$qsec-mean(mtcars$qsec)
summary(mtcars1)

## Exploratory analysis

## Select and evaluate Models
fit1 <- lm(mpg ~ am + wt, data=mtcars1)
fit2 <- lm(mpg ~ am + wt + qsec, data=mtcars1)
fit3 <- lm(mpg ~ am + wt + hp, data=mtcars1)
fit4 <- lm(mpg ~ am + wt + qsec+hp, data=mtcars1)

fit5<-lm(mpg~wt*am+qsec,data=mtcars1)
bestfit<-lm(mpg~wt+am:wt+qsec,data=mtcars1)


summary(fit1)$coefficients[,4]; summary(fit2)$coefficients[,4]; summary(fit3)$coefficients[,4]; summary(fit4)$coefficients[,4]
c(summary(fit1)$adj.r.squared,summary(fit2)$adj.r.squared,summary(fit3)$adj.r.squared,summary(fit4)$adj.r.squared)
vif(fit1);vif(fit2);vif(fit3);vif(fit4);vif(bestfit)


bestfit<-lm(mpg~wt+am:wt+qsec,data=mtcars1)
 
par(mfrow=c(2,2))
boxplot(mtcars$wt,mtcars$am)



par(mfrow=c(2,2))
plot(predict(fit2), resid(fit2))
plot(predict(fit3), resid(fit3))
plot(predict(fit4), resid(fit4))
plot(predict(fit5), resid(fit5))

par(mfrow=c(1,1))

expInt <- qt(0.975, 28)
error95 <- c(fit2.am1=qt(0.975, 28)*summary(fit2)$coefficients[2,2], bestfit.wtam1=qt(0.975, 28)*summary(bestfit)$coefficients[4,2])


cor(mtcars)

fit
