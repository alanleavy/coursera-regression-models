



## Question 1
## Give the value of μ that minimizes the least squares equation 
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
mu <- sum(x*w)/sum(w)
mu



## Question 2
## Fit the regression through the origin and get the slope treating y as the outcome and x as the regressor.
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
reg <- lm(y~x -1)
reg

## Question 2
## Do data(mtcars) from the datasets package and fit the regression model with mpg as the outcome and weight as the predictor. 
## Give the slope coefficient.
data(mtcars)
reg <- lm(mpg ~ wt, mtcars)
reg


## Question 4
## Consider data with an outcome (Y) and a predictor (X). The standard deviation of the predictor is one half that of the outcome. 
## The correlation between the two variables is .5. 
## What value would the slope coefficient for the regression model with Y as the outcome and X as the predictor?

## .5 = cov(X,Y)/SD(X)*SD(Y)
## slope =  .5 * SD(Y) / SD(X) =.5 *2 = 1



## Question 5
## Students were given two hard tests and scores were normalized to have empirical mean 0 and variance 1. 
## The correlation between the scores on the two tests was 0.4. 
## What would be the expected score on Quiz 2 for a student who had a normalized score of 1.5 on Quiz 1?

# E(Q2) = 0.4*Q1
0.4*1.5 = 0.6
## what about other way around ????


## Question 6
## Consider the data given by the following
## x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
## What is the value of the first measurement if x were normalized (to have mean 0 and variance 1)?
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
mu <- mean(x)
SDx <- sd(x)
centred <- x-mu
normalised <- centred / SDx
normalised

## Question 7
## Consider the following data set (used above as well). What is the intercept for fitting the model with x as the predictor and y as the outcome?
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
reg <- lm(y~x)
reg


## Question 9
## Consider the data given by
## x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
## What value minimizes the sum of the squared distances between these points and itself?

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)


## Question 10
## Let the slope having fit Y as the outcome and X as the predictor be denoted as β1. 
## Let the slope from fitting X as the outcome and Y as the predictor be denoted as γ1. 
## Suppose that you divide β1 by γ1; in other words consider β1/γ1. 
## What is this ratio always equal to?

# B1/γ1 == cor(X,Y) * sd(Y)/sd(X)  / cor(X,Y) *sd(X)/sd(Y)
# B1/γ1 == sd(Y)/sd(X)  /sd(X)/sd(Y)  == sd(Y)*sd(Y)/sd(X)*sd(X) == Var(Y)/Var(X)

##############
## Experimentation
X <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
Y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

# Normalise x
muX <- mean(X)
sdX <- sd(X)
centredX <- X-muX
normX<- centredX / sdX

# Normalise y
muY <- mean(Y)
sdY <- sd(Y)
centredY <- Y-muY
normY<- centredY / sdY

regXtoY<-lm(Y~X)
regYtoX<-lm(X~Y)

df <- data.frame(X,Y,normX,normY)

pXtoY <- ggplot(data=df)
pXtoY <-pXtoY + geom_point(aes(x=X,y=Y))
pXtoY <- pXtoY+ geom_abline(intercept = 1.567461, slope = -1.712846)
pXtoY <- pXtoY+ geom_abline(intercept = 13.42890043, slope = -22.41343874)
print(pXtoY)

pYtoX <- ggplot(data=df)
pYtoX <- pYtoX + geom_point(aes(x=Y,y=X))
pYtoX <- pYtoX+ geom_abline(intercept = 0.59914503, slope = -0.04461609 )
pYtoX <- pYtoX+ geom_abline(intercept = 0.915120799, slope = -0.583823648)
print(pYtoX)
