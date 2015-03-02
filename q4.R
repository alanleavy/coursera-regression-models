require(MASS)

# Question 1
# Consider the space shuttle data ?shuttle in the MASS library. 
# Consider modeling the use of the autolander as the outcome (variable name use). 
# Fit a logistic regression model with autolander (variable auto)
#   use (labeled as "auto" 1) versus not (0) as predicted by wind sign (variable wind). 
# Give the estimated odds ratio for autolander use comparing head winds, labeled as "head"
#   in the variable headwind (numerator) to tail winds (denominator).
# 
data(shuttle)
sh1<-shuttle
sh1$use <- as.numeric(sh1$use == "auto")
fitQ1a <- glm(use ~ wind , data=sh1, family = "binomial")
exp(fitQ1a$coeff)
# (Intercept)    windtail 
# 1.285714    1.032323 

fitQ1b <- glm(use ~ wind -1, data=sh1, family = "binomial")
exp(fitQ1b$coeff)
# 
# windhead windtail 
# 1.285714 1.327273 

# 1.285714 / 1.327273 0.9686884 !!!!

# 0.031
# 1.327   
# 0.969  1st choice
# -0.031






# Question 2
# Consider the previous problem. 
# Give the estimated odds ratio for autolander use comparing head winds (numerator) to tail winds 
# (denominator) adjusting for wind strength from the variable magn.


fitQ2a <- glm(use ~ wind + magn, data=sh1, family = "binomial")
exp(fitQ2a$coeff)
# (Intercept)    windtail  magnMedium     magnOut  magnStrong 
# 1.4383682   1.0325265   1.0000000   0.6841941   0.9376181 


fitQ2b <- glm(use ~ wind + magn-1, data=sh1, family = "binomial")
exp(fitQ2b$coeff)

# 
# windhead   windtail magnMedium    magnOut magnStrong 
# 1.4383682  1.4851533  1.0000000  0.6841941  0.9376181 
# 1.4383682 /1.4851533 = 0.9684981 !!!!
# 1.485
# 1.00
# 0.684
# 0.969 1st



# Question 3
# If you fit a logistic regression model to a binary variable, for example use of the autolander, 
#    then fit a logistic regression model for one minus the outcome (not using the autolander) 
#    what happens to the coefficients?
# 
sh1<-shuttle
sh1$use <- as.numeric(sh1$use == "auto")
sh1$useInv <-(sh1$use +1)%%2
fitQ3a <- glm(use ~ wind, data=sh1, family = "binomial")
fitQ3b <- glm(useInv ~ wind, data=sh1, family = "binomial")

exp(fitQ3a$coeff)
fitQ3a$coeff
exp(fitQ3b$coeff)
fitQ3b$coeff



# The coefficients change in a non-linear fashion.
# The intercept changes sign, but the other coefficients don't. 
# The coefficients get inverted (one over their previous value).
# The coefficients reverse their signs. - YES




# Question 4
# Consider the insect spray data InsectSprays. Fit a Poisson model using spray as a factor level. 
# Report the estimated relative rate comapring spray A (numerator) to spray B (denominator).
# 
# -0.056
# 0.9457  1
# 0.136
# 0.321
data(InsectSprays)
fitQ4a <- glm(count ~ spray,family="poisson", data = InsectSprays)
fitQ4a$coeff
exp(fitQ4a$coeff)
# (Intercept)      sprayB      sprayC      sprayD      sprayE      sprayF 
# 2.67414865  0.05588046 -1.94017947 -1.08151786 -1.42138568  0.13926207 
# 2.67414865 / 0.05588046 = 47.85481
exp(fitQ4a$coeff)
# (Intercept)      sprayB      sprayC      sprayD      sprayE      sprayF 
# 14.5000000   1.0574713   0.1436782   0.3390805   0.2413793   1.1494253 
fitQ4b <- glm(count ~ spray -1,family="poisson", data = InsectSprays)
fitQ4b$coeff
# sprayA    sprayB    sprayC    sprayD    sprayE    sprayF 
# 2.6741486 2.7300291 0.7339692 1.5926308 1.2527630 2.8134107 
# 2.6741486 / 2.7300291 = 0.9795312
exp(fitQ4b$coeff)
# sprayA    sprayB    sprayC    sprayD    sprayE    sprayF 
# 14.500000 15.333333  2.083333  4.916667  3.500000 16.666667 
# 14.500000/15.333333 = 0.9456522 !!!



# Question 5
# Consider a Poisson glm with an offset, t. 
# So, for example, a model of the form glm(count ~ x + offset(t), family = poisson) 
#   where x is a factor variable comparing a treatment (1) to a control (0) and 
#   t is the natural log of a monitoring time. 
# What is impact of the coefficient for x if we fit the model 
#   glm(count ~ x + offset(t2), family = poisson) where t2 <- log(10) + t? 
# In other words, what happens to the coefficients if we change the units of the offset variable. 
# (Note, adding log(10) on the log scale is multiplying by 10 on the original scale.)
# 
# The coefficient estimate is divided by 10.
# The coefficient is subtracted by log(10).
# The coefficient estimate is multiplied by 10.  guess 1
# The coefficient estimate is unchanged


# Question 6
# Consider the data
# 
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
# Using a knot point at 0, fit a linear model that looks like a hockey stick with two 
# lines meeting at x=0. Include an intercept term, x and the knot point term. 
# What is the estimated slope of the line after 0?


# 
# 2.037
# 1.013 - guess based on plot
# -1.024
# -0.183