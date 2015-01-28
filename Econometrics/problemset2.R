# Name: Clay Gibson
# Date: 17 January 2015
# Description: Problem Set 2

require(foreign)
setwd("/users/cgibson/Downloads")

# Read in Stata data file
dat <- read.dta("hprice1.dta", convert.dates = T, convert.factors = T)
colnames(dat)

# Part 1: Correlation Matrix
cor(dat)

# Part 2: Regression of lprice and bedrooms
reg2 <- lm(dat$lprice~dat$bdrms)
summary(reg2)
plot(dat$lprice ~ dat$bdrms)

# Part 3: 
reg3 <- lm(dat$lprice~dat$bdrms+dat$lsqrft)
summary(reg3)

# Part 4: 
reg4a <- lm(dat$lprice ~ dat$lsqrft)
res_reg4a <- residuals(reg4a)

reg4b <- lm(dat$bdrms ~ dat$lsqrft)
res_reg4b <- residuals(reg4b)

reg4c <- lm(res_reg4a ~ res_reg4b)
summary(reg4c)
plot(res_reg4a ~ res_reg4b)

# Part 5: 
reg5 <- lm(dat$lprice ~ dat$assess + dat$bdrms + dat$lotsize + dat$colonial)
summary(reg5)

# Part 7:
reg7 <- lm(dat$lprice ~ dat$lsqrft + dat$llotsize)
summary(reg7)
