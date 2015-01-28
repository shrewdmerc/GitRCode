# Name: Clay Gibson
# Class: Probability and Statistics
# Description: HW 10 p1

require(ggplot2)
require(rjags)
############################
# Part A: Setting up Model
############################
setwd("/Users/cgibson/Dropbox/Senior Year/Term 1/Statistics")
dat <- read.csv(file = "CHD.csv")

x <- dat$age
y <- dat$chd
n <- length(dat$age)

mymodel <- "
  model{
    for(i in 1:n){
      p[i] <- 1/(1 +exp(-a-b*x[i]))
      y[i] ~ dbern(p[i])
    }
    a ~ dnorm(0,.0001)
    b ~ dnorm(0,.0001)
  }
"

library(rjags)
load.module("dic")
jm <- jags.model(textConnection(mymodel), 
                 data = list(x=x,y=y,n=n))
s <- coda.samples(jm, c('a', 'b', 'deviance'),10000)
ss = as.data.frame(s[[1]])
a <- ss$a
b <- ss$b

#####################################
# Part B: Calculating the Deviance
#####################################
ss$calculated_deviance <- rep(0, nrow(ss))

lik <- function(a, b){
  prod(dbinom(y, 1, 1 / (1 + exp(-a-b*x))))
}

for(i in 1:nrow(ss)) {
  ss[i, 4] <- -2*log(lik(ss[i,1], ss[i,2]))
}

par(mfrow=c(1,1))
plot(ss$calculated_deviance, ss$deviance)

#####################################
# Part C: Plotting
#####################################
ss$half_age <- -ss$a/ss$b
ggplot(ss, aes(half_age)) + geom_histogram(fill='darkgreen')
quantile(ss$half_age,c(.025,.975)) 

#####################################
# Other interesting plotting
#####################################
logistic <- function(x) {
  1/(1+exp(-x))
}

j <- match(min(ss$deviance), ss$deviance)
chdj <- jitter(y,.1)
plot(x, chdj, xlim=c(0,80), col = "blue")
curve(logistic(a[j] + b[j]*x), add=T, col="red")
