# Name: Clay Gibson
# Class: STAT 238
# Description: HW9 Problem 1

###################
# PART A
###################
sample_mu <- 69.5

lik = function(th){
    dnorm(sample_mu,mean=th,sd=2)
}

prior = function(th){
  dnorm(th, mean = 68, sd = 1)
}

post = function(th){
  prior(th) * lik(th)
}

th0 <- 70
nit <- 10000
results <- rep(0,nit)
results[1] <- th0
th <- th0
for (i in 2:nit) {
  cand <- th + rnorm(1)
  ratio <- post(cand)/post(th)
  if(runif(1)<ratio) {th <- cand}
  results[i] <- th
}

###################
# PART B
###################
par(mfrow=c(1,1))
hist(results, col = "red")
mn <- mean(results)
mysd <- sd(results)
mn + c(-1.96, 1.96)*mysd
mean(results > 66.56 & results < 70.0664)
