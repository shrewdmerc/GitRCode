# Name: Clay Gibson
# Class: STAT 238
# Description: HW9 Problem 5 

# Read in the data
dat <- as.data.frame(matrix(c(8,7,9,13,10,14,16,12,17,11,10,12,16,15,12), nrow=5, byrow = F))
colnames(dat) <- c("NoBreakfast", "LightBreakfast", "FullBreakfast")

cond1 <- dat$NoBreakfast
cond2 <- dat$LightBreakfast
cond3 <- dat$FullBreakfast

###################
# PART A
###################

par(mfrow=c(3,1))
hist(cond1, breaks = 20, xlim = c(0,20), col = "red")
hist(cond2, breaks = 20, xlim = c(0,20), col = "red")
hist(cond3, breaks = 20, xlim = c(0,20), col = "red")

###################
# PART B
###################

n1 <- length(cond1)
n2 <- length(cond2)
n3 <- length(cond3)

mymodel <- "
model{
for(i in 1:n1){
cond1[i] ~ dnorm(mu1, tau1)
}
for(i in 1:n2){
cond2[i] ~ dnorm(mu2, tau2)
}
for(i in 1:n3){
cond3[i] ~ dnorm(mu3, tau3)
}
mu1 ~ dunif(0, 100)
tau1 ~ dgamma(.01,.01)
mu2 ~ dunif(0, 100)
tau2 ~ dgamma(.01,.01)
mu3 ~ dunif(0, 100)
tau3 ~ dgamma(.01,.01)
}
"
library(rjags)
jm = jags.model(textConnection(mymodel), 
                data=list(cond1 = cond1, cond2 = cond2, cond3 = cond3, n1=n1, n2=n2, n3=n3))

s = coda.samples(jm, c("mu1","mu2", "mu3", "tau1","tau2", "tau3"), 10000)
plot(s)
ss = as.data.frame(s[[1]])
names(ss)

par(mfrow=c(3,1))
hist(ss$mu1, breaks = 20, xlim = c(0,20), col = "red")
hist(ss$mu2, breaks = 40, xlim = c(0,20), col = "red")
hist(ss$mu3, breaks = 40, xlim = c(0,20), col = "red")

# Change Tau values to SDs
ss$tau1 <- sqrt(1/ss$tau1)
ss$tau2 <- sqrt(1/ss$tau2)
ss$tau3 <- sqrt(1/ss$tau3)

par(mfrow=c(3,1))
hist(ss$tau1, breaks = 80, xlim = c(0,8), col = "blue")
hist(ss$tau2, breaks = 80, xlim = c(0,8), col = "blue")
hist(ss$tau3, breaks = 80, xlim = c(0,8), col = "blue")

###################
# PART C
###################
hist(ss$mu1-ss$mu2, breaks = 80, xlim = c(-15,10), col = "darkgreen")
hist(ss$mu1-ss$mu3, breaks = 40, xlim = c(-15,10), col = "darkgreen")
hist(ss$mu2-ss$mu3, breaks = 80, xlim = c(-15,10), col = "darkgreen")

hist(ss$tau1-ss$tau2, breaks = 80, xlim = c(-10,10), col = "purple")
hist(ss$tau1-ss$tau3, breaks = 80, xlim = c(-10,10), col = "purple")
hist(ss$tau2-ss$tau3, breaks = 80, xlim = c(-10,10), col = "purple")

###################
# PART D
###################
mn1 <- mean(ss$mu1 - ss$mu2 < 0)
mn2 <- mean(ss$mu1 - ss$mu3 < 0)
conclusion <- mn1*mn2
conclusion
