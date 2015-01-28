# Name: Clay Gibson
# Class: Probability and Statistics
# Description: HW 10 p2

require(ggplot2)
require(rjags)
require(Hmisc)
source("http://www.stat.yale.edu/~jtc5/238/hw10-data.r")

############################
# Part A: Setting up Model
############################
n <- length(ks)
model1 <- "
  model {
    for (i in 1:n) {
      ks[i] ~ dbin(theta[i], ns[i])
      theta[i] ~ dunif(0,1)
    }
  }
"
model2 <- "
  model {
    for (i in 1:n) {
      ks[i] ~ dbin(theta[i], ns[i])
      theta[i] ~ dbeta(alpha, beta)
    }
    lambda ~ dexp(.0001)
    rho ~ dunif(0,1)
    alpha <- lambda * rho
    beta <- lambda * (1 - rho)
  }
"
jm1 <- jags.model(textConnection(model1), data = list(n=n, ns=ns, ks=ks))
jm2 <- jags.model(textConnection(model2), data = list(n=n, ns=ns, ks=ks))

s1 <- coda.samples(jm1, c('theta'),10000)
s2 <- coda.samples(jm2, c('theta', 'lambda', 'rho'), 10000)

##########################################
# Part B: Drawing posterior distributions
##########################################
ss1 <- as.data.frame(s1[[1]])
ss2 <- as.data.frame(s2[[1]])
theta3_1 <- ss1$"theta[3]"
theta3_2 <- ss2$"theta[3]"
qplot(theta3_1) + geom_histogram(fill='darkgreen')
qplot(theta3_2) + geom_histogram(fill='darkblue')
ggplot(ss2, aes(rho)) + geom_histogram(fill = 'red')
ggplot(ss2, aes(lambda)) + geom_histogram(fill = 'orange')

##########################################
# Part C: Mean estimates
##########################################
postmeans1 <- colMeans(ss1)
postmeans2 <- colMeans(ss2)[c(3:ncol(ss2))]
postmeans1
postmeans2
qplot(postmeans1, postmeans2) + geom_point(color = "red")

##########################################
# Part D: Mean estimates vs. True estimates
##########################################
source("http://www.stat.yale.edu/~jtc5/238/hw10-trueprobs.r")
plot(trueprobs, postmeans1, xlim = c(0,.5), ylim = c(0,.5)) +
  abline(0,1, col = 'red', lty = 2)
plot(trueprobs, postmeans2, xlim = c(0,.5), ylim = c(0,.5)) +
  abline(0,1, col = 'red', lty = 2)

#####################################
# Part E: Finding better predictor
#####################################
lm1 <- lm(trueprobs ~ postmeans1)
lm2 <- lm(trueprobs ~ postmeans2)
cor(trueprobs, postmeans1)
cor(postmeans2, trueprobs)

#####################################
# Part F: Finding better predictor
#####################################
df <- as.data.frame(trueprobs)
lower_quantile <- function(x) quantile(x, .025)
upper_quantile <- function(x) quantile(x, .975)

df$M1L <- apply(ss1,2, lower_quantile)
df$M1U <- apply(ss1,2, upper_quantile)
df$cover1 <- rep(0, n)
for(i in 1:n) {
  if (df[i, "trueprobs"] >= df[i, "M1L"] && df[i, "trueprobs"] <= df[i, "M1U"])
    df[i, "cover1"] <- "TRUE"
  else
    df[i, "cover1"] <- "FALSE"
}
df$M2L <- apply(ss2,2, lower_quantile)[c(3:ncol(ss2))]
df$M2U <- apply(ss2,2, upper_quantile)[c(3:ncol(ss2))]
df$cover2 <- rep(0, n)
for(i in 1:n) {
  if (df[i, "trueprobs"] >= df[i, "M2L"] && df[i, "trueprobs"] <= df[i, "M2U"])
    df[i, "cover2"] <- "TRUE"
  else
    df[i, "cover2"] <- "FALSE"
}
head(df,3)
tail(df,3)

#####################################
# Part G: Finding better predictor
#####################################
table(df$cover1)[2]/n
table(df$cover2)[2]/n
intervals1 <- df$M1U - df$M1L
intervals2 <- df$M2U - df$M2L
mean(intervals1)
mean(intervals2)

#####################################
# Part H: Finding better player
#####################################
m1best <- rep(0,nrow(ss1))
for(i in 1:nrow(ss1)) {
  m1best[i] <- which (ss1[i,] == max(ss1[i,1:ncol(ss1)]))
}
m2best <- rep(0,nrow(ss1))
for(i in 1:nrow(ss2)) {
  m2best[i] <- which (ss2[i,] == max(ss2[i,3:ncol(ss2)]))
}

hist(m1best, breaks=100)
hist(m2best-2, breaks=100)
table(m1best)/nrow(ss1)
table(m2best-2)/nrow(ss2)
