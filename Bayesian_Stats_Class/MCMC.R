## Metropolis sampling for a Bayesian example: ##############
## subliminal math improvement:

dat = read.csv("http://www.stat.yale.edu/%7Ejtc5/238/data/SubliminalMathImprovement-BPS-page400.csv")
dat
names(dat)
improvement = dat[,3]-dat[,2]
trt = improvement[dat[,1]==1]
ctrl = improvement[dat[,1]==2]

hist(trt)
hist(ctrl)

### draw a picture
x11()
xlim = range(improvement)
par(mfrow=c(2,1))
hist(trt,100,col="red",xlim=xlim)
hist(ctrl,100,col="red",xlim=xlim)
par(mfrow=c(1,1))

#  Outline of our tasks:
#  * Think of a model to describe the situation.
#  * Write a likelihood function   [ lik = function(th){...} ]
#  * Write a prior                 [ prior = function(th){...} ]
#  * Multiply to get posterior     [ post = function(th){prior(th) * lik(th)} ]
#    (OK, that last one won't much of a task)
#  * Do Metropolis: Propose random changes to th, and
#     accept or reject them in a Metropolisey way

lik = function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  prod(dnorm(trt,mean=mu1,sd=sig1))*prod(dnorm(ctrl,mean=mu2,sd=sig2))
}

prior = function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  return(dnorm(mu1,0,1000)*dnorm(mu2,0,1000)
         *dexp(sig1,rate=.01)*dexp(sig2,rate=.01))
}

post = function(th){
  prior(th) * lik(th)
}

post(c(10,2,10,2))
post(c(11,2,10,2))
post(c(11,2,9,2))
post(c(11,3,9,2))
post(c(11,5,9,2))
# This is the kind of thing Metropolis will do!

#Starting values
mu1 = 10; sig1 = 10; mu2 = 10; sig2 = 10
th0=c(mu1,sig1,mu2,sig2)

# Here is what does the MCMC (Metropolis method):
# Should give an error the first time
nit=10000
results = matrix(0, nrow=nit, ncol=4)
th = th0
results[1,] = th0
n.accept = 0
for(it in 2:nit){
  cand = th + rnorm(4,sd=c(2,1,2,1))
  ratio = post(cand)/post(th)
  if(runif(1)<ratio) {th=cand; n.accept = n.accept + 1}
  results[it,] = th
}

# To fix the error:
post = function(th){
  mu1=th[1]; sig1=th[2]; mu2=th[3]; sig2=th[4]
  if(sig1 <= 0 | sig2 <= 0) return(0)
  else return(prior(th) * lik(th))  # don't really need "else" here
}

# Now go back and run MCMC again.

# Look at results, and see whether we believe that mu1 > mu2
View(results)

mu1s = results[,1]
sig1s = results[,2]
mu2s = results[,3]
sig2s = results[,4]

hist(mu1s)
hist(sig1s)
hist(mu2s)
hist(sig2s)

par(mfrow=c(4,1))
plot(mu1s,type="l")
plot(sig1s,type="l")
plot(mu2s,type="l")
plot(sig2s,type="l")
par(mfrow=c(1,1))

plot(mu1s-mu2s)

hist(mu1s-mu2s)
mean(mu1s > mu2s)   # <-- answer to our original question

# Look at bit closer at how the chain ran, and maybe throw out a "burn-in period"
resultsPlot = function(results, start=1, end=nrow(results)){
  old.par = par(no.readonly = TRUE)
  on.exit(par(old.par))
  nvar = dim(results)[2]
  par(mfrow = c(nvar,1), mgp=c(2,1,0), mar=c(3,4,1,2))
  for(i in 1:nvar)plot(results[start:end,i], ylab = paste("variable",i))
}

resultsPlot(results)
resultsPlot(results, 1, 2000)

res = results[500:nit,]
mu1s = res[,1]
sig1s = res[,2]
mu2s = res[,3]
sig2s = res[,4]

hist(mu1s-mu2s)
mean(mu1s-mu2s > 0)

# ==> run 141113-animation.r
# Bumbles around... kind of like evolution...

#############################################################
# Next we're going to learn to use JAGS.  
# We'll do the same "subliminal" problem using JAGS
##################################
library(rjags)
n1 <- length(trt)
n2 <- length(ctrl)

subliminal.mod <- "
model{
for(i in 1:n1){
trt[i] ~ dnorm(mu1, tau1)
}
for(i in 1:n2){
ctrl[i] ~ dnorm(mu2, tau2)
}
mu1 ~ dnorm(0, .000001)
tau1 ~ dexp(.01)
mu2 ~ dnorm(0, 1e-6)
tau2 ~ dexp(.01)
}
"
jm = jags.model(textConnection(subliminal.mod), 
                data=list(trt=trt, ctrl=ctrl, n1=n1, n2=n2))
s = coda.samples(jm, c("mu1","mu2","tau1","tau2"), 10000)
ss = as.data.frame(s[[1]])
names(ss)
hist(ss$mu1 - ss$mu2)
mean(ss$mu1 - ss$mu2 > 0)
