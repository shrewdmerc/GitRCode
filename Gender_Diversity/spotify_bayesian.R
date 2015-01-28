# Name: Clay Gibson
# Class: PSYC 505
# Description: Bayesian Analysis of Name-blind applications

# Read in data
data <- as.data.frame(matrix(c(6,56,22,270), nrow=2, byrow = T))
row.names(data) <- c("Pilot", "Historical")
colnames(data) <- c("Phone", "Total")

###########################
# Frequentist Approach
###########################
require(statmod)
x <- matrix(c(22,248,6,50),nrow = 2, byrow = F)
fisher.test(x, alternative = "less")
chisq.test(x)
require(vcd)
assocstats(x)

#######################
# Bayesian Approach
#######################
# Likelihood function
lik = function(th){
  th1=th[1]; th2=th[2];
  prod(dbinom(data[1,1],size = data[1,2],th1),
       dbinom(data[2,1],size = data[2,2],th2))
}

# Prior function
prior <- function(th) {
  th1=th[1]; th2=th[2];
  # return(dunif(th1,0,.3)*dunif(th2,0,.3))
  return(dnorm(th1,mean = (22/270)*1.25, sd = .015)*dnorm(th2,mean = 22/270, sd = .015))
}

# Calculate the posterior for a given Theta
post = function(th){
  prior(th) * lik(th)
}

# Run a Monte-Carlo simulation
th0 <- c(.1,.1)
nit <- 10000
results <- matrix(0, nrow=nit, ncol=2)
results[1,] <- th0
th <- th0
for(it in 2:nit){
  # Candidate selection method is normal with SD of .01
  cand = th + rnorm(2,sd=c(.01,.01))
  ratio = post(cand)/post(th)
  if(runif(1)<ratio) {th=cand;}
  results[it,] = th
}

# Plot the posterior probability
ths <- as.data.frame(results)
colnames(ths) <- c("th1s", "th2s")
ggplot(data = ths, aes(th1s-th2s, y = ..density..)) + geom_histogram(fill="red") +
 xlab("Experimental % - Historical %") + xlim(-.1,.1) + geom_vline(xintercept = 0, color = "black", linetype = "longdash")

# Return probability estimate
mean(ths$th1s-ths$th2s>0)

################################
# Bayesian Approach using JAGS
################################
x1 <- data[1,1]
x2 <- data[2,1]
n1 <- data[1,2]
n2 <- data[2,2]

mymodel <- "
  model{
    x1 ~ dbin(th1, n1)
    x2 ~ dbin(th2, n2)
    th1 ~ dnorm((22/270)*1.25, 5000)
    th2 ~ dnorm(22/270, 5000)
  }
"
library(rjags)
jm = jags.model(textConnection(mymodel), 
                data=list(x1=x1, x2=x2, n1=n1, n2=n2))
s = coda.samples(jm, c("th1", "th2"), 100000)
plot(s)
ss = as.data.frame(s[[1]])

# These results are similar
mean(ss$th1-ss$th2>0)

################################
# Graphing our Priors
################################
# Create sample from prior distributions and graph them
a <- as.data.frame(rnorm(n = 10000,mean = (22/270)*1.25, sd = .015))
b <- as.data.frame(rnorm(n = 10000,mean = 22/270, sd = .015))
a$utt <- 'a'
b$utt <- 'b'
colnames(a) <- c("val", "id")
colnames(b) <- c("val", "id")
ab <- rbind(a,b)

ggplot(data = ab, aes(val)) + 
  geom_histogram(data=a, aes(color = "Experimental"), fill="yellow",color = "NA", alpha = 0.5, binwidth = .003) +
  geom_histogram(data=b, aes(color = "Control"), fill="red", color = "NA", alpha = 0.2, binwidth = .003)+
  xlab("Proportion")+
  ggtitle("Prior Distributions of Experimental (Yellow) and Control (Red)")
  








