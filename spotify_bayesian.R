# Name: Clay Gibson
# Class: Stereotyping and Prejudice
# Description: Bayesian Updating

# Read in data
data <- as.data.frame(matrix(c(6,56,22,270), nrow=2, byrow = T))
row.names(data) <- c("Pilot", "Historical")
colnames(data) <- c("Phone", "Total")

###################
# PART A
###################
lik = function(th){
  th1=th[1]; th2=th[2];
  prod(dbinom(data[1,1],size = data[1,2],th1),
       dbinom(data[2,1],size = data[2,2],th2))
}

prior <- function(th) {
  th1=th[1]; th2=th[2];
  return(dnorm(th1,mean = (22/270)*1.2, sd = .01)*dnorm(th2,mean = 22/270, sd = .01)
}

post = function(th){
  prior(th) * lik(th)
}

th0 <- c(.1,.1)
nit <- 50000
results <- matrix(0, nrow=nit, ncol=2)
results[1,] <- th0
th <- th0
for(it in 2:nit){
  cand = th + rnorm(2,sd=c(.005,.005))
  ratio = post(cand)/post(th)
  if(runif(1)<ratio) {th=cand;}
  results[it,] = th
}

ths <- as.data.frame(results)
colnames(ths) <- c("th1s", "th2s")
ggplot(data = ths, aes(th1s-th2s, y = ..density..)) + geom_histogram(fill="red") +
 xlab("Experimental % - Historical %") + xlim(-.2,.2) + geom_vline(xintercept = 0, color = "black", linetype = "longdash")

mean(ths$th1s-ths$th2s>0)
# .7695 assuming a uniform prior for both between 0 and .3
