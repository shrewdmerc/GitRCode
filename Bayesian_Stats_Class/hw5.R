mean <- 1000
val <- 60
pbinom(1000,size = 6000, prob = 1/6)
less_than <- pbinom(q = mean-val, size = 6000,prob = 1/6)
more_than <- pbinom(q = mean+val, size = 6000,prob = 1/6, lower.tail = F)
total <- less_than + more_than


#get the variance of binomial
variance <- function(n,p) {
  n*p*(1-p)
}

#get standard dev of binomial
sd <- function(n,p) {
  variance(n,p)/sqrt(n)
}

z_score <- (val)/sd(6000,1/6)
#########################################

probs <- c(.5,.3,.2)
multip <- c(2,5,3)
f1 <- 1
alphas <- probs

current_fortune <- function(ndays,f1,alphas) {
  my_fortune <- f1
  draws <- sample(1:3, ndays, replace=T, prob=probs)
  for (i in 1:ndays) {
    my_fortune <- my_fortune*alphas[draws[i]]*multip[draws[i]]
  }
  my_fortune
}

fortunes <- rep(0, 1460)
for (i in 1:1460) {
  fortunes[i] <- current_fortune(1460, f1, probs)
}

mean(fortunes)


