#imagine a frog can be on one of 3 lilly pads
#the probability of it going to a different pad given that its on a certain pad
#is given in matrix P
#pi0 is a starting probability 
#phi0 is a different starting probability

P <- matrix(c(.1,.45,.45,.1,.1,.8,.8,.1,.1), nrow=3, byrow=T)
pi0 <- c(.5,0,.5)
phi0 <- c(0,1,0)
#finds matrix power M^n
#use this for n = integer greater than 1
matpow <- function(M,n) {
  ans <- M
  for (i in 1:(n-1)) {
    ans <- ans %*% M
  }
  ans
}

pi0 %*% matpow(P,2)
phi0 %*% matpow(P,2)
