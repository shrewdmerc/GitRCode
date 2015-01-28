
reqsubjects <- function(sd, mua, mu0, beta, alpha, onesided) {
  if(onesided) {
    ((sd^2)*(qnorm(beta) + qnorm(1-alpha))^2) / 
      ((mua-mu0)^2)
  } else {
    ((sd^2)*(qnorm(beta) + qnorm(1-alpha/2))^2) / 
      ((mua-mu0)^2)
  }
}