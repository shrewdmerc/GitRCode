f <- c(1/15,2/15,3/15,4/15,5/15)
q <- matrix(c(0,.5,.5,0,0,1/3,0,1/3,1/3,0,1/3,1/3,0,0,1/3,0,1/2,0,0,1/2,0,0,1/2,1/2,0), ncol = 5, byrow = T)
p <- as.data.frame(matrix(rep(0,25), nrow = 5))

for(i in 1:5) {
  for(j in 1:5) {
    if (i != j && q[i,j] != 0)
      p[i,j] <- q[i,j]*min(1, ((f[j]*q[j,i])/(f[i]*q[i,j])))
  }
}

for(i in 1:5) {
  p[i,i] <- 1 - rowSums(p)[i]
}

nit <- 1000
vec <- numeric(nit)
vec[1] <- sample(1,1:5)
for(i in 2:nit) {
  vec[i] <- sample(1:5,1, prob = p[vec[i-1],])
}

table(vec)/1000

