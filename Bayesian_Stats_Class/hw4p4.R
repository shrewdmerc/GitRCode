

n = 100000
maxs = rep(0,n)

for(i in 1:n) {
  yous = runif(3, min=0, max=1)
  maxs[i] = max(yous)
}
hist(maxs, breaks=100)
curve(expr = -3*(x-.5)^2)
mean(maxs)

###########################################

n = 1000000
maxs = rep(0,n)

for(i in 1:n) {
  yous = runif(3, min=0, max=1)
  maxs[i] = median(yous)
}
hist(maxs, breaks=100)
mean(maxs)
