a <- c(1,2,3,4,5,6)
dat <- rep(0,9000)
for (i in 1:9000) {
  dat[i]<- sum(sample(a,100, replace = T))
}

hist(dat, breaks = 20)
mean(dat)
sd(dat)
var(dat)

