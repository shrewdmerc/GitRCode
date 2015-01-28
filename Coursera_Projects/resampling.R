
x <- c(1,1,1,0)
y <- c(0,1)
n <- length(x)
b <- 10000

resamples <- matrix(sample(y, n*b, replace = T),nrow = b, ncol = n)
means <- apply(resamples,1,mean)
hist(means)

(table(means)[4]+table(means)[5])/sum(table(means))


#####################

vals <- c(140,138,150,148,135, 132,135,151,146,130)
weeks <- c(rep(1,5), rep(2,5))
df <- data.frame(cbind(vals,weeks))

teststat <- function(w,g) 
  mean(w[g == "1"]) - mean(w[g == "2"])

observedStat <- teststat(df$vals,df$weeks)
permutations <-sapply(1:10000, function(i) teststat(df$vals, sample(df$weeks)))


########
xM <- -3
xG <- 1
sM <- 1.5
sG <- 1.8
nM <- nG <- 9
ts <- (xM-xG)/sqrt(sM^2/nM + sG^2/nG)
2*pnorm(-abs(ts))
ppois(q = 10,lambda = .01*1787)

qnorm(.75,0,1)

