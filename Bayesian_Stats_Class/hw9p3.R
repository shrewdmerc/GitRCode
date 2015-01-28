# Name: Clay Gibson
# Class: STAT 238
# Description: HW9 Problem 3

# Read in data
data <- as.data.frame(matrix(c(45,96,73,221,106,206), nrow=3, byrow = T))
row.names(data) <- c("Under 30", "30-49", "50+")
colnames(data) <- c("VerySatisfied", "NotSatisfied")
data$total <- data$VerySatisfied+data$NotSatisfied

###################
# PART A
###################
post = function(th){
  th1=th[1]; th2=th[2]; th3=th[3];
  n1 = data[1,3]; n2 = data[2,3]; n3 = data[3,3];
  prod(dbeta(th1, 46,97),
       dbeta(th2, 74,221),
       dbeta(th3, 107,207))
}

th0 <- c(.33,.33,.33)
nit <- 10000
results <- matrix(0, nrow=nit, ncol=3)
results[1,] <- th0
th <- th0

for(it in 2:nit){
  cand = th + rnorm(3,sd=c(.02,.02,.02))
  ratio = post(cand)/post(th)
  if(runif(1)<ratio) {th=cand;}
  results[it,] = th
}


th1s = results[,1]
th2s = results[,2]
th3s = results[,3]

###################
# PART B
###################

par(mfrow=c(3,1))
hist(th1s, xlim = c(0,.5), col = "red")
hist(th2s, xlim = c(0,.5), col = "red")
hist(th3s, xlim = c(0,.5), col = "red")
summary(th1s)

mean(th1s)+c(-1.96,1.96)*sd(th1s)
mean(th2s)+c(-1.96,1.96)*sd(th2s)
mean(th3s)+c(-1.96,1.96)*sd(th3s)

###################
# PART C
###################
mean(th1s < th2s & th2s < th3s)
mean(th1s < th3s & th3s < th2s)
mean(th2s < th1s & th1s < th3s)
mean(th2s < th3s & th3s < th1s)
mean(th3s < th1s & th1s < th2s)
mean(th3s < th2s & th2s < th1s)

