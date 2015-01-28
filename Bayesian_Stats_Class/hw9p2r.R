# Name: Clay Gibson
# Class: STAT 238
# Description: HW9 Problem 2 

# Read in data
data <- as.data.frame(matrix(c(45,96,73,221,106,206), nrow=3, byrow = T))
row.names(data) <- c("Under 30", "30-49", "50+")
colnames(data) <- c("VerySatisfied", "NotSatisfied")
data$total <- data$VerySatisfied+data$NotSatisfied

###################
# PART A
###################
lik = function(th){
  th1=th[1]; th2=th[2]; th3=th[3];
  prod(dbinom(data[1,1],size = data[1,3],th1),
       dbinom(data[2,1],size = data[2,3],th2),
       dbinom(data[3,1],size = data[3,3],th3))
}

prior = function(th){
  th1=th[1]; th2=th[2]; th3=th[3]; 
  return(dunif(th1,0,1)*dunif(th2,0,1)*dunif(th3,0,1))
}

prior(th)

post = function(th){
  prior(th) * lik(th)
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
