# Name: Clay Gibson
# Class: STAT 238
# Description: HW9 Problem 4

# Read in data
data <- as.data.frame(matrix(c(45,96,73,221,106,206), nrow=3, byrow = T))
row.names(data) <- c("Under 30", "30-49", "50+")
colnames(data) <- c("VerySatisfied", "NotSatisfied")
data$total <- data$VerySatisfied+data$NotSatisfied

###################
# PART A
###################
# Define prior and data values
th01 <- .33
th02 <- .33
th03 <- .33

x1 <- data[1,1]
x2 <- data[2,1]
x3 <- data[3,1]

# Define and run model
mymodel <- "
  model{
  x1 ~ dbin(th01, 141)
  x2 ~ dbin(th02, 294)
  x3 ~ dbin(th03, 312)
  th01 ~ dunif(0, 1)
  th02 ~ dunif(0, 1)
  th03 ~ dunif(0, 1)
  }
"
library(rjags)
jm = jags.model(textConnection(mymodel), 
                data=list(x1=x1, x2=x2, x3=x3))
s = coda.samples(jm, c("x1","x2","x3","th01", "th02", "th03"), 10000)
plot(s)
ss = as.data.frame(s[[1]])
names(ss)

# Graph the results
th1s <- ss$th01
th2s <- ss$th02
th3s <- ss$th03
par(mfrow=c(3,1))
hist(th1s, xlim = c(0,.5), col = "red")
hist(th2s, xlim = c(0,.5), col = "red")
hist(th3s, xlim = c(0,.5), col = "red")

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

