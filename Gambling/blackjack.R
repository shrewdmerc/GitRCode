# Name: Clay Gibson
# Date: January 22 2015
# Description: Blackjack mean and SD analysis

# Given number of hands and bet per hand
n <- 195000
b <- 10

# Calculate expected loss
expected_loss <- -1*n*b*.003

# Calculate estimated SD based on Stanford Wong empirical results
sdestimate1 <- (sqrt(1.32*n + .48*n*(n-1))/sqrt(n))*b

# Estimate SD based on binomial distribution
n = 1950
p <- .465
b <- 10
sdestimate2 <- sqrt(n*p*(1-p))*b

# Plotting 
curve(dnorm(x,mean = expected_loss,sd = sdestimate2),-400,400,add=F,col="red",
      xlab="Total", ylab="Likelihood")
curve(dnorm(x,mean = expected_loss,sd = sdestimate1),-400,400,add=T,col="blue",
      xlab="Total", ylab="Likelihood")
abline(v=expected_loss, col="red", lty = "dotted")

pnorm(q = 0, mean = expected_loss, sd = sdestimate1, lower.tail = F)

##############################################
##############################################
n <- c(100,200,400,800,1600,3200,6400,12800,25600,51200,102400, 204800)
b <- 10
storage <- matrix(data = rep(0,12*4),nrow = 12, ncol = 4)
# Calculate expected loss
for (i in 1:length(n)) {
  expected_loss <- -1*n[i]*b*.003
  sdestimate1 <- (sqrt(1.32*n[i] + .48*n[i]*(n[i]-1))/sqrt(n[i]))*b
  pno <- pnorm(q = 0, mean = expected_loss, sd = sdestimate1, lower.tail = F)
  storage[i,1] <- n[i]
  storage[i,2] <- round(expected_loss,1)
  storage[i,3] <- round(sdestimate1,1)
  storage[i,4] <- round(pno,1)
}

storage <- as.data.frame(storage)
colnames(storage) <- c("Hands", "EV", "SD", "Prob_Profit")

storage$hands <- n
qplot(storage$hands, storage$V3) + geom_point(size = 4, color = "darkgreen") +
  labs(x = "Number of Hands", y = "Probability of being ahead") 


# Calculate estimated SD based on Stanford Wong empirical results


myplot <- function(mean, sd) {
  
curve(dnorm(x,mean = mean,sd = sd),-400,400,add=F,col="blue",
      xlab="Total", ylab="Likelihood")
abline(v=-mean, col="red", lty = "dotted")
arrows(mean+sd,-1,mean+sd, dnorm(mean,mean+sd,sd), lty="dotted", code=0, col="red")
arrows(mean+2*sd,-1,mean+2*sd, dnorm(mean,mean+2*sd,sd), lty="dotted", code=0, col="red")
arrows(mean-sd,-1,mean-sd, dnorm(mean,mean-sd,sd), lty="dotted", code=0, col="red")
arrows(mean-2*sd,-1,mean-2*sd, dnorm(mean,mean-2*sd,sd), lty="dotted", code=0, col="red")
}


abline(v=-3.77+90.67, col="red", lty = "dotted")
abline(v=-3.77-90.67, col="red", lty = "dotted")
abline(v=-3.77+2*90.67, col="red", lty = "dotted")
abline(v=-3.77-2*90.67, col="red", lty = "dotted")