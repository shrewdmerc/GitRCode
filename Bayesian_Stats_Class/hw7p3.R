# Clay Gibson
# Stat 238
# R Simulation; HW7 P3


#Function 
flip <- function(first, second) {
  # Generate Coin Flips: Let 1 = H, 2 = T
  n <- 50
  rec <- rep(0,n)
  for(i in 1:n)
    rec[i] <- sample(1:2,1)
  
  # Find first instance of N1
  # Count starts at 2 because you need to flip twice before 
  # getting first state
  count = 2
  for(i in 2:n) {
    if((rec[i] == second) && (rec[i-1] == first)) break
    count <- count + 1
  }
  count
}

repetitions <- 10000
pathlengthsn1 <- rep(0,repetitions)
pathlengthsn2 <- rep(0, repetitions)

for(i in 1:repetitions) {
  pathlengthsn1[i] <- flip(1,1)
  pathlengthsn2[i] <- flip(1,2)
}

mean(pathlengthsn1)
mean(pathlengthsn2)

require(ggplot2)
qplot(pathlengthsn1, geom = "histogram", binwidth=2) +
  ggtitle("Histogram of N1") +
  xlab("Time to find first HH")+
  xlim(c(0,40)) +
  ylim(c(0,2600))
  
qplot(pathlengthsn2, geom = "histogram", binwidth=1) +
  ggtitle("Histogram of N2") +
  xlab("Time to find first HT")+
  xlim(c(0,20)) +
  ylim(c(0,1500))