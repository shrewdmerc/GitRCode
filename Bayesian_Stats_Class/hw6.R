# Clay Gibson
# Probability and Statistics
# 31 October 2014
# Problem Set 6

# Wright-fisher chain simulation, run until the process gets absorbed
# at either 0 or n. Added count variable to measure length of chain. 

wright <- function(n, x0, plot=TRUE){
  x <- x0
  path <- x
  count <- 0
  while(x>0 & x<n){
    count <- count + 1
    x <- rbinom(1, n, x/n)
    path <- c(path, x)
  }
  if(plot){
    plot((1:length(path))-1, path,type="l",ylim=c(0,n),xlab="generation",ylab="state",
         main = paste("Wright-Fisher process with\n n=",n,", x0=",x0,sep=""),
         yaxs="i")
  }
  return(c(x, count))
}

# Create a place to store data
tab <- data.frame(matrix(data = rep(x = 0, 1000), nrow = 500))

# Store 500 instances of this simulation
for (i in 1:500) {
  path <- wright(1000,300,plot = F)
  tab[i,1] = path[1] # Is either 0 or 1000
  tab[i,2] = path[2] # Is length of path
}
colnames(tab) <- c("Result", "Times")

results <- tab[,1]
times <- tab[,2]
table(results)

# Create a plot of the times
require(ggplot2)
ggplot(tab, aes(Times)) +
  geom_histogram(binwidth = 200, fill = "#5A8B63") +
  ggtitle("Distribution of Times") +
  xlab("Length of Path")+
  xlim(c(0,6000)) +
  geom_vline(xintercept = 1211.7)

colMeans(tab)

table(tab[,1])/500

