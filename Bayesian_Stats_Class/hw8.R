#############################################
# Metropolis: Random walk Metropolis method
#############################################

myhist <- function(x, breaks=100, main=deparse(substitute(x)),...){
  hist(x, breaks, col="red", main = main, xlab="",...)
}

# Normal distribution w/ mean = 3 and sd = .001
mew <- 0
sd <- 1
f <- function(x) { 1/sqrt(2*pi) * exp(-1*(x-mew)^2/(2*sd^2))}

# As in notes, use x for current state, y for candidate
nit <- 10000
path <- numeric(nit)
x <- 3 # initial state
path[1] <- x
scale <- .001

for(i in 2:nit){  # <-- This little loop is the guts of Metropolis
  y <- runif(1, x-scale, x+scale)
  r <- f(y)/f(x)
  if(runif(1) < r) x <- y
  path[i] <- x
}
plot(path[1:10000])

myhist(path, xlim = c(2.9,3.1))
curve(f(x)/integrate(f,0,1)$value, 0, 3.1, add=T)

