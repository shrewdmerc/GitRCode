# Clay Gibson
# Problem Set 8, Question 4

##############################################
PART A: Generate Approximate Sample for U
##############################################

# Starting vector is only 2's, this can be changed
init <- rep(2,12)

# In this version of the move function, we simulate
# moves in a random path for U.
move <- function(x) {
  n <- length(init)
  temp <- sample(n,2)
  i1 <- min(temp)
  i2 <- max(temp)
  delta1 <- c(1,-1)
  delta2 <- c(-1,1)
  if(runif(1) < 0.5) {
    if (x[i2] > 0) { # Do not want negative values
      x[c(i1,i2)] <- x[c(i1,i2)]+delta1
    }
  } else {
    if (x[i1] > 0) {
      x[c(i1,i2)] <- x[c(i1,i2)]+delta2
    }
  }
  x
}

# Generate random path for U
x = init
results <- list(x)
for(i in 2:100000) {
  x = move(x)
  results[[i]] <- x
}

# Print values as specified in P-set
for(i in 1:10) {
  print(results[[10000*i]])
}

# Here are the results
>
 [1] 4 4 1 0 5 2 4 0 1 0 3 0
 [1] 2 0 8 2 0 0 3 5 0 3 1 0
 [1] 3 1 1 0 3 2 2 0 0 2 0 10
 [1] 1 0 0 0 1 1 2 3 0 0 3 13
 [1] 2 3 1 0 3 2 1 2 0 0 10 0
 [1] 0 2 0 4 2 2 0 3 0 10 0 1
 [1] 3 1 8 0 0 1 0 0 5 2 1 3
 [1] 1 0 6 0 0 1 2 8 2 1 2 1
 [1] 0 1 6 2 2 3 1 3 1 1 2 2
 [1] 0 3 7 2 1 0 0 2 3 1 3 2
 
##############################################
PART A: Generate Approximate Sample for P
##############################################

# In this version of the move function, we simulate
# moves in a random path for P. This adds criteria
# for selecting candidate based on product of vector.
moveP <- function(x) {
  n <- length(init)
  temp <- sample(n,2)
  i1 <- min(temp)
  i2 <- max(temp)
  delta1 <- c(1,-1)
  delta2 <- c(-1,1)
  if(runif(1) < 0.5) {
    if (x[i2] > 0) {
      candidate <- prod(x[c(i1,i2)]+delta1)
      current <- prod(x[c(i1,i2)])
      if(candidate >= current)
        x[c(i1,i2)] <- x[c(i1,i2)]+delta1
      else if (runif(1) < candidate/current) {
        x[c(i1,i2)] <- x[c(i1,i2)]+delta1
      }
    }
  } else {
    if (x[i1] > 0) {
      candidate <- prod(x[c(i1,i2)]+delta2)
      current <- prod(x[c(i1,i2)])
      if(candidate >= current)
        x[c(i1,i2)] <- x[c(i1,i2)]+delta2
      else if (runif(1) < candidate/current) {
        x[c(i1,i2)] <- x[c(i1,i2)]+delta2
      }
    }
  }
  x
}

x = init
results <- list(x)
for(i in 2:10000) {
  x = moveP(x)
  results[[i]] <- x
}

for(i in 1:10) {
  print(resultsP[[10000*i]])
}

# Here are the results
> 
 [1] 2 1 2 1 4 3 1 1 2 2 2 3
 [1] 1 2 1 3 2 2 1 3 3 2 1 3
 [1] 3 1 1 1 3 1 1 7 2 2 1 1
 [1] 1 5 1 1 2 2 2 1 2 1 5 1
 [1] 1 2 5 2 1 3 2 1 1 1 2 3
 [1] 1 1 1 2 5 3 3 4 1 1 1 1
 [1] 1 3 1 2 5 2 3 1 1 1 1 3
 [1] 1 1 1 2 4 4 1 1 5 2 1 1
 [1] 1 2 4 1 1 3 2 2 1 2 4 1
 [1] 1 2 3 1 3 1 2 2 1 3 3 2
 
##############################################
PART B and C: Maxes of P and U
##############################################
maxesP <- rep(0,100000)
maxesU <- rep(0,100000)
maxesP[1] <- 2
maxesU[1] <- 2
for(i in 2:100000){
  maxesP[i] <- max(resultsP[[i]])
  maxesU[i] <- max(resultsU[[i]])
}

> table(maxesU)
	2     3     4     5     6     7     8     9    10    11    12    13    14 
    1   149  4796 17065 23734 19524 13744  8934  5211  2902  1889  1124   545 
   15    16    17    18    19 
  193    82    51    43    13 
> mean(maxesU)
[1] 7.09425


> table(maxesP)
    2     3     4     5     6     7     8     9    10 
    1 12133 45860 28271 10248  2682   641   130    34 

> mean(maxesP)
[1] 4.48096
  
