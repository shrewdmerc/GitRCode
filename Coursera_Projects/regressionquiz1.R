# Name: Clay Gibson
# Course: Independent Research
# Description: Regression Models Quiz 1 R Code

# Problem 1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
mew <- mean(x)
sum = 0
for(i in 1:length(x)){
  sum <- sum + w[i]*(x[i]-mew)^2
}
sum(x * w)/sum(w)

# Problem 2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x-1)

# Problem 3
data(mtcars)
y <- mtcars$mpg - mean(mtcars$mpg)
x <- mtcars$wt - mean(mtcars$wt)
lm(y ~ x)

# Problem 4
correlation <- .5
sdy <- 2
sdx <- 1
beta <- correlation*sdy/sdx
beta

# Problem 5
correlation <- .4
sdx <- 1
sdy <- 1
beta <- correlation*sdy/sdx
y <- 0 + beta*1.5
y

# Problem 6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
sdx <- sd(x)
mn <- mean(x)
ans <- (x-mn)/sdx
ans

# Problem 7 
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x)

# Problem 8 
x <- runif(n = 10000) - .5
y <- runif(n = 10000) - .5
lm(y ~ x)

# Problem 9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)
sum <- 0

# Problem 10
beta1 <- correlation1 * sdy/sdx
y1 <- correlation1 * sdx/sdy
beta1/y1 <- sdy/sdx*sdy/sdx
