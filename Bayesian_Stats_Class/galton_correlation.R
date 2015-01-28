# Name:         Clay Gibson
# Date:         12 November 2014
# Description:  Simple Regression Example

install.packages("UsingR")
library(UsingR)
data(galton)

myPlot <- function(beta) {
  y <- galton$child - mean(galton$child)
  x <- galton$parent - mean(galton$parent)
  freqData <- as.data.frame(table(x,y))
  names(freqData) <- c("child", "parent", "freq")
 
  plot(
    as.numeric(as.vector(freqData$parent)),
    as.numeric(as.vector(freqData$child)),
    pch = 21, col = "black", bg = "lightblue",
    cex = .1 * freqData$freq,
    xlab = "parent",
    ylab = "child"
  )
  abline(0, beta, lwd = 3)
}

myModel <- lm(formula = child ~ parent, data = galton)
myPlot(myModel$coefficients[2])
