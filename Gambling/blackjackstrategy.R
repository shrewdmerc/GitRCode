# Name: Clay Gibson
# Description: Simulating Blackjack, using specific strategy

require(ggplot2)

# Read in your strategy
setwd("/Users/cgibson/Dropbox/Senior Year/Term 1/Directed Reading/R Code")
strat <- read.csv("blackjackstrategy.csv", header = F)
strat[] <- lapply(strat, as.character)
colnames(strat) <- c(1:11)

blackjack <- function(gamblestart = 200, gamblestrategy = "proportional", p = .15, minbet = 10, graph = T) {
  total <- gamblestart # starting amount you're willing to gamble
  min <- 0
  nit <- 300 # average number of hands dealt per hour is ~ 80
  p <- p # percentage of your gambling amount bet per round
  deck <- c(2:9,10,10,10,10,11)
  last_win <- T
  martingale_starting_bet <- 5
  onetwothreesix <- c(1,2,3,6)
  onetwothreesix_val <- 1
  # set up storage
  bjresult <- c(phand=NULL, ppoints=NULL, pcards=NULL, dhand=NULL,
                dpoints=NULL, dcards=NULL, winner=NULL, total=NULL)
  
  # Simulate a round of blackjack nit times
  for(i in 1:nit) {
    gamble <- total 
    if (gamble <= 0)
      bet <- 0
    else if (gamblestrategy == "uniform") 
      bet <- minbet
    else if (gamblestrategy == "proportional")
      bet <- round(gamble*p)
    else if (gamblestrategy == "martingale") {
      if (last_win) 
        bet <- 2*bet
      else 
        bet <- martingale_starting_bet
    }
    else if (gamblestrategy == "minproportional") {
      if (total < minbet)
        bet <- 0
      else if (total <= gamblestart)
        bet <- minbet
      else
        bet <- (minbet) + p*(total-gamblestart)
    }
    winner <- 0
    pbust <- F; dbust <- F; pbj <- F; dbj <- F
    dhand <- sample(deck, 1)
    phand <- sample(deck, 2, replace = T)
    # Find your strategy based on the given table
    pmove <- strat[sum(phand), dhand]
  
    # Player plays their hand
    while(pbust == F) {
      # If your strategy is to HIT
      if (sum(phand) == 21 & length(phand) == 2)
        pbj <- T
      if (sum(phand) == 22 & length(phand) == 2)
        phand <- c(11,0)
      if (pmove == "H") {
        phand <- c(phand, sample(deck, 1))
        if (sum(phand) > 21)
          phand[phand==11] <- 1
        if (sum(phand) > 21)
          pbust <- T
        pmove <- strat[sum(phand), dhand]
      }
      
      # If your strategy is to DOUBLE
      else if (pmove == "D") {
        bet <- 2 * bet
        phand <- c(phand, sample(deck, 1))
        if (sum(phand) > 21)
          phand[phand==11] <- 1
        break
      }
      # If your strategy is to STAND
      else
        break
    }
    
    # Dealer plays their hand
    while(sum(dhand) < 17) {
      dhand <- c(dhand, sample(deck, 1))
      if (sum(dhand) == 21 & length(dhand) == 2)
        dbj <- T
      if (sum(dhand) > 21) 
        dhand[dhand==11] <- 1
      if (sum(dhand) > 21) 
        dbust <- T
    }
    
    # Determine the winner
    if (pbj == T & dbj == F) {
      winner <- 1
      last_win <- 1
      total <- total + (1.5)*bet
    }
    else if (dbj == T & pbj == F) {
      winner <- 0
      last_win <- 0
      total <- total - bet
    }
    else if(pbust) {
      winner <- 0
      last_win <- 0
      total <- total - bet
    }
    else if(pbust == F & dbust == T) {
      winner <- 1
      last_win <- 1
      total <- total + bet
    }
    else if (sum(phand) > sum(dhand)) {
      winner <- 1
      last_win <- 1
      total <- total + bet
    }
    else if (sum(phand) == sum(dhand)) {
      winner <- 2
      last_win <- 0
    }
    else {
      winner <- 0
      last_win <- 0
      total <- total - bet
    }
    
    # Output the results into a data frame
    bjresult <- rbind(bjresult, 
                      c(phand=paste(as.character(phand), collapse = " "), ppoints=sum(phand), pcards=length(phand), 
                        dhand=paste(as.character(dhand), collapse = " "), dpoints=sum(dhand), dcards=length(dhand),
                        winner=winner, total=total))
  }
  
  bjresult <- as.data.frame(bjresult)
  
  if(graph) {
    #qplot(as.numeric(bjresult$winner)) + geom_histogram()
    qplot(y = as.numeric(as.character(bjresult$total)), color = as.numeric(as.character(bjresult$total))) + geom_point(size=3)+
      scale_color_gradient2(midpoint = gamblestart,low="lightblue",mid = "lightblue",high="red" ) +
      xlab("Number of Rounds") +
      ylab("Total") +
      theme(legend.position="none") +
      geom_hline(aes(yintercept = gamblestart), linetype = "dotted", color = "darkgreen")
  }
  else bjresult
}  

findpeak <- function(gamblestart = 200, p = .15, gamblestrategy = "proportional", minbet = p*gamblestart) {
  best <- c(maxes=NULL, times=NULL, profittime = NULL)
  for(i in 1:100) {
    bj <- blackjack(gamblestart = gamblestart, p = p, gamblestrategy = gamblestrategy, graph = F, minbet = minbet)
    mymax <- max(as.numeric(as.character(bj$total)))
    mytime <- which.max(as.numeric(as.character(bj$total)))
    myprofittime <- mean(as.numeric(as.character(bj$total)) > gamblestart)
    best <- rbind(best, c(maxes = mymax, times = mytime, profittime = myprofittime))
  }
  best
}

track <- function(gamblestart = 200, p = .15, gamblestrategy = "minproportional", minbet = p*gamblestart) {
  vals <- NULL
  for(i in 1:500) {
    bj <- blackjack(gamblestart = gamblestart, p = p, gamblestrategy = gamblestrategy, graph = F, minbet = minbet)
    vals <- rbind(vals, as.numeric(as.character(bj$total)))
  }
  vals
}

######################
clay05 <- findpeak(gamblestart=100, p =.05)
clay10 <- findpeak(gamblestart=100, p =.1)
clay15 <- findpeak(gamblestart=100, p =.15)
clay20 <- findpeak(gamblestart=100, p =.2)
clay25 <- findpeak(gamblestart=100, p =.25)



minprop15 <- findpeak(gamblestart = 200, p = .15, gamblestrategy = "minproportional", minbet=15)
clay15 <- findpeak(gamblestart=200, p =.15)
uniform <- findpeak(gamblestart = 200, p = .15, gamblestrategy = "uniform", minbet=15)

minprop15t <- track(gamblestart = 200, p = .15, gamblestrategy = "minproportional", minbet = 15)
clay15t <- track(gamblestart = 200, p = .15, gamblestrategy = "proportional")
uniform15t <- track(gamblestart = 200, p = .15, gamblestrategy = "uniform", minbet=15)

par(mfrow = c(3,1))
plot(colMeans(minprop15t), ylim = c(0,200))
plot(colMeans(clay15t), ylim = c(0,200))
plot(colMeans(uniform15t), ylim = c(0,200))

hist(minprop15[,1], xlim=c(0,2000), breaks = 200, ylim=c(0,40), main="Proportional/Uniform Mix", xlab="Maximum Reached")
hist(clay15[,1], xlim=c(0,2000), breaks = 500, ylim=c(0,40),main="Proportional Betting (15%)", xlab="Maximum Reached")
hist(uniform[,1], xlim=c(0,2000), ylim=c(0,40), main="Uniform Betting ($15)", xlab="Maximum Reached")

minprop15t <- t(minprop15t)
clay15t <- t(clay15t)
uniform15t <- t(uniform15t)
