pwin <-.4737
bankroll <- 2000
min_bet <- 5
nit <- 500
storage <- matrix(rep(0, nit*3), ncol=3)
won_last <- T
current_bet <- min_bet

for(i in 1:nit) {
  x <- runif(1)
  if(bankroll <= 0)
    current_bet <- 0
  if(x<=pwin) {
    storage[i,1] <- 1
    bankroll <- bankroll+current_bet
    storage[i,2] <- bankroll
    storage[i,3]<- current_bet
    current_bet <- min_bet
    won_last <- T
  }
  else {
    storage[i,1] <- 0
    bankroll <- bankroll-current_bet
    storage[i,2] <- bankroll
    storage[i,3]<- current_bet
    current_bet <- current_bet*2
    won_last <- F
  }
}
storage <- as.data.frame(storage)

qplot(y = storage$V2, color = storage$V2) + geom_point(size=3)+
  scale_color_gradient2(midpoint = 200, low="lightblue",mid = "lightblue",high="red" ) +
  xlab("Number of Rounds") +
  ylab("Total") +
  theme(legend.position="none") +
  geom_hline(aes(yintercept = 20), linetype = "dotted", color = "darkgreen")