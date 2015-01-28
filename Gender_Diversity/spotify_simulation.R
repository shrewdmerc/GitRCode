# Name: Clay Gibson
# Class: PSYC 505
# Description: Simulating impact of small effect size on hierarchical 
#              structure (such as hiring funnel)

require(reshape2)
require(ggplot2)

# Simulate takes in a variety of parameters: 
#   1. x: the starting number of total applicants; default = 10000
#	  2. pow: the power by which people are cut down at each step (for example
#		  if pow = 2 and there are 1024 people at step 1, there will be 512 
#		  at step 2, 256 at step 3, etc.
#	  3. continuingD: do women face discrimination at EACH step (T) or only 
#		  at the first step (F)
# 	4. FstartM: mean of female talent distribution; default = 70
# 	5. FstartF: SD of female talent distribution; default = 10
# 	6. MstartM: mean of male talent distribution; default = 70
#	  7. MstartF: SD of male talent distribution; default = 10
#	  8. startingMper: proportion of starting applicants that are male; default= .5

simulate <- function(x=10000, pow=3, vardif=.03, continuingD=F,
                     FstartM = 70, FstartSD = 10, MstartM = 70,
                     MstartSD = 10, startingMper = .5) {
  level <- c(0:4)
  x <- x
  pow <- pow
  positions <- c(x, x*pow^-1, x*pow^-2,x*pow^-3,x*pow^-4)
  positions <- round(positions)
  avgwomen <- rep(0,5)
  avgmeans <- rep(0,5)
  df <- as.data.frame(cbind(level, positions, avgwomen, avgmeans))
  
  # Set up storage for iterations
  nit <- 100
  womens <- matrix(rep(0, 5*nit), nrow = 5)
  means <-  matrix(rep(0, 5*nit), nrow = 5)
  
  # What percentage of applicants are male/female
  starting_male_percentage <- startingMper
  starting_female_percentage <- 1-starting_male_percentage
  
  # What is the effect size? 
  variance_dif <- vardif
  
  # Simulate. 
    for (i in 1:nit) {
      # All men and women come from normal dist
      women <- rnorm(positions[1]*starting_female_percentage, mean = FstartM, sd = FstartSD)
      men <- rnorm(positions[1]*starting_male_percentage, mean = MstartM, sd = MstartSD)
      advantage <- variance_dif*var(men)
      men <- men+advantage
      means[1,i] <- mean(rbind(men,women))
      womens[1,i] <- starting_female_percentage
      
      round1 <- as.data.frame(cbind(men,women))
      round1 <- melt(as.matrix(round1))
      round1 <- round1[order(-round1$value),]
      round2 <- round1[0:positions[2],]
      temp <- table(round2$Var2)/positions[2]
      womens[2,i] <- temp[2]
      means[2,i] <- mean(round2$value)
      
      if (continuingD == F) advantage = 0
      men <- round2[round2$Var2 == "men",]
      women <- round2[round2$Var2 == "women",]
      men[,3] <- men[,3]+advantage
      round2 <- rbind(men,women)
      round2$value <- jitter(round2$value,10)
      round2 <- round2[order(-round2$value),]
      round3 <- round2[0:positions[3],]
      temp <- table(round3$Var2)/positions[3]
      womens[3,i] <- temp[2]
      means[3,i] <- mean(round3$value)
      
      men <- round3[round3$Var2 == "men",]
      women <- round3[round3$Var2 == "women",]
      men[,3] <- men[,3]+advantage
      round3<- rbind(men,women)
      round3$value <- jitter(round3$value,10)
      round3 <- round3[order(-round3$value),]
      round4 <- round3[0:positions[4],]
      temp <- table(round4$Var2)/positions[4]
      womens[4,i] <- temp[2]
      means[4,i] <- mean(round4$value)
      
      men <- round4[round4$Var2 == "men",]
      women <- round4[round4$Var2 == "women",]
      men[,3] <- men[,3]+advantage
      round4<- rbind(men,women)
      round4$value <- jitter(round4$value,10)
      round4 <- round4[order(-round4$value),]
      
      round5 <- round4[0:positions[5],]
      temp <- table(round5$Var2)/positions[5]
      womens[5,i] <- temp[2]
      means[5,i] <- mean(round5$value)
    }
  
  df$avgwomen <- rowMeans(womens)
  df$avgmeans <- rowMeans(means)
  df
  }

# How might Spotify's funnel be impacted by first-stage bias of
# small effect size? 
simulate(pow = 3.25, continuingD = F)
plot(simulate(pow = 3.25, continuingD = F)$avgwomen)

# How does the exponential element affect how quickly women
# are cut out? 
sim1.5 <- simulate(pow = 1.5, continuingD = F)$avgwomen
sim2 <- simulate(pow = 2, continuingD = F)$avgwomen
sim2.5 <- simulate(pow = 2.5, continuingD = F)$avgwomen
sim3 <- simulate(pow = 3, continuingD = F)$avgwomen
sim3.5 <- simulate(pow = 3.5, continuingD = F)$avgwomen
sim4 <- simulate(pow = 4, continuingD = F)$avgwomen
simulations <- as.data.frame(cbind(sim1.5,sim2,sim2.5,sim3,sim3.5,sim4))
simulations <- melt(as.matrix(simulations))
ggplot(simulations, aes(Var1, value))+geom_line(aes(color=Var2, group = Var2))+geom_point(aes(color=Var2))

scondf <- scondf$avgwomen
scondt <- scondt$avgwomen
sim <- as.data.frame(cbind(scondf,scondt))
sim <- melt(as.matrix(sim))
sim$value <- jitter(x = sim$value, factor = 200)
ggplot(sim, aes(Var1,value)) + geom_line(aes(color = Var2, group = Var2), size = 1.25) +
  geom_point(aes(color=Var2), size = 4)+
  labs(x = "Stage", y = "Proportion of Women") + 
  scale_colour_discrete(name  ="Condition",
                        breaks=c("scondf", "scondt"),
                        labels=c("One-time Discrimination", "Continuing Discrimination"))

df
