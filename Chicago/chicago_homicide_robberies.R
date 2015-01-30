# Name: Clay Gibson
# Date: 28 January 2014

require(ggplot2)
require(ggmap)
require(ff); require(ffbase)

setwd("/Users/cgibson/Downloads")
homicides <- read.csv("chicago_homicides.csv")
robberies <- read.csv("chicago_robberies.csv")
qmap(location = "chicago", zoom = 11, maptype = 'satellite') + 
  geom_point(data=robberies, aes(x=Longitude, y=Latitude), color = "darkred", cex=1.75, alpha=.1)+
  geom_point(data=homicides, aes(x=Longitude, y=Latitude), color = "gold", cex=1.75, alpha=.5)

# For each year, find out how many murders in each police beat
# Run correlation matrix to see how they stay similar over the years

handgun_robberies_and_attempts <- robberies[robberies$Description == "ARMED: HANDGUN" |
                                              robberies$Description == "ARMED: OTHER FIREARM" |  
                                              robberies$Description == "ATTEMPT: ARMED-OTHER FIREARM" |
                                              robberies$Description == "ATTEMPT: ARMED-HANDGUN", c(3,6,7,20,21)]

narcotics <- read.csv.ffdf(file="chicago_narcotics.csv", header=T)
narcotics <- narcotics[, c(3,6,7,20,21)]
qmap(location = "chicago", zoom = 11, maptype = 'satellite') + 
  geom_point(data=narcotics, aes(x=Longitude, y=Latitude), color= "darkgoldenrod1", cex=1.5, alpha=.1) +
  geom_point(data=handgun_robberies_and_attempts, aes(x=Longitude, y=Latitude), color= "aquamarine3", cex=1.75, alpha=.175) +
  geom_point(data=homicides, aes(x=Longitude, y=Latitude), color = "darkred", cex=1.75, alpha=.6)

narcotics$Description <- as.character(narcotics$Description)
narcotics$drug_type <- "Other"

# Takes a long time 
for(i in 1:nrow(narcotics)) {
  s = narcotics[i, "Description"]
  if(grepl(pattern = "CANNABIS", s))
    narcotics[i,"drug_type"] <- "Cannabis/Marijuana"
  else if(grepl(pattern = "MARIJUANA", s))
    narcotics[i,"drug_type"] <- "Cannabis/Marijuana"
  else if(grepl(pattern = "COCAINE", s))
    narcotics[i,"drug_type"] <- "Cocaine"
  else if(grepl(pattern = "CRACK", s))
    narcotics[i,"drug_type"] <- "Crack"
  else if(grepl(pattern = "HEROIN", s))
    narcotics[i,"drug_type"] <- "Heroin"
  else if(grepl(pattern = "PCP", s))
    narcotics[i,"drug_type"] <- "PCP"
  else if(grepl(pattern = "METHAMPHETAMINES", s))
    narcotics[i,"drug_type"] <- "Methamphetamines"
  else if(grepl(pattern = "AMPHETAMINES", s))
    narcotics[i,"drug_type"] <- "Amphetamines"
  else if(grepl(pattern = "BARBITUATES", s))
    narcotics[i,"drug_type"] <- "Barbituates"
}

# Plot drug arrests 
qmap(location = "chicago", zoom = 11, maptype = 'satellite') + 
  geom_point(data=narcotics[narcotics$drug_type == "Cannabis/Marijuana",], aes(x=Longitude, y=Latitude), color= "darkgoldenrod1", cex=1.5, alpha=.3) +
  geom_point(data=narcotics[narcotics$drug_type == "Cocaine",],aes(x=Longitude, y=Latitude), color= "chocolate", cex=1.5, alpha=.1) +
  geom_point(data=narcotics[narcotics$drug_type == "Crack",],aes(x=Longitude, y=Latitude), color= "coral", cex=1.5, alpha=.1) +
  geom_point(data=narcotics[narcotics$drug_type == "Heroin",],aes(x=Longitude, y=Latitude), color= "cyan3", cex=1.5, alpha=.1) +
  geom_point(data=narcotics[narcotics$drug_type == "Barbituates",],aes(x=Longitude, y=Latitude), color= "darkolivegreen1", cex=1.5, alpha=.1)

