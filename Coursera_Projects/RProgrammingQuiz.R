#Name:          Clay Gibson
#Date:          30 September 2014
#Class:         Independent Study Fall 2014
#Title:         Code for R Programming Quiz
#Description:   Below you will find code used in answering Coursera's R Programming quiz

#Problem 1
setwd("/Users/Clay/Dropbox/Senior Year/Term 1/Directed Reading/R Code/quiz3")
data <- read.csv("getdata-data-ss06hid (1).csv")
relevant <- data[,c("ACR","AGS")]
relevant <- relevant[complete.cases(relevant),]
relevant <- relevant[relevant$ACR == 3 & relevant$AGS == 6,]
rownames(head(relevant,3))

#Problem 3
gdp_data <- read.csv("gdp_data.csv",strip.white = T)
gdp_data <- gdp_data[c(5:330),c(1,2,5)]
colnames(gdp_data) <- c("Code","Rank", "GDP")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
gdp_data[,3]<- as.numeric((gsub(",","",gdp_data[,3])))
edu_data <- read.csv("countrydata.csv")
full_data <- merge(x = gdp_data, y=edu_data, by.x = "Code", by.y = "CountryCode")
full_data <- full_data[!(full_data$Rank == ""),]
full_data <- full_data[order(full_data$Rank, decreasing=T),]

#Problem 4
high_inc_nonOECD <- full_data[full_data$Income.Group == "High income: nonOECD",]
high_inc_OECD <- full_data[full_data$Income.Group == "High income: OECD",]
mean(high_inc_nonOECD$Rank,na.rm = T)
mean(high_inc_OECD$Rank,na.rm=T)
