#Name:          Clay Gibson
#Date:          30 October 2014
#Class:         Independent Study Fall 2014
#Title:         Comparing Micro Areas in Austin, TX
#Description:   Visualizing variables describing Mirco Areas in Austin, TX. 
#               Micro Areas are: North, Northeast, Northwest, South, Southeast, Southwest
#               See accompanying text file for variable codes

#Set working directory
setwd("/Users/cgibson/Dropbox/Senior Year/Term 1/Directed Reading/Austin Data/")

require("xlsx")
require("pracma")
require("ggplot2")

#Set up 2013 microdata datatable
data2013 <- read.xlsx("austin2013micro.xlsx", sheetIndex=1, header=T, as.data.frame=T)
row.names(data2013) <- data2013[,1]
data2013 <- data2013[,c(2:8)]
colnames(data2013) <- strcat(colnames(data2013),".2013")

#Set up 2012 microdata datatable
data2012 <- read.xlsx("austin2012micro.xlsx", sheetIndex=1, header=T)
data2012 <- data2012[c(1:53),c(1:8)]
row.names(data2012) <- data2012[,1]
data2012 <- data2012[,c(2:8)]
colnames(data2012) <- strcat(colnames(data2012),".2012")

#Create data table with both sets
full <- merge(data2012,data2013, by = "row.names",sort = F)
row.names(full) <- full[,1]
full <- full[,c(2:ncol(full))]

#Given a certain variable you're interested in, 
#Print a graph detailing the change form 2012-2013, split by Region
compare_regions <- function() {
  sprintf(row.names(data2013))
  n<- as.integer(readline(prompt="Enter an integer: "))
  if (n > 0 & n < 54 & !is.na(n)) { 
    p<-as.data.frame(cbind(as.vector(as.numeric(as.character(data2013[n,]))), gsub(".2.*$",replacement = "", x = gsub("^Austin..*?", "", colnames(data2013))), rep(2013,times=7)))
    q<-as.data.frame(cbind(as.vector(as.numeric(as.character(data2012[n,]))), gsub(".2.*$",replacement = "", x = gsub("^Austin..*?", "", colnames(data2012))), rep(2012,times=7)))
    pq <- rbind(q,p)
    pq$V1 <- as.numeric(as.character(pq$V1))
    pq$V2 <- factor(pq$V2)
    pq$V3 <- factor(pq$V3)
    ggplot(pq, aes(x=V3, y=V1, color = V2, group = V2)) + 
      geom_line()+geom_point() +
      labs(x="Year", y = rownames(data2013)[n], title = strcat(rownames(data2013)[n]," by Region"), color = "Region") +
      theme(plot.title = element_text(size = rel(1.5), face = "bold"))
  }
  else 
    sprintf(row.names(data2013))
}



