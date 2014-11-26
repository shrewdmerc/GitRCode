#Name:          Clay Gibson
#Date:          30 October 2014
#Class:         Independent Study Fall 2014
#Title:         Comparing Austin to Other High-Growth Regions in Texas
#Description:   Visualizing Austin compared to Dallas, Houston, and San Antonio
#               Call the mygraph() function and enter one of the following codes:
#                   1 -- Total Population
#                   2 -- Median Household Income
#                   3 -- Per Capita Income
#                   4 -- Housing Units
#                   5 -- Median House Value
#                   6 -- Median Gross Rent

require("xlsx")
require("pracma")
require("ggplot2")

#Set the accurate working directory
setwd("/Users/cgibson/Dropbox/Senior Year/Term 1/Directed Reading/Austin Data/AustinCompare/")

#Generate empty data frames and label data frames
austin <- as.data.frame(matrix(0,nrow = 6, ncol = 7))
dallas <- as.data.frame(matrix(0,nrow = 6, ncol = 7))
houston <- as.data.frame(matrix(0,nrow = 6, ncol = 7))
sanantionio <- as.data.frame(matrix(0,nrow = 6, ncol = 7))
cities <- list(austin,dallas,houston,sanantionio)
for(i in 1:4) {
  row.names(cities[[i]])  <- c("Total Population", "Median Household Income","Per Capita Income", "Housing Units", "Median House Value", "Median Gross Rent")
  colnames(cities[[i]]) <- c(2007:2013)
}

#Generate imporant data rows from raw data
nums <- rep(0,6)
for (i in 1:6) nums[i] = 4+((i-1)*3)

#Base parameters
year <- 2007
colToSelect <- 2

#Objective: Load the data for each city for each year.
#Method: Go through the data, starting with the earliest year. Load that 
#data frame. For each of the cities, load the statistics for that year. 
#Go onto next year until there's no more data left. 
for(i in 1:7) {
    filename <- strcat(strcat("ACS", as.character(year-2)), as.character(year))
    filename <- strcat(filename, ".xlsx")
    data <- read.xlsx(filename,sheetIndex = 1, header = T)
    year <- year + 1
    
    colToSelect <- 2
    for(j in 1:4) {
      pts <- as.character(data[nums,colToSelect]) #Grab important data points
      cities[[j]][,i] <- as.integer(gsub("[[:punct:]]","",pts)) #Format into integers
      colToSelect <- colToSelect + 2
    }
}

mygraph <- function() {
  sprintf(row.names(cities[[1]]))
  n <- as.integer(readline(prompt="Enter an integer: "))
  pq <- as.data.frame(matrix(data = 0,nrow = 28,ncol = 3))
  colnames(pq) <- c("Value", "Year", "City")
  name <- row.names(cities[[1]])[n]
  
  for (i in 1:4) {
    for(j in 1:7) {
      pq[(i-1)*7+j,1] <- cities[[i]][n,j]
      pq[(i-1)*7+j,2] <- colnames(cities[[i]][j])
      pq[(i-1)*7+j,3] <- c("Austin","Dallas","Houston","San Antonio")[i]
    }
  }
  
  ggplot(pq, aes(x=Year,y=Value, group=City, color =City)) +
    geom_line()+geom_point() +
    labs(x="Year", y = name, title = strcat(name," by Region"), color = "Region") +
    theme(plot.title = element_text(size = rel(1.5), face = "bold")) 
    # +  stat_smooth(method = "lm")
}


