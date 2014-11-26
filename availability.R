# Name:         Clay Gibson
# Date:         4 November 2014
# Description:  Given Private XML of Google Calendar, return availability
#               for the upcoming time period.

require(RCurl)
require(pracma)
require(XML)
require(lubridate)

# Sample URL to use to test the code
url <- "https://www.google.com/calendar/feeds/clay.gibson%40yale.edu/private-41ca745b0356500eae6fe47891c39391/basic"

# FindMyAvailability takes in a URL to the private XML code of a Google Calendar
# and returns availability (free time in-between GCal events). 
  begin = "9:00AM"
  end = "10:00PM"
  xdata <- getURL(url)
  dates <- gregexpr("When",text = xdata)
  
  # Set up storage
  # Note: for each GCal event, there are two instances where date is specified. 
  # Our storage only needs to be half as many instances of "When" we find.
  charstorage <- rep("0", length(dates[[1]])/2) 
  df <- data.frame(matrix(rep("Empty", length(charstorage)*3), ncol=3),
                   stringsAsFactors = F)
  colnames(df) <- c("Day", "StartTime", "EndTime")
  
  lengthstr <- 60 # Longest possible length of string containing date & time
  
  # Go through the XML, cleaning up the times of your GCal events
  # Creates a vector with entries like ""Nov 4, 2014 8pm to 10pm" 
  for(i in 1:length(charstorage)) {
    startchar <- dates[[1]][2*i - 1]
    charstorage[i] <- substring(xdata, startchar, startchar + lengthstr)
    charstorage[i] <- gsub("When: ","", charstorage[i])
    charstorage[i] <- substring(charstorage[i], 5, nchar(charstorage[i]))
    search <- gregexpr("[ap]m&amp", charstorage[i])
    if (search[[1]][1] == -1) 
      charstorage[i] <- "NULL"
    else
      charstorage[i] <- substring(charstorage[i], 0, search[[1]][1]+1 )
  }
  
  # Go through the vector of cleaned dates and times, enter appropriate
  # entries into the data frame df: Day, StartTime, EndTime.
  for(i in 1:length(charstorage)) {
    search <- gregexpr("2014", charstorage[i])
    df[i,1] <- substring(charstorage[i], 0, search[[1]][1]+3)
    
    # Find the instances of time (e.g. "7pm" or "3:30pm")
    search <- gregexpr("[0-9]*([:.][0-9]{2})?([ap]m)", charstorage[i])
    locs <- search[[1]]
    lengths <- attr(search[[1]], "match.length")
    df[i,2] <- substring(charstorage[i], locs[1], locs[1]+lengths[1])
    
    # Standardize start times as XX:XX[AP]M
    if(nchar(df[i,2]) < 6) {
      search <- gregexpr("[ap]m", df[i,2])
      df[i,2] <- strcat(substring(df[i,2], 0, search[[1]][1]-1),
                        strcat(":00", 
                               substring(df[i,2], search[[1]][1], nchar(df[i,2]))))
    }

    # Standardize end times as XX:XX[AP]M
    df[i,3] <- substring(charstorage[i], locs[2], locs[2]+lengths[2])
    if(nchar(df[i,3]) < 6) {
      search <- gregexpr("[ap]m", df[i,3])
      df[i,3] <- strcat(substring(df[i,3], 0, search[[1]][1]-1),
                        strcat(":00", 
                               substring(df[i,3], search[[1]][1], nchar(df[i,3]))))
    }
  }
  
  df$Day <- as.character(as.Date(df$Day, format = "%b %d, %Y"))
  
  # Convert the StartTimes and EndTimes into POSIXlt form
  for(i in 1:length(charstorage)) {
    df[i,2] <- strcat(df[i,1], strcat(" ", df[i,2])) 
    df[i,2] <- as.character(ymd_hm(df[i,2]))
    df[i,3] <- strcat(df[i,1], strcat(" ", df[i,3])) 
    df[i,3] <- as.character(ymd_hm(df[i,3]))
  }
    # If the StartTime is PM and EndTime is AM, change EndTime date
    # to the next day 
    end <- as.POSIXlt(df[i,3])
    if(strftime(end, format="%H") == "00") {
      df[i,3] <- "11:59PM"
      df[i,3] <- strcat(df[i,1], strcat(" ", df[i,3])) 
      df[i,3] <- as.character(ymd_hm(df[i,3]))
    }
  }

  df <- df[order(as.Date(df$StartTime)),]
  df


# print endtime to starttime
storage <- df

for(i in 1:length(charstorage)-2) {
    startlimit = "09:00AM"; endlimit = "10:00PM";
    day <- as.Date(df[i,1]); nextday <- as.Date(df[i+1,1])
    startlimit <- ymd_hm(paste(as.character(day), startlimit, sep = " "), tz = "EST")
    endlimit <- ymd_hm(paste(as.character(day), endlimit, sep = " "), tz = "EST")
   
    start <- as.POSIXlt(df[i,3])
    end <- as.POSIXlt(df[i+1,2])
    
    if (day == nextday) {
      if (start < startlimit & end < startlimit)
        start <- NULL
      else if (start < startlimit & end > startlimit & end < endlimit)
        start <- startlimit
      else if (start > startlimit & end > startlimit & end > endlimit)
        end <- endlimit
      
      if (start != NULL) {
        print(strcat(format(day, format="%A, %b %d: "), 
                     strcat(strftime(start, format="%I:%M%p to "),
                            strftime(end, format="%I:%M%p"))))
      }
    }
}

      

