# Name: Clay Gibson
# Date: 24 January 2015

require(bigmemory); require(ff); require(ffbase)
require(ggplot2); require(ggthemes)
require(effsize); require(pwr)
require(lubridate)
###############################################
# Only do this once to load in the large data 
# and take samples
###############################################
setwd("/Users/cgibson/Downloads")
fare_all <- read.csv.ffdf("trip_fare_1.csv")
data_all <- read.csv.ffdf("trip_data_1.csv")

# Previously saved random sample. Generated:
# mysample <- sample(nrow(fare_all),100000)
# write.csv(mysample, "sampled_nums.csv)
sample <- read.csv("sampled_nums.csv")

# Take the data from this random sample and save it
fare <- fare_all[sample,]
write.csv(fare, "dat_sampled.csv")
data <- data_all[sample,]
write.csv(fare, "dat_sampled_p2.csv")

###############################################
# Combining data sets 
###############################################
fare <- read.csv("dat_sampled.csv")
data <- read.csv("dat_sampled_p2.csv")
total <- cbind(fare,data)

# Get the interesting columns
total <- total[,c(2:12,20:27)]
# Save the data in different location for future use
setwd("/Users/cgibson/Dropbox/Senior Year/Term 2/Directed Reading II")
write.csv(total,"NYCtaxi_data_sampled.csv")

###############################################
# Analyzing the data
###############################################
# Read in 
setwd("/Users/cgibson/Dropbox/Senior Year/Term 2/Directed Reading II")
total <- read.csv("NYCtaxi_data_sampled.csv")
# Remove factors
total$payment_type <- as.character(total$payment_type)

# Tip payment, card vs. cash
card_v_cash <- total[total$payment_type == "CSH" |
                       total$payment_type == "CRD",]
# Plot Tips, card vs. cash
ggplot(card_v_cash, aes(tip_amount, fill = payment_type)) +
  geom_histogram(position="identity", alpha = .5) +
  xlim(0,17) +xlab("Tip Amount") + ylab("Count") +
  ggtitle("Tips by Payment Type")+
  theme_economist() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title=element_blank()) +
  scale_fill_discrete(
    breaks=c("CRD", "CSH"),
    labels=c("Card", "Cash"))

# Plot tips vs total fare
card <- total[total$payment_type == "CRD",]
ggplot(card, aes(x=fare_amount+surcharge+mta_tax+tolls_amount, y=tip_amount)) + geom_point(color = "darkblue", alpha = .4, size=3) +
  ylim(0,20) + xlim(0,100) +
  theme_economist() +
  ggtitle("Tip by Total Fare (Card Only)")+
  xlab("Total Fare (Fare+Surcharge+Tax+Tolls)") + ylab("Tip") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(intercept = 0, slope = .2, alpha = .8, color = "black", linetype = "dotted") +
  geom_abline(intercept = 0, slope = .25, alpha = .8, color = "black", linetype = "dotted") + 
  geom_abline(intercept = 0, slope = .3, alpha = .8, color = "black", linetype = "dotted") +
  # geom_hline(yintercept= 5, alpha = .8, color = "black", linetype = "dotted") +
  # geom_hline(yintercept= 0, alpha = .8, color = "black", linetype = "dotted")
  
  # Transforming Datetimes into Times in R
  # pickup_datetime = column 5
  # dropoff_datetime = column 13
  total[,5] <- as.character(total[,5])
total[,13] <- as.character(total[,13])
total$pickup_datetime_corrected <- ymd_hms(total$pickup_datetime)
total$dropoff_datetime_corrected <- ymd_hms(total$dropoff_datetime)
"1" = "Sunday",                                                         "7" = "Saturday"))
# Weekdays Peak between 8-9AM and between 7-8PM
weekdays <- total[wday(total$dropoff_datetime_corrected) != 1 &
                    wday(total$dropoff_datetime_corrected) != 7,]

ggplot(weekdays, aes(hour(dropoff_datetime_corrected))) + geom_density(fill="darkblue", color ="darkblue", alpha = .6) +
  theme_economist() + scale_x_continuous(breaks=c(0,4,8,12,16,20,23),
                                         labels=c("12:00AM", "4:00AM", 
                                                  "8:00AM", "12:00PM",
                                                  "4:00PM", "8:00PM", "11:59PM")) +
  ggtitle("Number of Transactions at a Given Time")+
  xlab("Hour") + ylab("Density")+
  geom_vline(xintercept = 8.75, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 19, color = "red", linetype = "dashed") 

# Average Trip Velocity (mph)
weekdays$velocity <- 60*60*weekdays$trip_distance/weekdays$trip_time_in_secs
ggplot(weekdays, aes(velocity))+ geom_histogram(binwidth=1, fill="navy") +
  xlim(0,50) +
  geom_vline(xintercept = 10, color = "red", linetype = "dotted") +
  ggtitle("Average Speed during Taxi Trip")+
  xlab("Average Trip Velocity (mph)") + ylab("Frequency") +
  theme_economist() 

weekdays$minute_of_the_day <- hour(weekdays$pickup_datetime_corrected)*60+minute(weekdays$dropoff_datetime_corrected)
weekdayrush <- weekdays[weekdays$velocity < 30,]
ggplot(weekdayrush, aes(x=minute_of_the_day,y=velocity))+geom_point(alpha=.15, color = "#003366") +
  ylim(0,30) + theme_economist() + geom_smooth(color="red", linetype = "longdash") +
  ggtitle("Average Speed during Taxi Trip")+
  xlab("Time") + ylab("Average Trip Velocity (mph)") +
  scale_x_continuous(breaks=c(0,4,8,12,16,20,23)*60,
                     labels=c("12:00AM", "4:00AM", 
                              "8:00AM", "12:00PM",
                              "4:00PM", "8:00PM", "11:59PM"))

ggplot(weekdays, aes(x=minute_of_the_day)) + geom_density(fill="darkblue", color ="darkblue", alpha = .6) +
  theme_economist() + scale_x_continuous(breaks=c(0,4,8,12,16,20,24)*60,
                                         labels=c("12:00AM", "4:00AM", 
                                                  "8:00AM", "12:00PM",
                                                  "4:00PM", "8:00PM", "11:59PM")) +
  ggtitle("Number of Transactions at a Given Time")+
  xlab("Hour") + ylab("Density")+
  geom_vline(xintercept = 19.75*60, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 13.65*60, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 9.25*60, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 22.25*60, color = "red", linetype = "dashed")

# Party nights, defined as Friday and Saturday
partynights <- total[wday(total$pickup_datetime_corrected) == 6 |
                       wday(total$pickup_datetime_corrected) == 7 |
                       wday(total$pickup_datetime_corrected == 1),]
partynights <- partynights[hour(partynights$pickup_datetime_corrected) > 22 |
                             hour(partynights$pickup_datetime_corrected) <= 3,]
partynights <- partynights[partynights$payment_type == "CRD",]
partynights$minute_of_the_day <- hour(partynights$pickup_datetime_corrected)*60+minute(partynights$pickup_datetime_corrected)

# Weekends
weekends <- total[wday(total$pickup_datetime_corrected) == 7 |
                    wday(total$pickup_datetime_corrected) == 1,]

# Mean and median tip on weekends and weekdays are equal
mean(weekends[weekends$payment_type == "CRD",]$tip_amount)
mean(weekdays[weekdays$payment_type == "CRD",]$tip_amount)
mean(partynights$tip_amount)
median(weekends[weekends$payment_type == "CRD",]$tip_amount)
median(weekdays[weekdays$payment_type == "CRD",]$tip_amount)
median(partynights$tip_amount)

# Tip as % of total bill
weekends$tip_as_percentage <- weekends$tip_amount/(weekends$fare_amount + weekends$surcharge + weekends$mta_tax + weekends$tolls_amount)
weekdays$tip_as_percentage <- weekdays$tip_amount/(weekdays$fare_amount + weekdays$surcharge + weekdays$mta_tax + weekdays$tolls_amount)
partynights$tip_as_percentage <- partynights$tip_amount/(partynights$fare_amount + partynights$surcharge + partynights$mta_tax + partynights$tolls_amount)

mean(weekends[weekends$payment_type == "CRD",]$tip_as_percentage)
mean(weekdays[weekdays$payment_type == "CRD",]$tip_as_percentage)
mean(partynights$tip_as_percentage)
median(weekends[weekends$payment_type == "CRD",]$tip_as_percentage)
median(weekdays[weekdays$payment_type == "CRD",]$tip_as_percentage)
median(partynights$tip_as_percentage)