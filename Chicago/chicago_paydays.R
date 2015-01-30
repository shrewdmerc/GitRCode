# Name: Clay Gibson
# Date: 28 January 2015

require(ggplot2); require(ggthemes)
setwd("/Users/cgibson/Downloads")
dat <- read.csv("Current_Employee_Names__Salaries__and_Position_Titles.csv")
dat$Employee.Annual.Salary <- as.character(dat$Employee.Annual.Salary)
dat$Employee.Annual.Salary <- gsub("\\$", "", dat$Employee.Annual.Salary)
dat$Employee.Annual.Salary <- as.numeric(dat$Employee.Annual.Salary)

table(dat$Department)

police <- dat[dat$Department == "POLICE",]
fire <- dat[dat$Department == "FIRE",]
