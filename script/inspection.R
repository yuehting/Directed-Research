# MS Information Capstone 
# Sebastian Deimen
# STEM opportunities in the US
# University of Arizona - Spring 2021
#
# Purpose: Inspecting the chosen variables

setwd("~/Directed_Research_R")
# install.packages("rockchalk")
# loading packages
library(tidyverse)
library(rockchalk) # for combining levels
library(dplyr)

 
load("data/opps.RData")

# for inspecting purposes I removed the three text attributes "description", 
# excerpt" and "title"
opps_inspect <- select(opps, -title, -excerpt, -description)
glimpse(opps_inspect)

# checking for incomplete cases
#nrow(filter(opps_inspect, is.na(opps_inspect.locationLatitude)))

# checking for any na value in "locationlatitude" and "locationLongitude"
opps_inspect %>% 
  filter(is.na(locationLatitude))
opps_inspect %>%
  filter(is.na(locationLongitude))

# adding a duration column ----
# transform the fromDate and toDate into a Date format to calculate the difference 
opps_inspect$fromDate <- as.POSIXct(opps_inspect$fromDate, 
                                    format = "%Y-%m-%d %H:%M") 
opps_inspect$StartDate <- format(as.Date(opps_inspect$fromDate), "%Y-%m-%d")
#check 
opps_inspect$fromDate
opps_inspect$toDate <- as.POSIXct(opps_inspect$toDate, 
                                  format = "%Y-%m-%d %H:%M")
opps_inspect$EndDate <- format(as.Date(opps_inspect$toDate), "%Y-%m-%d")
# check
opps_inspect$toDate
#  creating the duration_in_hour column
opps_inspect$duration_in_h <- as.integer(opps_inspect$toDate - opps_inspect$fromDate)/3600

# need to bin duration
opps_inspect$duration_in_h_group <- cut(opps_inspect$duration_in_h,
                                        breaks =  c(0,1,12,96,168,336,9504),
                                        labels = c("under 1 hour","under one day",
                                                   "one to four days",
                                                   "four to seven days",
                                                   "seven to 14 days","over 14 days"))

# Save opps_total and opps ----
save(opps_total, opps, opps_for_ohe, opps_inspect, file = "data/opps.RData")

cs <- opps_for_ohe$AoI_CS
cp <- opps_for_ohe$`AoI_Coding/Programming`

# checking if Computer Science and Coding.Programming are the same,
# because the counts look like
opps_for_ohe[opps_for_ohe$AoI_CS != opps_for_ohe$`AoI_Coding/Programming`,]
# but they are not

glimpse(opps_inspect)
summary(opps_inspect)
unique(opps_inspect["typeOfOpportunity"])
opps_inspect$typeOfOpportunity <- as.factor(opps_inspect$typeOfOpportunity)
summary(opps_inspect$typeOfOpportunity)


# Save opps_total and opps ----
save(opps_total, opps, opps_for_ohe, opps_inspect, file = "data/opps.RData")









