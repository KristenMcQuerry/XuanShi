##STA693 HW1  ##
##Xuan Shi ##
##install packages
# install.packages("tidyverse")
# install.packages("nycflights13")

## load libraries
library(tidyverse)
library(dplyr)
library(nycflights13)

## dataset for assignment
flights
summary(flights)

# check on-line for coding manuel #
# https://www2.stat.duke.edu/courses/Spring16/sta101.001/post/labs/intro_to_data.html#

#---------------- part a ------------#
# flights with arrival delay over 2 hours(120 minutes)
flights %>% 
  filter(arr_delay >= 120)

#--------------- part b ---------------#
# flights flew to Houston(IAH or HOU)
to_houston <- flights %>% 
  filter(dest %in% c("IAH", "HOU"))
unique(to_houston$dest)   # check if this only contains flights to Houston(IAH or HOU)

#--------------- part c ---------------#
# flights by UA, AA or Delta
three <- flights %>% 
  filter(carrier %in% c("UA", "AA", "DL"))
unique(three$carrier)   # check if this only contains flights by these three airlines

#--------------- part d ---------------#
# flights departed in summer
flight_summer <- flights %>% 
  filter(month %in% c(7,8,9))
unique(flight_summer$month)   # check if this only contains flights in summer

#--------------- part e ---------------#
# flights that arrived 2 hours late, but didn't leave late
flights  %>% 
  filter(arr_delay >= 120) %>% 
  filter(dep_delay <= 0)

#--------------- part f ---------------#
# flights delayed by at least an hour, and made up over 30 minutes
flights %>% 
  filter(dep_delay >= 60) %>% 
  mutate(madeup = dep_delay - arr_delay) %>% 
  filter(madeup >= 30)

#--------------- part g ---------------#
# flights departed between midnight and 6 am
flights %>% 
  filter(dep_time >= 0 & dep_time <= 600) 

#--------------- part h ---------------#
# sort flights to find maximum delayed flights
# that is, dep_delay in descending order
flights %>% 
  arrange(desc(dep_delay))    # longest delay is 1301 minutes

# sort flights that left earliest
flights %>% 
  arrange(dep_time)          # earliest time to leave is 1 am

#------------------- part i -----------#
## find fastest flights
# first, calculate average speed
flight_fast <- flights %>% 
  mutate(speed = distance/air_time) %>% 
  arrange(desc(speed))
flight_fast[1,]       # fastest flights

#------------------- part j ------#
# flights that traveled the longest distance
flight_far <- flights %>% 
  arrange(desc(distance))
flight_far$distance     # 4983 miles is the longest distance

# flights that traveled the shortest distance
flight_close <- flights %>% 
  arrange(distance)
flight_close$distance     # 17 miles is the shortest distance
