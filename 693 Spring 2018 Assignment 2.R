##install packages
# install.packages("tidyverse")
# install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)


## dataset for assignment
flights
glimpse(flights)

# ---------------- part 3 ----------------#
# flights that are cancelled per day
# create a variable to indicate cancelled flights
flights_2 <- flights %>% 
  mutate(cancel = is.na(air_time)) %>% 
  filter(cancel == "TRUE") %>% 
  group_by(day) %>% 
  summarise(count = n())

# plot of number of cancelled flights per day
plot(flights_2$day, flights_2$count,
     xlab = "Day", ylab = "Cancellation")

# looks like a quadratic pattern from plot

# proportion of cancelled vs average delay
flights_cancel <- flights %>% 
  mutate(cancel = is.na(air_time)) %>% 
  group_by(day) %>% 
  summarize(p_cancel = mean(cancel), ave_delay = mean(arr_delay, na.rm = TRUE)) 
plot(flights_cancel$ave_delay, flights_cancel$p_cancel,
     xlab = "Average Arrival Delay", ylab = "Proportion of Cancellation")
# from the plot, can see the two variables are positively correlated

#----------------- part 4 ------------#
# flights with worst ave. arrival delay
# so biggest ave. arrival delay
flights %>% 
  group_by(carrier) %>% 
  summarize(ave_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(ave_delay))
# F9 has the worst ave. arr. delay, Frontier Airlines  

# another way to define carriers with worst delay
# is to use the proportion of delay
flights %>% 
  mutate(delay = arr_delay > 0) %>% 
  group_by(carrier) %>% 
  summarize(prop_delay = mean(delay, na.rm = TRUE)*100) %>% 
  arrange(desc(prop_delay))
# FL: AirTran Airways has the largest proportion of delay

# test for independence of airports and carriers w.r.t delayed flights
flights %>% 
  group_by(carrier, dest) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# take subsets of flights that are not cancelled 
# and test for independence
flights %>% 
  filter(!is.na(air_time)) %>% 
  summarize(df=chisq.test(carrier, dest, correct=FALSE)$parameter,
            value = chisq.test(carrier, dest, correct=FALSE)$statistic,
            prob = chisq.test(carrier, dest, correct=FALSE)$p.value) %>% 
  mutate(test_stat = paste("X^2(", df, ")=", round(value,2), ", p-value =", format.pval(prob, digits = 2, eps = 0.0001)  )) %>% 
  select(test_stat)

# the test gives p-value = <1e-04, which rejects the null hypothesis of independence
# thus, airports and carriers are associated, and can not disentangle the effects

#--------------------- part 5 -----------#
flights %>% 
  group_by(tailnum) %>% 
  summarise(first(arr_delay > 60))

#---------------- part 6 ----------#
# plane with worst on-time record
flights %>% 
  group_by(tailnum) %>% 
  summarize(worst_delay = max(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(worst_delay))
# N384HA

#---------------- part 7 ----------#
# hour with best on-time record
# that is, the smallest proportion of delay
flights %>% 
  mutate(delay_hour = dep_delay > 0) %>% 
  group_by(hour) %>% 
  summarize(prop_delay_hour = mean(delay_hour, na.rm = TRUE)*100) %>% 
  arrange(prop_delay_hour)

# thus, 6am is the best to avoid delays

#------------ part 8 ----------#
# total minutes of delay per destination
# first, take the subsets of delayed flights
# group by destination and sum 
flights %>% 
  filter(arr_delay > 0) %>% 
  group_by(dest) %>% 
  summarise(total_arr_delay = sum(arr_delay, na.rm = TRUE))

# proportion of delay for each flight
flights %>%  
  mutate(delay = arr_delay > 0) %>% 
  group_by(dest, flight) %>% 
  summarise(prop_delay = mean(delay, na.rm = TRUE)*100)
  

#--------------------- part 9 ------------#
# flights with suspiciously big average speed
flights %>% 
  filter(!is.na(air_time)) %>% 
  mutate(speed = distance/air_time) %>% 
  arrange(desc(speed)) %>% 
  select(flight, speed)
  
# air time to the shortest flight to the same destination
flights %>% 
  filter(!is.na(air_time))  %>% 
  group_by(dest) %>% 
  mutate(air_time_short = min(air_time)) %>% 
  mutate(relative_air_time = air_time - air_time_short)

# flights that were most delayed in the air
flights %>% 
  mutate(delay_air = arr_delay - dep_delay) %>% 
  arrange(desc(delay_air)) %>% 
  select(flight, delay_air)
  
  
  
  
