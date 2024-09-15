#ZIRADDIN GULUMJANLI 2024
#Problem 2 Homework 2
#------------------------------------------------------------------------------

# import required libraries
library(tidyverse)  
library(lubridate)  

# IMPORTANT NOTE: ⚠️ Adjust based on your environment 
setwd("C:\\Users\\zirad\\Downloads") 

# Read the files using read_csv
drivers <- read_csv("drivers.csv")
lapTimes <- read_csv("lapTimes.csv")
races <- read_csv("races.csv")
tracks <- read_csv("tracks.csv")
results <- read_csv("results.csv")


# Problem 3 -----------------------------------------------------------

races$raceDate <- mdy(races$raceDate)
races$dayOfWeek <- weekdays(races$raceDate)
race_count_by_day <- races %>%
  group_by(dayOfWeek) %>%
  summarise(count = n())

# Display the result
print(race_count_by_day)

# Problem 4 -----------------------------------------------------------
drivers_80s <- drivers %>%
  filter(birthYear >= 1980 & birthYear <= 1989)

# Display the results from 1980 to 1989,included.
print(drivers_80s)

# Problem 5 -----------------------------------------------------------

filtered_drivers <- drivers %>%
  filter(!grepl("[oOiIeE]", firstName))

name_counts <- filtered_drivers %>%
  group_by(firstName) %>%
  summarise(count = n())

sorted_name_counts <- name_counts %>%
  arrange(desc(count))

print(sorted_name_counts)

most_common_name <- sorted_name_counts %>%
  slice()

# Problem 6 -----------------------------------------------------------

# STD computation
lap_sd <- lapTimes %>%
  group_by(positionOrder) %>%
  summarise(std_dev = sd(milliseconds, na.rm = TRUE))

# Ascending order of STDs
lap_sd_sorted <- lap_sd %>%
  arrange(std_dev)

# Display the result
print(lap_sd_sorted)

# The smallets STD 
smallest_sd <- lap_sd_sorted %>%
  slice(1)

print(smallest_sd)

# Problem 7 -----------------------------------------------------------

lap_races <- full_join(lapTimes, races, by = "raceID")
lap_races_tracks <- full_join(lap_races, tracks, by = "trackID")
print(lap_races_tracks)

# Problem 8 -----------------------------------------------------------

fastest_laps <- lap_races_tracks %>%
  arrange(milliseconds)

fastest_lap_city <- fastest_laps %>%
  slice(1) %>%
  select(city, milliseconds)

print(fastest_lap_city)

# Problem 9 -----------------------------------------------------------

results_drivers <- full_join(results, drivers, by = "driverID")
results_drivers_races <- full_join(results_drivers, races, by = "raceID")
print(results_drivers_races)

# Problem 10 -----------------------------------------------------------
results_drivers_races <- results_drivers_races %>%
  mutate(driverAge = raceYear - birthYear)

# Ave age of f1 drivers
average_age <- results_drivers_races %>%
  summarise(avg_age = mean(driverAge, na.rm = TRUE))

# Display the resut
print(average_age)

# Problem 11 -----------------------------------------------------------

last_place_2021 <- results_drivers_races %>%
  filter(raceYear == 2021 & finishOrder == 20)

driver_last_place_count <- last_place_2021 %>%
  group_by(firstName, lastName) %>%
  summarise(last_place_count = n()) %>%
  arrange(desc(last_place_count))

print(driver_last_place_count)

most_last_place_driver <- driver_last_place_count %>%
  slice(1)


# THANK YOU
