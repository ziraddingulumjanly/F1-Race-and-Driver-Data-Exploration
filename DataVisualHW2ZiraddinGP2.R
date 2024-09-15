# SYSE 541: Engineering Data Design and Visualization 
# Homework2; PROBLEM 2 ZIRADDIN GULUMJANLI 2024
library(tidyverse)  
library(lubridate)  

# Set the working directory where all CSV files are stored
setwd("C:\\Users\\zirad\\Downloads")  # Adjust based on your environment

# Read in the data from CSV files using read_csv from tidyverse
drivers <- read_csv("drivers.csv")
lapTimes <- read_csv("lapTimes.csv")
races <- read_csv("races.csv")
tracks <- read_csv("tracks.csv")
results <- read_csv("results.csv")


# Problem 3 -----------------------------------------------------------

races$raceDate <- mdy(races$raceDate)

# Extract the day of the week from the date
races$dayOfWeek <- weekdays(races$raceDate)

# Tabulate how many races took place on each day of the week
race_count_by_day <- races %>%
  group_by(dayOfWeek) %>%
  summarise(count = n())

# Display the result
race_count_by_day

# Problem 4 -----------------------------------------------------------
drivers_80s <- drivers %>%
  filter(birthYear >= 1980 & birthYear <= 1989)

# View the filtered data
print(drivers_80s)

# Problem 5 -----------------------------------------------------------
# Filter out drivers whose first name contains 'o', 'O', 'i', 'I', 'e', or 'E'
filtered_drivers <- drivers %>%
  filter(!grepl("[oOiIeE]", firstName))

# Group by first name, count how many drivers have each of those remaining first names
name_counts <- filtered_drivers %>%
  group_by(firstName) %>%
  summarise(count = n())

# Arrange the result in descending order for the number of occurrences for each name
sorted_name_counts <- name_counts %>%
  arrange(desc(count))

# View the result
print(sorted_name_counts)

# To answer question 3 (Which first name that is left is the most common?)
most_common_name <- sorted_name_counts %>%
  slice(1)

print(most_common_name)

# Problem 6 -----------------------------------------------------------
# Compute the standard deviation of lap time (milliseconds) for each positionOrder
lap_sd <- lapTimes %>%
  group_by(positionOrder) %>%
  summarise(std_dev = sd(milliseconds, na.rm = TRUE))

# Arrange the result in ascending order of the standard deviations
lap_sd_sorted <- lap_sd %>%
  arrange(std_dev)

# View the result
print(lap_sd_sorted)

# To answer question 4 (Which position has the smallest standard deviation and what is the value?)
smallest_sd <- lap_sd_sorted %>%
  slice(1)

print(smallest_sd)

# Problem 7 -----------------------------------------------------------
# Step 1: Join lapTimes with races using full_join on 'raceID'
lap_races <- full_join(lapTimes, races, by = "raceID")

# Step 2: Join the result with tracks using full_join on 'trackID'
lap_races_tracks <- full_join(lap_races, tracks, by = "trackID")

# View the resulting joined dataset
print(lap_races_tracks)

# Problem 8 -----------------------------------------------------------
fastest_laps <- lap_races_tracks %>%
  arrange(milliseconds)

# View the fastest lap (first row) and check the city where it occurred
fastest_lap_city <- fastest_laps %>%
  slice(1) %>%
  select(city, milliseconds)

print(fastest_lap_city)

# Problem 9 -----------------------------------------------------------
results_drivers <- full_join(results, drivers, by = "driverID")

# Step 2: Join the result with races using full_join on 'raceID'
results_drivers_races <- full_join(results_drivers, races, by = "raceID")

# View the resulting joined dataset
print(results_drivers_races)

# Problem 10 -----------------------------------------------------------
results_drivers_races <- results_drivers_races %>%
  mutate(driverAge = raceYear - birthYear)

# Calculate the average age of the drivers
average_age <- results_drivers_races %>%
  summarise(avg_age = mean(driverAge, na.rm = TRUE))

# View the average age
print(average_age)

# Problem 11 -----------------------------------------------------------
# Step: Filter for the 2021 season and for drivers who finished 20th
last_place_2021 <- results_drivers_races %>%
  filter(raceYear == 2021 & finishOrder == 20)

# Step: Group by driver's first and last name, then count how many times they finished last
driver_last_place_count <- last_place_2021 %>%
  group_by(firstName, lastName) %>%
  summarise(last_place_count = n()) %>%
  arrange(desc(last_place_count))

# View the result
print(driver_last_place_count)

# To answer question 7: Extract the driver(s) who finished last the most times
most_last_place_driver <- driver_last_place_count %>%
  slice(1)

print(most_last_place_driver)


# THANK YOU
