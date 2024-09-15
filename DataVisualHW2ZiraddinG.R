# SYSE 541: Engineering Data Design and Visualization 
# Homework2; PROBLEM 1 ZIRADDIN GULUMJANLI 2024

# load the necessary libraries 
library(tidyverse)

setwd("C:\\Users\\zirad\\Downloads")

# Read the CSV file
olympics <- read_csv("Olympics2021Medals.csv")
olympics


olympics_long <- olympics %>%
  pivot_longer(cols = Gold:Bronze, 
               names_to = "Medals", 
               values_to = "NumberOfMedals")

# Display the first six rows of the reshaped table
print(head(olympics_long))

olympics_wide <- olympics_long %>%
  pivot_wider(names_from = Medals, 
              values_from = NumberOfMedals) %>%
  select(Country, Bronze, Gold, Silver)

# Display the first few rows of the wide-format table
print(head(olympics_wide))


# Reshape the data from long back to wide format and sort by Country (new table name: olympics_wide_sorted)
olympics_wide_sorted <- olympics_long %>%
  pivot_wider(names_from = Medals, 
              values_from = NumberOfMedals) %>%
  select(Country, Bronze, Gold, Silver) %>%
  arrange(Country)  # Sort countries alphabetically

# Display the first few rows of the alphabetically sorted table
print(head(olympics_wide_sorted))

