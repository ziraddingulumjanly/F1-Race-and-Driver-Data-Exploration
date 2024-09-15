#ZIRADDIN GULUMJANLI 2024
#Problem 1 Homework 2
#------------------------------------------------------------------------------

# import the required libraries 
library(tidyverse)

# IMPORTANT NOTE: ⚠️ Adjust based on your environment 
setwd("C:\\Users\\zirad\\Downloads") 

# read csv file using read_csv
olympics <- read_csv("Olympics2021Medals.csv")
olympics

# Problem 4
olympics_long <- olympics %>%
  pivot_longer(cols = Gold:Bronze, 
               names_to = "Medals", 
               values_to = "NumberOfMedals")

# Problem 5
print(head(olympics_long))

olympics_wide <- olympics_long %>%
  pivot_wider(names_from = Medals, 
              values_from = NumberOfMedals) %>%
  select(Country, Bronze, Gold, Silver)

print(head(olympics_wide))

# Thank You

