# This project is based on a case study "Open Case Studies: Mental Health of American Youth"
# Wright, Carrie and Ontiveros, Michael and Jager, Leah and Taub, Margaret and Hicks, 
# Stephanie C. (2020). https://github.com/opencasestudies/ocs-bp-youth-mental-health. 
# Mental Health of American Youth.

# Data is from the US National Survey on Drug Use and Health (NSDUH)

# to subset and filter the data for specific groups, to replace specific values 
# with NA, rename variables, and perform functions on multiple variables
library(dplyr)
# to use and reassign data objects using the %<>% pipe operator
library(magrittr)
# to manipulate strings
library(stringr)
# to change the shape or format of tibbles to wide and long
library(tidyr)
# to create tibbles and convert values of a column to row names
library(tibble)
# to apply a function to each column of a tibble or each tibble in a list
library(purrr)

## Section 4: Data Analysis
# Pearson's chi-squared test for independence
# Question: Is the rate of reported major depressive episodes across the two years (2004 and 2018) 
# associated with gender?
# First need to create contingency table (2x2 table) using counts dataframe
chi_squared_11.2a <- counts %>%
  filter(data_type == "Major_Depressive_Episode") %>%
  filter(Year %in% c(2004, 2018)) %>%
  filter(Demographic %in% c("Male", "Female")) %>%
  # multiply numbers by 1000 because data was indicated in thousands
  mutate(Number = Number * 1000)

# reformat table from long to wide using pivot_wider()
chi_squared_11.2a %<>% select(Demographic, Year, Number) %>%
  pivot_wider(names_from = Year, values_from = Number)

# convert Demographic columns to names of rows
chi_squared_11.2a %<>% column_to_rownames("Demographic")

# run chi-squared test for independence
chisq.test(chi_squared_11.2a)

# prop_test to test the null hypothesis that proportion of episodes is same for each year
prop_test(chi_squared_11.2a, detailed = TRUE, correct = TRUE) %>% glimpse()

# transpose contingency table (aka flip over gender and year)
t(chi_squared_11.2a)

# Chi-squared test - Gender and Severe MDE Analysis (comparing 2006 and 2018)
chi_squared_11.3a <- counts %>%
  filter(data_type == "Severe_Major_Depressive_Episode") %>%
  filter(Year %in% c(2006, 2018)) %>%
  filter(Demographic %in% c("Male", "Female")) %>%
  mutate(Number = Number * 1000) %>%
  select(-data_type, -subgroup) %>%
  pivot_wider(names_from = Year, values_from = Number) %>%
  column_to_rownames("Demographic")

chisq.test(chi_squared_11.3a)
t(chi_squared_11.3a) %>%
  prop_test(detailed = TRUE, correct = TRUE) %>%
  glimpse()

# Chi-squared test - Gender and Treatment Analysis (comparing 2004 and 2018)
chi_squared_11.4a <- counts %>%
  filter(data_type == "Treatment") %>%
  filter(Year %in% c(2004, 2018)) %>%
  filter(Demographic %in% c("Male", "Female")) %>%
  mutate(Number = Number * 1000) %>%
  select(-data_type, -subgroup) %>%
  pivot_wider(names_from = Year, values_from = Number) %>%
  column_to_rownames("Demographic")

chisq.test(chi_squared_11.4a)
t(chi_squared_11.4a) %>%
  prop_test(detailed = TRUE, correct = TRUE) %>%
  glimpse()

