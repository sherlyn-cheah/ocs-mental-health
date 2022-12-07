## Section 2: Wrangling data

# to easily load and save data
library(here)
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
# write csv files
library(readr)

# convert df into tibble
table11.1a %>%
  as_tibble()

# create object for legend
legend11.1a <- table11.1a %>%
  as_tibble() %>%
  select('2004') %>%
  tail(n=1)

# use slice function to select everything other than legend (1st row to 2nd last (n-1) row) to remove it
# using double pipe operator to reassign input
table11.1a %<>%
  as_tibble() %>%
  slice(1:(n()-1))

# replace values of *, --, da, nc, nr into NA
table11.1a %<>%
  na_if("*") %>%
  na_if("--") %>%
  na_if("da") %>%
  na_if("nc") %>%
  na_if("nr")

# rename first column (new name = old name)
table11.1a %<>%
  rename(MHS_setting = 'Setting Where Mental Health ServiceWas Received')

# remove digits, "\r\n"s and punctuation marks from MHS_setting column
table11.1a %<>%
  mutate(MHS_setting = str_remove_all(MHS_setting, "[:digit:]|\r\n|[:punct:]"))

# remove extra spaces from MHS_setting column and replace with single space
table11.1a %<>%
  mutate(MHS_setting = str_replace_all(MHS_setting, "[:blank:]{1,}", " "))

# remove "a"s and commas from all columns except MHS_setting
table11.1a %<>%
  mutate(across(.cols= -MHS_setting, str_remove_all, "a|," ))

# change all number strings to be numeric
table11.1a %<>%
  mutate(across(.cols= -MHS_setting, as.numeric))

# add type and subtype variables
table11.1a %<>%
  mutate(type = c(rep("Specialty", 9), rep("Nonspecialty", 11))) %>%
  mutate(subtype = c("Specialty_total", rep("Outpatient", 5), rep("Inpatient", 3),
                     "Nonspecialty_total", rep("Education", 3), rep("General_medicine", 2),
                     rep("Juvenile_justice", 2), rep("Child_welfare", 2), "combination"))

# add variable with shorter label names for plots and statistical output
table11.1a %<>%
  mutate(short_label = c("Specialty total", "Outpatient total", "Therapist", "Clinic",
                         "Day program", "In-home therapist", "Inpatient total", "Hospital",
                         "Residential center", "Nonspecialty total", "School total",
                         "School therapist", "School program", "General medicine",
                         "Family doctor", "Justice system", "Justice system", "Welfare",
                         "Foster care", "Specialty combination"))

# remove empty rows (General Medicine, Juvenile Justice and Child Welfare)
table11.1a %<>%
  filter(MHS_setting!= "General Medicine" | MHS_setting!= "Juvenile Justice" |
           MHS_setting!= "Child Welfare")

# make table "longer" by presenting year in a column and each value in another column ("Number")
table11.1a %<>%
  pivot_longer(cols = contains("20"), # indicating all the years (which start with "20-")
               names_to = "Year", # label for "Year" column
               values_to = "Number") %>% # label for values column
  mutate(Year = as.numeric(Year)) # convert Year into numeric values

# create function for tables 11.1-
tbl11.1_dataprep <- function(TABLE, new_col, pivot_col){ # table input, new name of first column, name of pivot column
  # convert to tibble
  as_tibble(TABLE) %>% 
    # remove legend
    slice(1:(n()-1)) %>% 
    # replace with NAs
    na_if("*") %>% 
    na_if("--") %>%
    na_if("da") %>%
    na_if("nc") %>%
    na_if("nr") %>%
    # rename first col; ":=" operator allows expressions at both sides
    rename({{new_col}} := names(.)[1]) %>% 
    # remove digits, \r\n, punctuation
    mutate({{new_col}} := str_remove_all(pull(.,{{new_col}}), "[:digit:]|\r\n|[:punct:]")) %>% # need to add pull to run through all rows in "new_col"
    # replace extra spaces
    mutate({{new_col}} := str_replace_all(pull(.,{{new_col}}), "[:blank:]{1,}", " ")) %>% 
    # remove "a"s and commas
    mutate(across(.cols= -{{new_col}}, str_remove_all, "a|," )) %>% 
    # convert values to numeric
    mutate(across(.cols= -{{new_col}}, as.numeric)) %>% 
    # create type and subtype variables
    mutate(type = c(rep("Specialty", 9), rep("Nonspecialty", 11))) %>% 
    mutate(subtype = c("Specialty_total", rep("Outpatient", 5), rep("Inpatient", 3),
                       "Nonspecialty_total", rep("Education", 3), rep("General_medicine", 2),
                       rep("Juvenile_justice", 2), rep("Child_welfare", 2), "combination")) %>%
    # create short label variable
    mutate(short_label = c("Specialty total", "Outpatient total", "Therapist", "Clinic", 
                           "Day program", "In-home therapist", "Inpatient total", "Hospital",
                           "Residential center", "Nonspecialty total", "School total",
                           "School therapist", "School program", "General medicine",
                           "Family doctor", "Justice system", "Justice system", "Welfare",
                           "Foster care", "Specialty combination")) %>%
    # remove empty rows (only keep rows with NAs less than number of total columns in a row)
    filter(rowSums(is.na(select(., is.numeric))) < length(select(., is.numeric))) %>%
    # pivot table into longer format
    pivot_longer(cols = contains("20"), names_to = "Year", values_to = pivot_col) %>%
    # convert "Year" values into numeric
    mutate(Year = as.numeric(Year))
}

# use tbl11.1_dataprep function on table11.1b
table11.1b <- tbl11.1_dataprep(table11.1b, "MHS_setting", "Percent")

# modify function for demographic tables
dem_dataprep <- function(TABLE){
  # convert to tibble
  as_tibble(TABLE) %>%
    # remove legend
    slice(1:(n()-1)) %>% 
    # replace "*" and " " with NAs
    na_if("*") %>% 
    na_if("") %>%
    # rename first column to "Demographic"
    rename(Demographic := names(.)[1]) %>%
    # add "Age:-" for age categories by replacing "1" with "Age: 1"
    mutate(Demographic := str_replace(string = Demographic, pattern = "1", 
                                      replacement = "Age: 1")) %>%
    # add subgroup variables
    mutate(subgroup = c("Total", rep("Age", 4), rep("Gender", 3), rep("Race/Ethnicity", 9))) %>%
    # remove "a"s and commas from all columns with "20-"
    mutate(across(.cols= contains("20"), str_remove_all, "a|," )) %>%
    # change all number strings to be numeric
    mutate(across(.cols= contains("20"), as.numeric)) %>%
    # remove empty rows (only keep rows with NAs less than number of total columns in a row)
    filter(rowSums(is.na(select(., is.numeric))) < length(select(., is.numeric)))
}

# wrangle demographic tables
table11.2a <- dem_dataprep(table11.2a)
# add column for each table to describe data for merging purposes
table11.2a %<>% mutate(data_type = "Major_Depressive_Episode")

table11.2b <- dem_dataprep(table11.2b)
table11.2b %<>% mutate(data_type = "Major_Depressive_Episode")

table11.3a <- dem_dataprep(table11.3a)
table11.3a %<>% mutate(data_type = "Severe_Major_Depressive_Episode")

table11.3b <- dem_dataprep(table11.3b)
table11.3b %<>% mutate(data_type = "Severe_Major_Depressive_Episode")

table11.4a <- dem_dataprep(table11.4a)
table11.4a %<>% mutate(data_type = "Treatment")

table11.4b <- dem_dataprep(table11.4b)
table11.4b %<>% mutate(data_type = "Treatment")

# Check data for demographic tables (results output as tibble)
data_dem_check <- function(TABLE) {
  # check if data is tibble
  results<- tibble(tibble_check = case_when(is_tibble(TABLE) ~ "Good!", 
                                            TRUE ~ "Not a tibble"),
                   # check if legend (last row) is removed 
                   legend_check = case_when(TABLE %>% slice(n()) %>% pull('2018') %>%
                                              str_detect(pattern = "-- = not available") ~ "Legend might still be there",
                                            TRUE ~ "Good!"),
                   # check if all empty values have been converted to NA
                   na_check = case_when(any(str_detect(TABLE, pattern = "nc|\\*|--")) ~ "NA not fixed",
                                        TRUE ~ "Good!"),
                   # check if first variable is labeled as 'Demographic"
                   demo_label_check = case_when(names(TABLE)[1] == "Demographic" ~ "Good!",
                                                TRUE ~ "check first column"),
                   # check if extra white spaces (>1 space) have been removed
                   whitespace_check = case_when(any(str_detect(TABLE, pattern = "[:blank:]{2,}")) ~ "white spaces not fixed",
                                                TRUE ~ "Good!"),
                   # check if all age variables start with "Age:-" instead of "1-"
                   age_var_check = case_when(any(str_detect(pull(TABLE, Demographic), pattern = "^1")) ~ "Age data not fixed",
                                             TRUE ~ "Good!"),
                   # check if subgroup variable has been succesfully added
                   subgroup_check = case_when(any(names(TABLE) == "subgroup") ~ "Good!",
                                              TRUE ~ "No subgroup variable found!"),
                   # check if all "a" and "," has been removed from year values
                   year_val_check = case_when(TABLE %>% select(-Demographic, -subgroup, -data_type) %>%
                                                # map_df function preserves the df structure; creates df of TRUEs and FALSEs
                                                map_df(~str_detect(.x, pattern = "a|,")) %>%
                                                # if row is TRUE = 1; FALSE = 0
                                                rowSums(na.rm = TRUE) %>%
                                                # sum should be zero when all "a" and "," has been removed (aka no TRUEs)
                                                sum() == 0 ~ "Good!",
                                              TRUE ~ "There may be commas or 'a's in the year columns."),
                   # check if year variables are numeric
                   # map_dbl outputs double vectors
                   year_num_check = case_when(sum(map_dbl(TABLE, is.numeric)) == sum(str_count(names(TABLE), "20")) ~ "Good!",
                                              TRUE ~ "Variables are not numeric!"),
                   # check if all NA rows have been removed (filter out rows with all NAs -> calculate number of NA rows)
                   year_na_check = case_when(nrow(TABLE %>% select(-Demographic, -subgroup, -data_type) %>%
                                                    filter(rowSums(is.na(select(., is.numeric))) == length(select(., is.numeric)))) > 0 ~ "There are empty rows.",
                                             TRUE ~ "Good!"))
  
  ifelse(all(results == "Good!"),
         "Data looks good!", glimpse(results))
}

# "confirm fail" object to test if tibble check works
test_that_should_fail <- c(1,2,3)

# check all of the wrangled demographic tibbles using map() function from purrr
tables_tocheck <- list(table11.2a, table11.2b, table11.3a, table11.3b, table11.4a, table11.4b)
tables_tocheck %>% map(data_dem_check)

# combine demographic tibbles (count data/table a and percentage data/table b)
# bind_rows appends the tables together
counts <- bind_rows(table11.2a, table11.3a, table11.4a)
percents <- bind_rows(table11.2b, table11.3b, table11.4b)
# distinct function to check if all data types are now in the combined tibbles
counts %>% distinct(data_type)
percents %>% distinct(data_type)

# reformat combined demographic data to be longer format using pivot_longer()
counts %<>%
  pivot_longer(cols = contains("20"),
               names_to = "Year",
               values_to = "Number") %>%
  mutate(Year = as.numeric(Year))

percents %<>%
  pivot_longer(cols = contains("20"),
               names_to = "Year",
               values_to = "Percent") %>%
  mutate(Year = as.numeric(Year))

# replace abbreviations (AIAN, NHOPI) to their long forms
counts %<>% mutate(Demographic = str_replace(string = Demographic,
                                             pattern = "AIAN",
                                             replacement = "American Indian and Alaska Native"))
counts %<>% mutate(Demographic = str_replace(string = Demographic,
                                             pattern = "NHOPI",
                                             replacement = "Native Hawaiian or Other Pacific Islander"))
percents %<>% mutate(Demographic = str_replace(string = Demographic,
                                               pattern = "AIAN",
                                               replacement = "American Indian and Alaska Native"))
percents %<>% mutate(Demographic = str_replace(string = Demographic,
                                               pattern = "NHOPI",
                                               replacement = "Native Hawaiian or Other Pacific Islander"))

# save wrangled data
save(table11.1a, table11.1b, counts, percents, file = here("data", "wrangled", "wrangled_data.rda"))
# create csv files
write_csv(table11.1a, path = here("data", "wrangled", "table11.1a.csv"))
write_csv(table11.1b, path = here("data", "wrangled", "table11.1b.csv"))
write_csv(counts, path = here("data", "wrangled", "counts.csv"))
write_csv(percents, path = here("data", "wrangled", "percents.csv"))

