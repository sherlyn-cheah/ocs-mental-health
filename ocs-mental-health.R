# This project is based on a case study "Open Case Studies: Mental Health of American Youth"
# Wright, Carrie and Ontiveros, Michael and Jager, Leah and Taub, Margaret and Hicks, 
# Stephanie C. (2020). https://github.com/opencasestudies/ocs-bp-youth-mental-health. 
# Mental Health of American Youth.

# Data is from the US National Survey on Drug Use and Health (NSDUH)

# to easily load and save data
library(here)
# to scrape web pages
library(rvest)
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
# to create plots
library(ggplot2)
# to add labels directly to lines in plots
library(directlabels)
# to get the current linetype options
library(scales)
# to reorder factor for plot
library(forcats)
# to create a plot to see what the different linetypes look like
library(ggthemes)
# to perform proportion test
library(rstatix)
# to combine plots together
library(cowplot)
# to access and download OCS data files
library(OCSdata)
# write csv files
library(readr)


## Section 1: Scraping data
# webpage location of data
url <- "https://www.samhsa.gov/data/sites/default/files/cbhsq-reports/NSDUHDetailedTabs2018R2/NSDUHDetTabsSect11pe2018.htm"

# Table 11.1A
# XPath of Table 11.1A: /html/body/div[4]/div[1]/table
table11.1a <- url %>%
  read_html() %>%
  html_nodes(xpath= '/html/body/div[4]/div[1]/table') %>%
  html_table()
# output is a list, so need to extract element out of list using code below
table11.1a <- table11.1a[[1]]

# to scrap multiple tables w/o c+p code repeatedly, write a function "scraper" with XPATH input
scraper <- function(XPATH){
  url <- "https://www.samhsa.gov/data/sites/default/files/cbhsq-reports/NSDUHDetailedTabs2018R2/NSDUHDetTabsSect11pe2018.htm"
  
  table <- url %>%
    read_html() %>%
    html_nodes(xpath = XPATH) %>%
    html_table()
  output <- table[[1]]
  output
}

# continue scraping using "scraper" function
# Table 11.1B
table11.1b <- scraper("/html/body/div[4]/div[2]/table")

# Table 11.2A
table11.2a <- scraper("/html/body/div[4]/div[3]/table")

# Table 11.2B
table11.2b <- scraper("/html/body/div[4]/div[4]/table")

# Table 11.3A
table11.3a <- scraper("/html/body/div[4]/div[5]/table")

# Table 11.3B
table11.3b <- scraper("/html/body/div[4]/div[6]/table")

# Table 11.4A
table11.4a <- scraper("/html/body/div[4]/div[7]/table")

# Table 11.4B
table11.4b <- scraper("/html/body/div[4]/div[8]/table")

#save imported data into "/data/imported"
save(table11.1a, table11.1b, table11.2a, table11.2b, table11.3a, table11.3b,
     table11.4a, table11.4b, file = here("data", "imported", "imported_data.rda"))

## Section 2: Wrangling data
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

## Section 3: Data Visualisation
# Rate of Major Depressive Episodes (MDE) among youths across time in various demographic groups
percents %>%
  filter(data_type == "Major_Depressive_Episode") %>%
  ggplot(aes(x = Year, y = Percent, color = Demographic)) +
  geom_line(size = 1) +
  labs(title = "Major Depressive Episode among Persons Aged 12 to 17",
       subtitle = "By Demographic Characteristics, Percentages, 2004-2018") +
  theme(legend.position = "bottom")

# plot above is difficult to read, breakdown to different presentations
# Total rate across time
MDE_total <- percents %>%
  # filter out only TOTAL values from Demographic
  filter(data_type == "Major_Depressive_Episode", Demographic == "TOTAL") %>%
  # rename TOTAL for label on strip of text to be added above the plot
  mutate(Demographic = recode(Demographic, "TOTAL" = "Percent of respondents with MDE")) %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) +
  # add strip of text above plot; facet_wrap() typically used to create subplots
  facet_wrap( ~ Demographic) + 
  # create light gray rectangle to highlight area from 2011 onwards
  geom_rect(xmin = 2011, xmax = Inf,
            ymin = -Inf, ymax = Inf,
            fill = "light gray") +
  geom_line(aes(color = Demographic), size = 1.5) +
  # modify x axis scale to breakdown and display all years 
  scale_x_continuous(breaks = seq(2004, 2018, by = 1),
                     labels = seq(2004, 2018, by = 1),
                     limits = c(2004, 2018)) + 
  labs(title = "The Rate of Youths Aged 12 to 17 Reporting Having a
       \nMajor Depressive Episode (MDE) is Increasing") +
  # changes to classic theme with x and y axis lines and no gridlines
  theme_classic() + 
  # change angle of x axis labels
  theme(axis.text.x = element_text(angle = 90), 
        # remove legend
        legend.position = "none",
        # modify appearance of background and text of strip above plot
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(face = "bold",
                                  size = 14,
                                  color = "white")) +
  # change colour of line
  scale_color_manual(values = c("blue"))

# save plot (RDA file)
save(MDE_total, file = here("plots", "MDE_total.rda"))
# save plot (PNG file)
png(here("plots", "MDE_total.png"))
# dev.off() function to close graphical device and create new plot
dev.off()

# create theme for future similar plots
ocs_theme <- function() {
  theme_classic() +
    theme(axis.text.x = element_text(angle = 90),
          strip.background = element_rect(fill = "black"),
          strip.text = element_text(face = "bold",
                                    size = 14,
                                    color = "white"))
}

# MDE rate based on age and gender differences
MDE_age_gender <- percents %>%
  filter(data_type == "Major_Depressive_Episode",
         # filter out race and ethnicity subgroup
         subgroup != "Race/Ethnicity",
         # filter out TOTAL values
         Demographic != "TOTAL") %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) +
  # create light blue rectangle to highlight area from 2011 onwards
  geom_rect(xmin = 2011, xmax = Inf,
            ymin = -Inf, ymax = Inf,
            fill = "light blue") +
  geom_line(aes(color = Demographic), size = 1) + 
  scale_x_continuous(breaks = seq(2004, 2018, by = 1),
                     labels = seq(2004, 2018, by = 1),
                     limits = c(2004, 2018)) + 
  labs(title = "Major Depressive Episode among Persons Aged 12 to 17",
       subtitle = "By Demographic Characteristics, Percentages, 2004-2018") + 
  # create subplots based on subgroups
  facet_wrap( ~ subgroup) +
  # use theme function that we have created earlier for this plot
  ocs_theme()
  
# add labels to lines on plot
MDE_age_gender <- direct.label(MDE_age_gender, 
                               # dl.trans() used to move labels
                               list(dl.trans(y = y + 0.35,
                                             x = x - 0.1),
                                    # to prevent labels from overlapping one another
                                    "far.from.others.borders",
                                    # size of labels
                                    cex = .8,
                                    fontface = c("bold"),
                                # to move only the "14-15" label
                               dl.move("Age: 14-15", x = 2007, y = 9.7)))


# view colour palette (change number to view more color options)
show_col(hue_pal()(6))

# save colors for different age and gender groups
age_col_light <- c("#B79F00")
age_col <- c("#6BB100")
age_col_dark <- c("#00BD5F")
female_col <- c("#F564E3")
male_col <- c("#619CFF")

# change color of lines by listing colors in order as they appear in the table
MDE_age_gender <- MDE_age_gender +
  scale_color_manual(values = c(age_col_light, age_col, age_col_dark, female_col, male_col))

# save MDE_age_gender plot
# save plot (RDA file)
save(MDE_age_gender, file = here("plots", "MDE_age_gender.rda"))
# save plot (PNG file)
png(here("plots", "MDE_age_gender.png"))
# dev.off() function to close graphical device and create new plot
dev.off()

# MDE rate based on racial/ethnic differences
MDE_race <- percents %>%
  filter(data_type == "Major_Depressive_Episode",
         subgroup == "Race/Ethnicity",
         # removing NHOPI because there's not enough data points to create line graph
         Demographic != "Native Hawaiian or Other Pacific Islander") %>%
  # reorder legend based on last value (tail) of Percent variable for each Demographic group
  mutate(Demographic = fct_reorder(Demographic, Percent, tail, n = 1, .desc = TRUE)) %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) +
  geom_rect(xmin = 2011, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "light gray") +
  geom_line(aes(color = Demographic), size = 1) +
  facet_wrap(~ subgroup) + 
  scale_x_continuous(breaks = seq(2004, 2018, by = 1),
                     labels = seq(2004, 2018, by = 1),
                     limits = c(2004, 2018)) +
  # viridis color palettes are more discernible for individuals with color-blindness
  scale_color_viridis_d() +
  labs(title = "Major Depressive Episode\namong Persons Aged 12 to 17",
       subtitle = "By Demographic Characteristics, Percentage, 2004-2018") +
  ocs_theme()

# save MDE_race plot
# save plot (RDA file)
save(MDE_race, file = here("plots", "MDE_race.rda"))
# save plot (PNG file)
png(here("plots", "MDE_race.png"))
# dev.off() function to close graphical device and create new plot
dev.off()
  
# Rate of Youths reporting having MDE with severe impairment (total)
MDES_total <- percents %>%
  filter(data_type == "Severe_Major_Depressive_Episode",
         subgroup == "Total") %>%
  mutate(Demographic = recode(Demographic, "TOTAL" = "Percent of respondents with Severe MDE")) %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) +
  facet_wrap( ~ Demographic) +
  geom_rect(xmin = 2011, xmax = Inf,
            ymin = -Inf, ymax = Inf,
            fill = "light gray") +
  geom_line(aes(color = Demographic), size = 1) + 
  scale_x_continuous(breaks = seq(2006, 2018, by = 1),
                     labels = seq(2006, 2018, by = 1),
                     limits = c(2006, 2018)) +
  labs(title = "Major Depressive Episode with Severe Impairment\namong Persons Aged 12 to 17",
       subtitle = "By Demographic Characteristics, Percentages, 2006-2018") +
  ocs_theme() +
  theme(legend.position = "none") + 
  scale_color_manual(values = c("blue"))

# MDES rate based on age groups and gender differences
MDES_age_gender <- percents %>%
  filter(data_type == "Severe_Major_Depressive_Episode",
         subgroup != "Total",
         subgroup != "Race/Ethnicity") %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) + 
  geom_rect(xmin = 2011, xmax = Inf,
            ymin = -Inf, ymax = Inf,
            fill = "light gray") + 
  geom_line(aes(color = Demographic), size = 1) +
  scale_x_continuous(breaks = seq(2006, 2018, by = 1),
                     labels = seq(2006, 2018, by = 1),
                     limits = c(2006, 2018)) + 
  labs(title = "Major Depressive Episode with Severe Impairment\namong Persons Aged 12 to 17",
       subtitle = "By Demographic Characteristics, Percentages, 2006-2018") + 
  facet_wrap( ~ subgroup) +
  ocs_theme() + 
  scale_color_manual(values = c(age_col_light, age_col, age_col_dark, female_col, male_col)) + 
  theme(legend.position = "none")

MDES_age_gender <- direct.label(MDES_age_gender, list(dl.trans(y = y + 0.35,
                                                               x = x - 0.1),
                                                      "far.from.others.borders", 
                                                      cex = .8, 
                                                      fontface = c("bold")))

# MDES rate based on racial/ethnic differences
MDES_race <- percents %>%
  filter(data_type == "Severe_Major_Depressive_Episode",
         subgroup == "Race/Ethnicity") %>%
  mutate(Demographic = fct_reorder(Demographic, Percent, tail, n = 1, .desc = TRUE)) %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) +
  geom_rect(xmin = 2011, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "light gray") +
  geom_line(aes(color = Demographic), size = 1) +
  facet_wrap(~ subgroup) + 
  scale_x_continuous(breaks = seq(2006, 2018, by = 1),
                     labels = seq(2006, 2018, by = 1),
                     limits = c(2006, 2018)) +
  scale_color_viridis_d() +
  labs(title = "Major Depressive Episode with Severe Impairment\namong Persons Aged 12 to 17",
       subtitle = "By Demographic Characteristics: Percentages, 2006-2018") +
  ocs_theme()

# Rate of people reporting having an MDE and received treatment for depression (total)
treatment_total <- percents %>%
  filter(data_type == "Treatment",
         subgroup == "Total") %>%
  mutate(Demographic = recode(Demographic, "TOTAL" = "Percent of MDE respondents with treatment")) %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) +
  facet_wrap( ~ Demographic) +
  geom_rect(xmin = 2011, xmax = Inf,
            ymin = -Inf, ymax = Inf,
            fill = "light gray") +
  geom_line(aes(color = Demographic), size = 1) + 
  scale_x_continuous(breaks = seq(2004, 2018, by = 1),
                     labels = seq(2004, 2018, by = 1),
                     limits = c(2004, 2018)) +
  labs(title = "The Rate of Youths Aged 12 to 17 Receiving Treatment after
       \nReporting Having a Major Depressive Episode is Increasing") +
  ocs_theme() +
  theme(legend.position = "none") + 
  scale_color_manual(values = c("blue"))

# save treatment_total plot
# save plot (RDA file)
save(treatment_total, file = here("plots", "treatment_total.rda"))
# save plot (PNG file)
png(here("plots", "treatment_total.png"))
# dev.off() function to close graphical device and create new plot
dev.off()

# Rate of MDE respondents receiving treatment based on age groups and gender differences
treatment_age_gender <- percents %>%
  filter(data_type == "Treatment",
         subgroup != "Total",
         subgroup != "Race/Ethnicity") %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) + 
  geom_line(aes(color = Demographic), size = 1) +
  scale_x_continuous(breaks = seq(2004, 2018, by = 1),
                     labels = seq(2004, 2018, by = 1),
                     limits = c(2004, 2018)) + 
  labs(title = "Receipt of Treatment for Depression among
       \nPersons Aged 12 to 17 with Major Depressive Episode",
       subtitle = "By Demographic Characteristics: Percentages, 2004-2018") + 
  facet_wrap( ~ subgroup) +
  ocs_theme() + 
  scale_color_manual(values = c(age_col_light, age_col, age_col_dark, female_col, male_col)) + 
  theme(legend.position = "none")

treatment_age_gender <- direct.label(treatment_age_gender, list(dl.trans(y = y + 0.40,
                                                               x = x - 0.2),
                                                      "far.from.others.borders", 
                                                      cex = .8, 
                                                      fontface = c("bold"),
                                                      dl.move("Age: 14-15", x = 2015, y = 38),
                                                      dl.move("Age: 12-13", x = 2015, y = 30)))

# Rate of MDE respondents receiving treatment based on racial/ethnic differences
treatment_race <- percents %>%
  filter(data_type == "Treatment",
         subgroup == "Race/Ethnicity") %>%
  mutate(Demographic = fct_reorder(Demographic, Percent, tail, n = 1, .desc = TRUE)) %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) +
  geom_line(aes(color = Demographic), size = 1) +
  facet_wrap(~ subgroup) + 
  scale_x_continuous(breaks = seq(2009, 2018, by = 1),
                     labels = seq(2009, 2018, by = 1),
                     limits = c(2009, 2018)) +
  scale_color_viridis_d() +
  labs(title = "Receipt of Treatment for Depression among
       \nPersons Aged 12 to 17 with Major Depressive Episode",
       subtitle = "By Demographic Characteristics: Percentages, 2009-2018") +
  ocs_theme()

# Settings where youths are receiving treatment (total categories)
plotMHS <- table11.1b %>%
  # filter all the totals for each category (has been indicated with "xxx total" in short_label)
  filter(str_detect(short_label, "total")) %>%
  ggplot(aes(x = Year, y = Percent, group = MHS_setting, color = short_label)) + 
  geom_line(size = 1) + 
  # splitting subcategory plots for specialty and non-specialty
  facet_wrap(~ type) + 
  scale_x_continuous(breaks = seq(2009, 2018, by = 1),
                     labels = seq(2009, 2018, by = 1),
                     limits = c(2009, 2018)) +
  labs(title = "Settings Where Mental Health Services Were Received
       \namong Persons Aged 12 to 17",
       subtitle = "Percentages, 2009-2018") +
  ocs_theme()

plotMHS <- direct.label(plotMHS, list(dl.trans(y = y + 0.35, x = x - 0.1),
                                      "far.from.others.borders",
                                      cex = .8,
                                      dl.move("Outpatient total", x = 2015, y = 11),
                                      dl.move("Nonspecialty total", y = 15.5)))

# Settings where youths are receiving treatment (subcategories)
plotMHSS <- table11.1b %>%
  # adding ! in front filters categories that have not been indicated with "xxx total" in short_label
  filter(!str_detect(short_label, "total")) %>%
  ggplot(aes(x = Year, y = Percent, group = MHS_setting, color = short_label)) + 
  geom_line(size = 1) + 
  facet_wrap(~ type) + 
  scale_x_continuous(breaks = seq(2002, 2018, by = 1),
                     labels = seq(2002, 2018, by = 1),
                     limits = c(2002, 2018)) +
  labs(title = "Settings Where Mental Health Services Were Received
       \namong Persons Aged 12 to 17",
       subtitle = "Percentages, 2002-2018") +
  ocs_theme()

plotMHSS <- direct.label(plotMHSS, list(dl.trans(y = y + 0.3),
                                        "far.from.others.borders",
                                        cex = .8,
                                        dl.move("School therapist", 2010, 10),
                                        dl.move("Foster care", 2010, 1),
                                        dl.move("Therapist", 2009, 10.5)))

# Show different line types available for scale_linetype_manual()
show_linetypes(linetype_pal()(12), labels = TRUE)

# overall outcomes by gender and total
gender_outcomes <- percents %>%
  # %in% is used to identify if an element belongs to a vector/df
  filter(Demographic %in% c("Male", "Female", "TOTAL")) %>%
  ggplot(aes(x = Year, y = Percent, color = Demographic)) +
  geom_line(aes(linetype = data_type), size = 1) +
  # defining the line types for each data_type category
  scale_linetype_manual(values = c("solid", "2262", "13")) +
  # defining the line colours for each Demographic category
  scale_color_manual(values = c(female_col, male_col, "black")) +
  scale_x_continuous(breaks = seq(2004, 2018, by = 1),
                     labels = seq(2004, 2018, by = 1),
                     limits = c(2004, 2018)) +
  labs(title = "Major Depressive Episodes and Treatment Among Persons Aged 12 to 17",
       subtitle = "By Demographic Characteristics, Percentages, 2004-2018") +
  facet_wrap(~ Demographic, strip.position = "top") + 
  ocs_theme() + 
  # element_blank() to remove legend title
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  # remove color in legend only
  guides(color = "none")
  
# overall outcomes by age group
age_outcomes <- percents %>%
  filter(Demographic %in% c("Age: 12-13", "Age: 14-15", "Age: 16-17")) %>%
  ggplot(aes(x = Year, y = Percent, color = Demographic)) +
  geom_line(aes(linetype = data_type), size = 1) +
  scale_linetype_manual(values = c("solid", "2262", "13")) +
  scale_color_manual(values = c(age_col_light, age_col, age_col_dark)) +
  scale_x_continuous(breaks = seq(2004, 2018, by = 1),
                     labels = seq(2004, 2018, by = 1),
                     limits = c(2004, 2018)) +
  labs(title = "Major Depressive Episode\namong Persons Aged 12 to 17",
       subtitle = "By Demographic Characteristics, Percentages, 2004-2018") +
  facet_wrap(~ Demographic, strip.position = "top") + 
  ocs_theme() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  guides(color = "none")

# overall outcomes by race/ethnic groups
race_outcomes <- percents %>%
  filter(subgroup == "Race/Ethnicity", Demographic != "Native Hawaiian or Other Pacific Islander") %>%
  ggplot(aes(x = Year, y = Percent, color = Demographic)) +
  geom_line(aes(linetype = data_type), size = 1) +
  scale_linetype_manual(values = c("solid", "2262", "13")) +
  scale_x_continuous(breaks = seq(2004, 2018, by = 1),
                     labels = seq(2004, 2018, by = 1),
                     limits = c(2004, 2018)) +
  labs(title = "Major Depressive Episode\namong Persons Aged 12 to 17",
       subtitle = "By Demographic Characteristics, Percentages, 2004-2018") +
  # nrow to organise subcategories in 2*4 grid
  facet_wrap(~ Demographic, strip.position = "top", nrow = 4) + 
  ocs_theme() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  guides(color = "none")

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

