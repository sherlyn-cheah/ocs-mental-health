## Section 6: Homework

# Instructions: Ask students to scrape Tables 11.5A and 11.5B from the website, 
# which contain data about the receipt of treatment among youths who reported having 
# a severe episode. Ask students to create plots and perform Chi-square tests to evaluate 
# how groups compare over time.

# to scrape web pages
library(rvest)
# to create tibbles and convert values of a column to row names
library(tibble)
# to subset and filter the data for specific groups, to replace specific values 
# with NA, rename variables, and perform functions on multiple variables
library(dplyr)
# to use and reassign data objects using the %<>% pipe operator
library(magrittr)
# to manipulate strings
library(stringr)
# to apply a function to each column of a tibble or each tibble in a list
library(purrr)
# to change the shape or format of tibbles to wide and long
library(tidyr)
# write csv files
library(readr)
# to reorder factor for plot
library(forcats)
# for prop_test
library(rstatix)

# Scrape tables from website
# webpage location of data
url <- "https://www.samhsa.gov/data/sites/default/files/cbhsq-reports/NSDUHDetailedTabs2018R2/NSDUHDetTabsSect11pe2018.htm"

# use scraper function created in Section 1
# XPath of Table 11.5A: /html/body/div[4]/div[9]/table
table11.5a <- scraper("/html/body/div[4]/div[9]/table")

# XPath of Table 11.5B: /html/body/div[4]/div[10]/table
table11.5b <- scraper("/html/body/div[4]/div[10]/table")

# save imported data into "/data/imported"
save(table11.5a, table11.5b, file = here("data", "imported", "imported_data_homework.rda"))

# wrangle data
# use dem_dataprep function created in Section 2
table11.5a <- dem_dataprep(table11.5a)
table11.5a %<>% mutate(data_type = "Treatment_for_MDES")
  
table11.5b <- dem_dataprep(table11.5b)
table11.5b %<>% mutate(data_type = "Treatment_for_MDES")

# check data preparation
tables_tocheck <- list(table11.5a, table11.5b)
tables_tocheck %>% map(data_dem_check)

# reformat data to be longer format using pivot_longer()
table11.5a %<>%
  pivot_longer(cols = contains("20"),
               names_to = "Year",
               values_to = "Number") %>%
  mutate(Year = as.numeric(Year))

table11.5b %<>%
  pivot_longer(cols = contains("20"),
               names_to = "Year",
               values_to = "Percent") %>%
  mutate(Year = as.numeric(Year))

# save wrangled data
save(table11.5a, table11.5b, file = here("data", "wrangled", "wrangled_data_homework.rda"))
# create csv files
write_csv(table11.5a, path = here("data", "wrangled", "table11.5a.csv"))
write_csv(table11.5b, path = here("data", "wrangled", "table11.5b.csv"))

# create plots
MDES_treatment_total <- table11.5a %>%
  filter(Demographic == "TOTAL") %>%
  mutate(Demographic = recode(Demographic, "TOTAL" = "Percent of MDES respondents with treatment")) %>%
  ggplot(aes(x = Year, y = Number, group = Demographic)) +
  facet_wrap( ~ Demographic) +
  geom_rect(xmin = 2012, xmax = Inf,
            ymin = -Inf, ymax = Inf,
            fill = "light gray") +
  geom_line(aes(color = Demographic), size = 1) +
  scale_x_continuous(breaks = seq(2006, 2018, by = 1),
                     labels = seq(2006, 2018, by = 1),
                     limits = c(2006, 2018)) + 
  labs(title = "The Rate of Youths Aged 12 to 17 Receiving Treatment after Reporting 
       \nHaving a Major Depressive Episode with Severe Impairment is Increasing") +
  ocs_theme() +
  theme(legend.position = "none") + 
  scale_color_manual(values = c("blue"))

# save MDES_treatment_total plot
save(MDES_treatment_total, file = here("plots", "MDES_treatment_total.rda"))
png(here("plots", "MDES_treatment_total.png"))
dev.off()  

# Rate of MDES respondents receiving treatment based on age groups
MDES_treatment_age <- table11.5b %>%
  filter(subgroup == "Age") %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) + 
  geom_line(aes(color = Demographic), size = 1) +
  scale_x_continuous(breaks = seq(2006, 2018, by = 1),
                     labels = seq(2006, 2018, by = 1),
                     limits = c(2006, 2018)) + 
  labs(title = "Receipt of Treatment for Depression among
       \nPersons Aged 12 to 17 with Major Depressive Episode with Severe Impairment",
       subtitle = "By Demographic Characteristics: Percentages, 2006-2018") + 
  facet_wrap( ~ subgroup) +
  ocs_theme() + 
  scale_color_manual(values = c(age_col_light, age_col, age_col_dark)) + 
  theme(legend.position = "none")

MDES_treatment_age <- direct.label(MDES_treatment_age, list(dl.trans(y = y + 0.50, x = x + 0.2),
                                                                "far.from.others.borders", 
                                                                cex = .8, 
                                                                fontface = c("bold"),
                                                                dl.move("Age: 14-15", y = 43),
 
                                                                                                                           dl.move("Age: 12-13", y = 36)))
# save MDES_treatment_age plot
save(MDES_treatment_age, file = here("plots", "MDES_treatment_age.rda"))
png(here("plots", "MDES_treatment_age.png"))
dev.off()  

# Rate of MDES respondents receiving treatment based on gender differences
MDES_treatment_gender <- table11.5b %>%
  filter(subgroup == "Gender") %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) + 
  geom_line(aes(color = Demographic), size = 1) +
  scale_x_continuous(breaks = seq(2006, 2018, by = 1),
                     labels = seq(2006, 2018, by = 1),
                     limits = c(2006, 2018)) + 
  labs(title = "Receipt of Treatment for Depression among
       \nPersons Aged 12 to 17 with Major Depressive Episode with Severe Impairment",
       subtitle = "By Demographic Characteristics: Percentages, 2006-2018") + 
  facet_wrap( ~ subgroup) +
  ocs_theme() + 
  scale_color_manual(values = c(female_col, male_col)) + 
  theme(legend.position = "none")

MDES_treatment_gender <- direct.label(MDES_treatment_gender, list(dl.trans(y = y + 0.50, x = x + 0.2),
                                                            "far.from.others.borders", 
                                                            cex = .8, 
                                                            fontface = c("bold")))

# save MDES_treatment_gender plot
save(MDES_treatment_gender, file = here("plots", "MDES_treatment_gender.rda"))
png(here("plots", "MDES_treatment_gender.png"))
dev.off()  

# Rate of MDES respondents receiving treatment based on racial/ethnic differences
MDES_treatment_race <- table11.5b %>%
  filter(subgroup == "Race/Ethnicity") %>%
  mutate(Demographic = fct_reorder(Demographic, Percent, tail, n = 1, .desc = TRUE)) %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) +
  geom_line(aes(color = Demographic), size = 1) +
  facet_wrap(~ subgroup) + 
  scale_x_continuous(breaks = seq(2006, 2018, by = 1),
                     labels = seq(2006, 2018, by = 1),
                     limits = c(2006, 2018)) +
  scale_color_viridis_d() +
  labs(title = "Receipt of Treatment for Depression among
       \nPersons Aged 12 to 17 with Major Depressive Episode",
       subtitle = "By Demographic Characteristics: Percentages, 2006-2018") +
  ocs_theme()

# save MDES_treatment_race plot
save(MDES_treatment_race, file = here("plots", "MDES_treatment_race.rda"))
png(here("plots", "MDES_treatment_race.png"))
dev.off()  

# data analysis - chi-squared test

# Gender and MDES Treatment Analysis (comparing 2006 and 2018)
# create contingency tables
chi_squared_11.5a <- table11.5a %>%
  filter(Year %in% c(2006, 2018)) %>%
  filter(Demographic %in% c("Male", "Female")) %>%
  mutate(Number = Number * 1000) %>%
  select(-data_type, -subgroup) %>%
  pivot_wider(names_from = Year, values_from = Number) %>%
  column_to_rownames("Demographic")

chisq.test(chi_squared_11.5a)
t(chi_squared_11.5a) %>%
  prop_test(detailed = TRUE, correct = TRUE) %>%
  glimpse()

## Output
# Pearson's Chi-squared test with Yates' continuity correction
# data:  chi_squared_11.5a
# X-squared = 1792.1, df = 1, p-value < 2.2e-16

# Rows: 1
# Columns: 13
# $ n           <dbl> 1760000
# $ n1          <dbl> 627000
# $ n2          <dbl> 1133000
# $ estimate1   <dbl> 0.2137161
# $ estimate2   <dbl> 0.2418358
# $ statistic   <dbl> 1792.079
# $ p           <dbl> 0
# $ df          <dbl> 1
# $ conf.low    <dbl> -0.02940596
# $ conf.high   <dbl> -0.0268335
# $ method      <chr> "Prop test"
# $ alternative <chr> "two.sided"
# $ p.signif    <chr> "****"

## Interpretation of results
# There appears to be an influence of gender on the rate at which youths received treatment
# for Major Depressive Episode with Severe Impairment across the two years.
# The confidence interval values suggest that there is a small difference 
# (around 3% difference in the proportion of males across the two years receiving treatment).
# The range does not cross 0, suggesting that there is a significant difference in proportions.
# Males in 2006 made up of 21% of all youths receiving treatment for MDES that year, 
# and increased to 24% in 2018.
