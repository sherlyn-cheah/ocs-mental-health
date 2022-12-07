## Section 3: Data Visualisation

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
