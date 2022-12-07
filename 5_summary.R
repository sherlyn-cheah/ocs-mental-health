## Section 5: Summary

# to combine plots together
library(cowplot)
# to create plots
library(ggplot2)
# to easily load and save data
library(here)
# to subset and filter the data for specific groups, to replace specific values 
# with NA, rename variables, and perform functions on multiple variables
library(dplyr)
# to add labels directly to lines in plots
library(directlabels)

## Create Summary Plot
# ggdraw() can be used to create labels and other plot aspects to add on existing plots

# Create Title Element
title_plots <- ggdraw() +
  draw_label("Self-Reported Depression Among American Youths",
             fontface = "bold",
             size = 18,
             x = 0,
             # hjust = horizontal justification
             hjust = -0.01)

# Create subtitle element
subtitle_plots <- ggdraw() +
  draw_label("The percentage of youths (age 12-17) experiencing major depressive episode (MDE) has increased
              \n since 2011. Of these youths, the percentage receiving treatment for depression has also
              \n increased but remains limited to less than 42%.",
             size = 13,
             x = 0,
             hjust = -0.01)

# modify existing plots
MDE_total_for_mp <- MDE_total +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank(),
        # change colour of scale labels on axes 
        axis.text = element_text(color = "black"))

treatment_for_mp <- treatment_total +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank(),
        axis.text = element_text(color = "black"))

MDE_age_gender_for_mp <- MDE_age_gender +
  theme(plot.title = element_text(size = 14, color = "black"),
        plot.subtitle = element_blank(),
        axis.text = element_text(color = "black")) +
  labs(title = "Older youths and females report MDE at the highest rates
       \nand show the steepest increase")

# legend to be removed from MDE_race plot to avoid shrinking
MDE_race_for_mp_leg <- MDE_race +
  theme(plot.title = element_text(size = 14, color = "black"),
        plot.subtitle = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 14)) +
  labs(title = "All racial/ethnic groups show similar\nincreases since 2011") +
  guides(color = guide_legend(ncol = 2))

legend <- get_legend(MDE_race_for_mp_leg +
                       theme(legend.justification = "right"))

MDE_race_for_mp <- MDE_race_for_mp_leg +
  theme(legend.position = "none") 

# putting the plots together
# make rows of plots
row_1 <- plot_grid(MDE_total_for_mp, treatment_for_mp, nrow = 1)
# rel_width to modify relative column width
row_2 <- plot_grid(MDE_age_gender_for_mp, MDE_race_for_mp, nrow = 1, rel_widths = c(1, 0.6))

# saving main plot
png(filename = here("img", "mainplot_org.png"), res = 300, width = 10, height = 10, units = "in")
plot_grid(title_plots, subtitle_plots, row_1, row_2, legend, ncol = 1,
          rel_heights = c(0.1, 0.3, 0.8, 1, 0.3))
dev.off()

# make two separate plots for age and gender to look similar to the total and treatment plots
MDE_age <- percents %>%
  filter(data_type == "Major_Depressive_Episode",
         subgroup == "Age") %>%
  # recode text for strip above plot
  mutate(subgroup = recode(subgroup, "Age" = "Percent of each age group reporting MDE")) %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) +
  geom_rect(xmin = 2011, xmax = Inf,
            ymin = -Inf, ymax = Inf,
            fill = "light gray") +
  geom_line(aes(color = Demographic), size = 1) + 
  scale_x_continuous(breaks = seq(2004, 2018, by = 1),
                     labels = seq(2004, 2018, by = 1),
                     limits = c(2004, 2018)) + 
  facet_wrap( ~ subgroup) +
  ocs_theme() +
  scale_color_manual(values = c(age_col_light, age_col, age_col_dark))

MDE_age <- direct.label(MDE_age, list(dl.trans(y = y + 0.35,
                                             x = x - 0.1),
                                    "far.from.others.borders",
                                    cex = .8,
                                    fontface = c("bold"),
                                    dl.move("Age: 14-15", x = 2007, y = 9.7)))

MDE_gender <- percents %>%
  filter(data_type == "Major_Depressive_Episode",
         subgroup == "Gender") %>%
  mutate(subgroup = recode(subgroup, "Gender" = "Percent of each gender reporting MDE")) %>%
  ggplot(aes(x = Year, y = Percent, group = Demographic)) +
  geom_rect(xmin = 2011, xmax = Inf,
            ymin = -Inf, ymax = Inf,
            fill = "light gray") +
  geom_line(aes(color = Demographic), size = 1) + 
  scale_x_continuous(breaks = seq(2004, 2018, by = 1),
                     labels = seq(2004, 2018, by = 1),
                     limits = c(2004, 2018)) + 
  facet_wrap( ~ subgroup) +
  ocs_theme() +
  scale_color_manual(values = c(female_col, male_col))

MDE_gender <- direct.label(MDE_gender, list(dl.trans(y = y + 0.35,
                                             x = x - 0.1),
                                    "far.from.others.borders",
                                    cex = .8,
                                    fontface = c("bold")))

# create subtitle for gender and age plots
label <- expression(paste(bold("Older "), "youths and ", bold("females "), 
                          "report MDE at the highest rate and also show the steepest increase.",
                          # no spaces or characters for separator
                          sep = ""))

subtitle_plots_2 <- ggdraw() + draw_label(label, size = 16, x = 0, hjust = -0.01)

# change second row of main plot into age and gender plots only
row_2 <- plot_grid(MDE_age, MDE_gender, nrow = 1)

# update main plot
png(filename = here("img", "mainplot.png"), res = 300, width = 10, height = 10, units = "in")
plot_grid(title_plots, subtitle_plots, row_1, subtitle_plots_2, row_2, ncol = 1,
          rel_heights = c(0.1, 0.3, 1, 0.1, 1))
dev.off()

                           