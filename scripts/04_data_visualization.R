

library(tidyverse)
library(cowplot)

#==============================================================================


# Import the cleaned GBIF occurrence (filtering to only species with > 3 point)
d <- read_csv("data/gbif_cleaned/gbif_all.csv")
d <- d %>%
  left_join(
    .,
    d %>%
      group_by(species) %>%
      summarize(number_of_points = n()),
    by = "species"
  ) %>%
  filter(number_of_points > 3)

# Import the IUCN assessment data
a <- read_csv("data/IUCN/assessments.csv")

# Import the rCAT output
rcat <- read_csv("data/rcat/rCAT_output.csv") %>%
  mutate(
    iucn_assessment_year_binned = case_when(
      iucn_assessment_year < 2001 ~ "pre-2001",
      iucn_assessment_year < 2011 ~ "2001-2010",
      iucn_assessment_year < 2016 ~ "2011-2015",
      iucn_assessment_year > 2015 ~ "post-2015"
    ),
    iucn_assessment_year_binned = 
      factor(iucn_assessment_year_binned,
             levels = c("pre-2001", "2001-2010", "2011-2015", "post-2015")
      )
  )

#==============================================================================


# Visualize amount of GBIF data over time, per species

plot1 <- d %>%
  group_by(query_name, year) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year, y = n, group = query_name)) +
  geom_point(size = 0.05) +
  geom_line(size = 0.5, color = alpha("black", 0.2)) +
  coord_cartesian(ylim = c(0, 15000)) +
  ylab("number of GBIF occurrence records") +
  theme_minimal()

plot2 <- d %>%
  group_by(query_name, year) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year, y = n, group = query_name)) +
  geom_point(size = 0.05) +
  geom_line(size = 0.5, color = alpha("black", 0.2)) +
  coord_cartesian(xlim = c(1950, max(d$year)), ylim = c(0, 15000)) +
  ylab("number of GBIF occurrence records") +
  theme_minimal()
  
plot_grid(plot1, plot2, ncol = 1)

ggsave("outputs/GBIF_occurrences_over_time_per_species.jpg", 
       width = 7, height = 10)

#==============================================================================


# Visualize metric comparisons using old and recent data, per species

temp1 <- rcat %>%
  select(query_name, iucn_assessment_year_binned, AOOkm2_old, AOOkm2_recent)
temp1 <- temp1[complete.cases(temp1), ]

title <- paste0(
  "Number of species with both metrics: ",
  nrow(temp1),
  "\n",
  "Number of species where old < recent: ",
  filter(temp1, AOOkm2_old < AOOkm2_recent) %>% nrow(),
  "\n",
  "Number of species where old > recent: ",
  filter(temp1, AOOkm2_old > AOOkm2_recent) %>% nrow()
)

plot1 <- temp1 %>%
  gather(metric, value, 3:4) %>%
  ggplot(aes(x = metric, y = value, group = query_name)) +
  geom_point(aes(color = iucn_assessment_year_binned)) +
  geom_line(size = 0.5, color = alpha("black", 0.1)) +
  ggtitle(title) +
  scale_color_discrete(name = "Assessment Year") +
  theme_minimal()

temp2 <- rcat %>%
  select(query_name, iucn_assessment_year_binned, EOOkm2_old, EOOkm2_recent)
temp2 <- temp2[complete.cases(temp2), ]

title <- paste0(
  "Number of species with both metrics: ",
  nrow(temp2),
  "\n",
  "Number of species where old < recent: ",
  filter(temp2, EOOkm2_old < EOOkm2_recent) %>% nrow(),
  "\n",
  "Number of species where old > recent: ",
  filter(temp2, EOOkm2_old > EOOkm2_recent) %>% nrow()
)

plot2 <- temp2 %>%
  gather(metric, value, 3:4) %>%
  ggplot(aes(x = metric, y = value, group = query_name)) +
  geom_point(aes(color = iucn_assessment_year_binned)) +
  geom_line(size = 0.5, color = alpha("black", 0.1)) +
  ggtitle(title) +
  scale_color_discrete(name = "Assessment Year") +
  theme_minimal()

temp3 <- rcat %>%
  select(query_name, iucn_assessment_year_binned, 
         EOOkm2_clipped_old, EOOkm2_clipped_recent)
temp3 <- temp3[complete.cases(temp3), ]

title <- paste0(
  "Number of species with both metrics: ",
  nrow(temp3),
  "\n",
  "Number of species where old < recent: ",
  filter(temp3, EOOkm2_clipped_old < EOOkm2_clipped_recent) %>% nrow(),
  "\n",
  "Number of species where old > recent: ",
  filter(temp3, EOOkm2_clipped_old > EOOkm2_clipped_recent) %>% nrow()
)

plot3 <- temp3 %>%
  gather(metric, value, 3:4) %>%
  ggplot(aes(x = metric, y = value, group = query_name)) +
  geom_point(aes(color = iucn_assessment_year_binned)) +
  geom_line(size = 0.5, color = alpha("black", 0.1)) +
  ggtitle(title) +
  scale_color_discrete(name = "Assessment Year") +
  theme_minimal()

plot_grid(plot1, plot2, plot3, ncol = 1)

ggsave("outputs/old_recent_metric_comparison.jpg", 
       width = 8, height = 12)

#==============================================================================


# Compared stated AOO and EOO values from the IUCN assessments (where they 
# exist) with the metrics we've calculated

plot1 <- rcat %>%
  ggplot(aes(x = AOO_assessment, y = AOOkm2)) +
  geom_point(aes(color = iucn_assessment_year_binned)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  xlim(0, 150000) +
  ylim(0, 150000) +
  scale_color_discrete(name = "Assessment Year") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot2 <- rcat %>%
  ggplot(aes(x = EOO_assessment, y = EOOkm2)) +
  geom_point(aes(color = iucn_assessment_year_binned)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  ylim(0, 300000000) +
  scale_color_discrete(name = "Assessment Year") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot3 <- rcat %>%
  ggplot(aes(x = EOO_assessment, y = EOOkm2_manual)) +
  geom_point(aes(color = iucn_assessment_year_binned)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  ylim(0, 300000000) +
  scale_color_discrete(name = "Assessment Year") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot4 <- rcat %>%
  ggplot(aes(x = EOO_assessment, y = EOOkm2_clipped)) +
  geom_point(aes(color = iucn_assessment_year_binned)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  ylim(0, 300000000) +
  scale_color_discrete(name = "Assessment Year") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_grid(plot1, plot2, plot3, plot4, ncol = 2)

ggsave("outputs/IUCN_assessment_metrics_vs_GBIF_metrics.jpg", 
       width = 12, height = 7)
