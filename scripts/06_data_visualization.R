

library(tidyverse)
library(cowplot)
library(rethinking)
library(dotwhisker)

#==============================================================================


# Import the cleaned GBIF occurrence data
d <- read_csv("data/gbif_cleaned/gbif_all.csv")

# Import the IUCN assessment data
a <- read_csv("data/IUCN/assessments.csv")

# Import the rCAT output
category.levels <- 
  c("Least Concern", "Near Threatened", "Vulnerable",
    "Endangered", "Critically Endangered")

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
      ),
    AOOcat_long = case_when(
      AOOcat == "CR" ~ "Critically Endangered",
      AOOcat == "EN" ~ "Endangered",
      AOOcat == "VU" ~ "Vulnerable",
      AOOcat == "NT" ~ "Near Threatened",
      AOOcat == "LC" ~ "Least Concern"
    ),
    AOOcat_long = factor(AOOcat_long, levels = category.levels),
    EOOcat_long = case_when(
      EOOcat == "CR" ~ "Critically Endangered",
      EOOcat == "EN" ~ "Endangered",
      EOOcat == "VU" ~ "Vulnerable",
      EOOcat == "NT" ~ "Near Threatened",
      EOOcat == "LC" ~ "Least Concern"
    ),
    EOOcat_long = factor(EOOcat_long, levels = category.levels)
  )

#==============================================================================


# Tile plotting

line.size <- 3
color.over <- "gold1"
color.under <- "firebrick4"

data.for.plotting <- rcat %>%
  mutate(
    iucn_redlist_category = ifelse(
      iucn_redlist_category == "Lower Risk/least concern",
      "Least Concern", iucn_redlist_category),
    iucn_redlist_category = ifelse(
      iucn_redlist_category == "Lower Risk/near threatened",
      "Near Threatened", iucn_redlist_category),
    iucn_redlist_category = factor(
      iucn_redlist_category, levels = c(category.levels, "Data Deficient")
    )
  ) %>%
  group_by(iucn_redlist_category, EOOcat_long) %>%
  summarize(n = n()) %>%
  ungroup()

data.for.plotting %>%
  ggplot(aes(x = EOOcat_long, y = iucn_redlist_category)) +
  geom_tile(aes(fill = log10(n)), color = "black") +
  geom_text(
    aes(label = n), fontface = "bold", color = "white", size = 16
  ) +
  scale_fill_gradient(low = "lightgray", high = "black") +
  theme_bw() +
  labs(
    x = "REBA classification",
    y = "IUCN Red List Category classification"
  ) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    text = element_text(size = 20)
  ) +
  
  geom_segment(x = 1.5, y = 0, xend = 1.5, yend = 1.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 1.5, y = 1.5, xend = 2.5, yend = 1.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 2.5, y = 1.5, xend = 2.5, yend = 2.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 2.5, y = 2.5, xend = 3.5, yend = 2.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 3.5, y = 2.5, xend = 3.5, yend = 3.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 3.5, y = 3.5, xend = 4.5, yend = 3.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 4.5, y = 3.5, xend = 4.5, yend = 4.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 4.5, y = 4.5, xend = 5.5, yend = 4.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 5.5, y = 4.5, xend = 5.5, yend = 0,
               color = color.over, linetype = 2, size = line.size) +
  
  geom_segment(x = 0.5, y = 1.5, xend = 0.5, yend = 5.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 0.5, y = 1.5, xend = 1.5, yend = 1.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 1.5, y = 1.5, xend = 1.5, yend = 2.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 1.5, y = 2.5, xend = 2.5, yend = 2.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 1.5, y = 2.5, xend = 2.5, yend = 2.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 2.5, y = 2.5, xend = 2.5, yend = 3.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 2.5, y = 3.5, xend = 3.5, yend = 3.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 3.5, y = 3.5, xend = 3.5, yend = 4.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 3.5, y = 4.5, xend = 4.5, yend = 4.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 4.5, y = 4.5, xend = 4.5, yend = 5.5,
               color = color.under, linetype = 2, size = line.size) +
  
  geom_hline(yintercept = 5.5, size = 2)

ggsave("outputs/tile_plot.jpg", width = 14, height = 7)


data.for.plotting <- rcat %>%
  mutate(
    iucn_redlist_category = ifelse(
      iucn_redlist_category == "Lower Risk/least concern",
      "Least Concern", iucn_redlist_category),
    iucn_redlist_category = ifelse(
      iucn_redlist_category == "Lower Risk/near threatened",
      "Near Threatened", iucn_redlist_category),
    iucn_redlist_category = factor(
      iucn_redlist_category, levels = c(category.levels, "Data Deficient")
    )
  ) %>%
  group_by(iucn_redlist_category, EOOcat_long) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(
    iucn_numeric = as.numeric(iucn_redlist_category),
    EOOcat_numeric = as.numeric(EOOcat_long),
    color = case_when(
      iucn_numeric == EOOcat_numeric ~ "forestgreen",
      iucn_numeric == 6 ~ "darkgray",
      iucn_numeric < EOOcat_numeric ~ "gold1",
      iucn_numeric > EOOcat_numeric ~ "firebrick4",
    ),
    alpha = log10(n + 2)/max(log10(n + 2)),
    color = alpha(color, alpha = alpha)
  ) 

data.for.plotting %>%
  ggplot(aes(x = EOOcat_long, y = iucn_redlist_category)) +
  geom_tile(fill = data.for.plotting$color) +
  geom_text(
    aes(label = n), fontface = "bold", color = "black", size = 16) +
  theme_bw() +
  labs(
    x = "REBA classification",
    y = "IUCN Red List Category classification"
  ) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    text = element_text(size = 20)
  ) +
  geom_hline(yintercept = 5.5, size = 2)

ggsave("outputs/tile_plot_color.jpg", width = 14, height = 7)


data.for.plotting %>%
  ggplot(aes(x = iucn_redlist_category, y = EOOcat_long)) +
  geom_tile(fill = data.for.plotting$color) +
  geom_text(
    aes(label = n), fontface = "bold", color = "black", size = 16) +
  theme_bw() +
  labs(
    x = "IUCN Red List Category classification",
    y = "REBA classification"
  ) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    text = element_text(size = 20)
  ) +
  geom_vline(xintercept = 5.5, size = 2)

ggsave("outputs/tile_plot_color_flipped.jpg", width = 16, height = 8)


data.for.plotting <- rcat %>%
  left_join(
    ., 
    read_csv("data/NatureServe/ns_data.csv"), 
    by = "query_name"
  ) %>%
  filter(!is.na(scientificName)) %>%
  mutate(
    iucn_redlist_category = ifelse(
      iucn_redlist_category == "Lower Risk/least concern",
      "Least Concern", iucn_redlist_category),
    iucn_redlist_category = ifelse(
      iucn_redlist_category == "Lower Risk/near threatened",
      "Near Threatened", iucn_redlist_category),
    iucn_redlist_category = factor(
      iucn_redlist_category, levels = c(category.levels, "Data Deficient")),
    roundedGRank = factor(
      roundedGRank, levels = rev(c("GU", "GNR", "G1", "G2", "G3", "G4", "G5")))
  ) %>%
  group_by(roundedGRank, EOOcat_long) %>%
  summarize(n = n()) %>%
  ungroup()

data.for.plotting %>%
  ggplot(aes(x = EOOcat_long, y = roundedGRank)) +
  geom_tile(aes(fill = log10(n)), color = "black") +
  geom_text(
    aes(label = n), fontface = "bold", color = "white", size = 16
  ) +
  scale_fill_gradient(low = "lightgray", high = "black") +
  theme_bw() +
  labs(
    x = "REBA classification",
    y = "NatureServe global conservation status ranks"
  ) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    text = element_text(size = 20)
  ) +
  
  geom_segment(x = 1.5, y = 0, xend = 1.5, yend = 1.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 1.5, y = 1.5, xend = 2.5, yend = 1.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 2.5, y = 1.5, xend = 2.5, yend = 2.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 2.5, y = 2.5, xend = 3.5, yend = 2.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 3.5, y = 2.5, xend = 3.5, yend = 3.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 3.5, y = 3.5, xend = 4.5, yend = 3.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 4.5, y = 3.5, xend = 4.5, yend = 4.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 4.5, y = 4.5, xend = 5.5, yend = 4.5,
               color = color.over, linetype = 2, size = line.size) +
  geom_segment(x = 5.5, y = 4.5, xend = 5.5, yend = 0,
               color = color.over, linetype = 2, size = line.size) +
  
  geom_segment(x = 0.5, y = 1.5, xend = 0.5, yend = 5.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 0.5, y = 1.5, xend = 1.5, yend = 1.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 1.5, y = 1.5, xend = 1.5, yend = 2.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 1.5, y = 2.5, xend = 2.5, yend = 2.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 1.5, y = 2.5, xend = 2.5, yend = 2.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 2.5, y = 2.5, xend = 2.5, yend = 3.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 2.5, y = 3.5, xend = 3.5, yend = 3.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 3.5, y = 3.5, xend = 3.5, yend = 4.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 3.5, y = 4.5, xend = 4.5, yend = 4.5,
               color = color.under, linetype = 2, size = line.size) +
  geom_segment(x = 4.5, y = 4.5, xend = 4.5, yend = 5.5,
               color = color.under, linetype = 2, size = line.size) +
  
  geom_hline(yintercept = 5.5, size = 2)

ggsave("outputs/tile_plot_natureserve.jpg", width = 14, height = 7)

#==============================================================================


# Visualize amount of GBIF data over time, per species

plot1 <- d %>%
  group_by(query_name, year) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year, y = n, group = query_name)) +
  geom_point(size = 0.05) +
  geom_line(size = 0.5, color = alpha("black", 0.2)) +
  coord_cartesian(ylim = c(0, 25000)) +
  ylab("number of GBIF occurrence records") +
  theme_minimal()

plot2 <- d %>%
  group_by(query_name, year) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year, y = n, group = query_name)) +
  geom_point(size = 0.05) +
  geom_line(size = 0.5, color = alpha("black", 0.2)) +
  coord_cartesian(xlim = c(1950, max(d$year)), ylim = c(0, 25000)) +
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

#==============================================================================


# Make counterfactual plots across varying sample sizes for both plant types
# and threats

f <- read.csv("data/modeling_data/full_modeling_data.csv")
f$NOP_s <- scale(f$NOP)

m.correct.type.stan <- readRDS("data/fit_models/m.correct.type.stan.RDS")
m.stan.samples <- extract.samples(m.correct.type.stan)

sample.size.scenarios <- data.frame(raw_N = c(100, 1000, 10000, 20000)) %>%
  mutate(scaled_N = (raw_N - mean(f$NOP))/sd(f$NOP))

predicted.probs <- c()

for(x in sample.size.scenarios$scaled_N) {
  
  temp <- logistic(
    c(
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aTree,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aShrub,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aGraminoid,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aForb_Herb,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aAnnual,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aGeophyte,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aSucculent,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aMoss_Hydrophyte,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aVines_Epi_Litho,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aFern
    )
  )
  
  predicted.probs <- c(predicted.probs, temp)
}

d.preds <- data.frame(
  plant_group = rep(
    rep(
      c("Tree", "Shrub", "Graminoid", "Forb or Herb", 
        "Annual", "Geophyte", "Succulent", "Moss/Hydrophyte", 
        "Vines/Epiphyte/Lithophyte", "Fern"), 
      each = nrow(m.stan.samples$a)
    ),
    times = nrow(sample.size.scenarios)
  ),
  sample_size = rep(sample.size.scenarios$raw_N, each = nrow(m.stan.samples$a)*10),
  predicted_probs = predicted.probs
)

d.preds %>%
  pivot_wider(names_from = 1:2, values_from = 3) %>%
  unnest() %>%
  rethinking::precis(., prob = 0.99)

preds.plot1 <- d.preds %>%
  ggplot(aes(x = predicted_probs, color = as.factor(sample_size))) +
  geom_density(fill = alpha("gray", 0.15)) +
  xlab("Probability of correct Red List Category classification") +
  ylab("Density") +
  xlim(0, 1) +
  theme_minimal() +
  facet_wrap(~plant_group, ncol = 2, scales = "free_y") +
  guides(color = guide_legend(title = "Sample size")) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250)) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 16),
    axis.title = element_text(size = 20)
  )

ggsave("outputs/plant_type_preds_plot.jpg", plot = preds.plot1, 
       height = 8, width = 8)


m.under.type.stan <- readRDS("data/fit_models/m.under.type.stan.RDS")
m.stan.samples <- rstan::extract(m.under.type.stan@stanfit)

sample.size.scenarios <- data.frame(raw_N = c(100, 1000, 10000, 20000)) %>%
  mutate(scaled_N = (raw_N - mean(f$NOP))/sd(f$NOP))

predicted.probs <- c()

for(x in sample.size.scenarios$scaled_N) {
  
  temp <- logistic(
    c(
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aTree,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aShrub,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aGraminoid,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aForb_Herb,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aAnnual,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aGeophyte,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aSucculent,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aMoss_Hydrophyte,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aVines_Epi_Litho,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aFern
    )
  )
  
  predicted.probs <- c(predicted.probs, temp)
}

d.preds <- data.frame(
  plant_group = rep(
    rep(
      c("Tree", "Shrub", "Graminoid", "Forb or Herb", 
        "Annual", "Geophyte", "Succulent", "Moss/Hydrophyte", 
        "Vines/Epiphyte/Lithophyte", "Fern"), 
      each = nrow(m.stan.samples$a)
    ),
    times = nrow(sample.size.scenarios)
  ),
  sample_size = rep(sample.size.scenarios$raw_N, each = nrow(m.stan.samples$a)*10),
  predicted_probs = predicted.probs
)

d.preds %>%
  pivot_wider(names_from = 1:2, values_from = 3) %>%
  unnest() %>%
  rethinking::precis(., prob = 0.99)

interval <- 0.95

summary.table <- d.preds %>%
  group_by(plant_group, sample_size) %>%
  summarize(
    estimate = mean(predicted_probs),
    conf.low = HPDI(predicted_probs, interval)[1],
    conf.high = HPDI(predicted_probs, interval)[2]
  ) %>%
  ungroup()

type.under.plot <- summary.table %>%
  filter(sample_size != 20000) %>%
  rename(
    term = plant_group,
    model = sample_size
  ) %>%
  arrange(desc(model)) %>%
  dwplot() +
  xlim(0, 1) +
  xlab("Probability of REBA underclassification error") +
  scale_color_discrete(
    name = "Sample size",
    guide = guide_legend(reverse = TRUE)
  ) +
  theme_minimal()

ggsave("outputs/plant_type_under_plot.jpg", 
       plot = type.under.plot, height = 5, width = 5)


m.correct.threat.stan <- readRDS("data/fit_models/m.correct.threat.stan.RDS")
m.stan.samples <- extract.samples(m.correct.threat.stan)

sample.size.scenarios <- data.frame(raw_N = c(100, 1000, 10000, 20000)) %>%
  mutate(scaled_N = (raw_N - mean(f$NOP))/sd(f$NOP))

predicted.probs <- c()

for(x in sample.size.scenarios$scaled_N) {
  
  temp <- logistic(
    c(
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aResidential_and_commercial_development,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aAgriculture_and_aquaculture,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aEnergy_production_and_mining,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aTransportation_and_service_corridors,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aBiological_resource_use,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aHuman_intrusions_and_disturbance,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aNatural_system_modifications,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aInvasive_and_other_problematic_species_genes_and_disease,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aPollution,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aGeological_events,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aClimate_change_and_severe_weather,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aOther_options
    )
  )
  
  predicted.probs <- c(predicted.probs, temp)
}

d.preds <- data.frame(
  plant_group = rep(
    rep(
      c(
        "Residential & commercial development",
        "Agriculture & aquaculture", 
        "Energy production & mining", 
        "Transportation & service corridors", 
        "Biological resource use", 
        "Human intrusions & disturbance",
        "Natural system modifications",
        "Invasive species, genes, & diseases",
        "Pollution",
        "Geological events",
        "Climate change & severe weather",
        "Other options"
      ), 
      each = nrow(m.stan.samples$a)
    ),
    times = nrow(sample.size.scenarios)
  ),
  sample_size = rep(sample.size.scenarios$raw_N, each = nrow(m.stan.samples$a)*12),
  predicted_probs = predicted.probs
)

d.preds %>%
  pivot_wider(names_from = 1:2, values_from = 3) %>%
  unnest() %>%
  rethinking::precis(., prob = 0.99)

preds.plot2 <- d.preds %>%
  ggplot(aes(x = predicted_probs, color = as.factor(sample_size))) +
  geom_density(fill = alpha("gray", 0.15)) +
  xlab("Probability of correct Red List Category classification") +
  ylab("Density") +
  xlim(0, 1) +
  theme_minimal() +
  facet_wrap(~plant_group, ncol = 3, scales = "free_y") +
  guides(color = guide_legend(title = "Sample size")) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25)) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 16),
    axis.title = element_text(size = 20)
  )

ggsave("outputs/plant_threat_preds_plot.jpg", plot = preds.plot2, 
       height = 8, width = 12)


m.under.threat.stan <- readRDS("data/fit_models/m.under.threat.stan.RDS")
m.stan.samples <- rstan::extract(m.under.threat.stan@stanfit)

sample.size.scenarios <- data.frame(raw_N = c(100, 1000, 10000, 20000)) %>%
  mutate(scaled_N = (raw_N - mean(f$NOP))/sd(f$NOP))

predicted.probs <- c()

for(x in sample.size.scenarios$scaled_N) {
  
  temp <- logistic(
    c(
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aResidential_and_commercial_development,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aAgriculture_and_aquaculture,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aEnergy_production_and_mining,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aTransportation_and_service_corridors,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aBiological_resource_use,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aHuman_intrusions_and_disturbance,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aNatural_system_modifications,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aInvasive,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aPollution,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aGeological_events,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aClimate_change_and_severe_weather,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aOther_options
    )
  )
  
  predicted.probs <- c(predicted.probs, temp)
}

d.preds <- data.frame(
  plant_group = rep(
    rep(
      c(
        "Residential & commercial development",
        "Agriculture & aquaculture", 
        "Energy production & mining", 
        "Transportation & service corridors", 
        "Biological resource use", 
        "Human intrusions & disturbance",
        "Natural system modifications",
        "Invasive species, genes, & diseases",
        "Pollution",
        "Geological events",
        "Climate change & severe weather",
        "Other options"
      ), 
      each = nrow(m.stan.samples$a)
    ),
    times = nrow(sample.size.scenarios)
  ),
  sample_size = rep(sample.size.scenarios$raw_N, each = nrow(m.stan.samples$a)*12),
  predicted_probs = predicted.probs
)

d.preds %>%
  pivot_wider(names_from = 1:2, values_from = 3) %>%
  unnest() %>%
  rethinking::precis(., prob = 0.99)

interval <- 0.95

summary.table <- d.preds %>%
  group_by(plant_group, sample_size) %>%
  summarize(
    estimate = mean(predicted_probs),
    conf.low = HPDI(predicted_probs, interval)[1],
    conf.high = HPDI(predicted_probs, interval)[2]
  ) %>%
  ungroup()

threat.under.plot <- summary.table %>%
  filter(sample_size != 20000) %>%
  rename(
    term = plant_group,
    model = sample_size
  ) %>%
  arrange(desc(model)) %>%
  dwplot() +
  xlim(0, 1) +
  xlab("Probability of REBA underclassification error") +
  scale_color_discrete(
    name = "Sample size",
    guide = guide_legend(reverse = TRUE)
  ) +
  theme_minimal()

ggsave("outputs/plant_threat_under_plot.jpg", 
       plot = threat.under.plot, height = 5, width = 5)

#==============================================================================


# Make posterior plots for data deficient species

f <- read.csv("data/modeling_data/full_modeling_data.csv")
dd <- read.csv("data/modeling_data/dd_modeling_data.csv")
dd$NOP_s <- (dd$NOP - mean(f$NOP))/sd(f$NOP)

m.correct.type.stan <- readRDS("data/fit_models/m.correct.type.stan.RDS")
m.stan.samples <- extract.samples(m.correct.type.stan)

predicted.probs <- c()

for(i in 1:nrow(dd)) {
  
  temp <- logistic(
    c(
      m.stan.samples$a + 
        m.stan.samples$bN*dd$NOP_s[i] +
        m.stan.samples$aTree*dd$Tree[i] +
        m.stan.samples$aShrub*dd$Shrub[i] +
        m.stan.samples$aGraminoid*dd$Graminoid[i] +
        m.stan.samples$aForb_Herb*dd$Forb.or.Herb[i] +
        m.stan.samples$aAnnual*dd$Annual[i] +
        m.stan.samples$aGeophyte*dd$Geophyte[i] +
        m.stan.samples$aSucculent*dd$Succulent[i] +
        m.stan.samples$aMoss_Hydrophyte*dd$Moss.Hydrophyte[i] +
        m.stan.samples$aVines_Epi_Litho*dd$Vines.Epiphyte.Lithophyte[i] +
        m.stan.samples$aFern*dd$Fern[i]
    )
  )
     
  predicted.probs <- c(predicted.probs, temp)
}

d.preds <- data.frame(
  plant_species = rep(dd$scientificName, each = nrow(m.stan.samples$a)),
  predicted_probs = predicted.probs
)

d.preds %>%
  pivot_wider(names_from = 1, values_from = 2) %>%
  unnest() %>%
  rethinking::precis(., prob = 0.99)

dd.preds.plot <- d.preds %>%
  ggplot(aes(x = predicted_probs)) +
  geom_density(fill = alpha("gray", 0.15)) +
  xlab("Probability of correct Red List Category classification") +
  ylab("Density") +
  xlim(0, 1) +
  theme_minimal() +
  facet_wrap(~plant_species, ncol = 4, scales = "free_y") +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 20)
  )

ggsave("outputs/dd_preds_plot.jpg", plot = dd.preds.plot, 
       height = 8, width = 10)

#==============================================================================


# Make stacked proportion plots showing plant type and threats in relation to
# rapid classification category (relative to IUCN data)

# Import the plant type and threat data and join together with rCAT output

type.set <- read.csv("data/IUCN/plant_specific.csv") %>%
  distinct() %>%
  mutate(typeCode = code, typeName = name) %>%
  dplyr::select(scientificName, typeCode, typeName)

threat.set <- read.csv("data/IUCN/threats.csv") %>%
  distinct() %>%
  mutate(threatCode = code, threatName = name) %>%
  dplyr::select(scientificName, threatCode, threatName)

table.for.prop.plots <- rcat %>%
  left_join(., threat.set, by = c("query_name" = "scientificName")) %>%
  left_join(., type.set, by = c("query_name" = "scientificName"))

# Clean and filter this dataset

table.for.prop.plots.mod <- table.for.prop.plots %>%
  # Change the very few instances of "Lower Risk/least concern" to just
  # "Least Concern"
  # Change the very few instances of "Lower Risk/near threatened" to just
  # "Near Threatened"
  mutate(
    iucn_redlist_category = ifelse(
      iucn_redlist_category == "Lower Risk/least concern",
      "Least Concern", iucn_redlist_category
    ),
    iucn_redlist_category = ifelse(
      iucn_redlist_category == "Lower Risk/near threatened",
      "Near Threatened", iucn_redlist_category
    )
  ) %>%
  mutate(
    EOOcat_long = case_when(
      EOOcat == "CR" ~ "Critically Endangered",
      EOOcat == "EN" ~ "Endangered",
      EOOcat == "VU" ~ "Vulnerable",
      EOOcat == "NT" ~ "Near Threatened",
      EOOcat == "LC" ~ "Least Concern"
    )
  ) %>%
  # Separate the numeric threat code system into its component parts 
  # (i.e. 1 column containing 10.2.1 to 3 columns with 10, 2, and 1)
  separate(
    threatCode, 
    c("primaryThreat", "secondaryThreat", "tertiaryThreat"),
    sep = "\\."
  ) %>%
  # Create columns that will set up and ultimately become those that 
  # compare whether we are overclassifying, underclassifying, or 
  # correctly classifying species when compared to the IUCN scheme
  mutate(
    IUCNcat_categoryClass = case_when(
      iucn_redlist_category == "Critically Endangered" ~ 5,
      iucn_redlist_category == "Endangered" ~ 4,
      iucn_redlist_category == "Vulnerable" ~ 3,
      iucn_redlist_category == "Near Threatened" ~ 2,
      iucn_redlist_category == "Least Concern" ~ 1,
      iucn_redlist_category == "Data Deficient" ~ 0
    ),
    EOOcat_categoryClass = case_when(
      EOOcat_long == "Critically Endangered" ~ 5,
      EOOcat_long == "Endangered" ~ 4,
      EOOcat_long == "Vulnerable" ~ 3,
      EOOcat_long == "Near Threatened" ~ 2,
      EOOcat_long == "Least Concern" ~ 1
    ),
    overClassified = if_else(
      EOOcat_categoryClass > IUCNcat_categoryClass, 1, 0
    ),
    underClassified = if_else(
      EOOcat_categoryClass < IUCNcat_categoryClass, 1, 0
    ),
    correctlyClassified = if_else(
      EOOcat_categoryClass == IUCNcat_categoryClass, 1, 0),
    classificationCategory = case_when(
      overClassified == 1 ~ "Overclassified",
      underClassified == 1 ~ "Underclassified",
      correctlyClassified == 1 ~ "Correctly classified"
    ),
    classificationCategory = factor(
      classificationCategory, 
      levels = c("Underclassified", "Correctly classified", "Overclassified")
    )
  ) %>%
  # Condensing Plant Types to account for a lack of data in certain groups
  mutate(
      newTypeName = case_when(
        # Condensing all shrubs into a single group
        typeName == "Shrub - large" ~ "Shrub",
        typeName == "Shrub - small" ~ "Shrub",
        typeName == "Shrub - size unknown" ~ "Shrub",
        # Condensing all succulents into a single group
        typeName == "Succulent - shrub" ~ "Succulent",
        typeName == "Succulent - tree" ~ "Succulent",
        typeName == "Succulent - form unknown" ~ "Succulent",
        # Condensing all trees into a single group
        typeName == "Tree - large" ~ "Tree",
        typeName == "Tree - small" ~ "Tree",
        typeName == "Tree - size unknown" ~ "Tree",
        # Group Vines, Epiphyte, Lithophyte
        typeName == "Vines" ~ "Vines/Epiphyte/Lithophyte",
        typeName == "Epiphyte" ~ "Vines/Epiphyte/Lithophyte",
        typeName == "Lithophyte" ~ "Vines/Epiphyte/Lithophyte",
        # Group Moss, Hydrophyte
        typeName == "Moss" ~ "Moss/Hydrophyte",
        typeName == "Hydrophyte" ~ "Moss/Hydrophyte",
        TRUE ~ typeName
      )
  )

# Verify that the table we've created only lists a species as belonging to 
# one rapid classification category (relative to the IUCN categories)

table.for.prop.plots.mod %>%
  select(overClassified, underClassified, correctlyClassified) %>%
  rowSums() %>%
  max()

# Create the plant type plot

table.for.prop.plots.mod %>%
  # Filter out Data Deficient species as these can't be properly categorized
  filter(iucn_redlist_category != "Data Deficient") %>%
  distinct(query_name, newTypeName, classificationCategory) %>%
  mutate(
    classificationCategory = factor(
      classificationCategory, 
      levels = rev(levels(classificationCategory))
    )
  ) %>%
  ggplot(aes(x = newTypeName, fill = classificationCategory)) +
  geom_bar(position = "fill") +
  ylab("Proportion of species") +
  scale_fill_manual(values = c("gold1", "forestgreen", "firebrick4")) +
  theme_bw() +
  guides(fill = guide_legend(
    title = "Rapid classification\nplacement relative\nto the IUCN")
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    plot.margin = margin(5, 5, 5, 40, "pt")
  )

ggsave("outputs/plant_type_prop_plot.jpg", width = 12, height = 8)

# Create the plant threat plot

table.for.prop.plots.mod %>%
  # Filter out Data Deficient species as these can't be properly categorized
  filter(iucn_redlist_category != "Data Deficient") %>%
  # Filter out species for which we don't have threat data
  filter(!is.na(primaryThreat)) %>%
  distinct(query_name, primaryThreat, classificationCategory) %>%
  mutate(
    classificationCategory = factor(
      classificationCategory, 
      levels = rev(levels(classificationCategory))
    ),
    primaryThreat = case_when(
      primaryThreat == "1" ~ "Residential & commerical development",
      primaryThreat == "2" ~ "Agriculture & aquaculture",
      primaryThreat == "3" ~ "Energy production & mining",
      primaryThreat == "4" ~ "Transportation & service corridors",
      primaryThreat == "5" ~ "Biological resource use",
      primaryThreat == "6" ~ "Human intrusions & disturbance",
      primaryThreat == "7" ~ "Natural system modifications",
      primaryThreat == "8" ~ "Invasive species, genes, & diseases",
      primaryThreat == "9" ~ "Pollution",
      primaryThreat == "10" ~ "Geological events",
      primaryThreat == "11" ~ "Climate change & severe weather",
      primaryThreat == "12" ~ "Other options")
  ) %>%
  ggplot(aes(x = primaryThreat, fill = classificationCategory)) +
  geom_bar(position = "fill") +
  ylab("Proportion of species") +
  scale_fill_manual(values = c("gold1", "forestgreen", "firebrick4")) +
  theme_bw() +
  guides(fill = guide_legend(
    title = "Rapid classification\nplacement relative\nto the IUCN")
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    plot.margin = margin(5, 5, 5, 40, "pt")
  )

ggsave("outputs/plant_threat_prop_plot.jpg", width = 12, height = 8)
