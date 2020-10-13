

library(tidyverse)
library(cowplot)
library(rethinking)

#==============================================================================


# Import the cleaned GBIF occurrence 
# (filtering to only species with > 3 points)
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
color.over <- "forestgreen"
color.under <- "darkred"

rcat %>%
  mutate(
    iucn_redlist_category = ifelse(
      iucn_redlist_category == "Lower Risk/near threatened",
      "Near Threatened", iucn_redlist_category),
    iucn_redlist_category = factor(
      iucn_redlist_category, levels = c(category.levels, "Data Deficient")
    )
  ) %>%
  group_by(iucn_redlist_category, EOOcat_long) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = EOOcat_long, y = iucn_redlist_category)) +
  geom_tile(aes(fill = log10(n)), color = "black") +
  geom_text(aes(label = n, fontface = "bold"), 
            color = "white", size = 16) +
  scale_fill_gradient(low = "lightgray", high = "black") +
  theme_bw() +
  labs(
    x = "Automated Red List Category classification based on rCAT EOO calculation",
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


rcat %>%
  left_join(
    ., 
    read_csv("data/NatureServe/ns_data.csv"), 
    by = "query_name"
  ) %>%
  filter(!is.na(scientificName)) %>%
  mutate(
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
  ggplot(aes(x = EOOcat_long, y = roundedGRank)) +
  geom_tile(aes(fill = log10(n)), color = "black") +
  geom_text(aes(label = n, fontface = "bold"), 
            color = "white", size = 16) +
  scale_fill_gradient(low = "lightgray", high = "black") +
  theme_bw() +
  labs(
    x = "Automated Red List Category classification based on rCAT EOO calculation",
    y = "NatureServe Global Conservation Status Ranks"
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

#==============================================================================


# Make counterfactual plots across varying sample sizes for both plant types
# and threats

f <- read.csv("data/modeling_data/full_modeling_data.csv")
f$NOP_s <- scale(f$NOP)

m.type.stan <- readRDS("data/fit_models/m.type.stan.RDS")
m.stan.samples <- extract.samples(m.type.stan)

sample.size.scenarios <- data.frame(raw_N = c(100, 1000, 10000, 20000)) %>%
  mutate(scaled_N = (raw_N - mean(f$NOP))/sd(f$NOP))

predicted.probs <- c()

for(x in sample.size.scenarios$scaled_N) {
  
  temp <- logistic(
    c(
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aTree_large,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aTree_small,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aShrub,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aSucculent,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aGeophyte,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aAnnual_Graminoid,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aFern_Forb_Herb,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aVines_Epi_Hydro_Litho
    )
  )
  
  predicted.probs <- c(predicted.probs, temp)
}

d.preds <- data.frame(
  plant_group = rep(
    rep(
      c("Tree (large)", "Tree (small)", "Shrub", "Succulent", "Geophyte", "Annual/Graminoid", "Fern/Forb/Herb", "Vines/Epi/Hydro/Litho"), 
      each = nrow(m.stan.samples$a)
    ),
    times = nrow(sample.size.scenarios)
  ),
  sample_size = rep(sample.size.scenarios$raw_N, each = nrow(m.stan.samples$a)*8),
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
  theme_minimal() +
  facet_wrap(~plant_group, ncol = 2, scales = "free_y") +
  guides(color = guide_legend(title = "Sample size")) +
  scale_y_continuous(breaks = c(0, 3, 6, 9, 12, 15)) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 16),
    axis.title = element_text(size = 20)
  )

ggsave("outputs/plant_type_preds_plot.jpg", plot = preds.plot1, 
       height = 8, width = 8)


m.threat.stan <- readRDS("data/fit_models/m.threat.stan.RDS")
m.stan.samples <- extract.samples(m.threat.stan)

sample.size.scenarios <- data.frame(raw_N = c(100, 1000, 10000, 20000)) %>%
  mutate(scaled_N = (raw_N - mean(f$NOP))/sd(f$NOP))

predicted.probs <- c()

for(x in sample.size.scenarios$scaled_N) {
  
  temp <- logistic(
    c(
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aResidential_and_commercial_development,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aAgriculture_and_Aquaculture,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aEnergy_production_and_mining,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aTransportation_and_service_corridors,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aBiological_resource_use,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aHuman_intrusions_and_disturbance,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aNatural_system_modifications,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aInvasive_and_other_problematic_species_genes_and_disease,
      m.stan.samples$a + m.stan.samples$bN*x + m.stan.samples$aPollution,
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
        "Residential and commercial development",
        "Agriculture and aquaculture", 
        "Energy production and mining", 
        "Transportation and service corridors", 
        "Biological resource use", 
        "Human intrusions and disturbance",
        "Natural system modifications",
        "Invasive species, genes, and disease",
        "Pollution",
        "Climate change and severe weather",
        "Other options"
      ), 
      each = nrow(m.stan.samples$a)
    ),
    times = nrow(sample.size.scenarios)
  ),
  sample_size = rep(sample.size.scenarios$raw_N, each = nrow(m.stan.samples$a)*11),
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

#==============================================================================


# Make posterior plots for data deficient species

f <- read.csv("data/modeling_data/full_modeling_data.csv")
dd <- read.csv("data/modeling_data/dd_modeling_data.csv")
dd$NOP_s <- (dd$NOP - mean(f$NOP))/sd(f$NOP)

m.type.stan <- readRDS("data/fit_models/m.type.stan.RDS")
m.stan.samples <- extract.samples(m.type.stan)

predicted.probs <- c()

for(i in 1:nrow(dd)) {
  
  temp <- logistic(
    c(
      m.stan.samples$a + 
        m.stan.samples$bN*dd$NOP_s[i] +
        m.stan.samples$aAnnual_Graminoid*dd$Annual.Graminoid[i] +
        m.stan.samples$aFern_Forb_Herb*dd$Fern.Forb.or.Herb[i] +
        m.stan.samples$aGeophyte*dd$Geophyte[i] +
        m.stan.samples$aShrub*dd$Shrub[i] +
        m.stan.samples$aSucculent*dd$Succulent[i] +
        m.stan.samples$aTree_large*dd$Tree...large[i] +
        m.stan.samples$aTree_small*dd$Tree...small[i] +
        m.stan.samples$aVines_Epi_Hydro_Litho*dd$Vines.Epiphyte.Hydrophyte.Lithophyte[i]
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
  theme_minimal() +
  facet_wrap(~plant_species, ncol = 3, scales = "free_y") +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 20)
  )

ggsave("outputs/dd_preds_plot.jpg", plot = dd.preds.plot, 
       height = 8, width = 10)
