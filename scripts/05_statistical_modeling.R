

library(tidyverse)
library(rethinking)

#==============================================================================


# Import modeling data

f <- read_csv("data/modeling_data/full_modeling_data.csv") %>%
  rename(
    t5 = "5",
    t8 = "8",
    t11 = "11",
    t7 = "7",
    t2 = "2",
    t1 = "1",
    t9 = "9",
    t4 = "4",
    t6 = "6",
    t3 = "3",
    t12 = "12",
    Forb_Herb = "Forb or Herb",
    Vines_Epi_Litho = "Vines/Epiphyte/Lithophyte"
  ) %>%
  mutate(NOP_s = scale(NOP)) %>%
  rename(tNA = X47)


# Model fitting the effect of number of points and plant type

# Verify every species in this dataset has at least one associated plant
# type
col1 <- which(colnames(f) == "Tree")
col2 <- which(colnames(f) == "Fern")
type.count <- f %>%
  mutate(type.count = rowSums(.[col1:col2])) %>%
  pull(type.count)
assert_that(sum(type.count > 0) == length(type.count))

stan.data <- select(f, correctlyClassified, t5:NOP_s)

nrow(stan.data)

m.type.stan <- map2stan(
  data = stan.data,
  alist(
    correctlyClassified ~ dbinom(1, p),
    logit(p) <-   
      a + bN*NOP_s +
      aTree * Tree +
      aShrub * Shrub + 
      aGraminoid * Graminoid +
      aForb_Herb * Forb_Herb +
      aAnnual * Annual +
      aGeophyte * Geophyte +
      aSucculent * Succulent + 
      aHydrophyte * Hydrophyte +
      aVines_Epi_Litho * Vines_Epi_Litho +
      aFern * Fern,
    a ~ dnorm(0, 5),
    bN ~ dnorm(0, 5),
    sigma_plant_type ~ dcauchy(0, 5),
    aTree ~ dnorm(0, sigma_plant_type),
    aShrub ~ dnorm(0, sigma_plant_type),
    aGraminoid ~ dnorm(0, sigma_plant_type),
    aForb_Herb ~ dnorm(0, sigma_plant_type),
    aAnnual ~ dnorm(0, sigma_plant_type),
    aGeophyte ~ dnorm(0, sigma_plant_type),
    aSucculent ~ dnorm(0, sigma_plant_type), 
    aHydrophyte ~ dnorm(0, sigma_plant_type),
    aVines_Epi_Litho ~ dnorm(0, sigma_plant_type)
    aFern ~ dnorm(0, sigma_plant_type),
  ),
  chains = 4,
  iter = 7500,
  warmup = 2500,
  rng_seed = 8,
  control = list(adapt_delta = 0.99)
)

precis(m.type.stan, prob = 0.99)

plot(m.type.stan)


# Model fitting the effect of number of points and plant threats

f2 <- filter(f, tNA != 1) %>%
  # Drop the NA column
  dplyr::select(-tNA)

nrow(f2)

# Verify every species in this dataset has at least one associated threat
# type
col1 <- which(colnames(f2) == "t5")
col2 <- which(colnames(f2) == "t12")
threat.count <- f2 %>%
  mutate(threat.count = rowSums(.[col1:col2])) %>%
  pull(threat.count)
assert_that(sum(threat.count > 0) == length(threat.count))

stan.data <- select(f2, correctlyClassified, t5:NOP_s)

nrow(stan.data)

m.threat.stan <- map2stan(
  data = stan.data,
  alist(
    correctlyClassified ~ dbinom(1, p),
    logit(p) <-   
      a + bN*NOP_s +
      aResidential_and_commercial_development * t1 + 
      aAgriculture_and_Aquaculture * t2 + 
      aEnergy_production_and_mining * t3 + 
      aTransportation_and_service_corridors * t4 + 
      aBiological_resource_use * t5 + 
      aHuman_intrusions_and_disturbance * t6 + 
      aNatural_system_modifications * t7 + 
      aInvasive_and_other_problematic_species_genes_and_disease * t8 + 
      aPollution * t9 + 
      aClimate_change_and_severe_weather * t11 + 
      aOther_options * t12,
    a ~ dnorm(0, 5),
    bN ~ dnorm(0, 5),
    sigma_threats ~ dcauchy(0, 5),
    aResidential_and_commercial_development ~ dnorm(0, sigma_threats),
    aAgriculture_and_Aquaculture ~ dnorm(0, sigma_threats), 
    aEnergy_production_and_mining ~ dnorm(0, sigma_threats), 
    aTransportation_and_service_corridors ~ dnorm(0, sigma_threats), 
    aBiological_resource_use ~ dnorm(0, sigma_threats), 
    aHuman_intrusions_and_disturbance ~ dnorm(0, sigma_threats),
    aNatural_system_modifications ~ dnorm(0, sigma_threats),
    aInvasive_and_other_problematic_species_genes_and_disease ~ dnorm(0, sigma_threats),
    aPollution ~ dnorm(0, sigma_threats),
    aClimate_change_and_severe_weather ~ dnorm(0, sigma_threats),
    aOther_options ~ dnorm(0, sigma_threats)
  ),
  chains = 4,
  iter = 7500,
  warmup = 2500,
  rng_seed = 8,
  control = list(adapt_delta = 0.99)
)

precis(m.threat.stan, prob = 0.99)

plot(m.threat.stan)

#==============================================================================


# Save relevant models

saveRDS(m.type.stan, "data/fit_models/m.type.stan.RDS")
saveRDS(m.threat.stan, "data/fit_models/m.threat.stan.RDS")
