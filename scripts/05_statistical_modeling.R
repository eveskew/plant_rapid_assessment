

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
    Tree_large = "Tree - large",
    Tree_small = "Tree - small",
    Annual_Graminoid = "Annual/Graminoid",
    Fern_Forb_Herb = "Fern/Forb or Herb",
    Vines_Epi_Hydro_Litho = "Vines/Epiphyte/Hydrophyte/Lithophyte"
  ) %>%
  mutate(NOP_s = scale(NOP))


# Model fitting the effect of number of points and plant type

stan.data <- select(f, correctlyClassified, t5:NOP_s)

m.type.stan <- map2stan(
  data = stan.data,
  alist(
    correctlyClassified ~ dbinom(1, p),
    logit(p) <-   
      a + bN*NOP_s +
      aTree_large * Tree_large + 
      aTree_small * Tree_small + 
      aShrub * Shrub + 
      aSucculent * Succulent + 
      aGeophyte * Geophyte + 
      aAnnual_Graminoid * Annual_Graminoid + 
      aFern_Forb_Herb * Fern_Forb_Herb + 
      aVines_Epi_Hydro_Litho * Vines_Epi_Hydro_Litho,
    a ~ dnorm(0, 5),
    bN ~ dnorm(0, 5),
    sigma_plant_type ~ dcauchy(0, 5),
    aTree_large ~ dnorm(0, sigma_plant_type),
    aTree_small ~ dnorm(0, sigma_plant_type), 
    aShrub ~ dnorm(0, sigma_plant_type),
    aSucculent ~ dnorm(0, sigma_plant_type), 
    aGeophyte ~ dnorm(0, sigma_plant_type),
    aAnnual_Graminoid ~ dnorm(0, sigma_plant_type),
    aFern_Forb_Herb ~ dnorm(0, sigma_plant_type),
    aVines_Epi_Hydro_Litho ~ dnorm(0, sigma_plant_type)
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


# Model fitting the effect of number of points, plant type, and plant threats

m.stan3 <- map2stan(
  data = stan.data,
  alist(
    correctlyClassified ~ dbinom(1, p),
    logit(p) <-   
      a + bN*NOP_s +
      aTree_large * Tree_large + 
      aTree_small * Tree_small + 
      aShrub * Shrub + 
      aSucculent * Succulent + 
      aGeophyte * Geophyte + 
      aAnnual_Graminoid * Annual_Graminoid + 
      aFern_Forb_Herb * Fern_Forb_Herb + 
      aVines_Epi_Hydro_Litho * Vines_Epi_Hydro_Litho +
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
    sigma_plant_type ~ dcauchy(0, 5),
    sigma_threats ~ dcauchy(0, 5),
    aTree_large ~ dnorm(0, sigma_plant_type),
    aTree_small ~ dnorm(0, sigma_plant_type), 
    aShrub ~ dnorm(0, sigma_plant_type),
    aSucculent ~ dnorm(0, sigma_plant_type), 
    aGeophyte ~ dnorm(0, sigma_plant_type),
    aAnnual_Graminoid ~ dnorm(0, sigma_plant_type),
    aFern_Forb_Herb ~ dnorm(0, sigma_plant_type),
    aVines_Epi_Hydro_Litho ~ dnorm(0, sigma_plant_type),
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
  control = list(adapt_delta = 0.99)
)

precis(m.stan3, prob = 0.99)

plot(m.stan3)

#==============================================================================


# Save relevant models

saveRDS(m.type.stan, "data/fit_models/m.type.stan.RDS")
saveRDS(m.threat.stan, "data/fit_models/m.threat.stan.RDS")
