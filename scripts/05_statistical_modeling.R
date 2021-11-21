

library(tidyverse)
library(rethinking)
library(assertthat)

#==============================================================================


# Import full modeling data

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
    t10 = "10",
    t12 = "12",
    Forb_Herb = "Forb or Herb",
    Moss_Hydrophyte = "Moss/Hydrophyte",
    Vines_Epi_Litho = "Vines/Epiphyte/Lithophyte"
  ) %>%
  mutate(NOP_s = scale(NOP)) %>%
  rename(tNA = `NA`)

# Verify across the 13 threat categories that none are empty
f %>%
  select(t5:t12) %>%
  colSums()

# Verify across the 10 plant type categories that none are empty
f %>%
  select(Tree:Fern) %>%
  colSums()

# Sample size
nrow(f)


# Import reduced modeling data (without LC species)

nonLC <- read_csv("data/modeling_data/nonLC_modeling_data.csv") %>%
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
    t10 = "10",
    t12 = "12",
    Forb_Herb = "Forb or Herb",
    Moss_Hydrophyte = "Moss/Hydrophyte",
    Vines_Epi_Litho = "Vines/Epiphyte/Lithophyte"
  ) %>%
  mutate(NOP_s = scale(NOP)) %>%
  rename(tNA = `NA`)

nonLC %>%
  select(t5:t12) %>%
  colSums()

nonLC %>%
  select(Tree:Fern) %>%
  colSums()

# Subset to eliminate empty predictor columns
nonLC <- select(nonLC, -c(tNA, Graminoid, Annual, Fern))

# Verify across the 12 threat categories that none are empty
nonLC %>%
  select(t5:t12) %>%
  colSums()

# Verify across the 7 plant type categories that none are empty
nonLC %>%
  select(Tree:Vines_Epi_Litho) %>%
  colSums()

# Sample size
nrow(nonLC)

#==============================================================================


# Models fitting the effect of number of points and plant type


# Verify every species in this dataset has at least one associated plant
# type
col1 <- which(colnames(f) == "Tree")
col2 <- which(colnames(f) == "Fern")
type.count <- f %>%
  mutate(type.count = rowSums(.[col1:col2])) %>%
  pull(type.count)
assert_that(sum(type.count > 0) == length(type.count))

stan.data <- f %>%
  select(
    correctlyClassified, underClassified, overClassified, 
    t5:NOP_s
  )

nrow(stan.data)

# Correct classification model

m.correct.type.stan <- ulam(
  data = stan.data,
  alist(
    correctlyClassified ~ dbinom(1, p),
    logit(p) <-   
      a + bN * NOP_s +
      aTree * Tree +
      aShrub * Shrub + 
      aGraminoid * Graminoid +
      aForb_Herb * Forb_Herb +
      aAnnual * Annual +
      aGeophyte * Geophyte +
      aSucculent * Succulent + 
      aMoss_Hydrophyte * Moss_Hydrophyte +
      aVines_Epi_Litho * Vines_Epi_Litho +
      aFern * Fern,
    a ~ dnorm(0, 2),
    bN ~ dnorm(0, 1),
    sigma_plant_type ~ dexp(1),
    aTree ~ dnorm(0, sigma_plant_type),
    aShrub ~ dnorm(0, sigma_plant_type),
    aGraminoid ~ dnorm(0, sigma_plant_type),
    aForb_Herb ~ dnorm(0, sigma_plant_type),
    aAnnual ~ dnorm(0, sigma_plant_type),
    aGeophyte ~ dnorm(0, sigma_plant_type),
    aSucculent ~ dnorm(0, sigma_plant_type), 
    aMoss_Hydrophyte ~ dnorm(0, sigma_plant_type),
    aVines_Epi_Litho ~ dnorm(0, sigma_plant_type),
    aFern ~ dnorm(0, sigma_plant_type)
  ),
  chains = 4, cores = 4,
  iter = 7500, warmup = 2500,
  seed = 8,
  control = list(adapt_delta = 0.99),
  cmdstan = TRUE
)

precis(m.correct.type.stan, prob = 0.99)

rstan::traceplot(
  m.correct.type.stan@stanfit,
  pars = m.correct.type.stan@pars
)

# Save model
saveRDS(m.correct.type.stan, "data/fit_models/m.correct.type.stan.RDS")


# Fit the same model with underclassification as the outcome

m.under.type.stan <- ulam(
  data = stan.data,
  alist(
    underClassified ~ dbinom(1, p),
    logit(p) <-   
      a + bN * NOP_s +
      aTree * Tree +
      aShrub * Shrub + 
      aGraminoid * Graminoid +
      aForb_Herb * Forb_Herb +
      aAnnual * Annual +
      aGeophyte * Geophyte +
      aSucculent * Succulent + 
      aMoss_Hydrophyte * Moss_Hydrophyte +
      aVines_Epi_Litho * Vines_Epi_Litho +
      aFern * Fern,
    a ~ dnorm(0, 2),
    bN ~ dnorm(0, 1),
    sigma_plant_type ~ dexp(1),
    aTree ~ dnorm(0, sigma_plant_type),
    aShrub ~ dnorm(0, sigma_plant_type),
    aGraminoid ~ dnorm(0, sigma_plant_type),
    aForb_Herb ~ dnorm(0, sigma_plant_type),
    aAnnual ~ dnorm(0, sigma_plant_type),
    aGeophyte ~ dnorm(0, sigma_plant_type),
    aSucculent ~ dnorm(0, sigma_plant_type), 
    aMoss_Hydrophyte ~ dnorm(0, sigma_plant_type),
    aVines_Epi_Litho ~ dnorm(0, sigma_plant_type),
    aFern ~ dnorm(0, sigma_plant_type)
  ),
  chains = 4, cores = 4,
  iter = 7500, warmup = 2500,
  seed = 8,
  control = list(adapt_delta = 0.99),
  cmdstan = TRUE
)

precis(m.under.type.stan, prob = 0.99)

rstan::traceplot(
  m.under.type.stan@stanfit,
  pars = m.under.type.stan@pars
)

# Save model
saveRDS(m.under.type.stan, "data/fit_models/m.under.type.stan.RDS")


# Fit the same model, excluding LC species and including predictors
# related to criterion B

# Verify every species in this dataset has at least one associated plant
# type
col1 <- which(colnames(nonLC) == "Tree")
col2 <- which(colnames(nonLC) == "Vines_Epi_Litho")
type.count <- nonLC %>%
  mutate(type.count = rowSums(.[col1:col2])) %>%
  pull(type.count)
assert_that(sum(type.count > 0) == length(type.count))

stan.data <- nonLC %>%
  select(
    correctlyClassified, underClassified, overClassified, 
    B1_binary:NOP_s
  )

nrow(stan.data)

m.under.type.nonLC.stan <- ulam(
  data = stan.data,
  alist(
    underClassified ~ dbinom(1, p),
    logit(p) <-   
      a + bN * NOP_s +
      bB1 * B1_binary + bB2 * B2_binary +
      aTree * Tree +
      aShrub * Shrub +
      aForb_Herb * Forb_Herb +
      aGeophyte * Geophyte +
      aSucculent * Succulent + 
      aMoss_Hydrophyte * Moss_Hydrophyte +
      aVines_Epi_Litho * Vines_Epi_Litho,
    a ~ dnorm(0, 2),
    bN ~ dnorm(0, 1),
    bB1 ~ dnorm(0, 1),
    bB2 ~ dnorm(0, 1),
    sigma_plant_type ~ dexp(1),
    aTree ~ dnorm(0, sigma_plant_type),
    aShrub ~ dnorm(0, sigma_plant_type),
    aForb_Herb ~ dnorm(0, sigma_plant_type),
    aGeophyte ~ dnorm(0, sigma_plant_type),
    aSucculent ~ dnorm(0, sigma_plant_type), 
    aMoss_Hydrophyte ~ dnorm(0, sigma_plant_type),
    aVines_Epi_Litho ~ dnorm(0, sigma_plant_type)
  ),
  chains = 4, cores = 4,
  iter = 7500, warmup = 2500,
  seed = 8,
  control = list(adapt_delta = 0.99),
  cmdstan = TRUE
)

precis(m.under.type.nonLC.stan, prob = 0.99)

rstan::traceplot(
  m.under.type.nonLC.stan@stanfit,
  pars = m.under.type.nonLC.stan@pars
)

# Save model
saveRDS(m.under.type.nonLC.stan, "data/fit_models/m.under.type.nonLC.stan.RDS")

#==============================================================================


# Models fitting the effect of number of points and plant threats


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

stan.data <- f2 %>%
  select(
    correctlyClassified, underClassified, overClassified, 
    t5:NOP_s
  )

nrow(stan.data)

# Correct classification model

m.correct.threat.stan <- ulam(
  data = stan.data,
  alist(
    correctlyClassified ~ dbinom(1, p),
    logit(p) <-   
      a + bN * NOP_s +
      aResidential_and_commercial_development * t1 + 
      aAgriculture_and_aquaculture * t2 + 
      aEnergy_production_and_mining * t3 + 
      aTransportation_and_service_corridors * t4 + 
      aBiological_resource_use * t5 + 
      aHuman_intrusions_and_disturbance * t6 + 
      aNatural_system_modifications * t7 + 
      aInvasive * t8 + 
      aPollution * t9 + 
      aGeological_events * t10 +
      aClimate_change_and_severe_weather * t11 + 
      aOther_options * t12,
    a ~ dnorm(0, 2),
    bN ~ dnorm(0, 1),
    sigma_threats ~ dexp(1),
    aResidential_and_commercial_development ~ dnorm(0, sigma_threats),
    aAgriculture_and_aquaculture ~ dnorm(0, sigma_threats), 
    aEnergy_production_and_mining ~ dnorm(0, sigma_threats), 
    aTransportation_and_service_corridors ~ dnorm(0, sigma_threats), 
    aBiological_resource_use ~ dnorm(0, sigma_threats), 
    aHuman_intrusions_and_disturbance ~ dnorm(0, sigma_threats),
    aNatural_system_modifications ~ dnorm(0, sigma_threats),
    aInvasive ~ dnorm(0, sigma_threats),
    aPollution ~ dnorm(0, sigma_threats),
    aGeological_events ~ dnorm(0, sigma_threats),
    aClimate_change_and_severe_weather ~ dnorm(0, sigma_threats),
    aOther_options ~ dnorm(0, sigma_threats)
  ),
  chains = 4, cores = 4,
  iter = 7500, warmup = 2500,
  seed = 8,
  control = list(adapt_delta = 0.99),
  cmdstan = TRUE
)

precis(m.correct.threat.stan, prob = 0.99)

rstan::traceplot(
  m.correct.threat.stan@stanfit,
  pars = m.correct.threat.stan@pars
)

# Save model
saveRDS(m.correct.threat.stan, "data/fit_models/m.correct.threat.stan.RDS")


# Fit the same model with underclassification as the outcome

m.under.threat.stan <- ulam(
  data = stan.data,
  alist(
    underClassified ~ dbinom(1, p),
    logit(p) <-   
      a + bN * NOP_s +
      aResidential_and_commercial_development * t1 + 
      aAgriculture_and_aquaculture * t2 + 
      aEnergy_production_and_mining * t3 + 
      aTransportation_and_service_corridors * t4 + 
      aBiological_resource_use * t5 + 
      aHuman_intrusions_and_disturbance * t6 + 
      aNatural_system_modifications * t7 + 
      aInvasive * t8 + 
      aPollution * t9 + 
      aGeological_events * t10 +
      aClimate_change_and_severe_weather * t11 + 
      aOther_options * t12,
    a ~ dnorm(0, 2),
    bN ~ dnorm(0, 1),
    sigma_threats ~ dexp(1),
    aResidential_and_commercial_development ~ dnorm(0, sigma_threats),
    aAgriculture_and_aquaculture ~ dnorm(0, sigma_threats), 
    aEnergy_production_and_mining ~ dnorm(0, sigma_threats), 
    aTransportation_and_service_corridors ~ dnorm(0, sigma_threats), 
    aBiological_resource_use ~ dnorm(0, sigma_threats), 
    aHuman_intrusions_and_disturbance ~ dnorm(0, sigma_threats),
    aNatural_system_modifications ~ dnorm(0, sigma_threats),
    aInvasive ~ dnorm(0, sigma_threats),
    aPollution ~ dnorm(0, sigma_threats),
    aGeological_events ~ dnorm(0, sigma_threats),
    aClimate_change_and_severe_weather ~ dnorm(0, sigma_threats),
    aOther_options ~ dnorm(0, sigma_threats)
  ),
  chains = 4, cores = 4,
  iter = 7500, warmup = 2500,
  seed = 8,
  control = list(adapt_delta = 0.99),
  cmdstan = TRUE
)

precis(m.under.threat.stan, prob = 0.99)

rstan::traceplot(
  m.under.threat.stan@stanfit,
  pars = m.under.threat.stan@pars
)

# Save model
saveRDS(m.under.threat.stan, "data/fit_models/m.under.threat.stan.RDS")


# Fit the same model, excluding LC species and including predictors
# related to criterion B

# Verify every species in this dataset has at least one associated plant
# type
col1 <- which(colnames(nonLC) == "t5")
col2 <- which(colnames(nonLC) == "t12")
type.count <- nonLC %>%
  mutate(type.count = rowSums(.[col1:col2])) %>%
  pull(type.count)
assert_that(sum(type.count > 0) == length(type.count))

stan.data <- nonLC %>%
  select(
    correctlyClassified, underClassified, overClassified, 
    B1_binary:NOP_s
  )

nrow(stan.data)

m.under.threat.nonLC.stan <- ulam(
  data = stan.data,
  alist(
    underClassified ~ dbinom(1, p),
    logit(p) <-   
      a + bN * NOP_s + 
      bB1 * B1_binary + bB2 * B2_binary +
      aResidential_and_commercial_development * t1 + 
      aAgriculture_and_aquaculture * t2 + 
      aEnergy_production_and_mining * t3 + 
      aTransportation_and_service_corridors * t4 + 
      aBiological_resource_use * t5 + 
      aHuman_intrusions_and_disturbance * t6 + 
      aNatural_system_modifications * t7 + 
      aInvasive * t8 + 
      aPollution * t9 + 
      aGeological_events * t10 + 
      aClimate_change_and_severe_weather * t11 + 
      aOther_options * t12,
    a ~ dnorm(0, 2),
    bN ~ dnorm(0, 1),
    bB1 ~ dnorm(0, 1),
    bB2 ~ dnorm(0, 1),
    sigma_threats ~ dexp(1),
    aResidential_and_commercial_development ~ dnorm(0, sigma_threats),
    aAgriculture_and_aquaculture ~ dnorm(0, sigma_threats), 
    aEnergy_production_and_mining ~ dnorm(0, sigma_threats), 
    aTransportation_and_service_corridors ~ dnorm(0, sigma_threats), 
    aBiological_resource_use ~ dnorm(0, sigma_threats), 
    aHuman_intrusions_and_disturbance ~ dnorm(0, sigma_threats),
    aNatural_system_modifications ~ dnorm(0, sigma_threats),
    aInvasive ~ dnorm(0, sigma_threats),
    aPollution ~ dnorm(0, sigma_threats),
    aGeological_events ~ dnorm(0, sigma_threats),
    aClimate_change_and_severe_weather ~ dnorm(0, sigma_threats),
    aOther_options ~ dnorm(0, sigma_threats)
  ),
  chains = 4, cores = 4,
  iter = 7500, warmup = 2500,
  seed = 8,
  control = list(adapt_delta = 0.99),
  cmdstan = TRUE
)

precis(m.under.threat.nonLC.stan, prob = 0.99)

rstan::traceplot(
  m.under.threat.nonLC.stan@stanfit,
  pars = m.under.threat.nonLC.stan@pars
)

# Save model
saveRDS(m.under.threat.nonLC.stan, "data/fit_models/m.under.threat.nonLC.stan.RDS")
