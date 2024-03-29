

library(tidyverse)
library(CoordinateCleaner)
library(lubridate)
library(assertthat)

source("R/functions.R")
data("buffland")

#==============================================================================


# Import GBIF species occurrence data

# Clean up the "gbif_occurrences" folder, as needed
files <- list.files("data/gbif_occurrences", full.names = TRUE)

cleaning.table <- tibble(file_name = files) %>%
  mutate(
    species = gsub("data/gbif_occurrences/", "", file_name) %>%
      str_extract(., "[a-zA-Z]+_[a-z\\-]+") %>%
      gsub("_", " ", .),
    limit = as.numeric(str_extract(file_name, "[0-9]+"))
  ) %>%
  arrange(species, limit)

cleaning.table2 <- cleaning.table %>%
  group_by(species) %>%
  count() %>%
  filter(n > 1) %>%
  mutate(needs_cleaning = "yes") %>%
  select(-n)

cleaning.table <- left_join(cleaning.table, cleaning.table2, by = "species")

files.to.delete <- cleaning.table %>%
  filter(needs_cleaning == "yes") %>%
  distinct(species, .keep_all = TRUE) %>%
  pull(file_name)

sapply(files.to.delete, function(x) file.remove(x))

# Pull in every species-level data frame, keeping track of which species name
# was queried using the "query_name" column
d <- data.frame()
files <- list.files("data/gbif_occurrences", full.names = TRUE)

gbif.metadata <- read_csv("data/gbif_miscellaneous/gbif_metadata.csv")
nrow(gbif.metadata) # number of species queried on GBIF
n.species.wo.data <- filter(gbif.metadata, n_records == 0) %>% nrow()
n.species.wo.data
n.species.w.data <- filter(gbif.metadata, n_records > 0) %>% nrow()
n.species.w.data
assert_that(length(files) == n.species.w.data)

d <- plyr::ldply(files, custom_read_csv)

# Arrange by the data by (GBIF) species name and rename columns
d <- d %>%
  arrange(species, key) %>%
  dplyr::rename(
    latitude = decimalLatitude,
    longitude = decimalLongitude
  )

dim(d)

# Verify that the full data frame has the same number of species as we have 
# occurrence files
assert_that(length(unique(d$species)) == length(files))
# Verify that the full data frame has the same number of records as the 
# metadata suggests
assert_that(nrow(d) == sum(gbif.metadata$n_records))

#==============================================================================


# Data cleaning

# As a safeguard, filter out any records not from kingdom Plantae
table(d$kingdom, useNA = "ifany")

d <- filter(d, kingdom == "Plantae")

assert_that(unique(d$kingdom) == "Plantae")

# Add on IUCN assessment information
a <- read_csv("data/IUCN/assessments.csv") %>%
  mutate(iucn_assessment_year = year(assessmentDate))

d <- d %>%
  left_join(
    ., 
    select(a, scientificName, redlistCategory, iucn_assessment_year),
    by = c("query_name" = "scientificName")
  )

# Save the unfiltered GBIF data for Data Deficient species to disk
d %>%
  filter(redlistCategory == "Data Deficient") %>%
  write_csv("data/gbif_miscellaneous/unfiltered_gbif_data_deficient.csv")

# Filter out unwanted "basisOfRecord" values and data points that
# are dated after the IUCN assessment was done
table(d$basisOfRecord, useNA = "ifany")

d <- d %>%
  filter(
    basisOfRecord %in% c("HUMAN_OBSERVATION", "OBSERVATION"),
    year < iucn_assessment_year
  )

table(d$basisOfRecord, useNA = "ifany")
dim(d)

# CoordinateCleaner flagging
d <- d %>%
  mutate(
    # Flag any invalid coordinates 
    val = cc_val(., lat = "latitude", lon = "longitude", value = "flagged"),
    # Flag any identical coordinates 
    equ = cc_equ(., lat = "latitude", lon = "longitude", value = "flagged"),
    # Flag any coordinates near country centroids
    cen = cc_cen(., lat = "latitude", lon = "longitude", 
                 buffer = 1000, value = "flagged"),
    # Flag any coordinates near country capitals
    cap = cc_cap(., lat = "latitude", lon = "longitude", 
                 buffer = 1000, value = "flagged"),
    # Flag any coordinates in the vicinity of GBIF headquarters
    gbif = cc_gbif(., lat = "latitude", lon = "longitude", 
                   buffer = 1000, value = "flagged"),
    # Flag any coordinates in the vicinity of biodiversity institutions
    inst = cc_inst(., lat = "latitude", lon = "longitude", 
                   buffer = 1000, value = "flagged"),
    # Flag coordinates falling over the open ocean
    sea = cc_sea(., lat = "latitude", lon = "longitude", 
                 ref = buffland, value = "flagged")
  )

# CoordinateCleaner filtering
d <- d %>%
  filter(
    val == TRUE,
    equ == TRUE,
    cen == TRUE,
    cap == TRUE,
    gbif == TRUE,
    inst == TRUE,
    sea == TRUE
  ) %>%
  select(-val, -equ, -cen, -cap, -gbif, -inst, -sea)

dim(d)
# How many plant species represented?
n_distinct(d$query_name)
n_distinct(d$species)

#==============================================================================


# Filter to only species with >= 3 occurrence points

d <- d %>%
  left_join(
    .,
    d %>%
      group_by(species) %>%
      summarize(number_of_points = n()),
    by = "species"
  ) %>%
  filter(number_of_points >= 3)

dim(d)
# How many plant species represented?
n_distinct(d$query_name)
n_distinct(d$species)

# Generate and save metadata on this derived dataset for GBIF citation purposes
derived.dataset.metadata <- d %>%
  group_by(datasetKey) %>%
  summarize(nOccurrencesInDerivedDataset = n()) %>%
  ungroup()

assertthat::assert_that(
  sum(derived.dataset.metadata$nOccurrencesInDerivedDataset) == nrow(d)
)

write_csv(
  derived.dataset.metadata, 
  "data/gbif_miscellaneous/gbif_derived_dataset_metadata.csv"
)

#==============================================================================


# Write cleaned GBIF data to disk
write_csv(d, "data/gbif_cleaned/gbif_all.csv")
