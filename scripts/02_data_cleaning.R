

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

cleaning.table <- data_frame(file_name = files) %>%
  mutate(
    species = gsub("data/gbif_occurrences/", "", file_name) %>%
      str_extract(., "[a-zA-Z]+_[a-z]+") %>%
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
assert_that(length(files) == length(unique(d$species)))

#==============================================================================


# Data cleaning

# As a safeguard, filter out any records not from kingdom Plantae
table(d$kingdom, useNA = "ifany")

d <- filter(d, kingdom == "Plantae")

table(d$kingdom, useNA = "ifany")

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

# Remove coordinates falling over the open ocean
d <- cc_sea(
  d, 
  lat = "latitude", 
  lon = "longitude", 
  ref = buffland
)

dim(d)

# Write cleaned GBIF data to disk
write_csv(d, "data/gbif_cleaned/gbif_all.csv")
