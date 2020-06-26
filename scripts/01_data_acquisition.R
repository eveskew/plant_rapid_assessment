

library(tidyverse)
library(rgbif)

#==============================================================================


# Pull in IUCN assessment data to get the species names we need GBIF 
# occurrence data for
a <- read_csv("data/IUCN/assessments.csv") %>%
  arrange(scientificName)
species.names <- pull(a, scientificName)
length(species.names)

# Remove synonymous taxa from the data
# Note: these are IUCN taxa that, when queried on GBIF, return records that
# GBIF assigns to another existing taxa
s <- read_csv("data/gbif_miscellaneous/synonyms_to_remove.csv")
synonymns <- pull(s, species_to_remove)
length(synonymns)

species.names <- species.names[!(species.names %in% synonymns)]
length(species.names)

#==============================================================================


# Loop to get and save GBIF occurrence data for each species

# Define the variables we want from GBIF
fields.of.interest <- c(
  "key", "scientificName", "occurenceStatus",
  "decimalLatitude", "decimalLongitude", 
  "geodeticDatum", "coordinateUncertaintyInMeters",
  "issues", "protocol", "basisOfRecord", "acceptedScientificName", 
  "kingdom", "phylum", "class", "order", "family", "genus", "species",
  "genericName", "specificEpithet", "taxonRank", "taxonomicStatus",
  "year", "month", "day", "eventDate", "countryCode", "country"
  )

# Define the maximum number of records we want from GBIF
limit <- 50000

# Define a data frame to hold metadata regarding the data pull
gbif.metadata <- data.frame(
  species = character(0),
  limit = numeric(0),
  n_records = numeric(0)
)

# Looping the data pull
for (name in species.names) {

  print(paste0(toString(which(species.names == name)), " - ", name))

  records <- occ_search(
    scientificName = name,
    limit = limit,
    return = "data",
    hasCoordinate = TRUE,
    hasGeospatialIssue = FALSE,
    fields = fields.of.interest
  )

  if(is.data.frame(records$data)) {

    write_csv(
      records$data,
      paste0("data/gbif_occurrences/",
             str_replace(name, " ", "_"),
             "_",
             as.character(limit),
             ".csv")
    )

    temp <- data_frame(
      species = name,
      limit = limit,
      n_records = nrow(records$data)
    )

    gbif.metadata <- bind_rows(gbif.metadata, temp)

  } else {

    temp <- data_frame(
      species = name,
      limit = limit,
      n_records = 0
    )

    gbif.metadata <- bind_rows(gbif.metadata, temp)
  }

  write_csv(gbif.metadata, "data/gbif_miscellaneous/gbif_metadata.csv")
}
