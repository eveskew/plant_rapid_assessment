

library(tidyverse)
library(assertthat)

#==============================================================================


# Read in the relevant raw datasets

rcat <- read_csv("data/rcat/rCAT_output.csv") %>%
  mutate(scientificName = query_name) %>%
  dplyr::select(scientificName, everything(), -query_name)

threat.set <- read.csv("data/IUCN/threats.csv") %>%
  mutate(threatCode = code, threatName = name) %>%
  dplyr::select(scientificName, threatCode, threatName)

type.set <- read.csv("data/IUCN/plant_specific.csv") %>%
  mutate(typeCode = code, typeName = name) %>%
  dplyr::select(scientificName, typeCode, typeName)
   
# Join them all into a single dataset

d <- left_join(rcat, threat.set, by = "scientificName") %>%
  left_join(., type.set, by = "scientificName")

# Clean and filter this dataset

d2 <- d %>%
  # Change the very few instances of "Lower Risk/near threatened" to just
  # "Near Threatened"
  mutate(
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
    c("primaryThreat", "secondaryThreat", "tertiaryThreat")
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
      EOOcat_categoryClass == IUCNcat_categoryClass, 1, 0)
  ) %>%
  # Remove NAs
  filter(!is.na(correctlyClassified | underClassified | overClassified)) %>%
  # Select relevant columns
  dplyr::select(
    -gbif_name, -NOP_old:-EOO_clipped_recent_cat, -threatName, 
    -typeCode, -secondaryThreat, -tertiaryThreat
  ) %>%
  # Condensing Plant Types to account for a lack of data in certain groups
  mutate(
    newTypeName = case_when(
      # Condensing all succulents into a single group
      typeName == "Succulent - form unknown" ~ "Succulent",
      typeName == "Succulent - shrub" ~ "Succulent",
      typeName == "Succulent - tree" ~ "Succulent",
      # Assuming that if a tree is large, we'd likely know about it 
      typeName == "Tree - large" ~ "Tree - large",
      typeName == "Tree - small" ~ "Tree - small",
      typeName == "Tree - size unknown" ~ "Tree - small",
      # Condensing all shrubs into a single group
      typeName == "Shrub - large" ~ "Shrub",
      typeName == "Shrub - small" ~ "Shrub",
      typeName == "Shrub - size unknown" ~ "Shrub",
      # Group Annual and Graminoid
      typeName == "Annual" ~ "Annual/Graminoid",
      typeName == "Graminoid" ~ "Annual/Graminoid",
      # Group Fern and Forb or Herb
      typeName == "Fern" ~ "Fern/Forb or Herb",
      typeName == "Forb or Herb" ~ "Fern/Forb or Herb",
      # Group Vines, Epiphyte, Hydrophyte, Lithophyte
      typeName == "Vines" ~ "Vines/Epiphyte/Hydrophyte/Lithophyte",
      typeName == "Epiphyte" ~ "Vines/Epiphyte/Hydrophyte/Lithophyte",
      typeName == "Hydrophyte" ~ "Vines/Epiphyte/Hydrophyte/Lithophyte",
      typeName == "Lithophyte" ~ "Vines/Epiphyte/Hydrophyte/Lithophyte",
      # Geophyte
      typeName == "Geophyte" ~ "Geophyte"
    )
  )

# Widen threats data
wide.threats <- d2 %>%
  group_by(scientificName) %>%
  mutate(threatRow = row_number()) %>%
  mutate(isThreat = 1) %>%
  pivot_wider(
    names_from = primaryThreat, 
    values_from = isThreat, 
    values_fill = list(isThreat = 0)
  )

# Widen plant type data
wide.type <- d2 %>%
  group_by(scientificName) %>%
  mutate(newTypeRow = row_number()) %>%
  mutate(isType = 1) %>%
  pivot_wider(
    names_from = newTypeName, 
    values_from = isType, 
    values_fill = list(isType = 0)
  )

# Collapse into a single dataset

d3 <- wide.type %>%
  dplyr::select(-iucn_assessment_year:-newTypeRow) %>%
  left_join(wide.threats, ., by = "scientificName") %>%
  dplyr::group_by(
    scientificName, 
    overClassified, underClassified, correctlyClassified
  ) %>%
  summarise_at(
    vars(`5`:`Vines/Epiphyte/Hydrophyte/Lithophyte`), 
    max, na.rm = TRUE
  )

#==============================================================================


# Generate modeling dataset for "full" species set

full.data <- left_join(rcat, d3, by = "scientificName") %>%
  mutate(
    EOOcat_long = case_when(
      EOOcat == "CR" ~ "Critically Endangered",
      EOOcat == "EN" ~ "Endangered",
      EOOcat == "VU" ~ "Vulnerable",
      EOOcat == "NT" ~ "Near Threatened",
      EOOcat == "LC" ~ "Least Concern"
    )
  ) 

full.modeling.data <- full.data %>%
  # Remove those species for which there is no threat data
  filter(`NA` != 1) %>%
  # Remove those species with no North American occurrences in GBIF
  # filter(GBIF_points_in_NA == "Yes") %>% 
  # Don't model Data Deficient species here
  filter(iucn_redlist_category != "Data Deficient") %>%
  # Change the very few instances of "Lower Risk/near threatened" to just
  # "Near Threatened" to match rest of dataset
  mutate(
    iucn_redlist_category = ifelse(
      iucn_redlist_category == "Lower Risk/near threatened",
      "Near Threatened", iucn_redlist_category)
  ) %>%
  # Drop the NA column
  dplyr::select(-`NA`)

# Verify every species in this dataset has only one classification direction
col1 <- which(colnames(full.modeling.data) == "overClassified")
col2 <- which(colnames(full.modeling.data) == "correctlyClassified")
class.count <- full.modeling.data %>%
  mutate(class.count = rowSums(.[col1:col2])) %>%
  pull(class.count)
assert_that(sum(class.count == 1) == length(class.count))

# Verify every species in this dataset has at least one associated threat
# type
col1 <- which(colnames(full.modeling.data) == "5")
col2 <- which(colnames(full.modeling.data) == "12")
threat.count <- full.modeling.data %>%
  mutate(threat.count = rowSums(.[col1:col2])) %>%
  pull(threat.count)
assert_that(sum(threat.count > 0) == length(threat.count))

# Verify every species in this dataset has at least one associated plant
# type
col1 <- which(colnames(full.modeling.data) == "Tree - large")
col2 <- which(colnames(full.modeling.data) == "Vines/Epiphyte/Hydrophyte/Lithophyte")
type.count <- full.modeling.data %>%
  mutate(type.count = rowSums(.[col1:col2])) %>%
  pull(type.count)
assert_that(sum(type.count > 0) == length(type.count))

# Write to disk
write_csv(full.modeling.data, "data/modeling_data/full_modeling_data.csv")

#==============================================================================


# Generate modeling dataset for Data Deficient species

dd.modeling.data <- full.data %>%
  filter(iucn_redlist_category == "Data Deficient") %>%
  # Remove those species with no North American occurrences in GBIF
  # filter(GBIF_points_in_NA == "Yes") %>% 
  # Drop the NA column from threats
  dplyr::select(-`NA`)

# Verify every species in this dataset has at least one associated plant
# type
col1 <- which(colnames(dd.modeling.data) == "Tree - large")
col2 <- which(colnames(dd.modeling.data) == "Vines/Epiphyte/Hydrophyte/Lithophyte")
type.count <- dd.modeling.data %>%
  mutate(type.count = rowSums(.[col1:col2])) %>%
  pull(type.count)
assert_that(sum(type.count > 0) == length(type.count))

# Write to disk
write_csv(dd.modeling.data, "data/modeling_data/dd_modeling_data.csv")
