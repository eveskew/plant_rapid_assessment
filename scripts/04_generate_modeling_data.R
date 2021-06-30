

library(tidyverse)
library(assertthat)
library(natserv)

#==============================================================================


# Read in the relevant raw datasets

rcat <- read_csv("data/rcat/rCAT_output.csv") %>%
  mutate(scientificName = query_name) %>%
  dplyr::select(scientificName, everything(), -query_name)

threat.set <- read.csv("data/IUCN/threats.csv") %>%
  distinct() %>%
  mutate(threatCode = code, threatName = name) %>%
  dplyr::select(scientificName, threatCode, threatName)

type.set <- read.csv("data/IUCN/plant_specific.csv") %>%
  distinct() %>%
  mutate(typeCode = code, typeName = name) %>%
  dplyr::select(scientificName, typeCode, typeName)
   
# Join them all into a single dataset

d <- rcat %>%
  left_join(., threat.set, by = "scientificName") %>%
  left_join(., type.set, by = "scientificName")

# Clean and filter this dataset

d2 <- d %>%
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
      TRUE ~ typeName
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
  mutate(typeRow = row_number()) %>%
  mutate(isType = 1) %>%
  pivot_wider(
    names_from = newTypeName, 
    values_from = isType, 
    values_fill = list(isType = 0)
  )

# Collapse into a single dataset

d3 <- wide.type %>%
  dplyr::select(-iucn_assessment_year:-typeRow) %>%
  left_join(wide.threats, ., by = "scientificName") %>%
  dplyr::group_by(
    scientificName, 
    overClassified, underClassified, correctlyClassified
  ) %>%
  summarise_at(
    vars(`5`:`Fern`), 
    max, na.rm = TRUE
  )

#==============================================================================


# Generate NatureServe rank data for all available species

# Initialize empty data frame
ns <- data.frame()

# Loop through IUCN species names to pull NatureServe rank data 
for(i in d3$scientificName) {
  
  print(i)
  temp <- ns_search_spp(text = i)$results
  slice.row <- which(temp$scientificName == i)
  
  # If data was returned, only keep the row(s) matching the exact species
  # name and only keep select columns
  if(nrow(temp) > 0) {
    
    temp <- temp %>%
      select(
        recordType, elementGlobalId,
        scientificName, primaryCommonName,
        classificationStatus,
        roundedGRank, gRank
      ) %>%
      slice(slice.row) %>%
      mutate(query_name = i) %>%
      select(query_name, everything())
    
    ns <- bind_rows(ns, temp)
  }
}

# Get rid of duplicates if both provisional and standard classifications 
# were returned
ns.mod <- ns %>%
  arrange(scientificName, desc(classificationStatus)) %>%
  distinct(scientificName, .keep_all = TRUE)

write_csv(ns.mod, "data/NatureServe/ns_data.csv")

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
  ) %>%
  # Change the very few instances of "Lower Risk/least concern" to just
  # "Least Concern" to match rest of dataset
  # Change the very few instances of "Lower Risk/near threatened" to just
  # "Near Threatened" to match rest of dataset
  mutate(
    iucn_redlist_category = ifelse(
      iucn_redlist_category == "Lower Risk/least concern",
      "Least Concern", iucn_redlist_category),
    iucn_redlist_category = ifelse(
      iucn_redlist_category == "Lower Risk/near threatened",
      "Near Threatened", iucn_redlist_category)
  )

full.modeling.data <- full.data %>%
  # Don't model Data Deficient species here
  filter(iucn_redlist_category != "Data Deficient")
  
# Verify every species in this dataset has only one classification direction
col1 <- which(colnames(full.modeling.data) == "overClassified")
col2 <- which(colnames(full.modeling.data) == "correctlyClassified")
class.count <- full.modeling.data %>%
  mutate(class.count = rowSums(.[col1:col2])) %>%
  pull(class.count)
assert_that(sum(class.count == 1) == length(class.count))

# Write to disk
write_csv(full.modeling.data, "data/modeling_data/full_modeling_data.csv")

#==============================================================================


# Generate modeling dataset for Data Deficient species

dd.modeling.data <- full.data %>%
  filter(iucn_redlist_category == "Data Deficient")

# Verify every species in this dataset has at least one associated plant
# type
col1 <- which(colnames(dd.modeling.data) == "Tree")
col2 <- which(colnames(dd.modeling.data) == "Fern")
type.count <- dd.modeling.data %>%
  mutate(type.count = rowSums(.[col1:col2])) %>%
  pull(type.count)
assert_that(sum(type.count > 0) == length(type.count))

# Write to disk
write_csv(dd.modeling.data, "data/modeling_data/dd_modeling_data.csv")
