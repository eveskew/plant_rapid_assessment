

library(tidyverse)
library(rCAT)
library(rnaturalearth)
library(sf)
library(wesanderson)

source("R/functions.R")

#==============================================================================


# Import the cleaned GBIF occurrence data
d <- read_csv("data/gbif_cleaned/gbif_all.csv")

# Import the IUCN assessment data
a <- read_csv("data/IUCN/assessments.csv")

# Create recent (most recent 10 years) and old data sets
d_recent <- filter(d, year >= (iucn_assessment_year - 10))
d_old <- filter(d, year < (iucn_assessment_year - 10))

#==============================================================================


# Calculate AOO and EOO using the rCAT package

# Do a batch calculation of metrics using rCAT
rcat <- ConBatch(
  taxa = d$species, 
  lat = d$latitude, 
  long = d$longitude, 
  cellsize = 2000
) %>%
  # Rename the AOO variable
  rename(AOOkm2 = AOO2km) %>%
  # Convert relevant variables to numeric data
  mutate_at(c("NOP", "MER", "EOOkm2", "AOOkm2"), as.numeric) %>%
  # Join in data indicating the species name that was originally queried
  # in GBIF and the IUCN assessment year
  left_join(
    .,
    distinct(d, species, query_name, iucn_assessment_year),
    by = c("taxa" = "species")
  ) %>%
  # Join in data indicating the IUCN status of the queried species
  left_join(
    .,
    distinct(a, scientificName, redlistCategory),
    by = c("query_name" = "scientificName")
  ) %>%
  # Rename variables
  rename(
    gbif_name = taxa,
    iucn_redlist_category = redlistCategory
  ) %>%
  # Rearrange variables
  select(query_name, iucn_redlist_category, everything())

dim(rcat)

# Do a batch calculation of metrics using rCAT on the older data
rcat.old <- ConBatch(
  taxa = d_old$species, 
  lat = d_old$latitude, 
  long = d_old$longitude, 
  cellsize = 2000
) %>%
  # Rename the AOO variable
  rename(AOOkm2 = AOO2km) %>%
  # Convert relevant variables to numeric data
  mutate_at(c("NOP", "MER", "EOOkm2", "AOOkm2"), as.numeric) %>%
  # Join in data indicating the species name that was originally queried
  # in GBIF
  left_join(
    .,
    distinct(d, species, query_name),
    by = c("taxa" = "species")
  ) %>%
  select(-taxa) %>%
  # Rename variables
  rename(
    NOP_old = NOP,
    MER_old = MER,
    EOOkm2_old = EOOkm2,
    AOOkm2_old = AOOkm2,
    EOOcat_old = EOOcat,
    AOOcat_old = AOOcat
  )

# Do a batch calculation of metrics using rCAT on the more recent data
rcat.recent <- ConBatch(
  taxa = d_recent$species, 
  lat = d_recent$latitude, 
  long = d_recent$longitude, 
  cellsize = 2000
) %>%
  # Rename the AOO variable
  rename(AOOkm2 = AOO2km) %>%
  # Convert relevant variables to numeric data
  mutate_at(c("NOP", "MER", "EOOkm2", "AOOkm2"), as.numeric) %>%
  # Join in data indicating the species name that was originally queried
  # in GBIF
  left_join(
    .,
    distinct(d, species, query_name),
    by = c("taxa" = "species")
  ) %>%
  select(-taxa) %>%
  # Rename variables
  rename(
    NOP_recent = NOP,
    MER_recent = MER,
    EOOkm2_recent = EOOkm2,
    AOOkm2_recent = AOOkm2,
    EOOcat_recent = EOOcat,
    AOOcat_recent = AOOcat
  )

# Join in old and recent rCAT data
rcat <- rcat %>%
  left_join(., rcat.old, by = "query_name") %>%
  left_join(., rcat.recent, by = "query_name")

# Join in information on AOO and EOO as determined during the IUCN
# assessment
a2 <- read_csv("data/IUCN/all_other_fields.csv") %>%
  select(scientificName, AOO.range, EOO.range) %>%
  mutate(
    AOO.range.mod = AOO.range %>%
      str_replace_all("~", "") %>%
      str_replace_all(">", "") %>%
      str_replace_all(",", "") %>%
      str_replace_all("-[0-9]*", "") %>%
      as.numeric(),
    EOO.range.mod = EOO.range %>%
      str_replace_all("~", "") %>%
      str_replace_all(">", "") %>%
      str_replace_all(",", "") %>%
      str_replace_all("-[0-9]*", "") %>%
      as.numeric()
  ) %>%
  rename(
    AOO_assessment = AOO.range.mod,
    EOO_assessment = EOO.range.mod
  ) %>%
  mutate(
    AOO_assessment_cat = AOORating_custom(AOO_assessment),
    EOO_assessment_cat = EOORating_custom(EOO_assessment)
  )

rcat <- left_join(rcat, a2, by = c("query_name" = "scientificName"))

# Generate empty columns to hold our customized EOO calculations
rcat$EOOkm2_manual <- rep(NA, nrow(rcat))
rcat$EOOkm2_manual_old <- rep(NA, nrow(rcat))
rcat$EOOkm2_manual_recent <- rep(NA, nrow(rcat))
rcat$EOOkm2_clipped <- rep(NA, nrow(rcat))
rcat$EOOkm2_clipped_old <- rep(NA, nrow(rcat))
rcat$EOOkm2_clipped_recent <- rep(NA, nrow(rcat))

#==============================================================================


# Make species-level plots for all occurrence data

# Set up to plot

# Make a table indicating colors to be used for different types of 
# occurrence records
basisOfRecord.colors <- c(
  FOSSIL_SPECIMEN = "brown",
  HUMAN_OBSERVATION = wes_palette("Darjeeling1", 5)[1], 
  LITERATURE = wes_palette("Darjeeling1", 5)[3], 
  LIVING_SPECIMEN = "yellow", 
  MACHINE_OBSERVATION = "blue",
  MATERIAL_SAMPLE = wes_palette("Darjeeling1", 5)[4], 
  OBSERVATION = wes_palette("Darjeeling1", 5)[5], 
  PRESERVED_SPECIMEN = wes_palette("Darjeeling1", 5)[2], 
  UNKNOWN = "black"
)

# Which projection will we use to calculate EOO?
mod.proj <- "+proj=moll +datum=WGS84 +units=km"

# Grab a map of all countries in the world and convert one version to
# our desired projection
world <- ne_countries(returnclass = "sf", scale = 110)
world.mod <- st_transform(world, crs = mod.proj)

# Generate a global land surface shape (i.e., no country borders) in both
# lat/long and a proper projection format
# Note: this particular strategy was needed because "st_union()" on the 
# "world" object (in sf format) resulted in strange discontinuities 
# around rivers (like the Rio Grande) that threw off area calculations
world.union <- ne_countries(scale = 110) %>%
  maptools::unionSpatialPolygons(., IDs = rep("world", nrow(world))) %>%
  st_as_sf()
plot(world.union, col = "lightgray")

world.mod.union <- ne_countries(scale = 110) %>%
  maptools::unionSpatialPolygons(., IDs = rep("world", nrow(world))) %>%
  st_as_sf() %>%
  st_transform(crs = mod.proj) %>%
  st_buffer(dist = 0)
plot(world.mod.union, col = "lightgreen")
  

# Plot and calculate new EOO metrics

for(species.to.plot in rcat$gbif_name) {
  
  print(paste0("Plotting: ", species.to.plot))
  
  species.data <- filter(d, species == species.to.plot)
  species.data.old <- filter(d_old, species == species.to.plot)
  species.data.recent <- filter(d_recent, species == species.to.plot)
  rcat.data <- filter(rcat, gbif_name == species.to.plot)
  
  # Full data
  
  # Generate hull and clipped polygon using lat/long "projection"
  d.sf <- st_as_sf(
    species.data, 
    coords = c("longitude", "latitude"), 
    crs = "+proj=longlat +datum=WGS84"
  )
  hull <- st_convex_hull(st_union(d.sf))
  mask <- st_intersection(hull, world.union)
  
  # Generate hull and clipped polygon using real projection
  d.sf.mod <- st_transform(d.sf, crs = mod.proj)
  hull.mod <- st_convex_hull(st_union(d.sf.mod))
  mask.mod <- st_intersection(hull.mod, world.mod.union)
  
  # Record polygon area values in the rcat data frame
  rcat$EOOkm2_manual[rcat$gbif_name == species.to.plot] <- 
    ifelse(
      length(as.numeric(st_area(hull.mod))) == 0,
      NA,
      as.numeric(st_area(hull.mod))
    )
  rcat$EOOkm2_clipped[rcat$gbif_name == species.to.plot] <- 
    ifelse(
      length(as.numeric(st_area(mask.mod))) == 0,
      NA,
      as.numeric(st_area(mask.mod))
    )
  
  # Old data
  
  # Generate hull and clipped polygon using lat/long "projection"
  d.sf.old <- st_as_sf(
    species.data.old, 
    coords = c("longitude", "latitude"), 
    crs = "+proj=longlat +datum=WGS84"
  )
  hull.old <- st_convex_hull(st_union(d.sf.old))
  mask.old <- st_intersection(hull.old, world.union)
  
  # Generate hull and clipped polygon using real projection
  d.sf.old.mod <- st_transform(d.sf.old, crs = mod.proj)
  hull.old.mod <- st_convex_hull(st_union(d.sf.old.mod))
  mask.old.mod <- st_intersection(hull.old.mod, world.mod.union)
  
  # Record polygon area values in the rcat data frame
  rcat$EOOkm2_manual_old[rcat$gbif_name == species.to.plot] <- 
    ifelse(
      length(as.numeric(st_area(hull.old.mod))) == 0,
      NA,
      as.numeric(st_area(hull.old.mod))
    )
  rcat$EOOkm2_clipped_old[rcat$gbif_name == species.to.plot] <- 
    ifelse(
      length(as.numeric(st_area(mask.old.mod))) == 0,
      NA,
      as.numeric(st_area(mask.old.mod))
    )
  
  # Recent data
  
  # Generate hull and clipped polygon using lat/long "projection"
  d.sf.recent <- st_as_sf(
    species.data.recent, 
    coords = c("longitude", "latitude"), 
    crs = "+proj=longlat +datum=WGS84"
  )
  hull.recent <- st_convex_hull(st_union(d.sf.recent))
  mask.recent <- st_intersection(hull.recent, world.union)
  
  # Generate hull and clipped polygon using real projection
  d.sf.recent.mod <- st_transform(d.sf.recent, crs = mod.proj)
  hull.recent.mod <- st_convex_hull(st_union(d.sf.recent.mod))
  mask.recent.mod <- st_intersection(hull.recent.mod, world.mod.union)
  
  # Record polygon area values in the rcat data frame
  rcat$EOOkm2_manual_recent[rcat$gbif_name == species.to.plot] <- 
    ifelse(
      length(as.numeric(st_area(hull.recent.mod))) == 0,
      NA,
      as.numeric(st_area(hull.recent.mod))
    )
  rcat$EOOkm2_clipped_recent[rcat$gbif_name == species.to.plot] <- 
    ifelse(
      length(as.numeric(st_area(mask.recent.mod))) == 0,
      NA,
      as.numeric(st_area(mask.recent.mod))
    )
  
  # d.sf.mod <- st_transform(d.sf, crs = mod.proj)
  # 
  # hull.mod <- st_convex_hull(st_union(d.sf.mod))
  # 
  # hull <- st_transform(
  #   hull.mod,
  #   crs = "+proj=longlat +datum=WGS84"
  # )
  # 
  # points.buffer.mod <- st_buffer(d.sf.mod, dist = 2)
  # 
  # points.union.mod <- st_union(points.buffer.mod)
  # 
  # points.union <- st_transform(
  #   points.union.mod,
  #   crs = "+proj=longlat +datum=WGS84"
  # )
  # 
  # points.union.clip <- st_intersection(points.union, world.union)
  # 
  # points.union.clip.mod <- st_transform(
  #   points.union.clip,
  #   crs = mod.proj
  # )
  # 
  # title <- paste0(
  #   species.to.plot, 
  #   "; AOO: ", format(round(st_area(points.union.mod), 2), big.mark = ","),
  #   "; EOO: ", format(round(st_area(hull.mod), 2), big.mark = ",")
  # )
  
  title <- paste0(
    species.to.plot, 
    " (NOP: ", pull(rcat.data, NOP), ")",
    "\nAOO: ", format(round(pull(rcat.data, AOOkm2), 2), big.mark = ","),
    "\nEOO: ", format(round(pull(rcat.data, EOOkm2), 2), big.mark = ","),
    "\nEOO_manual: ", format(round(as.numeric(st_area(hull.mod)), 2), big.mark = ","),
    "\nEOO_clipped: ", format(round(as.numeric(st_area(mask.mod)), 2), big.mark = ",")
  )
  
  plot <- ggplot(data = world) +
    ggtitle(title) +
    geom_sf(fill = "gainsboro") +
    geom_sf(data = hull, color = "red", fill = NA, lty = 2) +
    geom_point(
      data = species.data,
      aes(x = longitude, y = latitude, color = basisOfRecord),
      size = 0.5
    ) +
    scale_color_manual(values = basisOfRecord.colors) +
    scale_fill_manual(values = basisOfRecord.colors) +
    xlab("Longitude") +
    ylab("Latitude") +
    xlim(min(species.data$longitude) - 10, max(species.data$longitude) + 10)  +
    ylim(min(species.data$latitude) - 10, max(species.data$latitude) + 10)  +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank()) 
  
  if(!(TRUE %in% str_detect(class(mask), "POINT"))) {
    plot <- plot + 
      geom_sf(data = mask, color = NA, fill = alpha("pink", 0.4))
  }
  
  ggsave(
    paste0("outputs/plots_for_validation/", species.to.plot, ".png"), 
    plot = plot, width = 8, height = 5, dpi = 150
  )
}

#==============================================================================


# Verify that the manually calculated clipped EOO metric is always less than
# or equal to the area of the manually calculated EOO metric
assertthat::assert_that(
  nrow(filter(rcat, EOOkm2_clipped > rcat$EOOkm2_manual)) == 0
  )

# Assign assessment categories based on the newly calculated EOO metrics
rcat <- rcat %>%
  mutate(
    EOO_manual_cat = EOORating_custom(EOOkm2_manual),
    EOO_clipped_cat = EOORating_custom(EOOkm2_clipped),
    EOO_manual_old_cat = EOORating_custom(EOOkm2_manual_old),
    EOO_clipped_old_cat = EOORating_custom(EOOkm2_clipped_old),
    EOO_manual_recent_cat = EOORating_custom(EOOkm2_manual_recent),
    EOO_clipped_recent_cat = EOORating_custom(EOOkm2_clipped_recent)
  )

#==============================================================================


# Determine which species do not actually have GBIF data from North America

# Generate a spatial layer representing North America

NA.union <- ne_countries(scale = 110, continent = "North America") %>%
  maptools::unionSpatialPolygons(., IDs = rep("North America", nrow(.))) %>%
  st_as_sf()
plot(NA.union, col = "lightgray")

# Loop over all species to determine whether they contain any North
# American occurrence records

rcat$GBIF_points_in_NA <- rep(NA, nrow(rcat))

for(species.to.calculate in rcat$gbif_name) {
  
  print(paste0(species.to.calculate))
  
  species.data <- filter(d, species == species.to.calculate)
  
  d.sf <- st_as_sf(
    species.data, 
    coords = c("longitude", "latitude"), 
    crs = "+proj=longlat +datum=WGS84"
  )
  
  test <- st_within(d.sf, NA.union) %>%
    unlist() %>%
    length()
  
  result <- ifelse(test > 0, "Yes", "No")
  
  rcat$GBIF_points_in_NA[rcat$gbif_name == species.to.calculate] <- result
}

#==============================================================================


# Arrange variables and save output to disk

rcat <- rcat %>%
  select(
    query_name, gbif_name, iucn_assessment_year, iucn_redlist_category, 
    AOO.range, AOO_assessment, AOO_assessment_cat, 
    EOO.range, EOO_assessment, EOO_assessment_cat,
    NOP, GBIF_points_in_NA, MER, 
    AOOkm2, AOOcat, EOOkm2, EOOcat,
    EOOkm2_manual, EOO_manual_cat, EOOkm2_clipped, EOO_clipped_cat,
    NOP_old, MER_old, 
    AOOkm2_old, AOOcat_old, EOOkm2_old, EOOcat_old,
    EOOkm2_manual_old, EOO_manual_old_cat, 
    EOOkm2_clipped_old, EOO_clipped_old_cat,
    NOP_recent, MER_recent, 
    AOOkm2_recent, AOOcat_recent, EOOkm2_recent, EOOcat_recent,
    EOOkm2_manual_recent, EOO_manual_recent_cat, 
    EOOkm2_clipped_recent, EOO_clipped_recent_cat
  )

# Write rCAT output to disk
write_csv(rcat, "data/rcat/rCAT_output.csv")
