

custom_read_csv <- function(string) {
  
  name <- string %>%
    gsub("data/gbif_occurrences/", "", .) %>%
    gsub(".csv", "", .) %>%
    gsub("_", " ", .) %>%
    word(1, 2)
  
  read_csv(string) %>%
    mutate(query_name = name) %>%
    select(query_name, everything())
}


custom_read_csv2 <- function(data) {
  
  read_csv(data,
    col_types = cols(
      gbifID = "d",
      kingdom = "c",
      genus = "c",
      species = "c",
      infraspecificEpithet = "c",
      taxonRank = "c",
      scientificName = "c",
      occurrenceStatus = "c",
      decimalLatitude = "d",
      decimalLongitude = "d",
      coordinateUncertaintyInMeters = "d",
      coordinatePrecision = "d",
      eventDate = "c",
      year = "d",
      taxonKey = "d",
      speciesKey = "d",
      basisOfRecord = "c",
      issue = "c"
    )
  )
}


projection_chooser <- function(cp) {
  
  if ((cp$lat < 70) & (cp$lat > -70)) {
    
    string <- paste(
      "+proj=cea +lon_0=", 
      cp$long,
      "+lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
      sep = ""
    )
  } else {
    
    string <- paste(
      "+proj=laea +lat_0=", 
      cp$lat, 
      " +lon_0=", 
      cp$long, 
      " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", 
      sep = "")
  }
  
  return(string)
}


EOORating_custom <- function(EOOArea) {

  cat <- ifelse(
    is.na(EOOArea),
    NA_character_,
    case_when(
    EOOArea < 100 ~ "CR",
    EOOArea < 5000 ~ "EN",
    EOOArea < 20000 ~ "VU",
    EOOArea < 30000 ~ "NT",
    is.numeric(EOOArea) ~ "LC"
    )
  )
  
  return(cat)
}


AOORating_custom <- function(AOOArea) {

  cat <- EOORating_custom(AOOArea * 10)
  
  return(cat)
}
