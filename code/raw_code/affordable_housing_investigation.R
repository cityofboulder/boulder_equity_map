setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(stringr)
library(ggplot2)
library(tidygeocoder)
library(lubridate)
library(sf)
library(tidycensus)
library(tigris)
library(rosm)
library(tmap)

df  <- read_csv("C://data//HHS//Affordable Housing Units - HHS Salesforce raw data.csv")

# How many NaNs in each column?
na_count  <- sapply(df, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)
na_count

# Convert data type
df  <-  df %>%
  mutate(INACTIVE_DATE__C = mdy(INACTIVE_DATE__C))
table(df$INACTIVE_DATE__C)

# Get rid of inactive dates before 2016, but keep NaN rows (these are still active).

print(nrow(df))
df  <- df %>%
  filter(is.na(INACTIVE_DATE__C) == TRUE | !INACTIVE_DATE__C < (as.Date('2016-01-01')))
print(nrow(df))

barplot(table(df$YEAR_BUILT__C))
barplot(table(df$AMI_CATEGORY__C))

################################
## Geocode Missing Lat/Lons
################################

df  <- df %>%
  mutate(ZIP__C = as.character(ZIP__C)) %>%
  mutate(ZIP__C = replace_na(ZIP__C, "")) %>%
  mutate(ADDRESS = paste(STREET_ADDRESS__C, "Boulder, CO", ZIP__C, sep=' '))

missing_geo  <- df %>%
  filter(is.na(LAT_LONG__LATITUDE__S) == TRUE)

df  <- df %>%
  filter(is.na(LAT_LONG__LATITUDE__S) == FALSE)

print(nrow(missing_geo))
print(nrow(df))

# Try geocoding the missing lat/lon

added_geo  <- missing_geo %>%
  geocode(ADDRESS, 
          method = "osm", 
          lat = Latitude,
          long = Longitude
  )
added_geo$LAT_LONG__LATITUDE__S <- added_geo$Latitude
added_geo$LAT_LONG__LONGITUDE__S <- added_geo$Longitude

table(added_geo[is.na(added_geo$LAT_LONG__LATITUDE__S) == TRUE,]$ADDRESS)




# Drop the remaining NAs
# added_geo  <- added_geo %>%
#   filter(is.na(LAT_LONG__LATITUDE__S) == FALSE)

# Drop lat/lon columns and add back to df

drops  <-  c("Latitude", "Longitude")
print(ncol(added_geo))
added_geo  <- added_geo[, !(names(added_geo) %in% drops)]
print(ncol(added_geo))
print(nrow(df))
df  <-  rbind(df, added_geo)
print(nrow(df))

# Export for manual cleaning of remaining missing addresses
write.csv(df, "C://data//HHS//affordable_housing_units_geocoded.csv")

df  <- read_csv("C://data//HHS//affordable_housing_units_geocoded_cleaned.csv")


# Turn it into a geo dataframe with a geometry column, set to EPSG:4326 projection (for now)
geo_df = st_as_sf(df, coords=c("LAT_LONG__LONGITUDE__S", "LAT_LONG__LATITUDE__S"), crs=4326)

ggplot() + geom_sf(data = geo_df, aes(fill = UNIT_TYPE__C))


##########################
## Import Census Geography
##########################

# Import table of tracts for City of Boulder
geo_corr <- read.csv("..//..//data//raw_data//geocorr_boulder_city.csv",
                     colClasses=c("tract"="character", 
                                  "block_group"="character"
                     )
)

boulder_tracts <- unique(geo_corr$tract)

# Import ACS 5-year profile variables for selected year
# Sometimes fails to import the geometry
boulder_vars <- get_acs(
  geography = "tract",
  variables = "DP02_0001",
  state = "CO",
  county = "Boulder",
  # tract = boulder_tracts,
  year = 2020,
  survey = "acs5",
  geometry = TRUE
) %>%
  separate(NAME, into = c("tract", "county", "state"),
           sep= ", ")

boulder_vars$tract <- str_replace(
  boulder_vars$tract, 
  "Census Tract ", 
  ""
)

# Subset for only Boulder tracts (previous code does not always filter correctly)
boulder_vars <- boulder_vars[boulder_vars$tract %in% boulder_tracts,]

ggplot(boulder_vars, aes(fill = estimate)) +
  geom_sf()


st_crs(boulder_vars)


st_crs(geo_df)
# Change the projection of the Boulder tract polygons
boulder_vars  <- st_transform(boulder_vars, crs = 4326)


ggplot() +
  geom_sf(data = boulder_vars, aes(fill = estimate)) +
  geom_sf(data = geo_df)


# Filter out points outside the polygons

points_sf_joined <- st_join(geo_df, boulder_vars) %>% # Spatial join for intersection
  filter(!is.na(GEOID))


ggplot() +
  geom_sf(data = boulder_vars, aes(fill = estimate)) +
  geom_sf(data = points_sf_joined)


#Group by GEOID to get a count of financial aid locations by polygon

count_by_tract <- points_sf_joined %>%
  group_by(GEOID) %>%
  summarise(num_units = n())

tract_counts = data.frame(count_by_tract$GEOID, count_by_tract$num_units) %>%
  rename(
    GEOID = count_by_tract.GEOID,
    num_units = count_by_tract.num_units
  )


boulder_vars <- merge(boulder_vars, tract_counts, by="GEOID") 



ggplot(boulder_vars, aes(fill = num_units)) +
  geom_sf()
