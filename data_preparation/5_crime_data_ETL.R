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
# library(basemaps)
library(tmaptools)
library(OpenStreetMap)
library(data.table)

# Spherical geometry (s2) causing errors with zoning data
# This unfortunately forces sf to assume lat/lon are planar
sf::sf_use_s2(FALSE)

#########################################
##### 1. Read in selected variables #####
#########################################

bg_df  <- read_csv("..//data//tidy_data//bg-level_candidate_variables_acs5_2020.csv")

bg_df$label <- str_replace_all(bg_df$label, "[^[:alnum:]]", " ")

bg_df <- bg_df %>%
  mutate(label = str_squish(label))

bg_list <-  as.list(unique(bg_df$GEOID))

df <- read.csv("C://data//PD//offense data 2016-2020.csv")

df <- df %>%
  transform(lng = as.numeric(lng)) %>%
  transform(lat = as.numeric(lat))

na_count  <- sapply(df, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)

df <- df[!is.na(df$lng),]


geo <- get_acs(
  geography = "block group",
  variables = "B01003_001",
  state = "CO",
  county = "Boulder",
  year = 2020,
  survey = "acs5",
  geometry = TRUE
)
geo <- geo[,names(geo) %in% c("GEOID", 
                              "geometry",
                              "estimate"
                              )
           ]
geo <- geo[geo$GEOID %in% bg_list,]

crime_geo <- st_as_sf(df, coords=c("lng", "lat"), crs=4269)
st_crs(crime_geo)

# points_sf_joined <- st_join(geo, crime_geo) %>% # Spatial join for intersection
#   filter(!is.na(GEOID))
# 
# 
# crime_bg <- points_sf_joined %>%
#   group_by(GEOID, estimate) %>%
#   summarise(count = n()) %>%
#   mutate(crime_per_capita = count / estimate)
# 
# 
# tmap_mode("view")
# # Map this
# tm_basemap("OpenStreetMap.France") +
#   tm_shape(crime_bg) + 
#   tm_polygons(col = "crime_per_capita",
#               style = "quantile",
#               n = 7,
#               # alpha = 0.7,
#               palette = "Purples",
#               title = "2020 ACS") + 
#   tm_layout(title = "Crime per Capita",
#             frame = FALSE,
#             legend.outside = TRUE)

#####################################################
##### 2. Filter for crimes in residential areas #####
#####################################################

# Import zoning shapefile to classify crimes in residential areas

zoning <- st_read("..//data//tidy_data//zoning_districts//Zoning_Districts.shp")
st_crs(zoning)



zoning <- st_transform(zoning, crs = 4269)

zoning <- zoning[, names(zoning) == "ZNDESC"]

zoning <- zoning %>%
  filter(grepl('Residential', ZNDESC))

# Simplify classifications

zoning$ZNDESC = "Residential"


# tmap_mode("view")
# # Map this
# tm_basemap("OpenStreetMap.France") +
#   tm_shape(zoning) +
#   tm_polygons(col = "ZNDESC",
#               # style = "quantile",
#               # n = 1,
#               # alpha = 0.7,
#               palette = "Purples",
#               title = "Boulder Zoning") +
#   tm_layout(title = "Residential Districts",
#             frame = FALSE,
#             legend.outside = TRUE)

# Merge with crime data

residential_crime <- st_join(crime_geo, zoning) %>%
  filter(!is.na(ZNDESC))

# tm_basemap("OpenStreetMap.France") +
#   tm_shape(residential_crime) +
#   tm_dots() +
#   tm_layout(title = "Residential Districts",
#             frame = FALSE,
#             legend.outside = TRUE)

# If we filter for only vehicle theft (which gets reported the most),
# we may need the allocation of vehicles available by bg:
# B992512_002 for allocated, B992512_003 for unallocated estimate

# Or B99082_001? That's the allocation of private vehicles.

#####################################################
######### 3. Crimes per capita (2016-2020) ##########
#####################################################

residential_crime <- residential_crime[, names(residential_crime) %in% c("nibrs_type",
                                                                        "nibrs_offense"
                                                                        )
                                       ]


pop_geo <- get_acs(
  geography = "block group",
  # Total pop
  variables = "B02001_001",
  state = "CO",
  county = "Boulder",
  year = 2020,
  survey = "acs5",
  geometry = TRUE
)
pop_geo <- pop_geo[,names(pop_geo) %in% c("GEOID", 
                              "geometry",
                              "estimate"
                              )
           ]
pop_geo <- pop_geo[pop_geo$GEOID %in% bg_list,]

crime_joined <- st_join(pop_geo, residential_crime) %>% # Spatial join for intersection
  filter(!is.na(GEOID))


crime_bg <- crime_joined %>%
  group_by(GEOID, estimate) %>%
  summarise(count = n()) %>%
  mutate(crime_per_capita = count / estimate)

# Map this
# tm_basemap("OpenStreetMap.France") +
#   tm_shape(crime_bg) +
#   tm_polygons(col = "crime_per_capita",
#               style = "quantile",
#               n = 7,
#               # alpha = 0.7,
#               palette = "Purples",
#               title = "2020 ACS") +
#   tm_layout(title = "Crime per Capita",
#             frame = FALSE,
#             legend.outside = TRUE)

############################################
######### 4. Normalize Variables ###########
############################################

Normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

norm_crime <- crime_bg[, names(crime_bg) %in% c("GEOID",
                                              "crime_per_capita")]
# Normalize columns
norm_crime$crime_per_capita <- Normalize(norm_crime$crime_per_capita)


norm_crime <- norm_crime %>%
  st_drop_geometry()


norm_crime$crime_per_capita <- as.numeric(norm_crime$crime_per_capita)

write.csv(norm_crime, "..//data//tidy_data//normalized_crime_vars.csv",
          row.names = FALSE)

