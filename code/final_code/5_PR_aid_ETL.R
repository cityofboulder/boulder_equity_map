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

bg_df  <- read_csv("..//..//data//tidy_data//bg-level_candidate_variables_acs5_2020.csv")

bg_df$label <- str_replace_all(bg_df$label, "[^[:alnum:]]", " ")

bg_df <- bg_df %>%
  mutate(label = str_squish(label))

bg_list <-  as.list(unique(bg_df$GEOID))

df <- read.csv("C://data//parks//Financial Aid data for IT.csv")

na_count  <- sapply(df, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)

drop_list <- c("lastname",
               "firstname")

df <- df[, !names(df) %in% drop_list]

# Need to clean addresses to remove anything after a hash or "Lot", "Apt", any 
# number after the street suffix

df <- df %>%
  mutate(
   address1 = toupper(address1)
  ) %>%
  mutate(
    city = toupper(city)
  ) %>%
  mutate(
    state = toupper(state)
  ) %>%
  mutate(
    address1 = gsub("LOT.*", "",
                         gsub("#[0-9].*", "",
                              gsub("UNIT.*", "",
                                   gsub(",.*", "",
                                        gsub("\\.", "",
                                             gsub("APT.*", "",
                                                  gsub("[A-Z0-9]-[0-9].*", "",
                                                       gsub("#.*", "", address1
                                                       ))))))))
  ) %>%
  mutate(
    full_addr = paste(df$address1,
                      df$city,
                      df$state,
                      sep = " "
  )
)

added_geo  <- df %>%
  geocode(full_addr,
          method = "osm",
          lat = Latitude,
          long = Longitude
  )

na_count  <- sapply(added_geo, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)
na_count


missing_geo <- added_geo[is.na(added_geo$Latitude),]
added_geo <- added_geo[!is.na(added_geo$Latitude),]



missing_geo <- missing_geo[,!names(missing_geo) %in% c('Latitude','Longitude')]
# missing_geo <- missing_geo %>%
#   mutate(gsub(" NA ", " BOULDER ", CLEAN_ADDRESS))


# Geocodio is very fast, but fudges things quite a bit. Need to be careful
# with the results of this one.
geo_codio_geo  <- missing_geo %>%
  geocode(full_addr,
          method = "geocodio",
          lat = Latitude,
          long = Longitude
  )
na_count  <- sapply(geo_codio_geo, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)
na_count

# Recombine the geocodio addresses to added_geo

added_geo <- rbind(added_geo, geo_codio_geo)

min(added_geo$Latitude)
max(added_geo$Latitude)

min(added_geo$Longitude)
max(added_geo$Longitude)


added_geo <- added_geo[added_geo$city == "BOULDER",]

# Variable for estimated total population is B02001_001

na_count  <- sapply(added_geo, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)
na_count

# Filter for duplicates
added_geo <- added_geo[!duplicated(added_geo),]

# Get geometry
geo <- get_acs(
  geography = "block group",
  variables = 'B02001_001',
  state = "CO",
  county = "Boulder",
  year = 2020,
  # tracts = boulder_tracts,
  survey = "acs5",
  geometry = TRUE
)
geo <- geo[,names(geo) %in% c("GEOID", 
                              "geometry",
                              "estimate"
                              )
           ]
geo <- geo[geo$GEOID %in% bg_list,]

client_geo <- st_as_sf(added_geo, coords=c("Longitude", "Latitude"), crs=4269)
st_crs(client_geo)
st_crs(geo)

points_sf_joined <- st_join(geo, client_geo) %>% # Spatial join for intersection
  filter(!is.na(GEOID))


client_bg <- points_sf_joined %>%
  group_by(GEOID, estimate) %>%
  summarise(count = n()) %>%
  mutate(client_percent = count / estimate)


# tmap_mode("view")
# # Map this
# tm_basemap("OpenStreetMap.France") +
#   tm_shape(client_bg) +
#   tm_polygons(col = "client_percent",
#               style = "quantile",
#               n = 7,
#               # alpha = 0.7,
#               palette = "Purples",
#               title = "P&R Aid Recipients") +
#   tm_layout(title = "% Aid Recipients",
#             frame = FALSE,
#             legend.outside = TRUE)
# 
# 
# Normalize
Normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

client_bg$client_percent <- Normalize(client_bg$client_percent)

client_bg <- client_bg %>%
  st_drop_geometry()
client_bg <- client_bg[, names(client_bg) %in% c("GEOID", "client_percent")]

write.csv(client_bg, "..//..//data//tidy_data//normalized_PnR_family_data.csv",
          row.names = FALSE)
