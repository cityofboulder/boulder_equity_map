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
library(stringr)
library(data.table)

#########################################
##### 1. Read in selected variables #####
#########################################

bg_df  <- read_csv("..//..//data//tidy_data//bg-level_candidate_variables_acs5_2020.csv")

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

points_sf_joined <- st_join(geo, crime_geo) %>% # Spatial join for intersection
  filter(!is.na(GEOID))


crime_bg <- points_sf_joined %>%
  group_by(GEOID, estimate) %>%
  summarise(count = n()) %>%
  mutate(crime_per_capita = count / estimate)


tmap_mode("view")
# Map this
tm_basemap("OpenStreetMap.France") +
  tm_shape(crime_bg) + 
  tm_polygons(col = "crime_per_capita",
              style = "quantile",
              n = 7,
              # alpha = 0.7,
              palette = "Purples",
              title = "2020 ACS") + 
  tm_layout(title = "Crime per Capita",
            frame = FALSE,
            legend.outside = TRUE)

