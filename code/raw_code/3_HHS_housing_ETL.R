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

#########################################
##### 1. Read in selected variables #####
#########################################

bg_df  <- read_csv("..//..//data//tidy_data//bg-level_candidate_variables_acs5_2020.csv")

bg_df$label <- str_replace_all(bg_df$label, "[^[:alnum:]]", " ")

bg_df <- bg_df %>%
  mutate(label = str_squish(label))

bg_list <-  as.list(unique(bg_df$GEOID))

df <- read_csv("C://data//HHS//affordable_housing_units_geocoded_cleaned.csv")
# Limit to units that are likely to remain affordable and meet city criteria
df <- df[df$COUNTS_AH_GOALS__C == "Yes"]

na_count  <- sapply(df, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)

# There are 418 entries that are single beds. Should probably remove these
# as they are in shelters which are group quarters.
table(df$UNIT_TYPE__C)

df <- df[!df$UNIT_TYPE__C %in% c("Bed"),]

# rentals <- df[df$'Record Type Name' == "Rental Unit",]
# 
# owned <- df[df$'Record Type Name' == "Homeownership Unit",]

# Possibly multiply houses by residence rate? 2.7? The data has shelter beds
# listed individually. Use UNIT_TYPE column

# ACS Variables:

# Number of housing units: B25001_001
# Number of people in owner-occupied housing units: B25008_002
# Number of people in renter-occupied units: B25008_003

# Median monthly housing costs only available at the tract level.

acs_geo <- get_acs(
  geography = "block group",
  variables = "B25001_001",
  state = "CO",
  county = "Boulder",
  year = 2020,
  survey = "acs5",
  geometry = TRUE
)
acs_geo <- acs_geo[,names(acs_geo) %in% c("estimate",
                                       "GEOID", 
                                       "geometry"
                                       )
                   ]

acs_geo <- acs_geo[acs_geo$GEOID %in% bg_list,]
st_crs(acs_geo)

#########################################
######## 2. % Affordable Overall ########
#########################################

housing <- df[, names(df) %in% c("ID",
                                 "LAT_LONG__LONGITUDE__S",
                                 "LAT_LONG__LATITUDE__S"
                                 )
              ]

housing_geo <- st_as_sf(housing, 
                        coords=c("LAT_LONG__LONGITUDE__S", 
                                 "LAT_LONG__LATITUDE__S"
                                 ), 
                        crs=4269
                        )
st_crs(housing_geo)


# Perform spatial join between points and census geometry

# Filter out points outside the polygons

points_sf_joined <- st_join(acs_geo, housing_geo) %>% # Spatial join for intersection
  filter(!is.na(GEOID))


ggplot() +
  geom_sf(data = points_sf_joined) +
  geom_sf(data = housing_geo, 
          # aes(fill = EXPECM__CLIENT_HOUSEHOLD__C)
  )


#Group by GEOID to get a count of financial aid locations by polygon

hs_by_bg <- points_sf_joined %>%
  group_by(GEOID, estimate) %>%
  summarise(num_units = n()) %>%
  mutate(perc_affordable = num_units / estimate)

# No units here
hs_by_bg$perc_affordable[hs_by_bg$GEOID == "080130125113"] <- 0

tmap_mode("view")
# Map this
tm_basemap("OpenStreetMap.France") +
  tm_shape(hs_by_bg) + 
  tm_polygons(col = "perc_affordable",
              style = "equal",
              n = 7,
              # alpha = 0.7,
              palette = "Purples",
              title = "HHS") + 
  tm_layout(title = "Households",
            frame = FALSE,
            legend.outside = TRUE)

# Normalize
Normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

hs_by_bg$perc_affordable <- Normalize(hs_by_bg$perc_affordable)


hs_by_bg <- hs_by_bg %>%
  st_drop_geometry()

hs_by_bg <- hs_by_bg[, names(hs_by_bg) %in% c("GEOID", "perc_affordable")]
                     
write.csv(hs_by_bg, "..//..//data//tidy_data//normalized_hhs_housing_vars.csv",
          row.names = FALSE)

