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

bg_df  <- read_csv("..//data//tidy_data//bg-level_candidate_variables_acs5_2020.csv")

bg_df$label <- str_replace_all(bg_df$label, "[^[:alnum:]]", " ")

bg_df <- bg_df %>%
  mutate(label = str_squish(label))

bg_list <-  as.list(unique(bg_df$NAME))

df <- read.csv("C://data//HHS//geo_coded_clients_cleaned.csv")

# Filter for food tax rebate, childcare subsidies, and family resource schools

df <- df %>%
  filter(grepl('FTR|CCS|FRS', EXPECM__ALL_ACTIVE_PROGRAMS__C))

table(df$EXPECM__ALL_ACTIVE_PROGRAMS__C)

# Some of the IDs for the FTR program were rejected. Need additional source
# to verify eligibility.

case_records <- read.csv("C://data//HHS//HHS Case Records.csv")

table(case_records$EXPECM__STATUS__C)
sum(is.na(case_records$EXPECM__STATUS__C))

inactive_case <- case_records[!case_records$EXPECM__STATUS__C %in% c("Active",
                                                                     "Completed"
                                                                     ),
                              ]
FTR_inactive_case <- inactive_case[
                                  inactive_case$EXPECM__PROGRAM_NAME__C == "Food Tax Rebate Program",
                                  ]

inactive_list <- unique(FTR_inactive_case$EXPECM__MAIN_CLIENT__C)

# Isolate the clients that were only FTR and drop any in the inactive list

ftr_df <- df[df$EXPECM__ALL_ACTIVE_PROGRAMS__C == "FTR",]
df <- df[df$EXPECM__ALL_ACTIVE_PROGRAMS__C != "FTR",]

ftr_df <- ftr_df[!ftr_df$ID %in% inactive_list,]
# The count seems more reasonable when I include clients who had the status
# "Completed".
df <- rbind(df, ftr_df)


# Some NA counts are deceptively low for columns with "Unknown" or "Other" 
# designations.
na_count  <- sapply(df, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)

# Do we aggregate individual participants or households? If households, will need
# count of households by BG. That variable is B11012_001 for ACS 5 year.
# Household size by enrollment?


hs_df <- df[, names(df)  %in% c("EXPECM__CLIENT_HOUSEHOLD__C",
                                "Latitude",
                                "Longitude")]
hs_df <- hs_df[!duplicated(hs_df["EXPECM__CLIENT_HOUSEHOLD__C"]),]

# ind_df <- df[, names(df)  %in% c("ID",
#                                 "Latitude",
#                                 "Longitude")]

write.csv(hs_df, "..//data//raw_data//hhs_client_points.csv")
####################################################
##### 1. Allocate and transform household data #####
####################################################

# Get household geometry
hs_geo <- get_acs(
  geography = "block group",
  variables = 'B11012_001',
  state = "CO",
  county = "Boulder",
  year = 2020,
  survey = "acs5",
  geometry = TRUE
)

hs_geo <- hs_geo[,names(hs_geo) %in% c("estimate",
                                       "GEOID",
                                       "NAME",
                                       "geometry"
                                       )
                 ]

hs_geo <- hs_geo[hs_geo$NAME %in% bg_list,]
st_crs(hs_geo)

# Geocode HHS household data

hhs_hs_geo <- st_as_sf(hs_df, coords=c("Longitude", "Latitude"), crs=4269)
st_crs(hhs_hs_geo)


# Perform spatial join between points and census geometry

# Filter out points outside the polygons

points_sf_joined <- st_join(hs_geo, hhs_hs_geo) %>% # Spatial join for intersection
  filter(!is.na(GEOID))


# ggplot() +
#   geom_sf(data = points_sf_joined) +
#   geom_sf(data = hhs_hs_geo, 
#           # aes(fill = EXPECM__CLIENT_HOUSEHOLD__C)
#           )

# Drop NAs
points_sf_joined <- na.omit(points_sf_joined)

#Group by GEOID to get a count of financial aid locations by polygon

hs_by_bg <- points_sf_joined %>%
  group_by(GEOID, estimate) %>%
  summarise(num_units = n()) %>%
  mutate(perc_hs_aid = num_units / estimate)

# bg with GEOID 080130125113 estimates that there are zero households in the bg.
# Which one is this? CU.

# weird_bg <- hs_by_bg[hs_by_bg$GEOID == "080130125113",]

# tmap_mode("view")
# 
# 
# tm_basemap("OpenStreetMap.France") +
#   tm_shape(weird_bg) + 
#   tm_polygons(col = "num_units",
#               style = "quantile",
#               n = 7,
#               alpha = 0.7,
#               palette = "Purples",
#               title = "HHS") + 
#   tm_layout(title = "Households",
#             frame = FALSE,
#             legend.outside = TRUE)

# Set that percent value to 0 instead of Inf. It's likely misallocated from an
# an adjacent bg.

hs_by_bg$perc_hs_aid[hs_by_bg$GEOID == "080130125113"] <- 0

# Map this
# tm_basemap("OpenStreetMap.France") +
#   tm_shape(hs_by_bg) + 
#   tm_polygons(col = "perc_hs_aid",
#               style = "quantile",
#               n = 7,
#               alpha = 0.7,
#               palette = "Purples",
#               title = "HHS") + 
#   tm_layout(title = "Households",
#             frame = FALSE,
#             legend.outside = TRUE) +
#   tm_shape(hhs_hs_geo) +
#   tm_dots()

# Normalize
Normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

hs_by_bg$perc_hs_aid <- Normalize(hs_by_bg$perc_hs_aid)

hs_by_bg <- hs_by_bg %>%
  st_drop_geometry()

hs_by_bg <- hs_by_bg[, names(hs_by_bg) %in% c("GEOID", "perc_hs_aid")]

write.csv(hs_by_bg, "..//data//tidy_data//normalized_hhs_client_vars.csv",
          row.names = FALSE)
