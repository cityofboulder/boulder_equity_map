setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(stringr)
library(ggplot2)
# library(tidygeocoder)
# library(lubridate)
library(sf)
library(tidycensus)
library(tigris)
library(rosm)
library(tmap)
library(basemaps)
library(tmaptools)
library(OpenStreetMap)
library(data.table)

library(nlcor)

#########################################
##### 1. Read in selected variables #####
#########################################

acs <- read_csv("..//..//data//tidy_data//normalized_acs_vars.csv")
hhs_client <- read_csv("..//..//data//tidy_data/normalized_hhs_client_vars.csv")
hhs_house <- read_csv("..//..//data//tidy_data//normalized_hhs_housing_vars.csv")
crime <- read_csv("..//..//data//tidy_data//normalized_crime_vars.csv")
p_r_client <- read_csv("..//..//data//tidy_data//normalized_PnR_family_data.csv")

# geo_corr <- read.csv("..//..//data//raw_data//geocorr_boulder_city.csv",
#                      colClasses=c("tract"="character", 
#                                   "block_group"="character"
#                      )
# )
# 
# geo_corr  <- mutate(
#   geo_corr,
#   block = paste("Block Group ", 
#                 geo_corr$block_group, 
#                 ", Census Tract ", 
#                 geo_corr$tract, 
#                 ", Boulder County, Colorado", 
#                 sep = ""
#   )
# )
# geo_corr <- mutate(
#   geo_corr,
#   tract = paste("Census Tract ",
#                 geo_corr$tract,
#                 ", Boulder County, Colorado",
#                 sep = ""
#   )
# )
# boulder_tracts <- unique(geo_corr$tract)

#########################################
######## 2. Merge Data Frames ###########
#########################################

full_df <- merge(acs, hhs_client, by = "GEOID", all.x = TRUE) %>%
  merge(hhs_house, by="GEOID", all.x = TRUE) %>%
  # merge(crime, by="GEOID", all.x = TRUE) %>%
  merge(p_r_client, by="GEOID", all.x = TRUE) %>%
  rename(perc_aff_hs_units = perc_affordable,
         perc_hhs_aid = perc_hs_aid,
         perc_pr_aid = client_percent,
         perc_public_assist = percent_aid)
# white is the exact inverse of % POC, so exclude
full_df <- full_df[,names(full_df) != "white"]


#########################################
########## 3. Map Variables #############
#########################################


poc_df <- full_df[, names(full_df) %in% c("GEOID", "percent_poc")]

# Get geometry
geo <- get_acs(
  geography = "block group",
  variables = 'B19083_001',
  state = "CO",
  county = "Boulder",
  year = 2020,
  # tracts = boulder_tracts,
  survey = "acs5",
  geometry = TRUE
)
geo <- geo[,names(geo) %in% c("GEOID", "geometry")]

poc_perc <- merge(x = poc_df, y = geo, by = "GEOID", all.x = TRUE)

poc_geo <- st_as_sf(poc_perc,
                      sf_column_name = "geometry",
                      crs=4326
)

tmap_mode("view")

# Percent POC map
tm_basemap("OpenStreetMap.France") +
  tm_shape(poc_geo) + 
  tm_polygons(col = "percent_poc",
              style = "equal",#"jenks",
              n = 4,
              alpha = 0.9,
              palette = "Purples",
              title = "% POC Index") + 
  tm_layout(title = "Percent POC\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)

# Now for BAA

baa_df <- full_df[, names(full_df) %in% c("GEOID", "black")]

baa_perc <- merge(x = baa_df, y = geo, by = "GEOID", all.x = TRUE)

baa_geo <- st_as_sf(baa_perc,
                    sf_column_name = "geometry",
                    crs=4326
)

# Percent POC map
tm_basemap("OpenStreetMap.France") +
  tm_shape(baa_geo) + 
  tm_polygons(col = "black",
              style = "equal",#"jenks",
              n = 4,
              alpha = 0.9,
              palette = "Purples",
              title = "% B/A-A Index") + 
  tm_layout(title = "Percent B/A-A\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)


# % Hispanic/Latinx

hl_df <- full_df[, names(full_df) %in% c("GEOID", "percent_h_l")]
hl_perc <- merge(x = hl_df, y = geo, by = "GEOID", all.x = TRUE)

hl_geo <- st_as_sf(hl_perc,
                    sf_column_name = "geometry",
                    crs=4326
)
tm_basemap("OpenStreetMap.France") +
  tm_shape(hl_geo) + 
  tm_polygons(col = "percent_h_l",
              style = "equal",#"jenks",
              n = 4,
              alpha = 0.9,
              palette = "Purples",
              title = "% Hispanic/Latino Index") + 
  tm_layout(title = "Percent H/L\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)

# Now for median income

income_df <- full_df[, names(full_df) %in% c("GEOID",
                                             "med_income")]
income_geo <- merge(x = income_df, y = geo, by = "GEOID", all.x = TRUE)

income_geo <- st_as_sf(income_geo,
                    sf_column_name = "geometry",
                    crs=4326
)

# Median Income map
tm_basemap("OpenStreetMap.France") +
  tm_shape(income_geo) + 
  tm_polygons(col = "med_income",
              style = "equal",#"jenks",
              n = 5,
              alpha = 0.9,
              palette = "Blues",
              title = "Income Index") + 
  tm_layout(title = "Median Income\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)


# Now for % below pov income

pov_df <- full_df[, names(full_df) %in% c("GEOID",
                                             "percent_blw_pov")]
pov_geo <- merge(x = pov_df, y = geo, by = "GEOID", all.x = TRUE)

pov_geo <- st_as_sf(pov_geo,
                       sf_column_name = "geometry",
                       crs=4326
)

# below poverty map
tm_basemap("OpenStreetMap.France") +
  tm_shape(pov_geo) + 
  tm_polygons(col = "percent_blw_pov",
              style = "equal",#"jenks",
              n = 5,
              alpha = 0.9,
              palette = "Blues",
              title = "Poverty Index") + 
  tm_layout(title = "% Below Poverty Line\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)