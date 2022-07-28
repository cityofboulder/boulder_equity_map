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



geo_corr <- read.csv("..//..//data//raw_data//geocorr_boulder_city.csv",
                     colClasses=c("tract"="character", 
                                  "block_group"="character"
                     )
)

geo_corr  <- mutate(
  geo_corr,
  block = paste("Block Group ", 
                geo_corr$block_group, 
                ", Census Tract ", 
                geo_corr$tract, 
                ", Boulder County, Colorado", 
                sep = ""
 )
)
# geo_corr <- mutate(
#   geo_corr,
#   tract = paste("Census Tract ", 
#                 geo_corr$tract, 
#                 ", Boulder County, Colorado", 
#                 sep = ""
#   )
# )
boulder_tracts <- unique(geo_corr$tract)


bg_df  <- read_csv("..//..//data//tidy_data//bg-level_candidate_variables_acs5_2020.csv")

race_df <- bg_df[bg_df$concept == "RACE",]

# Clean up special characters
race_df$label <- str_replace_all(race_df$label, "[^[:alnum:]]", " ")

race_df <- race_df %>%
  mutate(label = str_squish(label))

# Variable for estimated total population is B02001_001
# Variable for estimated 

race_df$estimate[(race_df$NAME == "Block Group 1, Census Tract 121.01, Boulder County, Colorado") &
          (race_df$variable == 'B02001_001')]

# bg_list1 <-  as.list(unique(race_df$NAME))
bg_list <-  as.list(unique(geo_corr$block))

race_perc <- data.frame()

for (i in bg_list) {
  total <- race_df$estimate[(race_df$NAME == i) &
                              (race_df$variable == 'B02001_001')]
  sub_df <- race_df[race_df$NAME == i,] %>%
    mutate(percent = estimate / total * 100)
  race_perc <- rbind(race_perc, sub_df)
}

race_perc <- race_perc[race_perc$variable != "B02001_001",]

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
geo <- geo[,names(geo) %in% c("GEOID", "geometry")]

race_perc <- merge(x = race_perc, y = geo, by = "GEOID", all.x = TRUE)


race_perc <- race_perc[race_perc$NAME %in% bg_list,]

###############################################
############# Exploratory Maps ################
###############################################

# data(ext)
# ext <- draw_ext()
# # Set defaults for basemap
# set_defaults(map_service = "osm", map_type = "streets")
# basemap_plot(ext)


race_perc <- st_as_sf(race_perc,
                      sf_column_name = "geometry",
                      crs=4326
                      )

# osm <- read_osm(race_perc)

perc_white <- race_perc[race_perc$variable == "B02001_002",]
perc_black <- race_perc[race_perc$variable == "B02001_003",]

tm_shape(perc_white) + 
  tm_polygons() 


tmap_mode("view")

tm_basemap("OpenStreetMap.France") +
tm_shape(perc_white) + 
  tm_polygons(col = "percent",
              style = "quantile",
              n = 7,
              alpha = 0.5,
              palette = "Purples",
              title = "2020 ACS") + 
  tm_layout(title = "Percent White\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)


nc_centers <- st_centroid(perc_black)


tm_basemap("OpenStreetMap.France") +
tm_shape(perc_black) + 
  tm_polygons(col = "percent",
              style = "quantile",
              n = 7,
              alpha = 0.7,
              palette = "Purples",
              title = "2020 ACS") + 
  tm_layout(title = "Percent Black\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)

###############################################
############# Income ##########################
###############################################

pov_df <- bg_df[bg_df$concept == "RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS",]

# Drop anything over the poverty line (1+)
pov_df <- pov_df[!pov_df$variable %in% c("C17002_004",
                                         "C17002_005",
                                         "C17002_006",
                                         "C17002_007",
                                         "C17002_008"
                                         ),]
totals <- pov_df[pov_df$variable == "C17002_001",]
pov_df <- pov_df[pov_df$variable != "C17002_001",]


bg_list <-  as.list(unique(pov_df$NAME))

pov_perc <- data.frame()

for (i in bg_list) {
  total <- totals$estimate[(totals$NAME == i)]
  
  sub_df <- pov_df[pov_df$NAME == i,] %>%
    mutate(percent = estimate / total * 100)
  pov_perc <- rbind(pov_perc, sub_df)
}

pov_perc$percent[is.na(pov_perc$percent)] <- 0

# Now, need to group by (sum) block group



# Need to remove superfluous columns from geo

pov_perc <- merge(x = pov_perc, y = geo, by = "GEOID", all.x = TRUE)


pov_perc <- st_as_sf(pov_perc,
                      sf_column_name = "geometry",
                      crs=4326
)

