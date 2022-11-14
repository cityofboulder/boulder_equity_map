setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(stringr)
library(ggplot2)
library(sf)
library(tidycensus)
library(tigris)
library(rosm)
library(tmap)
library(basemaps)
library(tmaptools)
library(OpenStreetMap)

#########################################
########## 1. Read in Data ##############
#########################################

# Get city zone files

zones <- st_read("..//..//data//tidy_data//zoning_districts//Zoning_Districts.shp")

st_crs(zones)

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
geo <- geo[,names(geo) %in% c("GEOID", "NAME", "geometry")]

st_crs(geo)

geo <- st_transform(geo, crs = st_crs(zones))

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
geo_corr <- mutate(
  geo_corr,
  tract = paste("Census Tract ",
                geo_corr$tract,
                ", Boulder County, Colorado",
                sep = ""
  )
)
bg_list <-  as.list(unique(geo_corr$block))

geo <- geo[geo$NAME %in% bg_list,]


#########################################
########## 2. Plot Data #################
#########################################


tmap_mode("view")

# Percent POC map
tm_basemap("OpenStreetMap.France") +
  tm_shape(zones) + 
  tm_polygons(col = "ZNDESC",
              # style = "equal",#"jenks",
              # n = 4,
              # alpha = 0.9,
              palette = "Purples",
              title = "Zoning") + 
  tm_layout(title = "Boulder Zoning vs Block Groups",
            frame = FALSE,
            legend.outside = TRUE) +
  tmap_options(max.categories = 44,
               check.and.fix = TRUE) +
  tm_shape(geo) +
  tm_polygons(col = "NAME",
              # style = "equal",#"jenks",
              # n = 4,
              # alpha = 0.9,
              palette = "Oranges",
              title = "Block Groups")


#########################################
########## 3. BG Area ###################
#########################################

st_area(geo)
# Get average polygon size in acres
mean(st_area(geo))
# Average size of block group is 568 acres
avg_acrs <- 2298936 * 0.00024710538146717
