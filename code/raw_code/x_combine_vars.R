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

acs <- read_csv("..//..//data//tidy_data//normalized_acs_vars.csv")
hhs_client <- read_csv("..//..//data//tidy_data/normalized_hhs_client_vars.csv")
hhs_house <- read_csv("..//..//data//tidy_data//normalized_hhs_housing_vars.csv")




#########################################
######## 2. Merge Data Frames ###########
#########################################

full_df <- merge(acs, hhs_client, by = "GEOID", all.x = TRUE) %>%
  merge(hhs_house, by="GEOID", all.x = TRUE)

z <- as.matrix(full_df[3:17])
pairs( z, panel=function(x,y){
  points(x,y)
  abline(lm(y~x), col='red')
  text(0,1.5,labels = paste('R2=',round((cor(x,y))^2,2)) ,col='red' )
})
