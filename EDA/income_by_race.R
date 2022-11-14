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
# library(basemaps)
library(tmaptools)
library(OpenStreetMap)
library(data.table)
library(classInt)
library(nlcor)

geo_corr <- read.csv("..//..//data//raw_data//geocorr_boulder_city.csv",
                     colClasses=c("tract"="character", 
                                  "block_group"="character"
                     )
)

geo_corr  <- mutate(
  geo_corr,
  block = paste("Census Tract ", 
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
boulder_tracts <- unique(geo_corr$tract)

# Get geometry
acs <- get_acs(
  geography = "tract",
  variables = c("B19013A_001", # median income white
                "B19013B_001", # median income b/a-a
                "B19013C_001", # median income AI/NA
                "B19013D_001", # median income asian
                "B19013E_001", # median income NH/PI
                "B19013F_001", # median income 2+
                "B19013G_001", # median income other
                "B19013H_001", # median income, white alone, not h/l
                "B19013I_001" # median income, h/l
  ),
  state = "CO",
  county = "Boulder",
  year = 2020,
  # tracts = boulder_tracts,
  survey = "acs5",
  geometry = FALSE
)
acs <- acs[acs$NAME %in% boulder_tracts,]

na_count  <- sapply(acs, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)

acs_no_na <- acs[complete.cases(acs),]

avg_income <- acs_no_na %>%
  group_by(variable) %>%
  summarize(mean_income = mean(estimate))
