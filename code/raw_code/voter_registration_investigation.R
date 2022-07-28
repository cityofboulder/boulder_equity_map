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
library(data.table)


# Pipe-delimited file
voters  <- read.table(
  file = "C://data//voter_records//CE-VR011B_EXTERNAL//CE-VR011B_EXTERNAL_20220701_020840_07.txt",
  sep = "|",
  #nrows=300,
  fill = TRUE,
  header=TRUE
  )

# How many NaNs in each column?
na_count  <- sapply(voters, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)
names(voters)

boulder_voters <- voters[voters$RES_CITY == "BOULDER",]

boulder_voters <- boulder_voters[, c("YOB",
                                     "GENDER",
                                     "PARTY",
                                     "POLPARTY_AFF_DATE",
                                     "STATUS",
                                     "STATUS_REASON",
                                     "HOUSE_NUMBER",
                                     "HOUSE_SUFFIX",
                                     "PRE_DIRECTION",
                                     "STREET_NAME",
                                     "STREET_TYPE",
                                     "POST_DIRECTION",
                                     "RES_ADDRESS",
                                     "RES_CITY",
                                     "RES_STATE",
                                     "RES_ZIP_CODE"
                                      
)]

address_cols = c("RES_ADDRESS",
                 "RES_CITY",
                 "RES_STATE",
                 "RES_ZIP_CODE")


boulder_voters[, address_cols][is.na(boulder_voters[address_cols])] <- ""

# Need to get rid of APT and UNIT elements, will throw off geolocator.

boulder_voters$RES_ADDRESS <- lapply(boulder_voters$RES_ADDRESS, 
                                     function(y) gsub(" APT [0-9]*", "", y))

boulder_voters$RES_ADDRESS <- lapply(boulder_voters$RES_ADDRESS, 
                                     function(y) gsub(" UNIT [0-9]*", "", y))

# Combine
boulder_voters <- boulder_voters %>%
  mutate(ADDRESS = paste(RES_ADDRESS, 
                         RES_CITY, 
                         RES_STATE,
                         RES_ZIP_CODE,
                         sep=' '))

# Try geocoding (estimated run time of around 8 hours)

# added_geo  <- boulder_voters %>%
#   geocode(ADDRESS, 
#           method = "osm", 
#           lat = Latitude,
#           long = Longitude
#   )

df <- data.table(added_geo)

df <- df[, RES_ADDRESS := as.character(RES_ADDRESS)]

write.csv(df, "C://data//voter_records//geocoded_voters.csv")

voters  <- read.csv(
  file = "C://data//voter_records//geocoded_voters.csv",
)

voters <- voters[voters$STATUS == "Active",]

# How many NaNs in each column?

na_count  <- sapply(voters, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)

# Still over 7,000 addresses missing coordinates

voters <- voters[!is.na(voters$Latitude),]
missing_geo <-  voters[is.na(voters$Latitude),]

# Try to rework the addresses and see what we can clean up this way.
missing_geo <- missing_geo %>%
  mutate(ADDRESS = paste(HOUSE_NUMBER,
                         STREET_NAME,
                         STREET_TYPE,
                         RES_CITY,
                         RES_STATE,
                         RES_ZIP_CODE,
                         sep=' '))

added_geo  <- missing_geo %>%
  geocode(ADDRESS,
          method = "osm",
          lat = Latitude,
          long = Longitude
  )

added_geo <- added_geo %>%
  mutate(Latitude = Latitude...21) %>%
  mutate(Longitude = Longitude...22)

junk_cols <- c("Latitude...19",
               "Latitude...21",
               "Longitude...20",
               "Longitude...22")

added_geo <- added_geo[, !(names(added_geo) %in% junk_cols)]

na_count  <- sapply(added_geo, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)

# Down to about 2,000 missing. Many of these appear to be residence halls
# at CU Boulder. Some are perfectly good addresses, unsure of what to make of 
# those. The residence halls could be spread across multiple tracts, so we can't
# use a single point to represent them. Will have to get street addresses for 
# Each Hall

res_hall_rows <- added_geo[added_geo$ADDRESS %like% " HALL ", ]

# That only accounts for 153 of them.

# If we filter out all of the inactive voter records, everything is geocoded.

voters <- voters %>%
  mutate(age = 2022 - YOB)

max(voters$Latitude)
min(voters$Latitude)

max(voters$Longitude)
min(voters$Longitude)


bad_coord <- voters[voters$Latitude == min(voters$Latitude),]

# One address got an incorrect coordinate, one should be
# 40.00566, -105.28975
# Why isn't this working??
voters$Longitude[voters$ADDRESS == "1025 5TH ST BOULDER CO 80302"] <- -105.28975
voters$Latitude[voters$ADDRESS == "1025 5TH ST BOULDER CO 80302"] <- 40.00566

# Chestnut Lane has been incorrectly geocoded across multiple street addresses
# As has 5th street (at 1033 and 1025)

voters$Longitude[voters$ADDRESS == "1033 5TH ST BOULDER CO 80302"] <- -105.28973
voters$Latitude[voters$ADDRESS == "1033 5TH ST BOULDER CO 80302"] <- 40.00578

# Chestnut Lane addresses are all on the same block, will use one coordinate
voters$Longitude[(voters$STREET_NAME == "CHESTNUT") &
                   (voters$STREET_TYPE == "LN")] <- -105.20890
voters$Latitude[(voters$STREET_NAME == "CHESTNUT") &
                  (voters$STREET_TYPE == "LN")] <- 40.05886




# Turn it into a geo data frame with a geometry column, set to EPSG:4326 projection (for now)
geo_df = st_as_sf(voters, coords=c("Longitude", "Latitude"), crs=4326)

ggplot() + geom_sf(data = geo_df, aes(fill = age))
