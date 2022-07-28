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

df  <- read_csv("C://data//HHS//Client Contacts - HHS Salesforce raw data.csv")
names(df)

# How many NaNs in each column?
na_count  <- sapply(df, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)
na_count

table(df$MAILINGSTATE)

# Need to subset for addresses that are only in co
# And unify that designation

# TO geocode these, I need to remove substrings from MAILINGSTREET.
# Anything following a "#" sign, "unit x", "Lot X", remove special characters 
# like periods

# Clean MAILINGSTREET first

df <- df %>%
  mutate(
    MAILINGSTREET = toupper(MAILINGSTREET)
 ) %>%
  mutate(
    MAILINGSTATE = toupper(MAILINGSTATE)
  ) %>%
  mutate(
    MAILINGCITY = toupper(MAILINGCITY)
  ) %>%
  mutate(
    MAILINGSTREET = gsub("#.*", "",
                         gsub("#[0-9].*", "",
                            gsub("UNIT.*", "",
                              gsub(",.*", "",
                                   gsub("\\.", "",
                                        gsub("APT.*", "",
                                             gsub("[A-Z0-9]-[0-9].*", "",
                                                  gsub("LOT.*", "",MAILINGSTREET
                              ))))))))
  )

table(df$MAILINGSTATE)

# # Subset df for appropriate states
df <- df[(is.na(df$MAILINGSTATE)) |
           (df$MAILINGSTATE %in% c("CO", "COLORADO", "UNITED STATES")),]

table(df$MAILINGSTATE)

# Assume all of these are Colorado, so set them all to CO
df$MAILINGSTATE <- "CO"


table(df$MAILINGCITY)

df <- df[(is.na(df$MAILINGCITY)) |
           (df$MAILINGCITY %in% c("BOULDER", 
                                  "BOUDLER",
                                  "BOULDER, CO",
                                  "BOULER",
                                  "BOUULDER",
                                  "CO"
                                  )),]
df$MAILINGCITY <- "BOULDER"

df <-  df %>%
  mutate(
    df,
    CLEAN_ADDRESS = paste(df$MAILINGSTREET,
                          df$MAILINGCITY,
                          df$MAILINGSTATE,
                          sep = " "
    )
)

# Try geocoding (estimated run time?)

added_geo  <- df %>%
  geocode(CLEAN_ADDRESS,
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
    geocode(CLEAN_ADDRESS,
          method = "geocodio",
          lat = Latitude,
          long = Longitude
  )
na_count  <- sapply(geo_codio_geo, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)
na_count

# Recombine the geocodio addresses to added_geo

added_geo <- rbind(added_geo, geo_codio_geo)
added_geo <- added_geo[added_geo$CLEAN_ADDRESS != "NA NA CO",]

na_count  <- sapply(added_geo, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)
na_count

still_missing <- added_geo[is.na(added_geo$Latitude),]

# write.csv(added_geo, "C://data//HHS//geo_coded_clients.csv")
