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
geo_corr <- mutate(
  geo_corr,
  tract = paste("Census Tract ",
                geo_corr$tract,
                ", Boulder County, Colorado",
                sep = ""
  )
)
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
  variables = 'B19083_001',
  state = "CO",
  county = "Boulder",
  year = 2020,
  # tracts = boulder_tracts,
  survey = "acs5",
  geometry = TRUE
)
geo <- geo[,names(geo) %in% c("GEOID", "geometry")]

geo_tr <- get_acs(
  geography = "tract",
  variables = 'B19083_001',
  state = "CO",
  county = "Boulder",
  year = 2020,
  # tracts = boulder_tracts,
  survey = "acs5",
  geometry = TRUE
)
geo_tr <- geo_tr[,names(geo_tr) %in% c("GEOID", "geometry")]

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
perc_white <- perc_white %>%
  mutate(perc_non_white = 100-percent)
perc_black <- race_perc[race_perc$variable == "B02001_003",]

tm_shape(perc_white) + 
  tm_polygons() 


tmap_mode("view")

# Percent White map
tm_basemap("OpenStreetMap.France") +
tm_shape(perc_white) + 
  tm_polygons(col = "percent",
              style = "quantile",
              n = 7,
              alpha = 0.7,
              palette = "Purples",
              title = "2020 ACS") + 
  tm_layout(title = "Percent White\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)

# Percent Non-White Map

tm_basemap("OpenStreetMap.France") +
  tm_shape(perc_white) + 
  tm_polygons(col = "perc_non_white",
              style = "jenks", #"quantile",
              n = 5,
              alpha = 0.8,
              palette = "Blues",
              title = "2020 ACS") + 
  tm_layout(title = "Percent Non-White",
            frame = FALSE,
            legend.outside = TRUE)



nc_centers <- st_centroid(perc_black)

# Percent black Map
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



# Subset for 3 bg
tm_basemap("OpenStreetMap.France") +
  tm_shape(perc_black[perc_black$GEOID %in% c("080130123001", "080130124011", "080130125113"),]) + 
  tm_polygons(#col = "percent",
              style = "quantile",
              n = 7,
              alpha = 0.8,
              palette = "Purples",
              title = "2020 ACS") + 
  tm_layout(title = "Percent Black\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)

###############################################
############# Income ##########################
###############################################

pov_df <- bg_df[bg_df$concept == "RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS",]


# Get totals
totals <- pov_df[pov_df$variable == "C17002_001",]
# Drop anything over the poverty line (1+)
pov_df <- pov_df[!pov_df$variable %in% c("C17002_004",
                                         "C17002_005",
                                         "C17002_006",
                                         "C17002_007",
                                         "C17002_008",
                                         "C17002_001"
                                         ),]

total_below_1 <- pov_df[names(pov_df) %in% c("GEOID",
                                             "NAME",
                                             "estimate")] %>%
  group_by(GEOID, NAME) %>%
  summarize(total_below_pov = sum(estimate))

bg_list <-  as.list(unique(pov_df$NAME))

pov_perc <- data.frame()

for (i in bg_list) {
  total <- totals$estimate[(totals$NAME == i)]
  
  sub_df <- total_below_1[total_below_1$NAME == i,] %>%
    mutate(percent = total_below_pov / total * 100)
  pov_perc <- rbind(pov_perc, sub_df)
}

# All NaNs are percentages for estimates of zero
pov_perc$percent[is.na(pov_perc$percent)] <- 0

# Now, need to group by (sum) block group



# Need to remove superfluous columns from geo

pov_perc <- merge(x = pov_perc, y = geo, by = "GEOID", all.x = TRUE)


pov_perc <- st_as_sf(pov_perc,
                      sf_column_name = "geometry",
                      crs=4326
)


tm_basemap("OpenStreetMap.France") +
  tm_shape(pov_perc) + 
  tm_polygons(col = "percent",
              style = "jenks", #"quantile",
              n = 5,
              # alpha = 0.7,
              palette = "Reds",
              title = "2020 ACS") + 
  tm_layout(title = "Percent Below Poverty Line",
            frame = FALSE,
            legend.outside = TRUE)



###################################################
################# Gini Index ######################
###################################################

# Gini index only available at tract level

boulder_tracts <- unique(geo_corr$tract)

gini_tr <- get_acs(
  geography = "tract",
  variables = 'B19083_001',
  state = "CO",
  county = "Boulder",
  year = 2020,
  survey = "acs5",
  geometry = TRUE
)

gini_tr <- gini_tr[gini_tr$NAME %in% boulder_tracts,]


# Compare scale of moe to estimates

gini_tr <- mutate(
  gini_tr, 
  moe_ratio = gini_tr$moe/gini_tr$estimate
)


tm_basemap("OpenStreetMap.France") +
  tm_shape(gini_tr) + 
  tm_polygons(col = "estimate",
              style = "jenks", #"quantile",
              alpha = 0.8,
              palette = "Purples",
              title = "2020 ACS") + 
  tm_layout(title = "Gini Index of Inequality",
            frame = FALSE,
            legend.outside = TRUE)





###########################################
### Mean MOE Ratio - Block Group ##########
###########################################

acs5_var <- load_variables(2020, "acs5")

acs5_block_groups <- get_acs(
  geography = "block group",
  variables = acs5_var$name,
  state = "CO",
  county = "Boulder",
  year = 2020,
  tracts = boulder_tracts,
  survey = "acs5",
  # geometry = TRUE
)
# Subset for only Boulder block groups
boulder_block_group_df <- acs5_block_groups[acs5_block_groups$NAME %in% bg_list,]


# Get rid of estimates that are missing
boulder_block_group_df <- boulder_block_group_df[!is.na(boulder_block_group_df$estimate), ]


# Compare scale of moe to estimates

boulder_block_group_df <- mutate(
  boulder_block_group_df, 
  moe_ratio = boulder_block_group_df$moe/boulder_block_group_df$estimate
)

# Exclude Inf and NA values
boulder_moe <- boulder_block_group_df[boulder_block_group_df$estimate != 0,]
boulder_moe <- boulder_moe[!is.na(boulder_moe$moe_ratio),]

block_group_variable_moe_ratios <-  boulder_moe %>%
  # Group by variable
  group_by(GEOID) %>%
  summarize(mean_moe_ratio = mean(moe_ratio))

block_group_variable_moe_ratios <- block_group_variable_moe_ratios[order(
  block_group_variable_moe_ratios$mean_moe_ratio),]

geo_moe <- merge(block_group_variable_moe_ratios, geo, by = "GEOID", all.x = TRUE)

geo_moe <- st_as_sf(geo_moe,
                     sf_column_name = "geometry",
                     crs=4326
)

tm_basemap("OpenStreetMap.France") +
  tm_shape(geo_moe) + 
  tm_polygons(col = "mean_moe_ratio",
              style = "jenks", #"quantile",
              n = 5,
              # alpha = 0.8,
              palette = "Reds",
              title = "2020 ACS") + 
  tm_layout(title = "Mean MOE:Estimate Ratio",
            frame = FALSE,
            legend.outside = TRUE)

###########################################
######### Mean MOE Ratio - Tract ##########
###########################################

acs5_tr <- get_acs(
  geography = "tract",
  variables = acs5_var$name,
  state = "CO",
  county = "Boulder",
  year = 2020,
  tracts = boulder_tracts,
  survey = "acs5",
  # geometry = TRUE
)
# Subset for only Boulder block groups
boulder_tr_df <- acs5_tr[acs5_tr$NAME %in% boulder_tracts,]


# Get rid of estimates that are missing
boulder_tr_df <- boulder_tr_df[!is.na(boulder_tr_df$estimate), ]


# Compare scale of moe to estimates

boulder_tr_df <- mutate(
  boulder_tr_df, 
  moe_ratio = boulder_tr_df$moe/boulder_tr_df$estimate
)

# Exclude Inf and NA values
boulder_tr_moe <- boulder_tr_df[boulder_tr_df$estimate != 0,]
boulder_tr_moe <- boulder_tr_moe[!is.na(boulder_tr_moe$moe_ratio),]

tr_variable_moe_ratios <-  boulder_tr_moe %>%
  # Group by variable
  group_by(GEOID) %>%
  summarize(mean_moe_ratio = mean(moe_ratio))

tr_variable_moe_ratios <- tr_variable_moe_ratios[order(
  tr_variable_moe_ratios$mean_moe_ratio),]

geo_tr_moe <- merge(tr_variable_moe_ratios, geo_tr, by = "GEOID", all.x = TRUE)

geo_tr_moe <- st_as_sf(geo_tr_moe,
                    sf_column_name = "geometry",
                    crs=4326
)

tm_basemap("OpenStreetMap.France") +
  tm_shape(geo_tr_moe) + 
  tm_polygons(col = "mean_moe_ratio",
              style = "jenks",
              n = 5,
              # alpha = 0.8,
              palette = "Reds",
              title = "2020 ACS") + 
  tm_layout(title = "Mean MOE:Estimate Ratio",
            frame = FALSE,
            legend.outside = TRUE)

############################################
################ Combine ###################
############################################


tm_basemap("OpenStreetMap.France") +
  tm_shape(perc_white) + 
  tm_polygons(col = "perc_non_white",
              style = "jenks", #"quantile",
              n = 10,
              alpha = 0.7,
              palette = "Purples",
              title = "% Non-White") + 
  tm_shape(pov_perc) + 
  tm_polygons(col = "percent",
              style = "jenks", #"quantile",
              n = 10,
              alpha = 0.7,
              palette = "Purples",
              title = "% Below Poverty Line") + 
  tm_shape(gini_tr) + 
  tm_polygons(col = "estimate",
              style = "jenks", #"quantile",
              n = 10,
              alpha = 0.7,
              palette = "Reds",
              title = "Gini Index")
