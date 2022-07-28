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


boulder_blocks  <-  unique(geo_corr$block)

# Load in the ACS5 and ACS5 Profile variable information

acs5_profile_vars <-  load_variables(year=2020, 
                                     dataset = "acs5/profile", 
                                     cache = TRUE
)

acs5_var <- load_variables(2020, "acs5")

##############################
### Block Group Analysis #####
##############################

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
boulder_block_group_df <- acs5_block_groups[acs5_block_groups$NAME %in% boulder_blocks,]


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
  group_by(variable) %>%
  summarize(mean_moe_ratio = mean(moe_ratio))

block_group_variable_moe_ratios <- block_group_variable_moe_ratios[order(
                                        block_group_variable_moe_ratios$mean_moe_ratio),]

smallest_moe_ratio <-  block_group_variable_moe_ratios[block_group_variable_moe_ratios$mean_moe_ratio < 1,]

variable_candidates_list <- as.list(smallest_moe_ratio$variable)

# We'll have to use these to pull concepts rather than individual variables,
# as this method will exclude anything race-related for smaller groups
variable_candidates <- acs5_var[acs5_var$name %in% variable_candidates_list,]
concept_list <- as.list(unique(variable_candidates$concept))


########################################################
######### Examine Candidate Concept Categories #########
########################################################

concept_list <-  c("RACE",
                   "MEDIAN AGE BY SEX",
                   "SEX BY AGE",
                   "EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER",
                   "MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS",
                   "SUPPLEMENTAL SECURITY INCOME (SSI) IN THE PAST 12 MONTHS FOR HOUSEHOLDS",
                   "PUBLIC ASSISTANCE INCOME IN THE PAST 12 MONTHS FOR HOUSEHOLDS",
                   "PUBLIC ASSISTANCE INCOME OR FOOD STAMPS/SNAP IN THE PAST 12 MONTHS FOR HOUSEHOLDS",
                   "INTERNET SUBSCRIPTIONS IN HOUSEHOLD",
                   "COMPUTERS IN HOUSEHOLD",
                   "CITIZEN, VOTING-AGE POPULATION BY AGE",
                   "RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS"
                   )


variable_candidates_list <- as.list(
  acs5_var$name[acs5_var$concept %in% concept_list]
)

sub_boulder_bg <- boulder_block_group_df[boulder_block_group_df$variable %in%
                                           variable_candidates_list,]

acs5_var <- acs5_var[,names(acs5_var) %in% c('name', 'label', 'concept')]
acs5_var <- rename(acs5_var, "variable" = "name")

sub_boulder_bg <- merge(sub_boulder_bg,
        acs5_var,
        by="variable")



write.csv(sub_boulder_bg, "..//..//data//tidy_data//bg-level_candidate_variables_acs5_2020.csv")




##############################
### Tract Analysis #####
##############################


acs5_tracts <- get_acs(
  geography = "tract",
  variables = acs5_var$variable,
  state = "CO",
  county = "Boulder",
  year = 2020,
  tracts = boulder_tracts,
  survey = "acs5",
  # geometry = TRUE
)

# Exclude tracts outside city limits
boulder_tracts_df <- acs5_tracts[acs5_tracts$NAME %in% boulder_tracts,]


# Compare scale of moe to estimates

boulder_tr_df <- mutate(
  boulder_tracts_df, 
  moe_ratio = boulder_tracts_df$moe/boulder_tracts_df$estimate
)

# Exclude Inf and NA values
boulder_tr_moe <- boulder_tr_df[boulder_tr_df$estimate != 0,]
boulder_tr_moe <- boulder_tr_moe[!is.na(boulder_tr_moe$moe_ratio),]

tr_var_moe_ratios <-  boulder_tr_moe %>%
  # Group by variable
  group_by(variable) %>%
  summarize(mean_moe_ratio = mean(moe_ratio))

tr_var_moe_ratios <- tr_var_moe_ratios[order(
  tr_var_moe_ratios$mean_moe_ratio),]

smallest_moe_ratio_tr <-  tr_var_moe_ratios[tr_var_moe_ratios$mean_moe_ratio < 1,]

tr_variable_candidates_list <- as.list(smallest_moe_ratio_tr$variable)

# We'll have to use these to pull concepts rather than individual variables,
# as this method will exclude anything race-related for smaller groups
tr_variable_candidates <- acs5_var[acs5_var$variable %in% tr_variable_candidates_list,]
tr_concept_list <- as.list(unique(tr_variable_candidates$concept))

########################################################
######### Examine Candidate Concept Categories #########
########################################################

tr_concept_list <-  c("RACE",
                   "TOTAL POPULATION",
                   "MEDIAN AGE BY SEX",
                   "SEX BY AGE",
                   "GINI INDEX OF INCOME INEQUALITY",
                   "EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER",
                   "MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS",
                   "SUPPLEMENTAL SECURITY INCOME (SSI) IN THE PAST 12 MONTHS FOR HOUSEHOLDS",
                   "PUBLIC ASSISTANCE INCOME IN THE PAST 12 MONTHS FOR HOUSEHOLDS",
                   "PUBLIC ASSISTANCE INCOME OR FOOD STAMPS/SNAP IN THE PAST 12 MONTHS FOR HOUSEHOLDS",
                   "INTERNET SUBSCRIPTIONS IN HOUSEHOLD",
                   "COMPUTERS IN HOUSEHOLD",
                   "MONTHLY HOUSING COSTS",
                   "DETAILED RACE",
                   "CITIZEN, VOTING-AGE POPULATION BY AGE",
                   "RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS"
)


variable_candidates_list <- as.list(
  acs5_var$variable[acs5_var$concept %in% tr_concept_list]
)

sub_boulder_tr <- boulder_tr_df[boulder_tr_df$variable %in%
                                           variable_candidates_list,]



sub_boulder_tr <- merge(sub_boulder_tr,
                        acs5_var,
                        by="variable")



write.csv(sub_boulder_tr, "..//..//data//tidy_data//tr-level_candidate_variables_acs5_2020.csv")


#################################################
########## ACS 5 DATA PROFILE VARIABLES #########
#################################################

profile_bg <- get_acs(
  geography = "tract",
  variables = acs5_profile_vars$name,
  state = "CO",
  county = "Boulder",
  year = 2020,
  tracts = boulder_tracts,
  survey = "acs5"
)