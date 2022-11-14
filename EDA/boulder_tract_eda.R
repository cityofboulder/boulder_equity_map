setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(cluster)
library(ggplot2)
library(tidycensus)
library(tidyverse)
library(stringr)
library(openxlsx)
library(tigris)
library(tmap)

acs5_var <- load_variables(2020, "acs5")
write.xlsx(acs5_var, '../../data/raw_data/acs5_variables.xlsx')

all_variables <- acs5_var$name

#################
# 1. Import data
#################

year <- 2020


acs5_profile_vars <-  load_variables(year=year, 
                                     dataset = "acs5/profile", 
                                     cache = TRUE
                                     )

# Get rid of Puerto Rico characteristics
acs5_profile_vars <-  acs5_profile_vars[!acs5_profile_vars$concept == 'SELECTED SOCIAL CHARACTERISTICS IN PUERTO RICO',]

# acs5_var <- load_variables(2020, "acs5")

# Import table of tracts for City of Boulder
geo_corr <- read.csv("..//..//data//raw_data//geocorr_boulder_city.csv",
                     colClasses=c("tract"="character", 
                                  "block_group"="character"
                     )
)

boulder_tracts <- unique(geo_corr$tract)

# Import ACS 5-year profile variables for selected year
boulder_vars <- get_acs(
  geography = "tract",
  variables = acs5_profile_vars$name,
  state = "CO",
  county = "Boulder",
  # tract = boulder_tracts,
  year = year,
  survey = "acs5",
  geometry = TRUE
) %>%
  separate(NAME, into = c("tract", "county", "state"),
           sep= ", ")
boulder_vars$tract <- str_replace(
  boulder_vars$tract, 
  "Census Tract ", 
  ""
)
# This brings in too many tracts, in spite of the boulder_tracts
# limitation...
boulder_vars_wide <- get_acs(
  geography = "tract",
  variables = acs5_profile_vars$name,
  state = "CO",
  county = "Boulder",
  # tract = boulder_tracts,
  year = year,
  survey = "acs5",
  output = 'wide'
) %>%
  separate(NAME, into = c("tract", "county", "state"),
           sep= ", ")


boulder_vars_wide$tract <- str_replace(
  boulder_vars_wide$tract, 
  "Census Tract ", 
  ""
  )

# Try subsetting tracts again
boulder_vars_wide <- boulder_vars_wide[boulder_vars_wide$tract %in% boulder_tracts,]
boulder_vars <- boulder_vars[boulder_vars$tract %in% boulder_tracts,]
# But this is missing 3 tracts. What is going on? SOLVED. Missing trailing zero.
# setdiff(boulder_tracts, sub_wide$tract)
# So is the long-form.SOLVED. Missing trailing zero.
# setdiff(boulder_tracts, unique(boulder_vars$tract))


unique(boulder_vars_wide$tract)

# Remove MOE columns
boulder_vars_wide <- boulder_vars_wide[, -grep("M$", colnames(boulder_vars_wide))]

# Transform column names
colnames(boulder_vars_wide)[5:1358] <- gsub(
                                  "E", 
                                  "", 
                                  as.character(colnames(boulder_vars_wide)[5:1358])
)

#################
# 2. EDA
#################

# NA_cols <- boulder_vars_wide %>% summarise_all(~ sum(is.na(.)))
# NA_cols_only <- lapply(apply(NA_cols,1, function(x) which))

# Subset for rows with NaNs in the estimates

missing_data <- boulder_vars[is.na(boulder_vars$estimate), ]

# We have 416 out of 1,354 variables that are missing estimates
variables_missing <- unique(missing_data$variable)

incomplete_tracts <- unique(missing_data$tract)

# Examine the 413 (105 after removing Puerto Rico values) variables with missing 
# information to see if they would be valuable to keep where possible.
named_vars_missing <- subset(acs5_profile_vars, name %in% variables_missing)
write.xlsx(named_vars_missing, '../../data/raw_data/acs5_variables_missing.xlsx')

# 308 of those are measures for Puerto Rico, those can be dropped. 

# Compare scale of moe to estimates

boulder_vars <- mutate(
  boulder_vars, 
  moe_ratio = boulder_vars$moe/boulder_vars$estimate
  )

# Exclude Inf and NA values
boulder_moe <- boulder_vars[boulder_vars$estimate != 0,]
boulder_moe <- boulder_moe[!is.na(boulder_moe$moe_ratio),]

boulder_moe <-  boulder_moe %>%
  group_by(tract) %>%
    summarize(mean_moe_ratio = mean(moe_ratio))

tm_shape(boulder_moe) + 
  tm_polygons(col = "mean_moe_ratio")



# Combine measures
summary_df <-  as.data.frame(boulder_tracts)

summary_df$perc_missing <- ''
summary_df$avg_moe_perc <- ''

# Calculate average number of missing variables and average moe/estimate ratio
# for each tract in Boulder

for (i in boulder_tracts) {
  
  temp_df <- boulder_vars[which(boulder_vars$tract == i),] 
  mean_moe_ratio <- mean(temp_df$moe_ratio[is.finite(temp_df$moe_ratio)]) * 100
  print(mean_moe_ratio)
  num_na <- sum(is.na(temp_df$estimate))
  total_var <- length(temp_df$estimate)
  perc_missing <- num_na/total_var * 100
  summary_df$perc_missing[summary_df$boulder_tracts == i] = perc_missing
  summary_df$avg_moe_perc[summary_df$boulder_tracts == i] = mean_moe_perc
}

summary_df$perc_missing <- as.numeric(summary_df$perc_missing)
summary_df$avg_moe_perc <- as.numeric(summary_df$avg_moe_ratio)
summary_df$perc_missing[is.nan(summary_df$perc_missing)] <-  0


# There appear to be many missing values. We could exclude tracts that have 
# fewer than 100 people. That might improve things?

# Total Population is DP05_0033
tract_pops <-  subset(boulder_vars, variable == "DP05_0033")

# Nope, all tracts are sizable.
