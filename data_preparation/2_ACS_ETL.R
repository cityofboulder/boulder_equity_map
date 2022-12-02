#########################################
##### 0. Set up R #####
#########################################

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
# library(tmap)
# library(basemaps)
# library(tmaptools)
# library(OpenStreetMap)

#########################################
##### 1. Read in selected variables #####
#########################################

# Work has already been done to select and download ACS data from 
# https://mcdc.missouri.edu/applications/geocorr2022.html
# Additional block groups added manually to include un-annexed sub-communities.
bg_df  <- read_csv("..//data//tidy_data//bg-level_candidate_variables_acs5_2020.csv")

bg_df$label <- str_replace_all(bg_df$label, "[^[:alnum:]]", " ")

bg_df <- bg_df %>%
  mutate(label = str_squish(label))

bg_list <-  as.list(unique(bg_df$NAME))

# Group variables
# Code book is available in ../data/raw_data/acs5_variables.xlsx
race_var <- c("B02001_001", # Total
              "B02001_002", # White
              "B02001_003", # Black or AA
              "B02001_004", # A Indian or AK Native
              "B02001_005", # Asian
              "B02001_006", # Native Hawaiian or PI
              "B02001_007", # Other
              # "B02001_008", # 2+
              "B02001_009" # 2+ incl. Other
              # "B02001_010" # 2+ excluding Other, 3+
              )

eth_var <- c("B03003_001", # Total
             # "B03003_002", # Not H/L
             "B03003_003" # H/L
              )

edu_var <- c("B15003_001", # Total
             "B15003_022", # Bachelor's
             "B15003_023", # MS
             "B15003_024", # Professional School degree
             "B15003_025"  # Doctorate
              )

econ_var1 <- c("B19013_001", # Median household income past year
              "B19058_001", # Pop total
              "B19058_002"  # Count with cash pub assistance or food stamps/SNAP
              )

econ_var2 <- c("C17002_001", # Total pop
               "C17002_002", # Under .5 income:pov line ratio
               "C17002_003"  # .5-.99 income:pov line ratio
               )

# tenure_var <- c("B25003_001", # Total homes
#                 "B25003_003"  # Renter occupied
#                 )

########################################
### 2. Adjust measures by population ###
########################################


#########################################
# Isolate and adjust race variables
#########################################

race_df <- bg_df[bg_df$variable %in% race_var,]

race_perc <- data.frame()
for (i in bg_list) {
  total <- race_df$estimate[(race_df$NAME == i) &
                              (race_df$variable == 'B02001_001')
                            ]
  sub_df <- race_df[race_df$NAME == i,] %>%
    mutate(percent = estimate / total * 100)
  race_perc <- rbind(race_perc, sub_df)
}

race_perc <- race_perc[race_perc$variable != "B02001_001",]
race_perc <- race_perc[, names(race_perc) %in% c("GEOID",
                                                 "variable",
                                                 "percent"
                                                 )
                       ]


## May need to return to this section to aggregate POC into one element

#########################################
# Isolate and adjust ethnicity variables
#########################################

eth_df <- bg_df[bg_df$variable %in% eth_var,]

eth_perc <- data.frame()
for (i in bg_list) {
  total <- eth_df$estimate[(eth_df$NAME == i) &
                              (eth_df$variable == "B03003_001")
                           ]
  sub_df <- eth_df[eth_df$NAME == i,] %>%
    mutate(percent_h_l = estimate / total * 100)
  eth_perc <- rbind(eth_perc, sub_df)
}

eth_perc <- eth_perc[eth_perc$variable != "B03003_001",]
eth_perc <- eth_perc[, names(eth_perc) %in% c("GEOID",
                                              "variable",
                                              "percent_h_l"
                                              )
]


#########################################
# Isolate and adjust education variables
#########################################

edu_df <- bg_df[bg_df$variable %in% edu_var,]

# Isolate totals to aggregate degree counts
edu_totals <- edu_df[edu_df$variable == "B15003_001",]
edu_df <- edu_df[edu_df$variable != "B15003_001",]

edu_df <- edu_df[, names(edu_df) %in% c("GEOID",
                                        "NAME",
                                        "variable",
                                        "estimate"
                                        )
                 ]

degree_df <- edu_df %>%
  group_by(GEOID, NAME) %>%
  summarize(degree_count = sum(estimate))


edu_perc <- data.frame()
for (i in bg_list) {
  total <- edu_totals$estimate[edu_totals$NAME == i]
  sub_df <- degree_df[degree_df$NAME == i,] %>%
    mutate(percent_post_hs = degree_count / total * 100)
  edu_perc <- rbind(edu_perc, sub_df)
}
edu_perc <- edu_perc[, names(edu_perc) != "degree_count"]
edu_perc$percent_post_hs[is.na(edu_perc$percent_post_hs)] <- 0

#########################################
# Isolate and adjust economic variables
#########################################


# % receiving public assistance income OR food stamps/SNAP
econ1_df <- bg_df[bg_df$variable %in% econ_var1,]

med_inc_df <- econ1_df[econ1_df$variable == "B19013_001",] %>%
  rename("med_income" = "estimate")
  
aid_df <- econ1_df[econ1_df$variable != "B19013_001",]

aid_perc <- data.frame()
for (i in bg_list) {
  total <- aid_df$estimate[(aid_df$NAME == i) &
                              (aid_df$variable == "B19058_001")
                           ]
  sub_df <- aid_df[aid_df$NAME == i,] %>%
    mutate(percent_aid = estimate / total * 100)
  aid_perc <- rbind(aid_perc, sub_df)
}

aid_perc <- aid_perc[aid_perc$variable != "B19058_001",]
aid_perc$percent_aid[is.na(aid_perc$percent_aid)] <- 0
aid_perc <- aid_perc[, names(aid_perc) %in% c("GEOID",
                                              "percent_aid"
                                              )
                    ]
#########################################                    
# % Below Poverty
#########################################

pov_df <- bg_df[bg_df$variable %in% econ_var2,]
total_pop_df <- pov_df[pov_df$variable == "C17002_001",]
pov_df <- pov_df[pov_df$variable != "C17002_001",]


total_below_1 <- pov_df[names(pov_df) %in% c("GEOID",
                                             "NAME",
                                             "estimate")] %>%
  group_by(GEOID, NAME) %>%
  summarize(total_below_pov = sum(estimate))

pov_perc <- data.frame()

for (i in bg_list) {
  total <- total_pop_df$estimate[(total_pop_df$NAME == i)]
  
  sub_df <- total_below_1[total_below_1$NAME == i,] %>%
    mutate(percent_blw_pov = total_below_pov / total * 100)
  pov_perc <- rbind(pov_perc, sub_df)
}

# All NaNs are percentages for estimates of zero
pov_perc$percent_blw_pov[is.na(pov_perc$percent_blw_pov)] <- 0

pov_perc <- pov_perc[, names(pov_perc) %in% c("GEOID",
                                              "percent_blw_pov"
                                              )
                     ]

############################################
### 3. Convert to wide-format dataframes ###
############################################

# edu_perc has the BG names, can join to it

race_wide <- race_perc %>%
  spread(variable, percent) %>%
  mutate(percent_poc = 100 - B02001_002)

eth_perc <- eth_perc[,names(eth_perc) != "variable"]

# We're missing 3 estimates for median income?

med_inc_df <- med_inc_df[, names(med_inc_df) %in% c("GEOID",
                                              "med_income"
                                              )
                         ]

full_df <- merge(edu_perc, race_wide, by = "GEOID", all.x = TRUE) %>%
  merge(eth_perc, by="GEOID", all.x = TRUE) %>%
  merge(med_inc_df, by="GEOID", all.x = TRUE) %>%
  merge(aid_perc, by="GEOID", all.x = TRUE) %>%
  merge(pov_perc, by="GEOID", all.x = TRUE)

############################################
######### 4. Normalize Variables ###########
############################################

Normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

norm_acs <- full_df

col_list <- as.list(names(select(full_df, c(3:15))))

# We have some missing values, which won't allow us to normalize
na_count  <- sapply(full_df, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)

full_df$GEOID[is.na(full_df$med_income) == TRUE]


# The missing values are in tracts mainly in the university area. Don't want to 
# set to zero, probably want to mask any effect here. Mean imputation?
norm_acs$med_income[is.na(norm_acs$med_income)] <- mean(norm_acs$med_income, na.rm = TRUE) 

# Normalize columns
norm_acs[3:15] <- lapply(norm_acs[3:15], function(x) Normalize(x))


# Rename columns for clarity
norm_acs <- norm_acs %>%
  rename("white" = "B02001_002",
         "black" = "B02001_003",
         "ai_ak" = "B02001_004",
         "asian" = "B02001_005",
         "nh_pi" = "B02001_006",
         "other" = "B02001_007",
         "2_plus_othr" = "B02001_009"
         )

write.csv(norm_acs, "..//data//tidy_data//normalized_acs_vars.csv", 
          row.names = FALSE)

############################################
######### 5. Examine Correlation ###########
############################################

pairs(norm_acs[,3:15])


z <- as.matrix(norm_acs[3:15])
pairs( z, panel=function(x,y){
  points(x,y)
  abline(lm(y~x), col='red')
  text(0,1.5,labels = paste('R2=',round((cor(x,y))^2,2)) ,col='red' )
})
