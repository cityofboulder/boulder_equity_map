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
# library(classInt)
library(BAMMtools)

#########################################
##### 1. Read in selected variables #####
#########################################

acs <- read_csv("..//..//data//tidy_data//normalized_acs_vars.csv")
hhs_client <- read_csv("..//..//data//tidy_data/normalized_hhs_client_vars.csv")
hhs_house <- read_csv("..//..//data//tidy_data//normalized_hhs_housing_vars.csv")
crime <- read_csv("..//..//data//tidy_data//normalized_crime_vars.csv")
p_r_client <- read_csv("..//..//data//tidy_data//normalized_PnR_family_data.csv")


#########################################
######## 2. Merge Data Frames ###########
#########################################

full_df <- merge(acs, hhs_client, by = "GEOID", all.x = TRUE) %>%
  merge(hhs_house, by="GEOID", all.x = TRUE) %>%
  # merge(crime, by="GEOID", all.x = TRUE) %>%
  merge(p_r_client, by="GEOID", all.x = TRUE) %>%
  rename(perc_aff_hs_units = perc_affordable,
         perc_hhs_aid = perc_hs_aid,
         perc_pr_aid = client_percent,
         perc_public_assist = percent_aid)
# white is the exact inverse of % POC, so exclude
full_df <- full_df[,names(full_df) != "white"]



na_count  <- sapply(full_df, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)

full_df[is.na(full_df)] <- 0

z <- as.matrix(full_df[3:17])
cor_plot <- pairs( z, panel=function(x,y){
  points(x,y)
  abline(lm(y~x), col='blue')
  text(0.8,0.9,labels = paste('R2=',round((cor(x,y)),2)) ,col='blue' )
})



#########################################
####### 3. Variable Selection ###########
#########################################

# Function to calculate weighted sums across columns of
# dataframe or matrix x. Can receive a uniform weight or
# a list of weights to weight each column differently
# https://rdrr.io/github/ejanalysis/analyze.stuff/src/R/wtd.rowSums.R

wtd.rowSums <- function(x, wts=1, na.rm=TRUE) {
  rowSums(t(t(x) * wts), na.rm=na.rm)
}


# Check all variables

variables <- names(full_df[3:17])
# variables <- names(full_df[3:14])
var_df <- full_df[3:17]

# Invert direction of med_income and education to preserve relationship to combined vars
var_df$med_income <- factor(var_df$med_income)
levels(var_df$med_income) <- rev(levels(var_df$med_income))
var_df$med_income <- as.numeric(as.character(var_df$med_income))

var_df$percent_post_hs <- factor(var_df$percent_post_hs)
levels(var_df$percent_post_hs) <- rev(levels(var_df$percent_post_hs))
var_df$percent_post_hs <- as.numeric(as.character(var_df$percent_post_hs))


# Calculate variance

for (variable in variables) {
  column <- full_df[,names(full_df) == variable]
  print(variable)
  print(paste("Variance: ",var(column)))
  print(paste("CV: ", (sd(column)/mean(column) * 100)))
  print("...............")
  hist(column,
       main = paste("Histogram of ", variable),
       xlab = variable)
}

# Calculate correlation

# Empty df to store correlation values in
all_corr_df <- data.frame(matrix(ncol=5, nrow = 0))

colnames(all_corr_df) <- c("variable", 
                           "Pearson Coef", 
                           "Pearson P.value",
                           "Spearman Coef",
                           "Spearman P.value"
)
for (variable in variables) {
  index_df <- var_df[,names(var_df) != variable]
  index <- wtd.rowSums(index_df, wts=(1/14))
  # print(index)
  test_var <- var_df[,names(var_df) == variable]
  
  correlation_p <- cor.test(test_var, index, method = "pearson")
  correlation_s <- cor.test(test_var, index, method = "spearman")
  
  reg <- lm(test_var~index)
  plot(index, 
       test_var,
       xlab = "proto-index",
       ylab = variable)
  abline(reg)
  
  # print(test_var)
  # print("----------------")
  
  data <- t(as.data.frame(c(variable,
                            correlation_p$estimate,
                            correlation_p$p.value,
                            correlation_s$estimate,
                            correlation_s$p.value
                            )
                          )
            )
  colnames(data) <- c("variable", 
                      "Pearson Coef", 
                      "Pearson P.value",
                      "Spearman Coef",
                      "Spearman P.value"
                      )
  all_corr_df <- rbind(all_corr_df, data)
}

temp_df <- full_df[, !names(full_df) %in% c("perc_public_assist",
                                            "perc_pr_aid",
                                            "perc_aff_hs_units",
                                            "percent_post_hs")]

# z <- as.matrix(temp_df[3:13])
# cor_plot <- pairs( z, panel=function(x,y){
#   points(x,y)
#   abline(lm(y~x), col='blue')
#   text(0.8,0.9,labels = paste('R2=',round((cor(x,y)),2)) ,col='blue' )
# })
# Limit variables 
baa_drop_vars <-  c("white",
                # "black",
                "ai_ak",
                "asian",
                "nh_pi",
                "other",
                "2_plus_othr",
                # "percent_blw_pov",
                "percent_poc",
                "perc_aff_hs_units",
                "perc_pr_aid",
                "perc_public_assist",
                "percent_post_hs"
                )
poc_drop_vars <-  c("white",
               "black",
               "ai_ak",
               "asian",
               "nh_pi",
               "other",
               "2_plus_othr",
               # "percent_blw_pov",
               # "percent_poc"
               "perc_aff_hs_units",
               "perc_pr_aid",
               "perc_public_assist",
               "percent_post_hs"
)

baa_df <- full_df[,!names(full_df) %in% baa_drop_vars]
poc_df <- full_df[,!names(full_df) %in% poc_drop_vars]

count_vars <- length(names(poc_df))

# z <- as.matrix(baa_df[3:count_vars])
# cor_plot <- pairs( z, panel=function(x,y){
#   points(x,y)
#   abline(lm(y~x), col='blue')
#   text(0.8,0.9,labels = paste('R2=',round((cor(x,y)),2)) ,col='blue' )
# })

# Want to loop through each variable and compare it
# to a column of weighted sums from a matrix/dataframe that
# excludes the variable to calculate Pearson correlation.
# Need to append relevant statistics into a dataframe with a 
# row for each variable.

# Should consider Spearman's rank correlation as alternative, due to outliers in
# distributions.

# Calculate variance

for (variable in variables) {
  column <- full_df[,names(full_df) == variable]
  print(variable)
  print(paste("Variance: ",var(column)))
  print(paste("CV: ", (sd(column)/mean(column) * 100)))
  print("...............")
}

# Calculate correlation for BAA index
variables <- names(baa_df[3:7])

var_df <- baa_df[3:7]
# Invert direction of med_income to preserve relationship to combined vars
var_df$med_income <- factor(var_df$med_income)
levels(var_df$med_income) <- rev(levels(var_df$med_income))
var_df$med_income <- as.numeric(as.character(var_df$med_income))

baa_corr_df <- data.frame(matrix(ncol=5, nrow = 0))

colnames(baa_corr_df) <- c("variable", 
                       "Pearson Coef", 
                       "Pearson P.value",
                       "Spearman Coef",
                       "Spearman P.value"
)
for (variable in variables) {
  index_df <- var_df[,names(var_df) != variable]
  index <- wtd.rowSums(index_df, wts=(1/(length(names(index_df)))))
  # print(index)
  test_var <- var_df[,names(var_df) == variable]
  
  correlation_p <- cor.test(test_var, index, method = "pearson")
  correlation_s <- cor.test(test_var, index, method = "spearman")
  # print(test_var)
  # print("----------------")
  reg <- lm(test_var~index)
  plot(index, 
       test_var,
       xlab = "proto-index",
       ylab = variable)
  abline(reg)
  
  data <- t(as.data.frame(c(variable,
                            correlation_p$estimate,
                            correlation_p$p.value,
                            correlation_s$estimate,
                            correlation_s$p.value
                            )
                          )
            )
  colnames(data) <- c("variable", 
                      "Pearson Coef", 
                      "Pearson P.value",
                      "Spearman Coef",
                      "Spearman P.value"
  )
  baa_corr_df <- rbind(baa_corr_df, data)
}
write.csv(baa_corr_df, "..//..//data//tidy_data//baa_corr_df.csv")

# Now for the POC index
variables <- names(poc_df[3:count_vars])

var_df <- poc_df[3:count_vars]

# Invert direction of med_income to preserve relationship to combined vars
# and for percent_post_hs
var_df$med_income <- factor(var_df$med_income)
levels(var_df$med_income) <- rev(levels(var_df$med_income))
var_df$med_income <- as.numeric(as.character(var_df$med_income))

var_df$percent_post_hs <- factor(var_df$percent_post_hs)
levels(var_df$percent_post_hs) <- rev(levels(var_df$percent_post_hs))
var_df$percent_post_hs <- as.numeric(as.character(var_df$percent_post_hs))

poc_corr_df <- data.frame(matrix(ncol=length(var_df), nrow = 0))

colnames(poc_corr_df) <- c("variable", 
                           "Pearson Coef", 
                           "Pearson P.value",
                           "Spearman Coef",
                           "Spearman P.value"
)
for (variable in variables) {
  index_df <- var_df[,names(var_df) != variable]
  index <- wtd.rowSums(index_df, wts=(1/length(names(index_df))))

  # print(index)
  test_var <- var_df[,names(var_df) == variable]
  
  correlation_p <- cor.test(test_var, index, method = "pearson")
  correlation_s <- cor.test(test_var, index, method = "spearman")
  # print(test_var)
  # print("----------------")
  reg <- lm(test_var~index)
  plot(index, 
       test_var,
       xlab = "proto-index",
       ylab = variable)
  abline(reg)
  
  data <- t(as.data.frame(c(variable,
                            correlation_p$estimate,
                            correlation_p$p.value,
                            correlation_s$estimate,
                            correlation_s$p.value
  )
  )
  )
  colnames(data) <- c("variable", 
                      "Pearson Coef", 
                      "Pearson P.value",
                      "Spearman Coef",
                      "Spearman P.value"
  )
  poc_corr_df <- rbind(poc_corr_df, data)
}
write.csv(poc_corr_df, "..//..//data//tidy_data//poc_corr_df.csv")

# z <- as.matrix(var_df)
# cor_plot <- pairs( z, panel=function(x,y){
#   points(x,y)
#   abline(lm(y~x), col='blue')
#   text(0.8,0.9,labels = paste('R2=',round((cor(x,y)),2)) ,col='blue' )
# })


#########################################
############# 4. Index ##################
#########################################

raw_values_df <- full_df[, names(full_df) %in% c("GEOID",
                                                 "NAME",
                                                 "percent_post_hs",
                                                 "percent_poc",
                                                 "percent_h_l",
                                                 "med_income",
                                                 "percent_blw_pov",
                                                 "perc_hhs_aid"
                                                 )
                        ]

# Invert negatively correlated variables so increased value means increased need
raw_values_df$med_income <- factor(raw_values_df$med_income)
levels(raw_values_df$med_income) <- rev(levels(raw_values_df$med_income))
raw_values_df$med_income <- as.numeric(as.character(raw_values_df$med_income))

raw_values_df$percent_post_hs <- factor(raw_values_df$percent_post_hs)
levels(raw_values_df$percent_post_hs) <- rev(levels(raw_values_df$percent_post_hs))
raw_values_df$percent_post_hs <- as.numeric(as.character(raw_values_df$percent_post_hs))

raw_values_df$index <- wtd.rowSums(raw_values_df[,3:8], wts=c(0,2,2,1,1,1))

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

geo_idx <- merge(x = raw_values_df, y = geo, by = "GEOID", all.x = TRUE)

geo_idx <- st_as_sf(geo_idx,
                    sf_column_name = "geometry",
                    crs=4326
)

#########################################
########## 5. Index Cleanup #############
#########################################

ranks <- getJenksBreaks(geo_idx$index, 6)

geo_idx <- geo_idx %>%
  mutate(INDEX = case_when(index >=ranks[1] & index < ranks[2] ~ 1,
                             index >=ranks[2] & index < ranks[3] ~ 2,
                             index >=ranks[3] & index < ranks[4] ~ 3,
                             index >=ranks[4] & index < ranks[5] ~ 4,
                             index >=ranks[4] ~ 5
                             ))
geo_idx$INDEX <- as.character(geo_idx$INDEX)

tmap_mode("view")
# tmap_mode("plot")
# Index map
tm_basemap("OpenStreetMap.France") +
  tm_shape(geo_idx) + 
  tm_polygons(col = "INDEX",
              # style = "jenks",
              # n = 5,
              alpha = 0.9,
              palette = "Blues",
              title = "Equity Index") + 
  tm_layout(title = "Index\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)



export_file <- geo_idx[,names(geo_idx) %in% c("GEOID",
                                              "NAME",
                                              "INDEX")]

export_file <- st_transform(export_file, crs = "EPSG:2876")
st_crs(export_file)

st_write(
  export_file,
  "..//..//data//tidy_data//draft_equity_index.shp",
  driver="ESRI Shapefile"
)