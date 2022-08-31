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
  
  # Plot nonlinear correlation
  # nlcor(test_var, index, plt = T)
  
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
                "perc_public_assist"
                # "percent_post_hs"
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
               "perc_public_assist"
               # "percent_post_hs"
)

baa_df <- full_df[,!names(full_df) %in% baa_drop_vars]
poc_df <- full_df[,!names(full_df) %in% poc_drop_vars]

count_vars <- length(names(poc_df))

z <- as.matrix(baa_df[3:count_vars])
cor_plot <- pairs( z, panel=function(x,y){
  points(x,y)
  abline(lm(y~x), col='blue')
  text(0.8,0.9,labels = paste('R2=',round((cor(x,y)),2)) ,col='blue' )
})

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
variables <- names(baa_df[3:count_vars])

var_df <- baa_df[3:count_vars]
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
z <- as.matrix(var_df)
cor_plot <- pairs( z, panel=function(x,y){
  points(x,y)
  abline(lm(y~x), col='blue')
  text(0.8,0.9,labels = paste('R2=',round((cor(x,y)),2)) ,col='blue' )
})


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

raw_values_df$index <- wtd.rowSums(raw_values_df[,3:8], wts=c(1,2,2,1,1,1))

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

tmap_mode("view")

# Index map
tm_basemap("OpenStreetMap.France") +
  tm_shape(geo_idx) + 
  tm_polygons(col = "index",
              style = "jenks",
              n = 5,
              alpha = 0.8,
              palette = "Blues",
              title = "Equity Index") + 
  tm_layout(title = "Index\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)

# Comparisons

tm_basemap("OpenStreetMap.France") +
  tm_shape(geo_idx) + 
  tm_polygons(col = "perc_hhs_aid",
              style = "jenks",
              n = 5,
              alpha = 0.8,
              palette = "Blues",
              title = "HHS Aid Recipients") + 
  tm_layout(title = "Aid Recipients\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)


# Combine all Race Variables for comparison

full_geo <- merge(x = full_df, y = geo, by = "GEOID", all.x = TRUE)

full_geo <- st_as_sf(full_geo,
                    sf_column_name = "geometry",
                    crs=4326
)

alpha <- 0.3

tm_basemap("OpenStreetMap.France") +
  tm_shape(full_geo) + 
  tm_polygons(col = "black",
              style = "jenks",
              n = 5,
              alpha = alpha,
              palette = "Blues",
              title = "% Black/African American\nby Block Group") + 
  tm_layout(title = "% Black/African American\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE) + 
  tm_shape(full_geo) + 
  tm_polygons(col = "ai_ak",
              style = "jenks",
              n = 5,
              alpha = alpha,
              palette = "Blues",
              title = "% American Indian/Native Alaskan\nby Block Group") + 
  tm_layout(title = "% American Indian/Native Alaskan\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE) + 
  tm_shape(full_geo) + 
  tm_polygons(col = "asian",
              style = "jenks",
              n = 5,
              alpha = alpha,
              palette = "Purples",
              title = "% Asian\nby Block Group") +
  tm_layout(title = "% Asian\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE) + 
  tm_shape(full_geo) + 
  tm_polygons(col = "nh_pi",
              style = "jenks",
              n = 5,
              alpha = alpha,
              palette = "Reds",
              title = "% Native Hawaiian/Pacific Islander\nby Block Group") + 
  tm_layout(title = "% Native Hawaiian/Pacific Islander\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE) + 
  tm_shape(full_geo) + 
  tm_polygons(col = "other",
              style = "jenks",
              n = 5,
              alpha = alpha,
              palette = "Greens",
              title = "% Other\nby Block Group") + 
  tm_layout(title = "% Other\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE) + 
  tm_shape(full_geo) + 
  tm_polygons(col = "percent_h_l",
              style = "jenks",
              n = 5,
              alpha = alpha,
              palette = "Blues",
              title = "% Hispanic/Latino\nby Block Group") + 
  tm_layout(title = "% Hispanic/Latino\nby Block Group",
            frame = FALSE,
            legend.outside = TRUE)#+ 
  # tm_shape(full_geo) + 
  # tm_polygons(col = "2_plus_othr",
  #             style = "jenks",
  #             n = 5,
  #             alpha = 0.3,
  #             palette = "Blues",
  #             title = "% 2+ Other\nby Block Group") + 
  # tm_layout(title = "",
  #           frame = FALSE,
  #           legend.outside = TRUE)


# index_df <- raw_values_df[,1:2]
# 
# # Can't get 5 levels for education, it gives duplicate breaks
# ed_breaks <- classIntervals(raw_values_df$percent_post_hs, n=4, style = 'jenks')
# poc_breaks <- classIntervals(raw_values_df$percent_poc, n=5, style = 'jenks')
# hl_breaks <- classIntervals(raw_values_df$percent_h_l, n=5, style = 'jenks')
# inc_breaks <- classIntervals(raw_values_df$med_income, n=5, style = 'jenks')
# pov_breaks <- classIntervals(raw_values_df$percent_blw_pov, n=5, style = 'jenks')
# hhs_breaks <- classIntervals(raw_values_df$perc_hhs_aid, n=5, style = 'jenks')
# 
# index_df$edu_idx <- cut(raw_values_df$percent_post_hs,
#                         breaks = ed_breaks$brks,
#                         labels=as.character(1:4)
#                         )
# 
# index_df$poc_idx <- cut(raw_values_df$percent_poc,
#                         breaks = poc_breaks$brks,
#                         labels=as.character(1:5)
# )
# 
# index_df$hl_idx <- cut(raw_values_df$percent_h_l,
#                         breaks = hl_breaks$brks,
#                         labels=as.character(1:5)
# )
# 
# index_df$inc_idx <- cut(raw_values_df$med_income,
#                         breaks = inc_breaks$brks,
#                         labels=as.character(1:5)
# )
# 
# index_df$pov_idx <- cut(raw_values_df$percent_blw_pov,
#                         breaks = pov_breaks$brks,
#                         labels=as.character(1:5)
# )
# 
# index_df$hhs_idx <- cut(raw_values_df$perc_hhs_aid,
#                         breaks = hhs_breaks$brks,
#                         labels=as.character(1:5)
# )

# Not sure why there are missing values in the indices. Zeroes should just be
# in the bottom cateogry?

index_df[is.na(index_df)] <- "1"
index_df[,3:8] <- sapply(index_df[,3:8], as.numeric)

index_df$index <- wtd.rowSums(index_df[,3:8], wts=(1))


################################################################################

# Combine Aid Variables alone

aid <- full_df[, names(full_df) %in% c("perc_hhs_aid",
                                       "perc_pr_aid",
                                       "perc_aff_hs_units",
                                       "perc_public_assist",
                                       "med_income",
                                       "percent_blw_pov"
                                       )
               ]
aid <- aid %>%
  rename("% Affordable Housing" = perc_aff_hs_units,
         "% HHS Aid Recipients" = perc_hhs_aid,
         "% Parks & Rec Aid Recipients" = perc_pr_aid,
         "% Federal Aid/SNAP Recipients" = perc_public_assist,
         "Median Income" = med_income,
         "% Below Poverty Line" = percent_blw_pov
         )
z <- as.matrix(aid)
cor_plot <- pairs( z, panel=function(x,y){
  points(x,y)
  abline(lm(y~x), col='blue')
  text(0.8,0.9,labels = paste('R2=',round((cor(x,y)),2)) ,col='blue' )
})






# # Get rid of "other", "black", and "perc_blw_pov"
# 
# full_df <- full_df[, !names(full_df) %in% c(#"other", 
#                                             # "2_plus_othr",
#                                             # "black", 
#                                             # "percent_poc"
#                                             # "percent_blw_pov",
#                                             "perc_aff_hs_units"
#                                             )
#                    ]
# 
# variables <- names(full_df[3:11])
# 
# var_df <- full_df[3:11]
# 
# corr_df2 <- data.frame(matrix(ncol=5, nrow = 0))
# 
# colnames(corr_df2) <- c("variable", 
#                        "Pearson Coef", 
#                        "Pearson P.value",
#                        "Spearman Coef",
#                        "Spearman P.value"
# )
# 
# for (variable in variables) {
#   index_df <- var_df[,names(var_df) != variable]
#   index <- wtd.rowSums(index_df, wts=(1/8))
#   test_var <- var_df[,names(var_df) == variable]
#   
#   correlation_p <- cor.test(test_var, index, method = "pearson")
#   correlation_s <- cor.test(test_var, index, method = "spearman")
#   
#   data <- t(as.data.frame(c(variable,
#                             correlation_p$estimate,
#                             correlation_p$p.value,
#                             correlation_s$estimate,
#                             correlation_s$p.value
#   )
#   )
#   )
#   colnames(data) <- c("variable", 
#                       "Pearson Coef", 
#                       "Pearson P.value",
#                       "Spearman Coef",
#                       "Spearman P.value"
#   )
#   corr_df2 <- rbind(corr_df2, data)
# }
# 
# z <- as.matrix(full_df[3:10])
# cor_plot <- pairs( z, panel=function(x,y){
#   points(x,y)
#   abline(lm(y~x), col='blue')
#   text(0.8,0.9,labels = paste('R2=',round((cor(x,y)),2)) ,col='blue' )
# })
# 
# 
# # Break variables into categories
# 
# acs_vars <- c("percent_poc",
#               "percent_h_l",
#               "med_income",
#               "perc_public_assist",
#               "percent_blw_pov",
#               "percent_post_hs"
#               )
# 
# boulder_vars <- c("perc_hhs_aid",
#                  "perc_aff_hs_units",
#                  "perc_pr_aid"
#                  )
# 
# race_eth_vars <- c("percent_poc",
#                    "percent_h_l"
#                    )
# 
# econ_vars <- c("med_income",
#                "perc_public_assist",
#                "percent_blw_pov"
#                )
# 
# r_e_df <- full_df[,names(full_df) %in% race_eth_vars]
# r_e_combo <- wtd.rowSums(r_e_df, wts=(1))
# fplus_df <- cbind(full_df, r_e_combo)
# 
# # using negative weights forvars with inverse relationship with
# # median income
# # Or should I run rev() on those columns? Probably more appropriate...
# 
# econ_df <- full_df[,names(full_df) %in% econ_vars]
# 
# # Reverse these? Gives a worse correlation. 
# # econ_df$perc_public_assist <- rev(econ_df$perc_public_assist)
# # econ_df$perc_blw_pov <- rev(econ_df$perc_blw_pov)
# 
# econ_combo <- wtd.rowSums(econ_df, wts=c(1, -1, -1))
# fplus_df <- cbind(fplus_df, econ_combo)
# 
# # bld_df <- full_df[,names(full_df) %in% boulder_vars]
# # bld_combo <- wtd.rowSums(bld_df, wts=(1))
# # fplus_df <- cbind(fplus_df, bld_combo)
# 
# 
# z <- as.matrix(fplus_df[3:13])
# cor_plot <- pairs( z, panel=function(x,y){
#   points(x,y)
#   abline(lm(y~x), col='blue')
#   text(0.8,0.9,labels = paste('R2=',round((cor(x,y)),2)) ,col='blue' )
# })
# 
# 
# 
# corr_df2 <- data.frame(matrix(ncol=3, nrow = 0))
# colnames(corr_df2) <- c("variable", "Pearson Coef", "P.value")
# fplus_df <- fplus_df[,names(fplus_df) %in% c("percent_post_hs",
#                                              "perc_hhs_aid",
#                                              #"perc_aff_hs_units",
#                                              #"perc_pr_aid",
#                                              "r_e_combo",
#                                              "econ_combo"
#                                              )
#                      ]
# var_df2 <- fplus_df
# variables <- names(var_df2)
# for (variable in variables) {
#   index_df <- var_df2[,names(var_df2) != variable]
#   index <- wtd.rowSums(index_df, wts=(1/8))
# 
#   test_var <- var_df2[,names(var_df2) == variable]
#   
#   correlation <- cor.test(test_var, index, method = "pearson")
#   # print(test_var)
#   # print("----------------")
#   
#   data <- t(as.data.frame(c(variable,
#                             correlation$estimate,
#                             correlation$p.value)))
#   colnames(data) <- c("variable", "Pearson Coef", "P.value")
#   corr_df2 <- rbind(corr_df2, data)
# }
# 
# # Just the combined vars
# cor.test(fplus_df$r_e_combo, fplus_df$econ_combo, method = "pearson")
# 
# # Just the race/ethnicity combo and median income
# cor.test(fplus_df$r_e_combo, full_df$med_income, method = "pearson")
# 
# # Just the race/ethnicity combo and % below poverty
# cor.test(fplus_df$r_e_combo, full_df$percent_blw_pov, method = "pearson")
