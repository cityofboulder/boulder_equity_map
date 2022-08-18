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

na_count  <- sapply(full_df, function(y) sum(length(which(is.na(y)))))
na_count  <-  data.frame(na_count)



z <- as.matrix(full_df[3:18])
cor_plot <- pairs( z, panel=function(x,y){
  points(x,y)
  abline(lm(y~x), col='blue')
  text(0.8,0.9,labels = paste('R2=',round((cor(x,y)),2)) ,col='blue' )
})



#########################################
####### 3. Variable Selection ###########
#########################################

# Limit to 9 variables
drop_vars <-  c("white",
                "black",
                "ai_ak",
                "asian",
                "nh_pi",
                "other",
                "2_plus_othr")

full_df <- full_df[,!names(full_df) %in% drop_vars]

z <- as.matrix(full_df[3:11])
cor_plot <- pairs( z, panel=function(x,y){
  points(x,y)
  abline(lm(y~x), col='blue')
  text(0.8,0.9,labels = paste('R2=',round((cor(x,y)),2)) ,col='blue' )
})

variables <- names(full_df[3:11])
var_df <- full_df[3:11]

# Function to calculate weighted sums across columns of
# dataframe or matrix x. Can receive a uniform weight or
# a list of weights to weight each column differently
# https://rdrr.io/github/ejanalysis/analyze.stuff/src/R/wtd.rowSums.R

wtd.rowSums <- function(x, wts=1, na.rm=TRUE) {
  rowSums(t(t(x) * wts), na.rm=na.rm)
}

# Want to loop through each variable and compare it
# to a column of weighted sums from a matrix/dataframe that
# excludes the variable to calculate Pearson correlation.
# Need to append relevant statistics into a dataframe with a 
# row for each variable.

# Should consider Spearman's rank correlation as alternative, due to outliers in
# distributions.

corr_df <- data.frame(matrix(ncol=3, nrow = 0))
colnames(corr_df) <- c("variable", "Pearson Coef", "P.value")

for (variable in variables) {
  index_df <- var_df[,names(var_df) != variable]
  index <- wtd.rowSums(index_df, wts=(1/8))
  print(index)
  test_var <- var_df[,names(var_df) == variable]
  
  correlation <- cor.test(test_var, index, method = "pearson")
  # print(test_var)
  # print("----------------")
  
  data <- t(as.data.frame(c(variable,
                          correlation$estimate,
                          correlation$p.value)))
  colnames(data) <- c("variable", "Pearson Coef", "P.value")
  corr_df <- rbind(corr_df, data)
}

# Try with Spearman

# sp_corr_df <- data.frame(matrix(ncol=3, nrow = 0))
# colnames(sp_corr_df) <- c("variable", "Spearman Coef", "P.value")
# 
# for (variable in variables) {
#   index_df <- var_df[,names(var_df) != variable]
#   index <- wtd.rowSums(index_df, wts=(1/8))
#   print(index)
#   test_var <- var_df[,names(var_df) == variable]
#   
#   correlation <- cor.test(test_var, index, method = "spearman")
#   # print(test_var)
#   # print("----------------")
#   
#   data <- t(as.data.frame(c(variable,
#                             correlation$estimate,
#                             correlation$p.value)))
#   colnames(data) <- c("variable", "Pearson Coef", "P.value")
#   sp_corr_df <- rbind(sp_corr_df, data)
# }


# Break variables into categories

acs_vars <- c("percent_poc",
              "percent_h_l",
              "med_income",
              "perc_public_assist",
              "percent_blw_pov",
              "percent_post_hs"
              )

boulder_vars <- c("perc_hhs_aid",
                 "perc_aff_hs_units",
                 "perc_pr_aid"
                 )

race_eth_vars <- c("percent_poc",
                   "percent_h_l"
                   )

econ_vars <- c("med_income",
               "perc_public_assist",
               "percent_blw_pov"
               )

r_e_df <- full_df[,names(full_df) %in% race_eth_vars]
r_e_combo <- wtd.rowSums(r_e_df, wts=(1))
fplus_df <- cbind(full_df, r_e_combo)

# using negative weights forvars with inverse relationship with
# median income
# Or should I run rev() on those columns? Probably more appropriate...

econ_df <- full_df[,names(full_df) %in% econ_vars]

# Reverse these? Gives a worse correlation. 
# econ_df$perc_public_assist <- rev(econ_df$perc_public_assist)
# econ_df$perc_blw_pov <- rev(econ_df$perc_blw_pov)

econ_combo <- wtd.rowSums(econ_df, wts=c(1, -1, -1))
fplus_df <- cbind(fplus_df, econ_combo)

# bld_df <- full_df[,names(full_df) %in% boulder_vars]
# bld_combo <- wtd.rowSums(bld_df, wts=(1))
# fplus_df <- cbind(fplus_df, bld_combo)


z <- as.matrix(fplus_df[3:13])
cor_plot <- pairs( z, panel=function(x,y){
  points(x,y)
  abline(lm(y~x), col='blue')
  text(0.8,0.9,labels = paste('R2=',round((cor(x,y)),2)) ,col='blue' )
})



corr_df2 <- data.frame(matrix(ncol=3, nrow = 0))
colnames(corr_df2) <- c("variable", "Pearson Coef", "P.value")
fplus_df <- fplus_df[,names(fplus_df) %in% c("percent_post_hs",
                                             "perc_hhs_aid",
                                             "perc_aff_hs_units",
                                             "perc_pr_aid",
                                             "r_e_combo",
                                             "econ_combo")]
var_df2 <- fplus_df
variables <- names(var_df2)
for (variable in variables) {
  index_df <- var_df2[,names(var_df2) != variable]
  index <- wtd.rowSums(index_df, wts=(1/8))

  test_var <- var_df2[,names(var_df2) == variable]
  
  correlation <- cor.test(test_var, index, method = "pearson")
  # print(test_var)
  # print("----------------")
  
  data <- t(as.data.frame(c(variable,
                            correlation$estimate,
                            correlation$p.value)))
  colnames(data) <- c("variable", "Pearson Coef", "P.value")
  corr_df2 <- rbind(corr_df2, data)
}

# Just the combined vars
cor.test(fplus_df$r_e_combo, fplus_df$econ_combo, method = "pearson")

# Just the race/ethnicity combo and median income
cor.test(fplus_df$r_e_combo, full_df$med_income, method = "pearson")

# Just the race/ethnicity combo and % below poverty
cor.test(fplus_df$r_e_combo, full_df$percent_blw_pov, method = "pearson")
