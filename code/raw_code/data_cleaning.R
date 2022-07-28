setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(cluster)
library(ggplot2)
library(tidycensus)
library(tidyverse)
library(stringr)
library(openxlsx)
library(mice)
library(VIM)
library(corrplot)
library(tigris)

#############################
# 1. Pull and format ACS Data
#############################

# Need to refine this list based on guidance from SG on 7/13.
acs5_var <- load_variables(2020, "acs5")
acs_input_variables <- c("B19083_001",
                         "B25064_001",
                         "B25076_001",
                         "B25077_001",
                         "B25078_001",
                         "B01001_003",
                         "B01001_004",
                         "B01001_005",
                         "B01001_006",
                         "B01001_007",
                         "B01001_008",
                         "B01001_009",
                         "B01001_010",
                         "B01001_011",
                         "B01001_012",
                         "B01001_013",
                         "B01001_014",
                         "B01001_015",
                         "B01001_016",
                         "B01001_017",
                         "B01001_018",
                         "B01001_019",
                         "B01001_020",
                         "B01001_021",
                         "B01001_022",
                         "B01001_023",
                         "B01001_024",
                         "B01001_025",
                         "B01001_026",
                         "B01001_027",
                         "B01001_028",
                         "B01001_029",
                         "B01001_030",
                         "B01001_031",
                         "B01001_032",
                         "B01001_033",
                         "B01001_034",
                         "B01001_035",
                         "B01001_036",
                         "B01001_037",
                         "B01001_038",
                         "B01001_039",
                         "B01001_040",
                         "B01001_041",
                         "B01001_042",
                         "B01001_043",
                         "B01001_044",
                         "B01001_045",
                         "B01001_046",
                         "B01001_047",
                         "B01001_048",
                         "B01001_049",
                         "B02001_002",
                         "B02001_003",
                         "B02001_004",
                         "B02001_005",
                         "B03001_003",
                         "B05001_006",
                         "B07009_002",
                         "B07009_003",
                         "B07009_004",
                         "B07009_005",
                         "B07009_006",
                         "B07204_004",
                         "B07204_007",
                         "B08014_002",
                         "B08301_010",
                         "B08303_003",
                         "B08303_004",
                         "B08303_005",
                         "B08303_006",
                         "B08303_007",
                         "B08303_008",
                         "B08303_009",
                         "B08303_010",
                         "B08303_011",
                         "B08303_012",
                         "B08303_013",
                         "B09005_005",
                         "B11001_003",
                         "B11009_003",
                         "B11009_005",
                         "B16001_002",
                         "B16001_003",
                         "B16001_005",
                         "B19001_002",
                         "B19001_003",
                         "B19001_004",
                         "B19001_005",
                         "B19001_006",
                         "B19001_007",
                         "B19001_008",
                         "B19001_009",
                         "B19001_010",
                         "B19001_011",
                         "B19001_012",
                         "B19001_013",
                         "B19001_014",
                         "B19001_015",
                         "B19001_016",
                         "B19001_017",
                         "B19058_002",
                         "B19059_002",
                         "B25002_003",
                         "B25003_003",
                         "B25024_002",
                         "B25024_003",
                         "B25024_004",
                         "B25024_005",
                         "B25024_006",
                         "B25024_007",
                         "B25024_008",
                         "B25024_009",
                         "B25024_010",
                         "B25034_002",
                         "B25034_003",
                         "B25034_010",
                         "C24050_002",
                         "C24050_003",
                         "C24050_004",
                         "C24050_005",
                         "C24050_006",
                         "C24050_007",
                         "C24050_008",
                         "C24050_009",
                         "C24050_010",
                         "C24050_011",
                         "C24050_012",
                         "C24050_013",
                         "C24050_014",
                         "C24050_015",
                         "C24050_029",
                         "C24050_043",
                         "C24050_057",
                         "C24050_071",
                         "B01003_001",
                         "B26001_001"
                          )

selected_vars <- subset(acs5_var, name %in% acs_input_variables)
# Clean up special characters
selected_vars$label <- str_replace_all(selected_vars$label, "[^[:alnum:]]", "")


co_tracts <- get_acs(
  geography = "tract",#"block group",
  variables = acs_input_variables, #all_variables,
  state = "CO", 
  year = 2020,
  output = 'wide'
)

# Remove MOE columns
co_tracts <- co_tracts[, -grep("M$", colnames(co_tracts))]

# co_tracts %>% summarise_all(~ sum(is.na(.)))


# Find small tracts. Looks like there are 11 tracts with less than 100 people.
# Remove.

subset(co_tracts, B01003_001E <= 100)
co_tracts = subset(co_tracts, B01003_001E > 100)

# Transform column names
colnames(co_tracts)[3:139] <- gsub(
                                  "E", 
                                  "", 
                                  as.character(colnames(co_tracts)[3:139])
                                  )
# This needs to be revisited. Leaves some columns named "EstimateTotal" with 
# no indication of what the estimate is.

# names(co_tracts)[
#   match(selected_vars$name, names(co_tracts))
#   ] <- selected_vars$label

# Drop any columns/variables that are blank for all tracts

co_tracts <- co_tracts[, colSums(is.na(co_tracts)) < nrow(co_tracts)]

####################
## 2. Impute values?
####################

# Perform predictive mean matching with 5 imputations.
# Reference: https://statisticsglobe.com/missing-data-imputation-statistics/
# https://statisticsglobe.com/predictive-mean-matching-imputation-method/

na_pattern <- md.pattern(co_tracts)
# Missing data: B19083_001, B25077_001, B25078_001, B25076_001, B25064_001
# Gini index of inequality, Median value dollars, Upper value quartile (dollars), 
# Lower value quartile (dollars), median gross rent

incomplete_var_list = list('B19083_001', 
                           'B25077_001', 
                           'B25078_001', 
                           'B25076_001', 
                           'B25064_001'
                           )

aggr_plot <- aggr(co_tracts, 
                  col=c('navyblue','red'), 
                  numbers=TRUE, 
                  sortVars=TRUE, 
                  labels=names(co_tracts), 
                  cex.axis=.7, 
                  gap=3, 
                  ylab=c("Histogram of missing data","Pattern")
                  )


imp <- mice(co_tracts, meth= 'pmm', m=10, seed=700)
imputation_summary <-  summary(imp)
densityplot(imp)

co_tracts_imputed <- complete(imp)

co_tracts_imputed %>% summarise_all(~ sum(is.na(.)))


#########################################
## 3. Split complete and incomplete cases
#########################################

# Extract 
# There are no block groups or tracts with complete cases... may need 
# flipCluster package.

# Try with imputed values
co_cc <- co_tracts[complete.cases(co_tracts), ]

# INCOMPELTE CASES
co_ic <- co_tracts[!complete.cases(co_tracts), ]


#########################################
## 4. Multicollinearity Check
#########################################

# d.fit <-  numeric()
# 
# for (i in names(co_tracts[,3:134])) {
#   print(i)
#   d <-  lm(as.matrix(co_tracts[, i]) ~ .,
#            data=co_tracts[, -c(which(x=names(co_tracts) == i, arr.ind=TRUE))]
#            )
#   d.fit <-  append(d.fit, summary(d)$adj.r.squared)
# }
# d.fit <-  as.numeric(d.fit)
# 
# r2_df <-  data.frame(var = names(co_tracts[,3:134]), rsq=d.fit)

# corrplot(cor(co_tracts[,3:134]), method="number")

#########################################
## 5. Cluster analysis
#########################################

set.seed(7777)

# Scale data
# range01 <-  function(x){(x-min(x))/max(x)-min(x)}
# 
# scaled_cc <- apply(co_tracts[complete.cases(co_tracts), 5:134 ], MARGIN=2, FUN=range01)
# scaled_ic <- apply(co_tracts[!complete.cases(co_tracts), 5:134 ], MARGIN=2, FUN=range01)


# Change to range standardization between zero and 1.
standardized_cc <- scale(co_cc[,4:134], center = TRUE, scale = TRUE)
standardized_ic <- scale(co_ic[,4:134], center = TRUE, scale = TRUE)

clusters <-  list()

fit <- NA
numberOfRuns <- 100
maxClusteringIterations <- 100

for (i in 1:numberOfRuns){
  print(paste("starting run", i, "of", numberOfRuns, sep=" "))
  class.250 <- kmeans(x=standardized_cc, centers=250, iter.max=maxClusteringIterations, nstart=1)
  fit[i] <- class.250$tot.withinss
  if (fit[i] < min(fit[1:(i-1)])){
    clusters <- class.250}
}
final <- clusters

#distance matrix for cluster centroids.
diss.ctr <- dist(final$centers)

# The Spielman paper did not standardize the data for the incomplete cases, 
# Seems like an error.
# Added the [1] index bc the output of dist appears to be a matrix, but dist.k 
# is looking for a single value?
# dist.k <- data.frame()
# for (row in 1:dim(standardized_ic)[1]){
#   for (k in 1:250){
#     dist.k[row, k] <- dist(rbind(final$centers[k,], standardized_ic[row,])) 
#   }
# }

# my code
dist.k <- data.frame()
for (row in 1:dim(standardized_ic)[1]){
   for (k in 1:250){
    dist_rbind <- dist(rbind(final$centers[k,], standardized_ic[row,]))[1]
    dist.k[row, k] <- dist_rbind
    }
}

# Cluster membership for incomplete cases
# Select column index for each observation (index is also cluster ID)

# Getting this warning:  "In x[[jj]][iseq] <- vjj :
# number of items to replace is not a multiple of replacement length"

mins <-  data.frame(cl=NA)
for (row in 1:dim(dist.k)[1]){
  mins[row,] <- ifelse(test=is.na(dist.k[row,]), yes=NA, no=which.min(dist.k[row,]))
}

# Build a final data frame

co_cc$cluster <- final$cluster
co_ic$cluster <- mins$cl
co_cl <- rbind(co_cc, co_ic)

# Ward's method, calculating silhouette

wards.ctr <- hclust(diss.ctr, method="ward")

sil <- NA
for (i in 2:250){
  sil[i-1] <- summary(silhouette(cutree(wards.ctr, k=i), diss.ctr))$avg.width
}
sil <-  data.frame(sil=sil, k=2:249)