# File: mapCensusNYC2010.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 01/24/22

#---------------------------#
#### C. TABLE OF CONTENTS####
#---------------------------#
# 0. Package Imports
# 1. Read in shapefile
# 2. Read in census data
# 3. Plot maps
# 4. Plot joint maps
# 5. Census data PCA

#---------------------------#
####  0. PACKAGE IMPORTS ####
#---------------------------#
library(tidyverse)
library(rgdal)
library(tmap)
library(sp)
library(PCPhelpers) # for EDA function
library(ggfortify) # for EDA
source(here::here("code", "functions", "make_map.R")) # map maker function
source(here::here("code", "functions", "make_adjacency_mat.R")) # adjacency mat function

#---------------------------#
####  1. READ SHAPEFILE  ####
#---------------------------#
# 1a. Read in shapefile
nyc.shp <- readOGR(dsn = here::here("data", "shapefilesNYC2010"))

# 1b. Convert shapefile to adjacency matrix & save
adj_mat <- make_adjacency_mat(nyc.shp, names = "BoroCT2010")
saveRDS(adj_mat, file = here::here("data", "shapefilesNYC2010", "adjacencyMatrix.rds")) # can be read back in with readRDS

#---------------------------#
#### 2. READ CENSUS DATA ####
#---------------------------#
nyc.census <- read_csv(here::here("data", "censusNYC2010", "nyc_census_2010.csv"))

# 2a. Rename to match nyc.shp file names
nyc.census <- nyc.census %>%
  mutate(BoroName = ifelse(county == "Bronx County", "Bronx", 
                           ifelse(county == "Kings County", "Brooklyn", 
                                  ifelse(county == "New York County", "Manhattan",
                                         ifelse(county == "Queens County", "Queens", "Staten Island")))))

# 2b. join data by BoroName and census tract code...
nyc.shp@data <- left_join(x = nyc.shp@data, y = nyc.census, by = c("CT2010" = "tracta", "BoroName"))
nyc.shp@data <- nyc.shp@data %>% 
  mutate(tractno = gsub("[^0-9.-]", "", name))

# 2c. sanity check that we have indeed matched up the right tracts in that join (should = 1)
sum(nyc.shp@data$tractno == nyc.shp@data$CTLabel) / nrow(nyc.shp@data)

# 2d. start of code to fix parks and airports having populations if needed...
# nyc.shp@data %>% filter(startsWith(NTAName, "park-cemetery-etc"))

#---------------------------#
####    3. PLOT MAPS     ####
#---------------------------#
tmap_options(
  show.warnings = FALSE,
  show.messages = FALSE
)

vars <- c(colnames(nyc.census)[startsWith(colnames(nyc.census), "c_")], colnames(nyc.census)[startsWith(colnames(nyc.census), "s_")])

# to save the maps as png files
#vars %>% walk(~make_map(., data.shp = nyc.shp, dir = here::here("figures", "singleMaps")))

# to make a large list of maps:
#singleMaps <- vars %>% map(~make_map(., data.shp = nyc.shp))

#---------------------------#
#### 4. PLOT JOINT MAPS  ####
#---------------------------#
destination <- here::here("figures", "jointMaps")

make_map(c("c_perc_black", "c_perc_ai", "c_perc_asian", 
           "c_perc_pacific_islander", "c_perc_multiracial",
           "c_perc_hispanic", "c_perc_white"),
         data.shp = nyc.shp,
         dir = destination,
         filename = "race",
         tm_w = 10, tm_h = 6)

make_map(c("c_perc_male", "c_perc_female"),
         data.shp = nyc.shp,
         dir = destination,
         filename = "sex",
         cols = 2)

make_map(c("c_perc_nonfam_hh", "c_perc_married_hh", 
           "c_perc_single_householder_fam_hh", 
           "c_perc_living_alone", "c_perc_roommates", "c_perc_fam_hh",
           "c_perc_child_hh", "c_perc_child_fam_hh"),
         data.shp = nyc.shp,
         dir = destination,
         filename = "households",
         cols = 4)

make_map(c("c_perc_owner_occ", "c_perc_renter_occ"),
         data.shp = nyc.shp,
         dir = destination,
         filename = "housing",
         cols = 2)

make_map(vars[startsWith(vars, "s_mtw_perc")],
         data.shp = nyc.shp,
         dir = destination,
         filename = "mode_transport_to_work",
         cols = 3)

make_map(vars[startsWith(vars, "s_edu_perc")],
         data.shp = nyc.shp,
         dir = destination,
         filename = "education",
         cols = 3)

make_map(vars[startsWith(vars, "s_occ_perc")],
         data.shp = nyc.shp,
         dir = destination,
         filename = "occupation",
         cols = 5)

make_map(c("s_perc_util_gas", "s_perc_gas", "s_perc_elect",
           "s_perc_oil_ker", "s_perc_coal_coke", "s_perc_wood",
           "s_perc_solar", "s_perc_other", "s_perc_no_fuel"),
         data.shp = nyc.shp,
         dir = destination,
         filename = "heating",
         cols = 3)

make_map(c("s_perc_owner_no_vehi", "s_perc_owner_vehi", 
           "s_perc_renter_no_vehi", "s_perc_renter_vehi"),
         data.shp = nyc.shp,
         dir = destination,
         filename = "vehicles",
         cols = 2)

#---------------------------#
#### 5. CENSUS DATA PCA  ####
#---------------------------#
# 5a. grab variables of interest, scale them by column.
cd <- nyc.census %>% 
  mutate(c_perc_vacant_units = c_vacant_unt / c_tot_housing_unt * 100) %>%
  select(starts_with("c_perc"), 
         starts_with("s_perc"), 
         starts_with("s_mtw_perc"),
         starts_with("s_edu_perc"),
         starts_with("s_occ_perc"),
         starts_with("s_med_"),
         "s_employment_status_perc_unemployed",
         "s_foreign_lang_perc_spoken",
         "s_median_household_income",
         "s_median_family_income",
         "s_percapita_income",
         "s_weighted_avg_ttw",
         "c_median_age_tot",
         "c_mean_household_size_tot",
         "c_mean_family_size_tot",
         "c_generations_3plus",
         "c_population") %>%
  apply(., MARGIN = 2, FUN = scale, center = T)

# 5b. define column groupings.
colg <- data.frame(chem = colnames(cd),
                   group = c(rep("sex", 2),
                             rep("race", 7),
                             rep("households", 8),
                             rep("housing", 2),
                             "other",
                             rep("income", 2),
                             rep("heating", 9),
                             rep("other", 6),
                             rep("mode transport", 5),
                             rep("education", 5),
                             rep("occupation", 10),
                             rep("other", 6),
                             rep("income", 2),
                             rep("other", 6)
                           )
        )

# 5c. exploratory data analysis..!
cd.eda <- eda(mat = cd, 
              pcs = paste0("PC", 1:4),
              colgroups = colg,
              rowgroups = nyc.census$BoroName,
              rowgroups_name = "Borough",
              scale_flag = F)
