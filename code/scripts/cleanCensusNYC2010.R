# File: cleanCensusNYC2010.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 2/1/22

#---------------------------#
#### C. TABLE OF CONTENTS####
#---------------------------#
# N. Notes
# 0. Package Imports
# 1. Read in shapefile
# 2. Read in census data
# 3. Extra stuff to sort out later...

#---------------------------#
####       N. NOTES      ####
#---------------------------#
# This script takes in the data from data/raw, cleans it, and then saves it to data/clean.
# After running this script, the data in data/clean should be used for subsequent analyses.

#---------------------------#
####  0. PACKAGE IMPORTS ####
#---------------------------#
source(here("code", "scripts", "requirements.R"))

#---------------------------#
####  1. READ SHAPEFILE  ####
#---------------------------#
# 1a. Read in shapefile
nyc.shp <- readOGR(dsn = here("data", "raw", "shapefilesNYC2010"))

# 1b. Convert shapefile to adjacency matrix & save
adj_mat <- make_adjacency_mat(nyc.shp, names = "BoroCT2010", save_as = here::here("data", "clean", "adjacencyMatrix.rds"))
writeMat(adj_mat = adj_mat, con = here("data", "clean", "adjacencyMatrix.mat"))

#---------------------------#
#### 2. READ CENSUS DATA ####
#---------------------------#
# 2a. Read in data. Then, rename variables in nyc.census to follow consistent patterns, 
# then rename boroughs to match nyc.shp county names
nyc.census <- read_csv(here("data", "raw", "censusNYC2010", "nyc_census_2010.csv")) %>%
  rename(CT2010 = tracta,
         s_mtw_pCarpool = s_mtw_perc_carpooled,
         s_mtw_pDroveAlone = s_mtw_perc_drove_alone,
         s_mtw_pPublicTransport = s_mtw_perc_public_transport,
         s_mtw_pBike = s_mtw_perc_bike,
         s_mtw_pWalk = s_mtw_perc_walk,
         s_ttw_weightedAvg = s_weighted_avg_ttw,
         s_edu_pNoHS = s_edu_perc_nohs,
         s_edu_pHS = s_edu_perc_hs,
         s_edu_pSomeCollege = s_edu_perc_some_college,
         s_edu_pCollege = s_edu_perc_college,
         s_edu_pGradSchool = s_edu_perc_gradschool,
         s_inc_medHousehold = s_median_household_income,
         s_inc_medFamily = s_median_family_income,
         s_inc_perCapita = s_percapita_income,
         s_tot_incPublicAssistanceHouseholds = s_tot_public_assistance_households,
         s_inc_pPublicAssistanceHouseholds = s_perc_public_assistance_income,
         s_occ_pAggriculture = s_occ_perc_aggriculture,
         s_occ_pConstructionManufacturing = s_occ_perc_construction_manufacturing,
         s_occ_pTrade = s_occ_perc_trade,
         s_occ_pTransportWarehousingUtils = s_occ_perc_transport_warehousing_utils,
         s_occ_pInfo = s_occ_perc_info,
         s_occ_pFinance = s_occ_perc_finance,
         s_occ_pProfessional = s_occ_perc_professional,
         s_occ_pEduHealth = s_occ_perc_edu_health,
         s_occ_pArtsRecAccomodationsFood = s_occ_perc_arts_rec_accomodations_food,
         s_occ_pPublicAdmin = s_occ_perc_public_admin,
         s_tot_emp = s_tot_employment_status,
         s_tot_fla = s_tot_foreign_lang,
         s_emp_pUnemployed = s_employment_status_perc_unemployed,
         s_fla_pSpoken = s_foreign_lang_perc_spoken,
         s_hou_medNumRooms = s_med_num_rooms,
         s_hou_medYrBuilt = s_med_yr_bult,
         s_hou_medRent = s_med_rent,
         s_hou_medVal = s_med_hous_val,
         s_tot_occupiedUnits = s_tot_occupied_units, # occupied housing units for htg
         s_tot_ownerOccupied = s_tot_owner_occupied, # total for % owner vehicles
         s_tot_renterOccupied = s_tot_renter_occupied, # total for % renter vehicles
         s_htg_pUtilGas = s_perc_util_gas,
         s_htg_pGas = s_perc_gas,
         s_htg_pElectric = s_perc_elect,
         s_htg_pOil = s_perc_oil_ker,
         s_htg_pCoal = s_perc_coal_coke,
         s_htg_pWood = s_perc_wood,
         s_htg_pSolar = s_perc_solar,
         s_htg_pOther = s_perc_other,
         s_htg_pNoFuel = s_perc_no_fuel,
         s_plg_pComp = s_perc_comp_plump,
         s_plg_pIncomp = s_perc_incom_plump,
         s_veh_pOwnerNoVehi = s_perc_owner_no_vehi,
         s_veh_pOwnerVehi = s_perc_owner_vehi,
         s_veh_pRenterNoVehi = s_perc_renter_no_vehi,
         s_veh_pRenterVehi = s_perc_renter_vehi,
         c_tot_hou = c_tot_housing_unt,
         c_hou_occupiedUnits = c_occup_unt,
         c_hou_vacantUnits = c_vacant_unt,
         c_tot_pop = c_population,
         c_sex_male = c_male_pop,
         c_sex_female = c_female_pop,
         c_rac_black = c_black_pop,
         c_rac_ai = c_ai_pop,
         c_rac_asian = c_asian_pop,
         c_rac_pacificIslander = c_pacific_islander_pop,
         c_rac_multiracial = c_multiracial_pop,
         c_rac_hispanic = c_hispanic_pop,
         c_rac_white = c_white_pop,
         c_sex_pMale = c_perc_male,
         c_sex_pFemale = c_perc_female,
         c_rac_pBlack = c_perc_black,
         c_rac_pAsian = c_perc_asian,
         c_rac_pPacificIslander = c_perc_pacific_islander,
         c_rac_pAI = c_perc_ai,
         c_rac_pMultiracial = c_perc_multiracial,
         c_rac_pHispanic = c_perc_hispanic,
         c_rac_pWhite = c_perc_white,
         c_age_medAge = c_median_age_tot,
         c_tot_hhs = c_total_households,
         c_hhs_meanHHSize = c_mean_household_size_tot,
         c_hhs_typeFam = c_hh_type_family,
         c_hhs_typeFamMarried = c_hh_type_family_husband_wife,
         c_hhs_typeFamOther = c_hh_type_family_other_family,
         c_hhs_typeFamSingleMale = c_hh_type_family_single_male,
         c_hhs_typeFamSingleFemale = c_hh_type_family_single_female,
         c_hhs_typeNonFam = c_hh_type_nonfamily,
         c_hhs_typeLivingAlone = c_living_alone,
         c_hhs_typeRoommates = c_roommates,
         c_hhs_withChildren = c_hh_with_children,
         c_hhs_famWithChildren = c_hh_family_with_children,
         c_hhs_gen3Plus = c_generations_3plus,
         c_hhs_genUnder3 = c_generations_under3,
         c_hhs_meanFamSize = c_mean_family_size_tot,
         c_tot_occupiedHousingUnits = c_total_occupied_housing_units,
         c_hou_ownerOccupied = c_owner_occupied_units,
         c_hou_renterOccupied = c_renter_occupied_units,
         c_hhs_pFam = c_perc_fam_hh,
         c_hhs_pNonFam = c_perc_nonfam_hh,
         c_hhs_pMarried = c_perc_married_hh,
         c_hhs_pSingleHouseholderFam = c_perc_single_householder_fam_hh,
         c_hhs_pLivingAlone = c_perc_living_alone,
         c_hhs_pRoommates = c_perc_roommates,
         c_hhs_pChild = c_perc_child_hh,
         c_hhs_pFamWithChild = c_perc_child_fam_hh,
         c_hou_pOwnerOccupied = c_perc_owner_occ,
         c_hou_pRenterOccupied = c_perc_renter_occ
  ) %>%
  mutate(BoroName = ifelse(county == "Bronx County", "Bronx", 
                           ifelse(county == "Kings County", "Brooklyn", 
                                  ifelse(county == "New York County", "Manhattan",
                                         ifelse(county == "Queens County", "Queens", "Staten Island")))),
         BoroCode = ifelse(BoroName == "Manhattan", "1", 
                           ifelse(BoroName == "Bronx", "2", 
                                  ifelse(BoroName == "Brooklyn", "3", 
                                         ifelse(BoroName == "Queens", "4", "5")))),
         BoroCT2010 = paste0(BoroCode, CT2010),
         c_hou_pVacantUnits = c_hou_vacantUnits / c_tot_hou * 100) %>%
  left_join(., y = nyc.shp@data) %>%
  select(BoroName, BoroCode, CT2010, BoroCT2010, NTAName, name, starts_with("s_"), starts_with("c_"))

# 2b. save census data to clean data folder:
write_csv(nyc.census, here("data", "clean", "nyc_census_2010.csv"))

# 2c. start of code to fix parks and airports having populations if needed later...
#nyc.shp@data %>% filter(startsWith(NTAName, "park-cemetery-etc"))

#------------------------------#
#### 3. EXTRA STUFF TO SORT ####
#------------------------------#
vars <- c(colnames(nyc.census)[startsWith(colnames(nyc.census), "c_")], colnames(nyc.census)[startsWith(colnames(nyc.census), "s_")])

# 5a. define colours for plots
colours <- c("#A5CEE3", # light blue - sex (2)
             "#1F78B4", # dark blue - race (7)
             "#33A02B", # dark green - households (11)
             "#B2DF89", # light green - housing (7)
             "#E72A8A", # bright pink - income
             "#E3211C", # red - heating
             "#CAB2D5", # light purple - plumbing
             "#FDBF6F", # light orange - vehicles
             "#FF7F00", # dark orange - mode transport
             "#6A3D9A", # dark purple - education
             "#B15928", # brown - occupation
             "#666666", # tan - language
             "#FB9A99", # pink - age
             "#000000") # gray - population

# 5b. grab variables of interest, scale them by column.
cd <- nyc.census %>% 
  select_at(vars(matches("^(.+)_(.+)_p(.+)")))
  
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

# 5c. define column groupings.
colg <- data.frame(chem = colnames(cd),
                   group = c(rep("sex", 2),
                             rep("race", 7),
                             rep("households", 8),
                             rep("housing", 3),
                             rep("income", 2),
                             rep("heating", 9),
                             rep("plumbing", 2),
                             rep("vehicles", 4),
                             rep("mode transport", 5),
                             rep("education", 5),
                             rep("occupation", 10),
                             rep("housing", 4),
                             rep("occupation", 1),
                             rep("language", 1),
                             rep("income", 2),
                             rep("mode transport", 1),
                             rep("age", 1),
                             rep("households", 3),
                             rep("population", 1)
                   )
)






