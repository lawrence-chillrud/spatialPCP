# File: mapCensusNYC2010.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 01/24/22

#---------------------------#
#### C. TABLE OF CONTENTS####
#---------------------------#
# 0. Package Imports
# 1. Read in shapefile
# 2. Read in census data
# 3. Plot single maps
# 4. Plot joint maps
# 5. Census data EDA
# 6. Census data PCA

#---------------------------#
####  0. PACKAGE IMPORTS ####
#---------------------------#
source(here::here("code", "scripts", "requirements.R"))

#---------------------------#
####  1. READ SHAPEFILE  ####
#---------------------------#
# 1a. Read in shapefile
nyc.shp <- readOGR(dsn = here::here("data", "shapefilesNYC2010"))

# 1b. Convert shapefile to adjacency matrix & save
adj_mat <- make_adjacency_mat(nyc.shp, names = "BoroCT2010", save_as = here::here("data", "shapefilesNYC2010", "adjacencyMatrix.rds"))

#---------------------------#
#### 2. READ CENSUS DATA ####
#---------------------------#
# 2a. Read in data
nyc.census <- read_csv(here::here("data", "censusNYC2010", "nyc_census_2010.csv"))

# 2b. Rename counties in nyc.census to match nyc.shp county names
nyc.census <- nyc.census %>%
  mutate(BoroName = ifelse(county == "Bronx County", "Bronx", 
                           ifelse(county == "Kings County", "Brooklyn", 
                                  ifelse(county == "New York County", "Manhattan",
                                         ifelse(county == "Queens County", "Queens", "Staten Island")))))

# 2c. join data by BoroName and census tract code (= 'CT2010' in shapefile, 'tracta' in census data)...
nyc.shp@data <- left_join(x = nyc.shp@data, y = nyc.census, by = c("CT2010" = "tracta", "BoroName"))

# 2d. sanity check that we have indeed matched up the right tracts in that join (should = 1)
nyc.shp@data <- nyc.shp@data %>% 
  mutate(tractno = gsub("[^0-9.-]", "", name))
sum(nyc.shp@data$tractno == nyc.shp@data$CTLabel) / nrow(nyc.shp@data)

# 2e. Finish making a variable:
nyc.shp@data <- nyc.shp@data %>% 
  mutate(c_perc_vacant_units = c_vacant_unt / c_tot_housing_unt * 100)

# 2f. start of code to fix parks and airports having populations if needed later...
#nyc.shp@data %>% filter(startsWith(NTAName, "park-cemetery-etc"))

#---------------------------#
#### 3. PLOT SINGLE MAPS ####
#---------------------------#
# 3a. set up tmap options
tmap_options(
  show.warnings = FALSE,
  show.messages = FALSE
)

# 3b. create list of variables we are interested in for easy indexing later.
vars <- c(colnames(nyc.census)[startsWith(colnames(nyc.census), "c_")], colnames(nyc.census)[startsWith(colnames(nyc.census), "s_")])

# 3c. to save the maps as png files:
#vars %>% walk(~make_map(vars = ., data.shp = nyc.shp, dir = here::here("figures", "singleMaps"), tm_dpi = 100))

# 3d. to make a large list of maps:
#singleMaps <- vars %>% map(~make_map(vars = ., data.shp = nyc.shp))

#---------------------------#
#### 4. PLOT JOINT MAPS  ####
#---------------------------#
# 4a. set destination directory for the joint maps.
destination <- here::here("figures", "jointMaps")

# 4b. make 9 different joint maps...
make_map(data.shp = nyc.shp,
         vars = c("c_perc_black", "c_perc_ai", "c_perc_asian", 
                  "c_perc_pacific_islander", "c_perc_multiracial",
                  "c_perc_hispanic", "c_perc_white"),
         dir = destination,
         filename = "race",
         tm_w = 10, tm_h = 6)

make_map(data.shp = nyc.shp,
         vars = c("c_perc_male", "c_perc_female"),
         dir = destination,
         filename = "sex",
         cols = 2)

make_map(data.shp = nyc.shp,
         vars = c("c_perc_nonfam_hh", "c_perc_married_hh", 
                  "c_perc_single_householder_fam_hh", 
                  "c_perc_living_alone", "c_perc_roommates", "c_perc_fam_hh",
                  "c_perc_child_hh", "c_perc_child_fam_hh"),
         dir = destination,
         filename = "households",
         cols = 4)

make_map(data.shp = nyc.shp,
         vars = c("c_perc_owner_occ", "c_perc_renter_occ"),
         dir = destination,
         filename = "housing",
         cols = 2)

make_map(data.shp = nyc.shp,
         vars = vars[startsWith(vars, "s_mtw_perc")],
         dir = destination,
         filename = "mode_transport_to_work",
         cols = 3)

make_map(data.shp = nyc.shp,
         vars = vars[startsWith(vars, "s_edu_perc")],
         dir = destination,
         filename = "education",
         cols = 3)

make_map(data.shp = nyc.shp,
         vars = vars[startsWith(vars, "s_occ_perc")],
         dir = destination,
         filename = "occupation",
         cols = 5)

make_map(data.shp = nyc.shp,
         vars = c("s_perc_util_gas", "s_perc_gas", "s_perc_elect",
                  "s_perc_oil_ker", "s_perc_coal_coke", "s_perc_wood",
                  "s_perc_solar", "s_perc_other", "s_perc_no_fuel"),
         dir = destination,
         filename = "heating",
         cols = 3)

make_map(data.shp = nyc.shp,
         vars = c("s_perc_owner_no_vehi", "s_perc_owner_vehi", 
                  "s_perc_renter_no_vehi", "s_perc_renter_vehi"),
         dir = destination,
         filename = "vehicles",
         cols = 2)

#---------------------------#
#### 5. CENSUS DATA EDA  ####
#---------------------------#
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

# 5d. exploratory data analysis..!
cd.eda <- eda(mat = cd, 
              pcs = paste0("PC", 1:4),
              colgroups = colg,
              rowgroups = nyc.census$BoroName,
              rowgroups_name = "Borough",
              scale_flag = F)

# 5e. just looking at pc1:
pc1 <- eda(mat = cd, 
           pcs = paste0("PC", 1),
           colgroups = colg,
           rowgroups = nyc.census$BoroName,
           rowgroups_name = "Borough",
           scale_flag = F)

pc1_loadings <- pc1$load$data %>%
  arrange(group, desc(Loading))
  
pc1_loadings$chem <- factor(pc1_loadings$chem, levels = pc1_loadings$chem)
pc1_loadings %>%
  ggplot(aes(x = chem, y = Loading, color = group)) +
  geom_point() + 
  geom_segment(aes(yend = 0, xend = chem)) +
  scale_colour_manual(values = colours[order(unique(colg$group))]) +
  theme_bw() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1), strip.background = element_rect(fill = "white"), 
        axis.title.x = element_blank(), axis.title.y = element_blank())

# 5f. table showing groupings
cd.eda$load$data %>% 
  group_by(chem) %>% 
  slice_head() %>% 
  ungroup() %>% 
  count(group) %>% 
  arrange(desc(n)) %>%
  mutate(group = cell_spec(group, "html", color = "white", background = factor(group, unique(colg$group), colours))) %>%
  rbind(data.frame(group = c("total"), n = c(sum(.$n)))) %>%
  kbl("html", escape = F) %>%
  kable_classic() %>%
  kable_styling(full_width = F, bootstrap_options = c("striped"), html_font = "monaco") %>%
  row_spec(., row = 15, color = "white", background = "black") 

# 5g. table showing breakdown of PCs
cd.eda$var_raw %>%
  kbl(col.names = c("PC", "Eigenvalue", "% Variance", "% Cumulative Variance")) %>%
  kable_classic() %>%
  kable_styling(full_width = F, bootstrap_options = c("striped"), html_font = "monaco")

# 5h. finding out the tracts that are missing from shapefile (all water)
setdiff(paste0(nyc.census$tracta, nyc.census$BoroName), paste0(nyc.shp@data$CT2010, nyc.shp@data$BoroName))
nyc.census[nyc.census$BoroName == "Brooklyn" & nyc.census$tracta == "990100",]$name # Lower NY Bay
nyc.census[nyc.census$BoroName == "Queens" & nyc.census$tracta == "990100",]$name # Atlantic Ocean
nyc.census[nyc.census$BoroName == "Staten Island" & nyc.census$tracta == "008900",]$name # NY Bay

#---------------------------#
#### 6. CENSUS DATA PCA  ####
#---------------------------#
# 6a. create a copy of the census data with NAs, and one without
cd.na <- cd
cd[is.na(cd)] <- 0 # impute missing values with 0

# 6b. PCA on census data matrix (cd) WITH the missing values
pca.nafull <- pca_jg(cd.na, rank = ncol(cd))
pca.nafirst <- pca_jg(cd.na, rank = 1)

# 6c. linear model formula: dependent (response) ~ independent (terms)
lm(pca.nafirst$scores[,1] ~ pca.nafull$scores[,1]) %>% summary()

# 6d. PCA on census data matrix (cd) without missings
pca <- prcomp(cd)
lm(pca.nafirst$scores[,1] ~ pca$x[,1]) %>% summary()

# 6e. joining PCA scores with shapefile:
nyc.shp@data <- left_join(x = nyc.shp@data, 
                          y = cbind(nyc.census, pca$x[, 1:3]))

# 6f. make map of PCs 1-3
pcs_mapped <- make_map(data.shp = nyc.shp,
                       vars = paste0("PC", 1:3),
                       dir = destination,
                       filename = "PCS1-3",
                       tm_w = 10, tm_h = 6, tm_dpi = 1000)

# 6g. Make individual maps
make_map(data.shp = nyc.shp, vars = c("PC1"), dir = destination, filename = "PC1", tm_dpi = 500)
make_map(data.shp = nyc.shp, vars = c("PC2"), dir = destination, filename = "PC2", tm_dpi = 500)
make_map(data.shp = nyc.shp, vars = c("PC3"), dir = destination, filename = "PC3", tm_dpi = 500)

# 6h. top 4 loadings map
cd.eda$load$data %>%
  filter(PC == "PC3") %>%
  arrange(desc(Loading)) %>%
  slice_head(n = 4) %>%
  select(chem) %>%
  mutate(chem = as.character(chem)) %>%
  unlist() %>% 
  make_map(data.shp = nyc.shp, vars = ., cols = 2, dir = destination, filename = "PC3-top4", tm_dpi = 500)

# 6i. bottom 4 loadings map
cd.eda$load$data %>%
  filter(PC == "PC3") %>%
  arrange(Loading) %>%
  slice_head(n = 4) %>%
  select(chem) %>%
  mutate(chem = as.character(chem)) %>%
  unlist() %>% 
  make_map(data.shp = nyc.shp, vars = ., cols = 2, dir = destination, filename = "PC3-bot4", tm_dpi = 500)

# 6j. summary tables of PCs:
make_summary_table <- function(df = cd.eda$load$data, pc = "PC1", top = T, n = 15) {
  
  if (top) { rows <- 1:n } else { rows <- (n+1):(2*n) }
  
  tab <- df %>%
    arrange(desc(Loading)) %>%
    filter(PC == pc) %>%
    slice(c(1:n, nrow(.):(nrow(.) - n + 1))) %>%
    select(-PC) %>%
    mutate('#' = rep(1:15, 2)) %>%
    select('#', chem, Loading, group)
  
  tab %>%
    slice(rows) %>%
    mutate(group = cell_spec(group, "html", color = "white", background = factor(group, unique(colg$group), colours))) %>%
    kbl(format = "html", escape = F) %>%
    kable_classic() %>%
    column_spec(3, color = ifelse(top, "black", "white"), background = spec_color(tab$Loading, option = "E", direction = ifelse(top, 1, -1))) %>%
    kable_styling(full_width = F, bootstrap_options = c("striped"), html_font = "monaco")
  
}

tops <- paste0("PC", 1:3) %>% map(~make_summary_table(pc = ., top = T))
bots <- paste0("PC", 1:3) %>% map(~make_summary_table(pc = ., top = F))


