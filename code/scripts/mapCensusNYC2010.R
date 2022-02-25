# File: mapCensusNYC2010.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 01/24/22

#---------------------------#
#### C. TABLE OF CONTENTS####
#---------------------------#
# 0. Package Imports
# 1. Read in census shapefile
# 2. Plot single maps
# 3. Plot joint maps
# 4. Census data EDA
# 5. Census data PCA

#---------------------------#
####  0. PACKAGE IMPORTS ####
#---------------------------#
source(here::here("code", "scripts", "requirements.R"))

#-------------------------#
####  1. READ IN DATA  ####
#-------------------------#
nyc.shp <- load_census_data()
nyc.census <- nyc.shp@data

#---------------------------#
#### 2. PLOT SINGLE MAPS ####
#---------------------------#
# 2a. set up tmap options
tmap_options(
  show.warnings = FALSE,
  show.messages = FALSE
)

# 2b. create list of variables we are interested in for easy indexing later.
vars <- c(colnames(nyc.census)[startsWith(colnames(nyc.census), "c_")], colnames(nyc.census)[startsWith(colnames(nyc.census), "s_")])

# 2c. to save the maps as png files:
#vars %>% walk(~make_map(vars = ., data.shp = nyc.shp, dir = here::here("figures", "singleMaps"), tm_dpi = 100))

# 2d. to make a large list of maps:
#singleMaps <- vars %>% map(~make_map(vars = ., data.shp = nyc.shp))

#---------------------------#
#### 3. PLOT JOINT MAPS  ####
#---------------------------#
# 3a. set destination directory for the joint maps.
destination <- here("figures", "jointMaps")

# 3b. make 9 different joint maps...
make_map(data.shp = nyc.shp,
         vars = vars[startsWith(vars, "c_rac_p")][2:8],
         dir = destination,
         filename = "race",
         tm_w = 10, tm_h = 6)

make_map(data.shp = nyc.shp,
         vars = vars[startsWith(vars, "c_sex_p")],
         dir = destination,
         filename = "sex",
         cols = 2)

make_map(data.shp = nyc.shp,
         vars = vars[startsWith(vars, "c_hhs_p")],
         dir = destination,
         filename = "households",
         cols = 4)

 make_map(data.shp = nyc.shp,
         vars = c(vars[str_detect(vars, "c_hou_p")], vars[str_detect(vars, "s_hou_med")]),
         dir = destination,
         filename = "housing",
         cols = 4)

make_map(data.shp = nyc.shp,
         vars = vars[startsWith(vars, "s_mtw_p")],
         dir = destination,
         filename = "mode_transport_to_work",
         cols = 3)

make_map(data.shp = nyc.shp,
         vars = vars[startsWith(vars, "s_edu_p")],
         dir = destination,
         filename = "education",
         cols = 3)

make_map(data.shp = nyc.shp,
         vars = vars[startsWith(vars, "s_occ_p")],
         dir = destination,
         filename = "occupation",
         cols = 5)

make_map(data.shp = nyc.shp,
         vars = vars[startsWith(vars, "s_htg_p")],
         dir = destination,
         filename = "heating",
         cols = 3)

make_map(data.shp = nyc.shp,
         vars = vars[startsWith(vars, "s_veh_p")],
         dir = destination,
         filename = "vehicles",
         cols = 2)

#---------------------------#
#### 4. CENSUS DATA EDA  ####
#---------------------------#
# 4a. define colours for plots
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
             "#000000", # gray - population
             "#b7ded8", # turquoise
             "#b15828"
             ) 

# 4b. grab variables of interest, scale them by column.
cd <- nyc.census %>% 
  select(contains("_p"), 
         contains("_med"),
         contains("_mean"),
         contains("_gen"),
         "s_ttw_weightedAvg") %>%
  select(-c_rac_pacificIslander) %>%
  apply(., MARGIN = 2, FUN = scale, center = T)

# 4c. get categories for each column:
cd_categories <- colnames(cd) %>% 
  str_extract("_(.*)_") %>% 
  str_remove_all("_")

# 4d. remember the old column names
old_colnames <- colnames(cd)

# 4e. remove categories from the column name and rename the columns with these shorter names:
new_colnames <- colnames(cd) %>% 
  str_remove(cd_categories) %>%
  str_remove("(.)__")

colnames(cd) <- new_colnames

# 4f. define column groupings.
colg <- data.frame(chem = colnames(cd), group = cd_categories)

# 4g. exploratory data analysis..!
cd.eda <- eda(mat = cd, 
              pcs = paste0("PC", 1:4),
              colgroups = colg,
              rowgroups = nyc.census$BoroName,
              rowgroups_name = "Borough",
              scale_flag = F)

# 4h. just looking at pc1:
pc1 <- eda(mat = cd %>% select(-BoroName), 
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

# 4i. table showing groupings
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
  row_spec(., row = 17, color = "white", background = "black") 

# 4j. table showing breakdown of PCs
cd.eda$var_raw %>%
  kbl(col.names = c("PC", "Eigenvalue", "% Variance", "% Cumulative Variance")) %>%
  kable_classic() %>%
  kable_styling(full_width = F, bootstrap_options = c("striped"), html_font = "monaco")

# 4k. finding out the tracts that are missing from shapefile (all water)
setdiff(paste0(nyc.census$tracta, nyc.census$BoroName), paste0(nyc.shp@data$CT2010, nyc.shp@data$BoroName))
nyc.census[nyc.census$BoroName == "Brooklyn" & nyc.census$tracta == "990100",]$name # Lower NY Bay
nyc.census[nyc.census$BoroName == "Queens" & nyc.census$tracta == "990100",]$name # Atlantic Ocean
nyc.census[nyc.census$BoroName == "Staten Island" & nyc.census$tracta == "008900",]$name # NY Bay

#---------------------------#
#### 5. CENSUS DATA PCA  ####
#---------------------------#
# 5a. create a copy of the census data with NAs, and one without
cd.na <- cd
cd[is.na(cd)] <- 0 # impute missing values with 0

# 5b. PCA on census data matrix (cd) WITH the missing values
pca.nafull <- pca_jg(cd.na, rank = ncol(cd))
pca.nafirst <- pca_jg(cd.na, rank = 1)

# 5c. linear model formula: dependent (response) ~ independent (terms)
lm(pca.nafirst$scores[,1] ~ pca.nafull$scores[,1]) %>% summary()

# 5d. PCA on census data matrix (cd) without missings
pca <- prcomp(cd)
lm(pca.nafirst$scores[,1] ~ pca$x[,1]) %>% summary()

# 5e. joining PCA scores with shapefile:
nyc.shp@data <- left_join(x = nyc.shp@data, 
                          y = cbind(nyc.census, pca$x[, 1:3]))

# 5f. make map of PCs 1-3
pcs_mapped <- make_map(data.shp = nyc.shp,
                       vars = paste0("PC", 1:3),
                       dir = destination,
                       filename = "PCS1-3",
                       tm_w = 10, tm_h = 6, tm_dpi = 1000)

# 5g. Make individual maps
make_map(data.shp = nyc.shp, vars = c("PC1"), dir = destination, filename = "PC1", tm_dpi = 500)
make_map(data.shp = nyc.shp, vars = c("PC2"), dir = destination, filename = "PC2", tm_dpi = 500)
make_map(data.shp = nyc.shp, vars = c("PC3"), dir = destination, filename = "PC3", tm_dpi = 500)

nyc.shp@data <- nyc.shp@data %>%
  rename_at(vars(old_colnames), ~new_colnames)

# 5h. top 4 loadings map
cd.eda$load$data %>%
  filter(PC == "PC1") %>%
  arrange(desc(Loading)) %>%
  slice_head(n = 4) %>%
  select(chem) %>%
  mutate(chem = as.character(chem)) %>%
  unlist() %>% 
  make_map(data.shp = nyc.shp, vars = ., cols = 2, dir = destination, filename = "PC1-top4", tm_dpi = 500)

# 5i. bottom 4 loadings map
cd.eda$load$data %>%
  filter(PC == "PC1") %>%
  arrange(Loading) %>%
  slice_head(n = 4) %>%
  select(chem) %>%
  mutate(chem = as.character(chem)) %>%
  unlist() %>% 
  make_map(data.shp = nyc.shp, vars = ., cols = 2, dir = destination, filename = "PC1-bot4", tm_dpi = 500)

# 5j. summary tables of PCs:
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


