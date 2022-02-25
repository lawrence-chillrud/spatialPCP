# File: pcaByBorough.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 02/24/22

#---------------------------#
#### C. TABLE OF CONTENTS####
#---------------------------#
# 0. Package Imports
# 1. Read in data
# 2. Split data by borough
# 3. PCA by borough
# 4. Plot loadings

#---------------------------#
####  0. PACKAGE IMPORTS ####
#---------------------------#
source(here::here("code", "scripts", "requirements.R"))

#-------------------------#
####  1. READ IN DATA  ####
#-------------------------#
# 1a. read in census data:
nyc.census <- read_csv(here("data", "clean", "nyc_census_2010.csv"))

# 1b. select only the variables we care about:
cd <- nyc.census %>%
  select(BoroName,
         contains("_p"), 
         contains("_med"),
         contains("_mean"),
         contains("_gen"),
         "s_ttw_weightedAvg") %>%
  select(-c_rac_pacificIslander) %>%
  as_tibble()

#----------------------------------#
####  2. SPLIT DATA BY BOROUGH  ####
#----------------------------------#
# 2a. splitting by borough name, then setting the proper name for each item in
# the resulting list:
boroughs <- cd %>% group_split(BoroName)
borough_names <- boroughs %>% map_chr(~.$BoroName %>% unique)
names(boroughs) <- borough_names

#-----------------------------------#
####  3. PERFORM PCA BY BOROUGH  ####
#-----------------------------------#
# 3a(i). Define pc we want from pca:
pc <- 4

borough_loadings <- map(.x = boroughs, .f = function (mat) {
  # 3a. remove BoroName col from each df in the list, then scale by column:
  borough_name <- unique(mat$BoroName)
  scaled_mat <- mat %>% 
    select(-BoroName) %>% 
    apply(., MARGIN = 2, FUN = scale, center = T)
  
  # 3b. remember the old column names:
  old_colnames <- colnames(scaled_mat)
  
  # 3c. get categories for each column:
  categories <- old_colnames %>% 
    str_extract("_(.*)_") %>% 
    str_remove_all("_")
  
  # 3d. remove categories from col names:
  new_colnames <- old_colnames %>% 
    str_remove(categories) %>%
    str_remove("(.)__")
  
  # 3e. rename cols w/shorter names:
  colnames(scaled_mat) <- new_colnames
  
  # 3f. define column groupings:
  colg <- data.frame(census_var = new_colnames, group = categories)
  
  # 3g. impute missings as 0 for pca
  scaled_mat[is.na(scaled_mat)] <- 0
  
  # 3h. perform pca:
  pca <- prcomp(scaled_mat)
  pca_ld <- as.data.frame.matrix(pca$rotation)
  pca_ld$census_var <- row.names(pca_ld)
  
  left_join(pca_ld, colg) %>% 
      pivot_longer(cols = starts_with("PC"), names_to = "PC", values_to = "Loading") %>%
      filter(PC == paste0("PC", pc)) %>%
      arrange(group, desc(Loading)) %>%
      mutate(census_var = factor(census_var, levels = census_var)) %>%
      mutate(Borough = borough_name)
}) %>% reduce(rbind)

#-------------------------------------------#
####  4. PLOT LOADINGS FOR EACH BOROUGH  ####
#-------------------------------------------#
# 4a. define colours in preparation for plot
colours <- c("#FB9A99", # pink - mtw
             "#000000", # black - edu 
             "#B2DF89", # light green - income 
             "#1F78B4", # dark blue - occupation
             "#E72A8A", # bright pink - employment
             "#CAB2D5", # light purple - heating
             "#E3211C", # red - heating
             "#FDBF6F", # light orange - vehicles
             "#FF7F00", # dark orange - mode transport
             "#6A3D9A", # dark purple - education
             "#B15928", # brown - occupation
             "#666666", # tan - language
             "#33A02B", # dark green - households
             "#A5CEE3", # light blue - housing
             "#b7ded8", # turquoise - age
             "#b15828"
) 

# 4b. plot
borough_loadings %>%
  ggplot(aes(x = census_var, y = Loading, color = group)) +
  geom_point() + 
  geom_segment(aes(yend = 0, xend = census_var)) +
  scale_colour_manual(values = colours[order(unique(colg$group))]) +
  facet_wrap(~ Borough, ncol = 1) + 
  theme_bw() + 
  theme(legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.direction="horizontal",
        legend.box = "horizontal",
        legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1), strip.background = element_rect(fill = "white"), 
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  guides(colour = guide_legend(nrow = 1, byrow = T))

