# File: pcaProjections.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 02/24/22

#----------------------------#
#### C. TABLE OF CONTENTS ####
#----------------------------#
# 0. Package Imports
# 1. Read in data
# 2. Split data by borough
# 3. SVD by borough
# 4. Project one borough onto another

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
source(here::here("code", "scripts", "requirements.R"))

#-----------------------#
#### 1. READ IN DATA ####
#-----------------------#
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

#--------------------------------#
#### 2. SPLIT DATA BY BOROUGH ####
#--------------------------------#
# 2a. splitting by borough name, then setting the proper name for each item in
# the resulting list:
boroughs <- cd %>% group_split(BoroName)
borough_names <- boroughs %>% map_chr(~.$BoroName %>% unique)
names(boroughs) <- borough_names

#---------------------------------#
#### 3. PERFORM SVD BY BOROUGH ####
#---------------------------------#
# U_bronx W =  Y_staten
# W ?= Sigma_staten V^T_staten ???

boroughs <- map(.x = boroughs, .f = function (mat) {
  # 3a. remove BoroName col from each df in the list, then scale by column:
  scaled_mat <- mat %>% 
    select(-BoroName) %>% 
    apply(., MARGIN = 2, FUN = scale, center = T)
  
  # 3b. impute missings as 0 for svd
  scaled_mat[is.na(scaled_mat)] <- 0
  
  scaled_mat
  
})

# 3c. Take SVD of each borough:
svds <- map(.x = boroughs, svd)

#----------------------------------------#
#### 4. PROJECT BOROUGHS ONTO ANOTHER ####
#----------------------------------------#
# 4a. create all combinations of projections:
grid <- expand.grid(M = borough_names, `V^T` = borough_names)

# 4b. solver for the projection:
solve <- function(M, V) {
  
  X <- matrix(NA, nrow(M), ncol(M))
  
  for (i in 1:nrow(M)) {
    X[i, ] <- lm(M[i, ] ~ 0 + V) %>% coef()
  }
  
  return(X)
  
}

# 4c. loop through each combination of projection in grid, and record relative
# error statistic..!
stats <- foreach(i = icount(nrow(grid)), .combine = rbind) %do% {
  X <- solve(M = boroughs[[grid$M[i]]], V = svds[[grid$`V^T`[i]]]$v)
  SCORES <- svds[[grid$M[i]]]$u %*% diag(svds[[grid$M[i]]]$d)
  rel_err <- norm(X - SCORES, "F") / norm(SCORES, "F")
  cbind(grid[i,], rel_err = round(rel_err, 5))
} 

stats_just_pc1 <- foreach(i = icount(nrow(grid)), .combine = rbind) %do% {
  X <- solve(M = boroughs[[grid$M[i]]], V = svds[[grid$`V^T`[i]]]$v)
  SCORES <- svds[[grid$M[i]]]$u %*% diag(svds[[grid$M[i]]]$d)
  rel_err <- norm(X[,1] - SCORES[,1], "2") / norm(SCORES[,1], "2")
  cbind(grid[i,], rel_err = round(rel_err, 5))
} 
stats_just_pc2 <- foreach(i = icount(nrow(grid)), .combine = rbind) %do% {
  X <- solve(M = boroughs[[grid$M[i]]], V = svds[[grid$`V^T`[i]]]$v)
  SCORES <- svds[[grid$M[i]]]$u %*% diag(svds[[grid$M[i]]]$d)
  rel_err <- norm(X[,2] - SCORES[,2], "2") / norm(SCORES[,2], "2")
  cbind(grid[i,], rel_err = round(rel_err, 5))
} 
#----------------------------#
#### 5. VISUALIZE RESULTS ####
#----------------------------#
colours <- c("#2c6cae", "#33a02b", "#737373", "#6a3d9a", "#ff7f00" )

stats_just_pc1 %>%
  mutate(rel_err = rel_err*100) %>%
  mutate(M = cell_spec(M, "html", color = "white", background = factor(M, unique(M), colours))) %>%
  mutate(`V^T` = cell_spec(`V^T`, "html", color = "white", background = factor(`V^T`, unique(`V^T`), colours))) %>%
  mutate(rel_err = cell_spec(rel_err, "html", color = "white", background = spec_color(rel_err, direction = 1, begin = 0.2, end = .9, option = "B", scale_from = c(0, max(c(stats$rel_err*100, stats_just_pc1$rel_err*100)))))) %>%
  kbl("html", escape = F, col.names = c("M", "V^T", "% Relative Error"), caption = "% Relative Error of X: Only Testing PC1") %>%
  kable_classic() %>%
  #column_spec(3, color = "white", background = spec_color(stats$rel_err*100, direction = 1, begin = 0, end = 1, option = "B", scale_from = c(0, max(c(stats$rel_err*100, stats_just_pc1$rel_err*100))))) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped"), html_font = "monaco")

symsub_dist(U = svds$Queens$v[,1], V = svds$Brooklyn$v[,1])

