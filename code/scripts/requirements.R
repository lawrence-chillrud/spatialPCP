# File: requirements.R
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 01/27/22

# 0. install package manager pacman if not already installed:
if (!("pacman" %in% installed.packages()[, "Package"])) install.packages("pacman")

# 1. install/load all necessary packages for spatialPCP:
pacman::p_load("ggfortify",
               "here",
               "kableExtra",
               "rgdal",
               "R.matlab",
               "sp",
               "spdep",
               "tidyverse",
               "tmap"
               )

# 2. load all functions in the functions folder:
list.files(here::here("code", "functions"), full.names = T) %>% walk(source)

# 3. load pcpr and PCPhelpers:
if ("pcpr" %in% installed.packages()[, "Package"]) library(pcpr)
if ("PCPhelpers" %in% installed.packages()[, "Package"]) library(PCPhelpers)