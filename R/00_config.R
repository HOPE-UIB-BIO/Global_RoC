#----------------------------------------------------------#
#
#
#                 Global Rate-of-Change patterns
#
#                         Config file
#
#                     Ondrej Mottl - 2020
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 1. Load libraries and functions -----
#----------------------------------------------------------#

# delete existing workspace to start clean
rm(list = ls())

# Package version control
library(renv)
# renv::init()
# renv::snapshot(lockfile = "DATA/lock/revn.lock")
renv::restore(lockfile = "DATA/lock/revn.lock")

# libraries
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(maps)
library(mgcv)
library(gratia)
library(NbClust)

# source scripts and functions
files.sources = list.files("R/functions/")
sapply(paste0("R/functions/", files.sources, sep = ""), source)


#----------------------------------------------------------#
# 2. Import data  -----
#----------------------------------------------------------#

Dataset_work <-  
  read_rds("DATA/input/Dataset_20210316.rds")


#-------------------------------------------#
# 2.1 Env Data -----
#-------------------------------------------#
ngrip <-  read.csv("DATA/input/NGRIP.csv")
names(ngrip) <-  c("Age", "var")
ngrip$Age <-  ngrip$Age - 50
ngrip$var <-  ngrip$var - ngrip$var[1]

land_use <-  read.csv("DATA/input/LandUse.csv")

epica_CO2 <- 
  read.csv("DATA/input/EPICACO2.csv", header = T) %>%
  as_tibble()

names(epica_CO2) <-  c("Depth", "Age", "var", "Sigma")

#----------------------------------------------------------#
# 3. variable definition  -----
#----------------------------------------------------------#

age_treshold <-  18e3 
ROC_treshold <-  2 

time_bin <-   500
time_bin_sensitivity <-  250

cluster_method <-  "mcquitty"
distance <-  "manhattan"

#----------------------------------------------------------#
# 4. Graphical denition -----
#----------------------------------------------------------#
text_size <-  7

# Colour definition
getPalette <-  colorRampPalette(brewer.pal(6, "Set2"))

pallete_1 <-  getPalette(6)

names(pallete_1) <-
  c("North America",
    "Latin America",
    "Europe",
    "Africa",
    "Asia",
    "Oceania")

Palette_Euroasia <- read_rds("DATA/input/Palettes/Palette_Euroasia.rds")

Palette_Americas <- read_rds("DATA/input/Palettes/Palette_Americas.rds")


#----------------------------------------------------------#
# 5. region boundaries definition -----
#----------------------------------------------------------#

region_coord <- 
  tibble(
    REGION = names(pallete_1),
    long_min = c(-165, -100, -8, -18, 40, 95),
    long_max = c(-55, -37, 40, 50, 182, 175),
    lat_min = c(25, -54, 38, -32, 11, -45),
    lat_max = c(68, 25, 70, 35, 75, 10) )
