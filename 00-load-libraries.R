# 00-load-libraries.R
# This script loads libraries common to analysis scripts used in MS_access

library(here)
library(skimr)
library(janitor)
library(readxl)
library(kableExtra)
# plotting
library(ggplot2)
library(RColorBrewer) 
library(wesanderson) 
library(conflicted)
library(tidyverse)
#library(viridis)

conflict_prefer("select", "dplyr") # (need to use raster::select for rasters)
conflict_prefer("filter", "dplyr")

