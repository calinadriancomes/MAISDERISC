# R/00_packages.R
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(MASS)      # glm.nb
  library(pscl)      # optional: 
  library(broom)
  library(DHARMa)    # diagnostic pentru modele count 
  library(scales)
})
