########################################################################################
#                                                                                      #
# Master_FEP_MRI_MetaAnal.R from Blackman et al                                        #
#                                                                                      #
# "How common are neuroradiological abnormalities in first-episode psychosis?          #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
# https://github.com/grahamblackman/FEP_MRI_anormality_Meta                            #
########################################################################################

#
# This script loads the relevant libraries, defines key functions, and executes the
# subscripts.

# The following line will clear all objects from memory. Useful for a 'fresh start' when
# debugging but not necessary for the script to run
# rm(list= ls()) 

# The following line clears all the plots from RStudio if this script is being run inside it
# Useful for similar 'fresh start' reasons
# dev.off(dev.list()["RStudioGD"])

# Load packages
#
library("meta") # for meta analysis
library("metafor") # for meta analysis
library("readxl")# for reading excel documents
library("skimr") # for summary
library("tidyverse") # multiple packages, inc ggplots2
library("lubridate") # dealing with dates
library("MASS") # Functions and datasets to support "Modern Applied Statistics with S
library("janitor") # cleaning data
library("devtools") # to download packages in development
library("lme4") #linear model 4, required for meta analysis
library("expss") #Tables, Labels and Some Useful Functions from Spreadsheets (eg excel and SPSS' Statistics)
library("scales") 

# Ensure names map to explicit package functions and are not overwritten

escalc <- metafor::escalc
rma <- metafor::rma
transf.ipft.hm <- metafor::transf.ipft.hm
forest <- metafor::forest
expss <- expss::count_if
funnel <- metafor::funnel

# Load data
#
datafile_path = paste("FEP_MRI_data.xlsx")
data_all <- read_xlsx(datafile_path, sheet = "sheet1")

# Execute subscripts

source("meta_mri_processing.R")
source("meta_mri_descriptive.R", echo = TRUE)
source("meta_mri_analysis_prev.R", echo = TRUE)
source("meta_mri_analysis_prev_subtype.R", echo = TRUE) 
source("meta_mri_analysis_RR.R", echo = TRUE)
source("meta_mri_visualization.R")
source("meta_mri_visualization_to_file.R")

# Optional scripts

# source("meta_mri_visualization_addendum.R", echo=TRUE) # additional visualizations not included in the publication but useful for the open archive
# source("meta_mri_analysis_or.R", echo=TRUE) # calculation of odd ratio between FEP and HC.  Not included in the publication as RR reported.

# Display R version and package versions

version

print_package_version <- function(package_name) {
  sprintf("Package version for %s is %s", package_name, packageVersion(package_name)) # for meta analysis
}

print_package_version("meta")
print_package_version("metafor")
print_package_version("readxl")
print_package_version("skimr")
print_package_version("tidyverse")
print_package_version("lubridate")
print_package_version("MASS")
print_package_version("janitor")
print_package_version("devtools")
print_package_version("lme4")
print_package_version("expss")
print_package_version("scales") 
