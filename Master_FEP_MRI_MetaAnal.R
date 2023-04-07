########################################################################################
#                                                                                      #
# Master_FEP_MRI_MetaAnal.R from Blackman et al                                        #
#                                                                                      #
# "Prevalence of neuroradiological abnormalities in First Episode Psychosis:           #
#  a Meta-Analysis"                                                                    #
#                                                                                      #
# https://github.com/grahamblackman/FEP_MRI_anormality_Meta                            #
#                                                                                      #
########################################################################################

# ** to begin, you will need to set the working drive to where the r scripts and excel data file are stored**
#
# This script loads the relevant libraries, defines key functions, and executes the
# subscripts.
#
# The following line will clear all objects from memory. Useful for a 'fresh start' when
# debugging but not necessary for the script to run
# rm(list= ls()) 

# The following line clears all the plots from RStudio if this script is being run inside it
# Useful for similar 'fresh start' reasons
# dev.off(dev.list()["RStudioGD"])

# For the following write_to_file variable
#  set to 0 to display (e.g. RStudio)
#  set to 1 to write figures to ./output/ directory (must already exist in the working directory)
write_to_file = 0

# Load packages
library("meta") # For meta analysis
library("metafor") # For meta analysis
library("readxl")# For reading excel documents
library("skimr") # For summary
library("tidyverse") # Multiple packages, inc ggplots2
library("lubridate") # Dealing with dates
library("MASS") # Functions and datasets to support "Modern Applied Statistics with S
library("janitor") # Cleaning data
library("devtools") # To download packages in development
library("lme4") # Linear model 4, required for meta analysis
library("expss") # Tables, Labels and Some Useful Functions from Spreadsheets (eg excel and SPSS' Statistics)
library("scales") # For visualisation
library("dmetar") # Influential study diagnostics for meta-analysis

# Ensure names map to explicit package functions and are not overwritten
escalc <- metafor::escalc
rma <- metafor::rma
transf.ipft.hm <- metafor::transf.ipft.hm
forest <- metafor::forest
expss <- expss::count_if
funnel <- metafor::funnel

# Load data
#
#setwd("")
datafile_path = paste("FEP_MRI_data.xlsx")
data_all <- read_xlsx(datafile_path, sheet = "sheet1")

# Execute subscripts

source("meta_mri_processing.R")
source("meta_mri_descriptive.R", echo = TRUE)
source("meta_mri_analysis_prev.R", echo = TRUE) # note to Vaughan 4/4/23- as there is major imbalance in how many studies
source("meta_mri_analysis_prev_subtype.R", echo = TRUE) 
source("meta_mri_analysis_RR.R", echo = TRUE)  # note to Vaughan 4/4/23- For the L1O meta analysis, it now looks like we don't have any influential studies, but would be appreciative of your thoughts on this
source("meta_mri_visualization.R", echo = TRUE)  
source("meta_mri_visualization_subtypes.R", echo = TRUE)  # note to Vaughan 4/4/23- this is a new script I wrote that automatically creates the forest plots (of the subtypes) from the meta analytic estimates. I've adpated the code from our other study (thanks again for developing!)


# Optional scripts

# source("meta_mri_visualization_addendum.R") # additional visualizations not included in the publication but useful for the open archive

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
print_package_version("dmetar") 
