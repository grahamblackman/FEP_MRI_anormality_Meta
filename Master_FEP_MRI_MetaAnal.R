########################################################################################
#                                                                                      #
# Master_FEP_MRI_MetaAnal.R from Blackman et al                                        #
#                                                                                      #
# "How command are neuroradiological abnormalities in first-episode psychosis?         #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
########################################################################################

#
# This script loads the relevant libraries, defines key functions, and executes the
# subscripts.
#

# The following line will clear all objects from memory. Useful for a 'fresh start' when
# debugging but not necessary for the script to run
rm(list= ls()) 

# The following line clears all the plots from RStudio if this script is being run inside it
# Useful for similar 'fresh start' reasons
dev.off(dev.list()["RStudioGD"])

# Load packages
#
library("meta") # for meta analysis
library("metafor") # for meta analysis
library("readxl")# for reading excel documents
library("skimr") # for summary
library("tidyverse") # multiple packages, inc ggplots2
library("xlxs") # for reading excel documents
library("lubridate") # dealing with dates
library("MASS") # Functions and datasets to support "Modern Applied Statistics with S
library("janitor") # cleaning data
library("devtools") # to be able to download packages in development:
library("lme4") #linear model 4, required for meta analysis
library("expss") #Tables, Labels and Some Useful Functions from Spreadsheets (eg excel and SPSS' Statistics)
library("scales") 

# Ensure names map to explicit package functions and are not overwritten
#
escalc <- metafor::escalc
rma <- metafor::rma
transf.ipft.hm <- metafor::transf.ipft.hm
forest <- metafor::forest
expss <- expss::count_if
funnel <- metafor::funnel

# Load data
#
data_all <- read_xlsx("FEP_MRI_data.xlsx", sheet = "sheet1")

# Execute subscripts
#
source("meta_mri_processing.R")
source("meta_mri_descriptive.R", echo = TRUE)
source("meta_mri_analysis_prev.R")
source("meta_mri_analysis_prev_subtype.R") 
source("meta_mri_visualization.R")

# The following is a script that includes additional visualizations not included in the publication but useful
# for the open archive

# source("meta_mri_visualization_addendum.R", echo=TRUE)
