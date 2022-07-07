########################## MASTER ########################## 

# see notes files regarding:

########################## 
#remove history 
rm(list= ls()) 

##########################  packages ###########################

require("meta") # for meta analysis
require("metafor") # for meta analysis
require("readxl")# for reading excel documents
require("skimr") # for summary
require("tidyverse") # multiple packages, inc ggplots2
require("xlxs") # for reading excel documents
require("lubridate") # dealing with dates
require("MASS") # Functions and datasets to support "Modern Applied Statistics with S
require("janitor") # cleaning data
require("devtools") # to be able to download packages in development:
require("lme4") #linear model 4, required for meta analysis
require("expss") #Tables, Labels and Some Useful Functions from Spreadsheets (eg excel and SPSS' Statistics)
require("scales") 


########################## 
#pre set packages for functions
escalc<-metafor::escalc
rma<-metafor::rma
transf.ipft.hm<-metafor::transf.ipft.hm
forest<-metafor::forest
expss<-expss::count_if
funnel<-metafor::funnel

########################## data loading ###########################

data_all <-read_xlsx("FEP_MRI_data.xlsx",sheet = "sheet1")
# search_data<-read_xlsx("FEP_MRI_data.xlsx",sheet = "sheet2")

###options (commented out)

##skim data to check missing values: 
#view(skim(data))

## to list files in director:
#list.files () # files in directory

# to glimpse data: 
# view(glimpse(data))

#structure  of each variable:
# str(data)

# summary of variables (for numeric values descriptive stats): 
# summary(data)

########################## source scripts ###########################

source("meta_mri_processing.r") # 
source("meta_mri_descriptive.r")
source("meta_mri_analysis_prev.r") #  
source("meta_mri_analysis_prev_subtype.r") 
source("meta_mri_visualization.r") #





