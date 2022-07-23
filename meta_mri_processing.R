########################################################################################
#                                                                                      #
# meta_mri_processing.R from Blackman et al                                            #
#                                                                                      #
# "How common are neuroradiological abnormalities in first-episode psychosis?         #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
########################################################################################

#
# This script ensures key variables are set to the correct data types and subsets the main
# dataframes into dataframes that contain specific study types for later analysis
#

####### preparing data for analyses ######

# Create 'data' dataframe that excludes Zanetti et al study (study only reports white matter abnormalities)
data <- data_all %>%
  filter(author!='Zanetti')

# Ensure variables are of the correct type
#

# Code year as numeric
data$year <- as.numeric(data$year)
data_all$year <- as.numeric(data_all$year)

# Code psychosis duration as numeric
data$psychosis_duration_wks <- as.numeric(data$psychosis_duration_wks)
data_all$psychosis_duration_wks <- as.numeric(data_all$psychosis_duration_wks)

# Code age as numeric
data$age_fep <- as.numeric(data$age_fep)
data_all$age_fep <- as.numeric(data_all$age_fep)

# Code neurorad binary as character
data$neurorad_binary <- as.character(data$neurorad_binary)
data_all$neurorad_binary <- as.character(data_all$neurorad_binary)

# Code female as numeric
data$female_fep <- as.numeric(data$female_fep)
data_all$female_fep <- as.numeric(data_all$female_fep)

# Code 'age under 35' as numeric
data$age_under_35 <- as.numeric(data$age_under_35)
data_all$age_under_35 <- as.numeric(data_all$age_under_35)

# Code 'current AntiPsychotic exposure' as numeric
data$current_AP_exposure <- as.numeric(data$current_AP_exposure)
data_all$current_AP_exposure <- as.numeric(data_all$current_AP_exposure)

# Code 'whole brain evaluated' as numeric
data$whole_brain_binary <- as.numeric(data$whole_brain_binary)
data_all$whole_brain_binary <- as.numeric(data_all$whole_brain_binary)

# Calculate variable containing number of normal scans for...

# First episode psychosis patients
data <- data %>%
  mutate("fep_normal" = fep_total - fep_abnormal)

# Healthy controls
data <- data %>%
  mutate("hc_normal" = hc_total - hc_abnormal)

# Create dataframes with subsets of studies


# Create dataframe 'data_cr' that includes only  studies reporting clinically relevant abnormalities...
data_cr <- data %>%
  filter(include_CR == "yes")

# ... and ensure fep_cr_abnormal variable is set to data type numeric 
data_cr$fep_cr_abnormal <- as.numeric(data_cr$fep_cr_abnormal)

# Create data_subtype dataframe with only studies reporting subtype abnormalities
data_subtype <- data %>%
  filter(include_subtype == "yes")

# Create data_subtype_all dataframe with studies reporting anatomical subtypes [inc Zannetti et al]
data_subtype_all <- data_all %>%
  filter(include_subtype == "yes")

# Create data_cr_subtype dataframe with studies reporting anatomical subtypes AND whether abnormalities were clinically relevant
data_cr_subtype <- data %>%
  filter(include_CR_subtype == "yes")
