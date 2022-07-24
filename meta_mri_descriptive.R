########################################################################################
#                                                                                      #
# meta_mri_descriptive.R from Blackman et al                                           #
#                                                                                      #
# "How common are neuroradiological abnormalities in first-episode psychosis?         #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
########################################################################################

#
# This script calculates and displays descriptive statistics
#


# Descriptive statistics relating to studies
#

# Calculate earliest and latest publication for included studies
max_study_year <- max(data_all$year, na.rm=TRUE)
sprintf("Latest publication year of included study: %s", max_study_year)

min_study_year <- min(data_all$year, na.rm=TRUE)
sprintf("Earliest publication year of included study: %s", min_study_year)

# Calculate total number of studies reporting first-episode psychosis patients with any abnormality
tot_study_fep <- sum(complete.cases(data_all$fep_total))
sprintf("Total number of studies reporting first-episode psychosis patients with any abnormality: %s", tot_study_fep)

# Calculate total number of number of studies reporting first-episode psychosis patients with any clinically relevant abnormality
tot_study_cr_fep <- sum(complete.cases(data_cr$fep_total))
sprintf("Total number of number of studies reporting first-episode psychosis patients with any clinically relevant abnormality: %s", tot_study_cr_fep)

# Descriptive statistics relating to data reported in studie

# Pooled sample 

# Calculate total number of first-episode psychosis patients with any abnormality
tot_sam_fep <- sum(data_all$fep_total, na.rm=TRUE)
sprintf("Total number of first-episode psychosis patients with any abnormality: %s", tot_sam_fep)

# Calculate total number of first-episode psychosis patients with any clinically relevant abnormality
tot_sam_cr_fep <- sum(data_cr$fep_total, na.rm=TRUE)
sprintf("Total number of first-episode psychosis patients with any clinically relevant abnormality: %s", tot_sam_cr_fep)

# Calculate minimum and maximum sample size reported across included studies
sam_size_min <- min(data_all$fep_total, na.rm=TRUE)
sprintf("Minimum sample size reported in included studies: %s", sam_size_min)
sam_size_max <- max(data_all$fep_total, na.rm=TRUE)
sprintf("Maximum sample size reported in included studies: %s", sam_size_max)

# Minimum and maximum age across studies
study_age_min <- min(data_all$age_fep, na.rm=TRUE)
sprintf("Minimum age of patient reported in included studies: %s", study_age_min)
study_age_max <- max(data_all$age_fep, na.rm=TRUE)
sprintf("Maximum age of patient reported in included studies: %s", study_age_max)
study_age_mean <- mean(data_all$age_fep, na.rm=TRUE)
sprintf("Mean age of patient reported in included studies: %.2f", study_age_mean)

# Number of studies with age reported
tot_study_age <- sum(complete.cases(data_all$age_fep))
sprintf("Number of studies with age reported: %s", tot_study_age)

# Proportion of female participants
prop_fem_fep_min <- min(data_all$female_fep/data_all$fep_total,na.rm=TRUE)
sprintf("Minimum proportion of female participants across studies: %.2f", prop_fem_fep_min)
prop_fem_fep_max <- max(data_all$female_fep/data_all$fep_total,na.rm=TRUE)
sprintf("Maximum proportion of female participants across studies: %.2f", prop_fem_fep_max)

# Psychosis duration
# Total number of studies reporting duration of psychosis
tot_study_psych_dur <- sum(complete.cases(data_all$psychosis_duration_wks))
sprintf("Total number of studies reporting duration of psychosis: %s", tot_study_psych_dur)

psych_dur_min <- min(data_all$psychosis_duration_wks, na.rm=TRUE)
sprintf("Minimum psychosis duration in weeks across studies: %.2f", psych_dur_min)
psych_dur_max <- max(data_all$psychosis_duration_wks, na.rm=TRUE)
sprintf("Maximum psychosis duration in weeks across studies: %.2f", psych_dur_max)

# Number of studies with healthy control group
hea_con <- sum(complete.cases(data_all$hc_total))
sprintf("Total number of studies with healthy control group: %s", hea_con)

# Calculate and show number of studies by continent
study_continent <- count(data_all, study_continent)
study_continent
tot_aus <- study_continent[1,2]
tot_eur <- study_continent[2,2]
tot_nam <- study_continent[3,2]
tot_sam <- study_continent[4,2]

# Descriptive statisics about screening of patients

# Calculate number of studies which screened out cases of possible organic psychosis
study_screen <- count_if("yes", data_all$study_screen_positive_exclude)
sprintf("Total number of studies that screened out patients based on clinical exam and/or psych assessment: %s", study_screen)
screen_exam <- count_if("yes", data_all$screen_exam)
sprintf("Total number of studies that screened out patients based on exam: %s", screen_exam)
screen_history <- count_if("yes", data_all$screen_history)
sprintf("Total number of studies that screened out patients based on psych assessment: %s", screen_history)

# Calculate and show recruitment pathway
recruitment <- count(data_all,recruitment)
recruitment
recruit_clin <- recruitment[1,2]
recruit_res <- recruitment[2,2]

# Calculate antipsychotic exposure
antipsych_current_stat <- sum(complete.cases(as.numeric(data_all$current_AP_exposure)))
sprintf("Total number of studies that report antipsychotic exposure at time of MRI: %s", antipsych_current_stat)

antipsych_prop_min <- min(data_all$current_AP_exposure/data_all$fep_total, na.rm=TRUE)
sprintf("Minimum proportion of patients with antipsychotic exposure across studies: %.2f", antipsych_prop_min)
antipsych_prop_max <- max(data_all$current_AP_exposure/data_all$fep_total, na.rm=TRUE)
sprintf("Maximum proportion of patients with antipsychotic exposure across studies: %.2f", antipsych_prop_max)

# Number of studies reporting field strength
scan_field <- sum(complete.cases(data_all$scan_field))
sprintf("Total number of studies reporting MRI field strength: %s", scan_field)

# Number of studies reporting field strength of FMRI [**to check**]
sprintf("Number of studies reporting each reported field strength of FMRI:")
count(data_all, scan_field)

# Scan-rater type by number of studies
sprintf("Scan-rater type in each type of study (nr = not specified):")
count(data_all, rater)

# Number of studies where scan-raters were blinded to status
scan_blind <- count_if("yes", data_all$scan_blind)
sprintf("Number of studies where scan-raters were blinded: %s", scan_blind)

## Quality assessment and risk of bias 
qual_min <- min(data_all$quality_assess_MTD)
sprintf("Lowest quality score scross studies: %s", qual_min)
qual_max <- max(data_all$quality_assess_MTD)
sprintf("Highest quality score scross studies: %s", qual_max)

bias_low <- count_if(7 %thru% 10, data_all$quality_assess_MTD)
sprintf("Number of studies scoring in the low range for bias: %s", bias_low)
bias_med <- count_if(4 %thru% 6, data_all$quality_assess_MTD)
sprintf("Number of studies scoring in the medium range for bias: %s", bias_med)
bias_high <- count_if(0 %thru% 3, data_all$quality_assess_MTD)
sprintf("Number of studies scoring in the high range for bias: %s", bias_high)