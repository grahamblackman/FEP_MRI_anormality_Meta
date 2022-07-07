############# Analysis script###############
# to do:
# check if any cases of 'previous antipsychotic exposure'

# #notes:
#  data_all = includes zanetti et al
#  data = excludes zanetti et al

############## descriptive analysis ############## 

# databse search 

tot_database<-1682 # to update
tot_full_review<-100  # to update

# studies included

#earliest and latest study included
max_study_year<-max(data_all$year,na.rm=TRUE)
min_study_year<-min(data_all$year,na.rm=TRUE)

#number of studies  
#FEP patients: any abnorm
tot_study_fep<-sum(complete.cases(data_all$fep_total))

#FEP patients: clin rel abnorm
tot_study_cr_fep<-sum(complete.cases(data_cr$fep_total)) # #number of studies reporting clinical relevant findings

#pooled sample 
#FEP patients: any abnorm
tot_sam_fep<-sum(data_all$fep_total,na.rm=TRUE)
#FEP patients: clin rel abnorm
tot_sam_cr_fep<-sum(data_cr$fep_total,na.rm=TRUE)

#minimum and maximum sample size
sam_size_min<-min(data_all$fep_total,na.rm=TRUE)
sam_size_max<-max(data_all$fep_total,na.rm=TRUE)

#minimum and maximum mean age per study
study_age_min<-min(data_all$age_fep,na.rm=TRUE)
study_age_max<-max(data_all$age_fep,na.rm=TRUE)

#number of studies with age reported
tot_study_age<-sum(complete.cases(data_all$age_fep))

#number of studies with age reported
tot_study_age<-sum(complete.cases(data_all$age_fep))

#proportion female 
prop_fem_fep_min<-min(data_all$female_fep/data_all$fep_total,na.rm=TRUE)
prop_fem_fep_max<-max(data_all$female_fep/data_all$fep_total,na.rm=TRUE)

# psychosis duration
tot_study_psych_dur<-sum(complete.cases(data_all$psychosis_duration_wks))
psych_dur_min<-min(data_all$psychosis_duration_wks,na.rm=TRUE)
psych_dur_max<-max(data_all$psychosis_duration_wks,na.rm=TRUE)

#number of studies with HC group
hea_con<-sum(complete.cases(data_all$hc_total))

# study continent
study_continent<-count(data_all,study_continent)
tot_aus<-study_continent[1,2]
tot_eur<-study_continent[2,2]
tot_nam<-study_continent[3,2]
tot_sam<-study_continent[4,2]

# screening of patients
# studies which screened out possible organic psychosis
study_screen<- count_if("yes",data_all$study_screen_positive_exclude)
screen_exam<- count_if("yes",data_all$screen_exam)
screen_history<- count_if("yes",data_all$screen_history)

# recruitment:
recruitment<-count(data_all,recruitment)
recruit_clin<-recruitment[1,2]
recruit_res<-recruitment[2,2]

#antipsychotic exposure
antipsych_current_stat<-sum(complete.cases(as.numeric(data_all$current_AP_exposure)))
antipsych_prop_min <- min(data_all$current_AP_exposure/data_all$fep_total,na.rm=TRUE)
antipsych_prop_max <- max(data_all$current_AP_exposure/data_all$fep_total,na.rm=TRUE)

## antipsychotic exposure [temporarily commented out pending creation of new var]
# prop_antipsy_expo_min<-min(data_all$prop_current_AP_expo)
# prop_antipsy_expo_max<-max(data_all$prop_current_AP_expo)

# # antipsychotic duration 
# [insufficient data]

#number of studies reporting field strength
scan_field<-sum(complete.cases(data_all$scan_field))

# #frequency counts [to complete]
scan_field_strength_onepointfive_T<- count_if("1.5",data_all$scan_field)
scan_field_strength_three_T<- count_if("3",data_all$scan_field)

#rater type [completed - MTD]
scan_rep_neurorad <- count_if("1", data_all$neurorad_binary)
scan_rep_rad <- count_if("1", data_all$rad_binary)
scan_rep_neuro <- count_if("1", data_all$neuro_bin)
scan_rep_psych <- count_if("1", data_all$psyc_binary)
scan_rep_unspec <- count_if("nr", data_all$rater)

#rater type (binary) 
scan_rep_bin_rad <- count_if("y", data_all$all_rater_radiol_bin)
scan_rep_bin_nonrad <- count_if("n", data_all$all_rater_radiol_bin)
scan_rep_bin_notreported <- count_if("nr", data_all$all_rater_radiol_bin)

# #blinding 
scan_blind<-count_if("yes", data_all$scan_blind)

# scan_blind<-count(data_all,scan_field)
# scan_blind_y<-scan_blind[x,x]

## Quality assessment and risk of bias 
qual_min<-min(data_all$quality_assess_MTD)
qual_max<-max(data_all$quality_assess_MTD)
bias_low<-count_if(7 %thru% 10, data_all$quality_assess_MTD)
bias_med<-count_if(4 %thru% 6, data_all$quality_assess_MTD)
bias_high<-count_if(0 %thru% 3, data_all$quality_assess_MTD)