###############processing script###################

## notes:

##content:
# missing data: nil
# data wrangling: yes
# create variables: yes

#output object: 
# data
# data_cr

####### preparing data for analyses ######

# exclude Zanetti
data <- data_all[-12,]


############### data wrangling  ###############
# year as numeric
data$year<-as.numeric(data$year)
data_all$year<-as.numeric(data_all$year)

# psychosis duration as numeric
data$psychosis_duration_wks<-as.numeric(data$psychosis_duration_wks)
data_all$psychosis_duration_wks<-as.numeric(data_all$psychosis_duration_wks)

# age as numeric
data$age_fep<-as.numeric(data$age_fep)
data_all$age_fep<-as.numeric(data_all$age_fep)

# neurorad binary as character
data$neurorad_binary<-as.character(data$neurorad_binary)
data_all$neurorad_binary<-as.character(data_all$neurorad_binary)

#female as numeric
data$female_fep<-as.numeric(data$female_fep)
data_all$female_fep<-as.numeric(data_all$female_fep)

#age_under_35 as numeric
data$age_under_35<-as.numeric(data$age_under_35)
data_all$age_under_35<-as.numeric(data_all$age_under_35)

#current_AP_exposure as numeric
data$current_AP_exposure<-as.numeric(data$current_AP_exposure)
data_all$current_AP_exposure<-as.numeric(data_all$current_AP_exposure)

#whole_brain_binary as binary
data$whole_brain_binary<-as.numeric(data$whole_brain_binary)
data_all$whole_brain_binary<-as.numeric(data_all$whole_brain_binary)


############### create variables ###############


# #create prop patients treated with AP [under development]
# current_AP_exposure<-as.numeric(data_all$current_AP_exposure)
# current_AP_exposure<-as.numeric(data$current_AP_exposure)
# mutate(data,"prop_current_AP_expo"=current_AP_exposure/fep_total)

# calc number of normal scans

data<-mutate(data,"fep_normal"=fep_total-fep_abnormal)
data<-mutate(data,"hc_normal"=hc_total-hc_abnormal)


############### study selection ###############

# select  studies reporting CR abnormalities
data_cr<-data %>% filter( include_CR == "yes")

# select  studies reporting subtype abnormalities
data_subtype<-data %>% filter(include_subtype == "yes")

# select  studies reporting WM abnormalities [inc Zannetti et al]
data_subtype_all<-data_all %>% filter(include_subtype == "yes")

# select studies reporting subtype abnormalities
data_cr_subtype<-data %>% filter( include_CR_subtype == "yes")

# freq of CR abnormalities as numeric
data_cr$fep_cr_abnormal <-as.numeric(data_cr$fep_cr_abnormal)

############### 

# paste(round(100*0.5, 2), "%", sep="")

# scatter_fun_1 = function(y) {
#   ggplot(immune, aes(x = 1:nrow(immune), y = .data[[y]]) ) +
#     geom_point(size =0.4, alpha=3/10) +
#     geom_smooth(method = "loess", se = FALSE, color = "grey50") +
#     theme_classic() + 
#     labs(title="", x ="Index")
# }


###############  functions  ###############  

percent= function(y) {
  paste(round(100*y, 0), "%", sep="")
  }

percent(0.23)
