########################################################################################
#                                                                                      #
# meta_mri_analysis_RR.R from Blackman et al                                           #
#                                                                                      #
# "Prevalence of neuroradiological abnormalities in First Episode Psychosis:           #
#  a Meta-Analysis"                                                                    #
#                                                                                      #
########################################################################################

# This script calculates the meta analysis of Risk Ratios (RR) between FEP and healthy control for..
#
#  1. All abnormalities and clinically relevant abnormalities 

#
#
# All abnormalities
#
ab_RR_meta <- meta::metabin(fep_abnormal, fep_total, hc_abnormal, hc_total,
                      studlab = author_year, 
                      label.e = "FEP", label.c = "Controls",
                      fixed = FALSE, random = TRUE,
                      method.tau = "REML", 
                      sm = "RR", backtransf = TRUE,                           
                      data = data)
precision_ab_RR_meta <- ab_RR_meta$statistic


# clinically relevant abnormalities

cr_ab_RR_meta <- metabin(fep_cr_abnormal, fep_total, hc_cr_abnormal, hc_total,
                      studlab = author_year, 
                      label.e = "FEP", label.c = "Controls",
                      fixed = FALSE, random = TRUE,
                      method.tau = "REML", 
                      sm = "RR", backtransf = TRUE,                           
                      data = data)
precision_cr_ab_RR_meta <- cr_ab_RR_meta$statistic


###################  Leave one out (L1O) sensitivity analysis 

# all abnormalities
leave1out_ab_RR_meta <- metainf(ab_RR_meta, pooled="random", sortvar=precision_ab_RR_meta)
leave1out_ab_RR_meta
# forest(leave1out_ab_RR_meta, layout="JAMA") 

# clinically relevant abnormalities
leave1out_cr_ab_RR_meta <- metainf(cr_ab_RR_meta, pooled="random", sortvar=precision_cr_ab_RR_meta)
leave1out_cr_ab_RR_meta
# forest(leave1out_cr_ab_RR_meta, layout="JAMA") 
