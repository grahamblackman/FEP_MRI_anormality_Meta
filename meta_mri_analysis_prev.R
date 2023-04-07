########################################################################################
#                                                                                      #
# meta_mri_analysis_prev.R from Blackman et al                                         #
#                                                                                      #
# "Prevalence of neuroradiological abnormalities in First Episode Psychosis:           #
#  a Meta-Analysis"                                                                    #
#                                                                                      #
########################################################################################

#
# This script calculates:  meta analysis of proportions, subgroup,  meta regression, leave one out ensitivity analysis, other sensitivity analyses and Eggers test

# # Note:  meta::metaprop and metafor::rma functions are used for running meta analysis (rma is needed for moderator and leave one out
#       sensitivity analyses) 

################### meta analysis 

# Calculate effect sizes and sampling variance for proportion of those with any abnormality compared to
# total sample using Freeman-Tukey double arcsine transformed proportion
ies_da_fep_ab <- escalc(xi = fep_abnormal, ni = fep_total, data = data, measure="PFT", add=0) 

# Calculate effect size and sampling variance for proportion of those with any clinically relevant
# abnormality and total sample using Freeman-Tukey double arcsine transformed proportion
ies_da_fep_cr_ab <- escalc(xi = fep_cr_abnormal, ni = fep_total, data = data_cr, measure="PFT", add=0)

# Calculate pooled effect size for overall proportion of those with any abnormality compared to total sample
# using transformed double-arcsine transformed summary effect size and random effects model using DL estimator
pes_da_fep_ab <- rma(yi, vi, data = ies_da_fep_ab, method = "DL", level = 95) # any abnormality

# Calculate pooled effect size for overall proportion of those with any clinically relevant abnormality
# compared to total sample using transformed double-arcsine transformed summary effect size
# and random effects model using DL estimator
pes_da_fep_cr_ab <- rma(yi, vi, data = ies_da_fep_cr_ab, method = "DL", level = 95) # clinically relevant abnormalities

# Any abnormality
pes_fep_ab <- stats::predict(pes_da_fep_ab, transf = transf.ipft.hm, targ=list(ni = data$fep_total)) #
summary(pes_fep_ab)
print(pes_fep_ab, digits=2)
prop_fep_ab <- pes_fep_ab$pred
per_fep_ab <- label_percent()(prop_fep_ab)
prop_LCI_fep_ab <- pes_fep_ab$ci.lb
prop_UCI_fep_ab <- pes_fep_ab$ci.ub
per_LCI_fep_ab <- label_percent()(pes_fep_ab$ci.lb)
per_UCI_fep_ab <- label_percent()(pes_fep_ab$ci.ub)
nns_fep_ab <- (ceiling(1/pes_fep_ab$pred)) #number needed to scan
nns_LCI_fep_ab <- (ceiling(1/pes_fep_ab$ci.ub)) #number needed to scan upper and lower estimate
nns_UCI_fep_ab <- (ceiling(1/pes_fep_ab$ci.lb))
isqu_fep_ab <- pes_da_fep_ab$I2 # NB based on transformed data

# Clinically relevant abnormalities
pes_fep_cr_ab <- stats::predict(pes_da_fep_cr_ab, transf=transf.ipft.hm, targ=list(ni=data_cr$fep_total)) 
summary(pes_fep_cr_ab)
print(pes_fep_cr_ab, digits=2)
prop_fep_cr_ab <- pes_fep_cr_ab$pred
per_fep_cr_ab <- label_percent()(prop_fep_cr_ab)
prop_LCI_fep_cr_ab <- pes_fep_cr_ab$ci.lb
prop_UCI_fep_cr_ab <- pes_fep_cr_ab$ci.ub
per_LCI_fep_cr_ab <- label_percent()(pes_fep_cr_ab$ci.lb)
per_UCI_fep_cr_ab <- label_percent()(pes_fep_cr_ab$ci.ub)
nns_fep_cr_ab <- (ceiling(1/pes_fep_cr_ab$pred)) 
nns_LCI_fep_cr_ab <- (ceiling(1/pes_fep_cr_ab$ci.ub)) 
nns_UCI_fep_cr_ab <- (ceiling(1/pes_fep_cr_ab$ci.lb))
isqu_fep_cr_ab <- pes_da_fep_cr_ab$I2 


################### meta analysis of proportions [ metaprop  function]

# Description: Calculation of an overall proportion from studies reporting a single proportion. Inverse variance method and generalised linear mixed model (GLMM) are available for pooling. For GLMMs, the rma.glmm function from R package metafor (Viechtbauer 2010) is called internally.

# This is redundant given the same results reported above but included for completeness and cross-checking

# Arguments
# sm="PFT",  [summary measure = Freeman-Tukey Double arcsine transformation]
# method.tau="DL" [method is used to estimate the between-study variance= DerSimonian-Laird estimator  ]
# method.ci="WS" [method  used to calculate confidence = wlson score ]

pes_fep_ab_summary <- meta::metaprop(fep_abnormal, fep_total, author_year, data=data, sm="PFT",
                                     method.tau="DL", method.ci="WS")
pes_fep_ab_summary

pes_fep_cr_ab_summary <- meta::metaprop(fep_cr_abnormal, fep_total, author_year, data=data_cr, sm="PFT", 
                                        method.tau="DL", method.ci="WS")
pes_fep_cr_ab_summary

###################  subgroup analysis

# by sample type  -----------------------------------------------------

# sample: Clinical vs Research

# clinically relevant abnormalities
pes_fep_cr_ab_summary_by_recruitment <- meta::metaprop(fep_cr_abnormal, fep_total, author_year, data=data_cr, sm="PFT",
                                                    method.tau="DL", method.ci="NAsm", subgroup=recruitment) 
pes_fep_cr_ab_summary_by_recruitment


# by field strength -------------------------------------------------------

## scan resolution: 3.0T or < 3.0T

# clinically relevant abnormalities
pes_fep_cr_ab_summary_by_field <- meta::metaprop(fep_cr_abnormal, fep_total, author_year, data=data, sm="PFT",
                                           method.tau="DL", method.ci="NAsm", subgroup=scan_field_3T, na.rm = TRUE) 
pes_fep_cr_ab_summary_by_field

# by rater ----------------------------------------------------------------

## rater type: neuro-radiologist versus neuro-radiologist

# clinically relevant abnormalities
pes_fep_cr_ab_summary_by_neurorad <- meta::metaprop(fep_cr_abnormal, fep_total, author_year, data=data_cr, sm="PFT",
                                              method.tau="DL", method.ci="NAsm", subgroup=neurorad_binary)
pes_fep_cr_ab_summary_by_neurorad


########## meta-regression: continuous variables ########## 

# metareg: mean age clinically relevant abnormalities
metareg_age_fep_cr_ab <- rma(yi, vi, data = ies_da_fep_cr_ab, mods = ~age_fep, method="DL")
print(metareg_age_fep_cr_ab)
metareg_pval_age_fep_cr_ab <- metareg_age_fep_cr_ab$pval[2]
metareg_pval_age_fep_cr_ab

# metareg: year clinically relevant abnormalities
metareg_year_fep_cr_ab <- rma(yi, vi, data = ies_da_fep_cr_ab, mods = ~year, method="DL")
print(metareg_year_fep_cr_ab)
metareg_pval_year_fep_cr_ab <- metareg_year_fep_cr_ab$pval[2]
metareg_pval_year_fep_cr_ab

# metareg: psychosis duration clinically relevant abnormalities [not performed, insufficient studies]

###################  Leave one out (L1O) sensitivity analysis 

####  all abnormalities

precision <- sqrt(ies_da_fep_ab$vi) 
leave1out_FEP_ab <- metainf(pes_fep_ab_summary, pooled="random") 
leave1out_FEP_ab
leave1out_fep_ab <- forest(metainf(pes_fep_ab_summary, pooled="random", sortvar=precision), layout="JAMA") 

####  clinically relevant abnormalities

precision_cr <- sqrt(ies_da_fep_cr_ab$vi) 
leave1out_FEP_cr_ab <- metainf(pes_fep_cr_ab_summary, pooled="random") 
leave1out_FEP_cr_ab
leave1out_fep_cr_ab <- forest(metainf(pes_fep_cr_ab_summary, pooled="random", sortvar=precision_cr), layout="JAMA") 


########## other sensitivity analyses  ########## 

# Studies with a mean patient age above 35 years:
exc_over35_fep_ab <- update.meta(pes_fep_ab_summary, exclude = pes_fep_ab_summary$data$age_under_35 < 1)
print(exc_over35_fep_ab)

# studies that performed radiological assessment on restricted brain regions: 
exc_restrictbrain_fep_ab <- update.meta(pes_fep_ab_summary, exclude = pes_fep_ab_summary$data$whole_brain_binary < 1)
print(exc_restrictbrain_fep_ab)

# studies that screened out patients with evidence of a secondary cause of psychosis (e.g. abnormal neurological exam):
exc_organicnr_fep_ab <- update.meta(pes_fep_ab_summary, exclude = pes_fep_ab_summary$data$organic_exclude_binary < 1)
print(exc_organicnr_fep_ab)

# studies where MRI assessment was not performed by a radiologist:  
exc_raterrad_fep_ab <- update.meta(pes_fep_ab_summary, exclude = pes_fep_ab_summary$data$all_rads_binary < 1)
print(exc_raterrad_fep_ab)


########## publication bias  ########## 

# Egger's regression test (Regression Test for Funnel Plot Asymmetry)

# all abnormalities
eggers_reg_fep_ab <- regtest(pes_da_fep_ab, model="rma", predictor="sei")
print(eggers_reg_fep_ab)

# clinically relevant abnormalities
eggers_reg_fep_cr_ab <- regtest(pes_da_fep_cr_ab, model="rma", predictor="sei")
print(eggers_reg_fep_cr_ab)

