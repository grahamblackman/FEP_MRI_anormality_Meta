########################################################################################
#                                                                                      #
# meta_mri_analysis_prev.R from Blackman et al                                         #
#                                                                                      #
# "Prevalence of neuroradiological abnormalities in First Episode Psychosis:           #
#  a Meta-Analysis"                                                                    #
#                                                                                      #
########################################################################################

#
# This script calculates the meta analysis of proportions...
#
# Note:  meta::metaprop and metafor::rma functions are used for running meta analysis (rma is needed for moderator and leave one out
#       sensitivity analyses) 
#       
# ... and Eggers / trim and fill / funnel plot at the end of the script
#

# Useful article:
#  How to Conduct a Meta-Analysis of Proportions in R: A Comprehensive Tutorial

# other key resources:
#  "Conducting Meta-Analyses in R with the metafor Package, Wolfgang Viechtbauer" 
#  cran.r-project.org/web/packages/meta/meta.pdf
#  https://rpubs.com/pekong/532068
#  rdocumentation: metaprop

###################  calculating effect sizes

# key functions: 
# escalc()
# rma()= Function to fit the meta-analytic fixed- and random/mixed-effects models with or without moderators via linear (mixed-effects) models.
# See the documentation of the metafor-package for more details on these models.
# predict() functions.

# The escalc() function, individual effect sizes and their corresponding
# sampling variances are estimated by fitting a meta-analytic model. One can
# decide whether to transform these effect sizes with the measure= argument. In
# the case of a meta-analysis of proportions, this function has the following
# general format:
  
# general format: individual effect size = escalc(xi=cases, ni=total, data=dat, measure="PR"/"PLO"/"PFT")
# individual effect size = ies
# measure="PR" no transformation
# measure="PFT" double arcsine transformation

# Input:
#  xi= freq of event
#  ni = sample size

# Output: 
#  yi= effect size
#  vi= sample variance
#  then pool the individual effect sizes and their sampling variances based on
#  the inverse variance method with the rma() function. 

###################

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

# Convert to non-transformed measurement scale (i.e., proportion) and yield a true summary proportion

# Any abnormality
pes_fep_ab <- stats::predict(pes_da_fep_ab, transf = transf.ipft.hm, targ=list(ni = data$fep_total)) #class: list.rma
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
pes_fep_cr_ab <- stats::predict(pes_da_fep_cr_ab, transf=transf.ipft.hm, targ=list(ni=data_cr$fep_total)) #class: list.rma
summary(pes_fep_cr_ab)
print(pes_fep_cr_ab, digits=2)
prop_fep_cr_ab <- pes_fep_cr_ab$pred
per_fep_cr_ab <- label_percent()(prop_fep_cr_ab)
prop_LCI_fep_cr_ab <- pes_fep_cr_ab$ci.lb
prop_UCI_fep_cr_ab <- pes_fep_cr_ab$ci.ub
per_LCI_fep_cr_ab <- label_percent()(pes_fep_cr_ab$ci.lb)
per_UCI_fep_cr_ab <- label_percent()(pes_fep_cr_ab$ci.ub)
nns_fep_cr_ab <- (ceiling(1/pes_fep_cr_ab$pred)) #number needed to scan
nns_LCI_fep_cr_ab <- (ceiling(1/pes_fep_cr_ab$ci.ub)) #number needed to scan upper and lower estimate
nns_UCI_fep_cr_ab <- (ceiling(1/pes_fep_cr_ab$ci.lb))
isqu_fep_cr_ab <- pes_da_fep_cr_ab$I2 # NB based on transformed data [convert?]


# method= argument  which of the following between-study
# variance estimators will be used (the default method is REML): method="DL"
# random effects using the DerSimonian-Laird estimator method="REML" #random
# effects using the restricted maximum-likelihood estimator

# When a study contains proportions equal to 0, R will automatically add 0.5 to the observed data (i.e., the number of event of interest, namely the cases variable). Since the double-arcsine transformation does not require any adjustments to be made to
# the data in such a situation, we can explicitly switch add=0.5 to add=0 to stop the default adjustment. Returning to the running example, the summary proportion is generated using option 2
# (i.e., the logit transformation) on the grounds that all of the observed proportions in the dataset are far below 0.2 and there are no zero events. Thus, we would execute: ies.logit=escalc(xi=cases, ni=total, measure="PLO", data=dat) pes.logit=rma(yi, vi, data=ies.logit, method="DL", level=95)
# pes=predict(pes.logit, transf=transf.ilogit)

# digits=specifies the number of decimal places to which the printed results should be rounded (the default is 4). 


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


###################  meta analysis subgroup analysis [using metaprop]

# by recruitment type -----------------------------------------------------

# any abnormality
pes_fep_ab_summary_by_recruitment <- meta::metaprop(fep_abnormal, fep_total, author_year, data=data, sm="PFT",
                                                 method.tau="DL", method.ci="NAsm", subgroup=recruitment) 
pes_fep_ab_summary_by_recruitment

# clinically relevant abnormalities
pes_fep_cr_ab_summary_by_recruitment <- meta::metaprop(fep_cr_abnormal, fep_total, author_year, data=data_cr, sm="PFT",
                                                    method.tau="DL", method.ci="NAsm", subgroup=recruitment) 
pes_fep_cr_ab_summary_by_recruitment

# by field strength -------------------------------------------------------

# any abnormality
pes_fep_ab_summary_by_field <- meta::metaprop(fep_abnormal, fep_total, author_year, data=data, sm="PFT",
                                                 method.tau="DL", method.ci="NAsm", subgroup=scan_field_3T) 
pes_fep_ab_summary_by_field

# clinically relevant abnormalities
pes_fep_cr_ab_summary_by_field <- meta::metaprop(fep_cr_abnormal, fep_total, author_year, data=data, sm="PFT",
                                           method.tau="DL", method.ci="NAsm", subgroup=scan_field_3T, na.rm = TRUE) 
pes_fep_cr_ab_summary_by_field

# by rater ----------------------------------------------------------------

# any abnormality
pes_fep_ab_summary_by_neurorad <- meta::metaprop(fep_abnormal, fep_total, author_year, data=data, sm="PFT",
                                           method.tau="DL", method.ci="NAsm", subgroup=neurorad_binary)
pes_fep_ab_summary_by_neurorad

# clinically relevant abnormalities
pes_fep_cr_ab_summary_by_neurorad <- meta::metaprop(fep_cr_abnormal, fep_total, author_year, data=data_cr, sm="PFT",
                                              method.tau="DL", method.ci="NAsm", subgroup=neurorad_binary)
pes_fep_cr_ab_summary_by_neurorad

###################  outlier study detection 

# identify studentized residuals larger than 2 (or 3 if enough studies) 
# R comment: 'rstudent' function requires object of class "rma". multi-array average (RMA)

stud.res <- rstudent(pes_da_fep_ab) # calc Residual Values (only works on DA transformation - need to convert back?)
abs.z <- abs(stud.res$z) # standardized residuals (externally standardized for rstudent).
stud.res[order(-abs.z)] # ordered


###################  influential (outlier) study detection 

# resources:
#  Viechtbauer, Wolfgang, and Mike W-L Cheung. 2010. “Outlier and Influence Diagnostics for Meta-Analysis.” Research Synthesis Methods 1 (2). Wiley Online Library: 112–25.
#  Baujat, Bertrand, Cédric Mahé, Jean-Pierre Pignon, and Catherine Hill. 2002. “A Graphical Method for Exploring Heterogeneity in Meta-Analyses: Application to a Meta-Analysis of 65 Trials.” Statistics in Medicine 21 (18). Wiley Online Library: 2641–52.

# Leave one out (L1O) sensitivity analysis 

####  any abnormality
# L1O based on 'metafor::leave1out' function 
leave1out_fep_ab <- leave1out(pes_da_fep_ab, digits = 2) #  based on DA transformation)
print(leave1out_fep_ab)

#  redundent
# influence_fep_ab <- influence(pes_da_fep_ab)
# print(influence_fep_ab) #  [asterix for influential cases -   based on 1 o 4 criteria]

# L1O based on 'meta::metainf' function
precision <- sqrt(ies_da_fep_ab$vi) 
leave1out_FEP_ab <- metainf(pes_fep_ab_summary, pooled="random") 
leave1out_FEP_ab
# leave1out_fep_ab <- forest(metainf(pes_fep_ab_summary, pooled="random", sortvar=precision), layout="JAMA") 

####  clinically relevant abnormalities

# L1O based on 'meta::metainf' function
precision_cr <- sqrt(ies_da_fep_cr_ab$vi) 
leave1out_FEP_cr_ab <- metainf(pes_fep_cr_ab_summary, pooled="random") 
leave1out_FEP_cr_ab
#leave1out_fep_cr_ab <- forest(metainf(pes_fep_cr_ab_summary, pooled="random", sortvar=precision_cr), layout="JAMA") 

# #  redundent
# #L1O based on 'metafor::leave1out' function 
# leave1out_fep_cr_ab <- leave1out(pes_da_fep_cr_ab, digits = 2) # works nb based on DA transformation)
# print(leave1out_fep_cr_ab)
# forest(x =leave1out_fep_cr_ab$estimate, ci.lb = leave1out_fep_cr_ab$ci.lb, ci.ub = leave1out_fep_cr_ab$ci.ub)
# 
# #  redundent
# influence_fep_cr_ab <- influence(pes_da_fep_cr_ab)
# print(influence_fep_cr_ab) #  asterix for influential cases -   based on 1 o 4 criteria (see help page for details)

#################### Rerun meta analysis excluding outliers:  

# any abnormality [redundent - no outliner]
# pes_fep_ab_noutlier_summary <- meta::metaprop(fep_abnormal, fep_total, author_year, data=data[-c(6),], sm="PFT",
#                                   method.tau="DL", method.ci="NAsm")
# #forest(pes_fep_ab_noutlier_summary, layout="JAMA")

## clinically relevant abnormalities


########## Explaining heterogeneity [subgroup and meta regression]
  

########### subgroup analysis 

## rater type: radiologist versus non-radiologist

# all abnormalities [redundent]
# updateforest_fep_ab_rater<-update.meta(pes_fep_ab_summary, subgroup = all_rater_radiol_bin)
# updateforest_fep_ab_rater 

# clinically relevant abnormalities  [redundant; unbalanced]
updateforest_fep_cr_ab_rater <- update.meta(pes_fep_cr_ab_summary, subgroup = all_rater_radiol_bin)
updateforest_fep_cr_ab_rater 

## research verses clinical

# all abnormalities [redundent]
# updateforest_fep_ab_recruitment <- update.meta(pes_fep_ab_summary, subgroup = recruitment)
# updateforest_fep_ab_recruitment 

# clinically relevant abnormalities
updateforest_fep_cr_ab_recruitment <- update.meta(pes_fep_cr_ab_summary, subgroup = recruitment)
updateforest_fep_cr_ab_recruitment 


## scan resolution: 3.0T or < 3.0T

# all abnormalities
updateforest_fep_ab_scanresolution <- update.meta(pes_fep_ab_summary, subgroup = scan_field_3T)
updateforest_fep_ab_scanresolution 

# clinically relevant abnormalities
updateforest_fep_cr_ab_scanresolution <- update.meta(pes_fep_cr_ab_summary, subgroup = scan_field_3T)
updateforest_fep_cr_ab_scanresolution


## brain scan: whole vs restricted brain 

# all abnormalities
updateforest_fep_ab_brain <- update.meta(pes_fep_ab_summary, subgroup = whole_brain_binary)
updateforest_fep_ab_brain 

# clinically relevant abnormalities [redun]
# updateforest_fep_cr_ab_brain <- update.meta(pes_fep_cr_ab_summary, subgroup = whole_brain_binary) 
# updateforest_fep_cr_ab_brain

########## meta-regression: continuous variables ########## 

# metareg: mean age any abnormality [redundent]
# metareg_age_fep_ab <- rma(yi, vi, data = ies_da_fep_ab, mods = ~age_fep, method="DL")
# print(metareg_age_fep_ab)
# metareg_pval_age_fep_ab <- metareg_age_fep_ab$pval[2]
# metareg_pval_age_fep_ab

# metareg: mean age clinically relevant abnormalities
metareg_age_fep_cr_ab <- rma(yi, vi, data = ies_da_fep_cr_ab, mods = ~age_fep, method="DL")
print(metareg_age_fep_cr_ab)
metareg_pval_age_fep_cr_ab <- metareg_age_fep_cr_ab$pval[2]
metareg_pval_age_fep_cr_ab

# metareg: year any abnormality [redundent]
# metareg_year_fep_ab <- rma(yi, vi, data = ies_da_fep_ab, mods = ~year, method="DL")
# print(metareg_year_fep_ab)
# metareg_pval_year_fep_ab <- metareg_year_fep_ab$pval[2]
# metareg_pval_year_fep_ab

# metareg: year clinically relevant abnormalities
metareg_year_fep_cr_ab <- rma(yi, vi, data = ies_da_fep_cr_ab, mods = ~year, method="DL")
print(metareg_year_fep_cr_ab)
metareg_pval_year_fep_cr_ab <- metareg_year_fep_cr_ab$pval[2]
metareg_pval_year_fep_cr_ab

# metareg: psychosis duration any abnormality [redundent]
# metareg_psychdur_fep_ab <- rma(yi, vi, data = ies_da_fep_ab, mods = ~psychosis_duration_wks, method="DL")
# print(metareg_psychdur_fep_ab)
# metareg_pval_psychdur_fep_ab <- metareg_psychdur_fep_ab$pval[2]
# metareg_pval_psychdur_fep_ab

# metareg: psychosis duration clinically relevant abnormalities [redundent: insufficient studies]
metareg_psychdur_fep_cr_ab <- rma(yi, vi, data = ies_da_fep_cr_ab, mods = ~psychosis_duration_wks, method = "DL")
print(metareg_psychdur_fep_cr_ab)
metareg_pval_psychdur_fep_cr_ab <- metareg_psychdur_fep_cr_ab$pval[2]


########## sensitivity analyses (assessment of robustness) ########## 

# Studies with a mean patient age above 35 years: excluded 2 studies --- 10 studies included
exc_over35_fep_ab <- update.meta(pes_fep_ab_summary, exclude = pes_fep_ab_summary$data$age_under_35 < 1)
print(exc_over35_fep_ab)
# forest_exc_over35_fep_ab <- forest(exc_over35_fep_ab)

exc_over35_fep_ab <- update.meta(pes_fep_cr_ab_summary, exclude = pes_fep_cr_ab_summary$data$age_under_35 < 1)
print(exc_over35_fep_ab)
# forest_exc_over35_fep_ab <- forest(exc_over35_fep_ab)


# studies that performed radiological assessment on restricted brain regions: excluded 4 studies --- 8 studies included
exc_restrictbrain_fep_ab <- update.meta(pes_fep_ab_summary, exclude = pes_fep_ab_summary$data$whole_brain_binary < 1)
print(exc_restrictbrain_fep_ab)
# forest_exc_restrictbrain_fep_ab <- forest(exc_restrictbrain_fep_ab) #excluded studies still show in plot but not accounted in meta-analysis

# studies that screened out patients with evidence of a secondary cause of psychosis (e.g. abnormal neurological exam): excluded 2 studies --- 10 studies included
exc_organicnr_fep_ab <- update.meta(pes_fep_ab_summary, exclude = pes_fep_ab_summary$data$organic_exclude_binary < 1)
print(exc_organicnr_fep_ab)
# forest_exc_organicnr_fep_ab <- forest(exc_organicnr_fep_ab)  #excluded studies still show in plot but not accounted in meta-analysis

# studies where MRI assessment was not performed by a radiologist:  excluded 3 studies --- 9 studies included
exc_raterrad_fep_ab <- update.meta(pes_fep_ab_summary, exclude = pes_fep_ab_summary$data$all_rads_binary < 1)
print(exc_raterrad_fep_ab)
# forest_exc_raterrad_fep_ab <- forest(exc_raterrad_fep_ab)  #excluded studies still show in plot but not accounted in meta-analysis

# SA not performed:
#studies of poor quality: No study has low RoB
# Leave-one-out sensitivity analysis (see section: influential study detection) 


########## publication bias  ########## 

# Egger's regression test (Regression Test for Funnel Plot Asymmetry)

# all abnormalities
eggers_reg_fep_ab <- regtest(pes_da_fep_ab, model="rma", predictor="sei")
print(eggers_reg_fep_ab)

# clinically relevant abnormalities
eggers_reg_fep_cr_ab <- regtest(pes_da_fep_cr_ab, model="rma", predictor="sei")
print(eggers_reg_fep_cr_ab)


# trim and fill: correction for publication bias (see Duval and Tweedie, 2000) 

trimfill_fep_ab <- trimfill(pes_fep_ab_summary)
trimfill_fep_ab
#funnel(trimfill_fep_ab)
#forest(trimfill_fep_ab)


trimfill_fep_cr_ab <- trimfill(pes_fep_cr_ab_summary)
trimfill_fep_cr_ab
#funnel(trimfill_fep_cr_ab)
#forest(trimfill_fep_cr_ab)

