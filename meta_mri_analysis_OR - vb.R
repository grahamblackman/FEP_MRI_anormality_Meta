########################################################################################
#                                                                                      #
# meta_mri_analysis_OR.R from Blackman et al                                         #
#                                                                                      #
# "How common are neuroradiological abnormalities in first-episode psychosis?          #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
########################################################################################

# This script calculates the meta analysis of odds ratios (OR) between FEP and healthy control for...

# all abnormalities
# subtypes (e.g vascular abnormalities) 
# Note: clinically relevant abnormalities not calculated as only one study reports sufficient data

# generates forests plots 

###################  

# key reference:
#   https://www.jstatsoft.org/article/view/v036i03

###################  

# key functions (within metafor package): 
#  escalc(): individual effect sizes and their corresponding sampling variances are estimated by fitting a meta-analytic model. One can decide whether to transform these effect sizes with the 'measure' argument. 
#  rma()= Function to fit the meta-analytic fixed- and random/mixed-effects models with or without moderators via linear (mixed-effects) models. See the documentation of the metafor-package for more details on these models.

# current queries:
# is the ouput the 'log odds ratio', if so, does this need to be transformed to 'odds ratio'
# are there sufficient studies to calc the OR for clinically relevant abnormalities? (to double check, but initial review suggests only 2  studies report this in HC)
# use metafor or metagen package for generating forest plot

###################  calculating OR effect sizes ( FEP vs HC) ###################  

# Input:
#  ai= event in experimental group
#  bi= non-event in experimental group
#  ci= event in control group 
#  di= non-event in control group
#  NB The log odds ratio is equal to the log of (ai*di)/(bi*ci). log transformation performed to make outcome measures symmetric around 0 and helps to make the distribution of these outcome measure
#  closer to normal

# Output: 
#  yi= effect size
#  vi= sample variance

# then pool the individual effect sizes and their sampling variances based on the inverse variance method with the rma() function. To do this, we can
# execute:

# general structure or IEF of OR in 
# data <- escalc(measure = "OR", ai = , bi = , ci = , di = , data = , append = TRUE)

# calc indivudual effect sizes -------------------------------------------

# all abnormalities
ies_or_fep_ab <- escalc(measure = "OR", ai = fep_abnormal, bi = fep_normal, ci = hc_abnormal, di = hc_normal, data = data, append = TRUE)


# abnormality subtypes

# ----> QUERY

# This was the original - that contained "di = hc_total-hc_normal"
#
#  ies_or_fep_wm <- escalc(measure = "OR", ai = fep_white_matter, bi = fep_total-fep_white_matter, ci = hc_whitematter, di = hc_total-hc_normal, data = data, append = TRUE) 
#

# I think this should be "di = hc_total-hc_whitematter" as below.
#
# Do check! I've written the rest of these following the same format so important to get right

# abnormality subtypes
ies_or_fep_wm <- escalc(measure = "OR", ai = fep_white_matter, bi = fep_total-fep_white_matter, ci = hc_whitematter, di = hc_total-hc_whitematter, data = data, append = TRUE) 
ies_or_fep_vasc <- escalc(measure = "OR", ai = fep_vascular, bi = fep_total-fep_vascular, ci = hc_vascular, di = hc_total-hc_vascular, data = data, append = TRUE) 
ies_or_fep_cyst <- escalc(measure = "OR", ai = fep_cyst, bi = fep_total-fep_cyst, ci = hc_cyst, di = hc_total-hc_cyst, data = data, append = TRUE) 
ies_or_fep_atrophy <- escalc(measure = "OR", ai = fep_atrophy, bi = fep_total-fep_atrophy, ci = hc_atrophy, di = hc_total-hc_atrophy, data = data, append = TRUE) 
ies_or_fep_tumour <- escalc(measure = "OR", ai = fep_tumour, bi = fep_total-fep_tumour, ci = hc_tumour, di = hc_total-hc_tumour, data = data, append = TRUE) 
ies_or_fep_ventricular <- escalc(measure = "OR", ai = fep_ventricular, bi = fep_total-fep_ventricular, ci = hc_ventricular, di = hc_total-hc_ventricular, data = data, append = TRUE) 
ies_or_fep_pituitary <- escalc(measure = "OR", ai = fep_pituitary, bi = fep_total-fep_pituitary, ci = hc_pituitary, di = hc_total-hc_pituitary, data = data, append = TRUE) 
ies_or_fep_other <- escalc(measure = "OR", ai = fep_other, bi = fep_total-fep_other, ci = hc_other, di = hc_total-hc_other, data = data, append = TRUE) 

#
# Pooled summary effect size (using random effects model) ----------------------
#

# all abnormalities
pes_or_fep_ab <- rma(yi, vi, data=ies_or_fep_ab, level=95) 

# abnormality subtypes
pes_or_fep_wm <- rma(yi, vi, data=ies_or_fep_wm, level=95) # WM abnormalities
pes_or_fep_vasc <- rma(yi, vi, data=ies_or_fep_vasc, level=95) # vasc abnormalities
pes_or_fep_cyst <- rma(yi, vi, data=ies_or_fep_cyst, level=95) # cyst abnormalities
pes_or_fep_atro <- rma(yi, vi, data=ies_or_fep_atrophy, level=95) # atrophy abnormalities
pes_or_fep_tum <- rma(yi, vi, data=ies_or_fep_tumour, level=95) # tumour abnormalities
pes_or_fep_vent <- rma(yi, vi, data=ies_or_fep_ventricular, level=95) # ventricular abnormalities
pes_or_fep_pit <- rma(yi, vi, data=ies_or_fep_pituitary, level=95) # pituitary abnormalities
pes_or_fep_oth <- rma(yi, vi, data=ies_or_fep_other, level=95) # other abnormalities

#
# Summary statistics -------------------------------------------------------
#

# all abnormalities
summary(pes_or_fep_ab) 

# abnormality subtypes
summary(pes_or_fep_wm) # WM abnormalities
summary(pes_or_fep_vasc) # vasc abnormalities
summary(pes_or_fep_cyst) # cyst abnormalities
summary(pes_or_fep_atro) # atrophy abnormalities 
summary(pes_or_fep_tum) # tumour abnormalities
summary(pes_or_fep_vent) # ventricular abnormalities
summary(pes_or_fep_pit) # pituitary abnormalities
summary(pes_or_fep_oth) # other abnormalities

#
# Forest plots (using metafor)------------------------------------------------
#

# all abnormalities
forest(pes_or_fep_ab, slab = paste(data$author_year)) 

# abnormality subtypes
forest(pes_or_fep_wm, slab = paste(data$author_year)) 
forest(pes_or_fep_vasc, slab = paste(data$author_year)) 
forest(pes_or_fep_cyst, slab = paste(data$author_year)) 
forest(pes_or_fep_atro, slab = paste(data$author_year)) 
forest(pes_or_fep_tum, slab = paste(data$author_year)) 
forest(pes_or_fep_vent, slab = paste(data$author_year)) 
forest(pes_or_fep_pit, slab = paste(data$author_year)) 
forest(pes_or_fep_oth, slab = paste(data$author_year)) 


