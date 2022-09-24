########################################################################################
#                                                                                      #
# meta_mri_analysis_RR.R from Blackman et al                                           #
#                                                                                      #
# "How common are neuroradiological abnormalities in first-episode psychosis?          #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
########################################################################################

# This script calculates the meta analysis of Risk Ratios (RR) between FEP and healthy control for...
#
#  1. All abnormalities
#  2. Abnormality subtypes (e.g vascular abnormalities) 
#
# Note: clinically relevant abnormalities not calculated as only one study reports sufficient data

#
# All abnormalities
#

ab_RR_meta <- metabin(fep_abnormal, fep_total, hc_abnormal, hc_total,
                      studlab = author_year, 
                      label.e = "FEP", label.c = "Controls",
                      fixed = FALSE, random = TRUE,
                      method.tau = "REML", 
                      sm = "RR", #backtransf = FALSE,                           
                      data = data)
forest(ab_RR_meta, layout="meta")

#
# Abnormality subtypes
#

# White matter
wm_RR_meta <- metabin(fep_white_matter, fep_total, hc_whitematter, hc_total,
                      studlab = author_year, 
                      label.e = "FEP", label.c = "Controls",
                      fixed = FALSE, random = TRUE,
                      method.tau = "REML", 
                      sm = "RR",
                      data = data)
forest(wm_RR_meta, layout="meta")

# Vascular
vasc_RR_meta <- metabin(fep_vascular, fep_total, hc_vascular, hc_total,
                      studlab = author_year, 
                      label.e = "FEP", label.c = "Controls",
                      fixed = FALSE, random = TRUE,
                      method.tau = "REML", 
                      sm = "RR",
                      data = data)
forest(vasc_RR_meta, layout="meta")

# Cyst
cyst_RR_meta <- metabin(fep_cyst, fep_total, hc_cyst, hc_total,
                        studlab = author_year, 
                        label.e = "FEP", label.c = "Controls",
                        fixed = FALSE, random = TRUE,
                        method.tau = "REML", 
                        sm = "RR",
                        data = data)
forest(cyst_RR_meta, layout="meta")

# Atrophy
atrophy_RR_meta <- metabin(fep_atrophy, fep_total, hc_atrophy, hc_total,
                        studlab = author_year, 
                        label.e = "FEP", label.c = "Controls",
                        fixed = FALSE, random = TRUE,
                        method.tau = "REML", 
                        sm = "RR",
                        data = data)
forest(atrophy_RR_meta, layout="meta")

# Tumour
tumour_RR_meta <- metabin(fep_tumour, fep_total, hc_tumour, hc_total,
                           studlab = author_year, 
                           label.e = "FEP", label.c = "Controls",
                           fixed = FALSE, random = TRUE,
                           method.tau = "REML", 
                           sm = "RR",
                           data = data)
forest(tumour_RR_meta, layout="meta")

# Ventricular
ventricular_RR_meta <- metabin(fep_ventricular, fep_total, hc_ventricular, hc_total,
                          studlab = author_year, 
                          label.e = "FEP", label.c = "Controls",
                          fixed = FALSE, random = TRUE,
                          method.tau = "REML", 
                          sm = "RR",
                          data = data)
forest(ventricular_RR_meta, layout="meta")

# Pituitary
pituitary_RR_meta <- metabin(fep_pituitary, fep_total, hc_pituitary, hc_total,
                               studlab = author_year, 
                               label.e = "FEP", label.c = "Controls",
                               fixed = FALSE, random = TRUE,
                               method.tau = "REML", 
                               sm = "RR",
                               data = data)
forest(pituitary_RR_meta, layout="meta")

# Other
other_RR_meta <- metabin(fep_other, fep_total, hc_other, hc_total,
                             studlab = author_year, 
                             label.e = "FEP", label.c = "Controls",
                             fixed = FALSE, random = TRUE,
                             method.tau = "REML", 
                             sm = "RR",
                             data = data)
forest(other_RR_meta, layout="meta")
