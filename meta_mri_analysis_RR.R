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
#  2. Abnormality subtypes (e.g vascular abnormalities) 
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


#---------------- [draft: L10 RR]
leave1out_ab_RR_meta <- metainf(ab_RR_meta, pooled="random", sortvar=precision_ab_RR_meta)
leave1out_ab_RR_meta
#forest(leave1out_ab_RR_meta)

leave1out_cr_ab_RR_meta <- metainf(cr_ab_RR_meta, pooled="random", sortvar=precision_cr_ab_RR_meta)
leave1out_cr_ab_RR_meta
#forest(leave1out_cr_ab_RR_meta)

#---------------- 


# subtypes: not  reported in manuscript --------------------------

# White matter
wm_RR_meta <- metabin(fep_white_matter, fep_total, hc_whitematter, hc_total,
                      studlab = author_year, 
                      label.e = "FEP", label.c = "Controls",
                      fixed = FALSE, random = TRUE,
                      method.tau = "REML", 
                      sm = "RR",
                      data = data)
wm_RR_meta
#forest(wm_RR_meta, layout="meta")

# Vascular
vasc_RR_meta <- metabin(fep_vascular, fep_total, hc_vascular, hc_total,
                      studlab = author_year, 
                      label.e = "FEP", label.c = "Controls",
                      fixed = FALSE, random = TRUE,
                      method.tau = "REML", 
                      sm = "RR",
                      data = data)
vasc_RR_meta
#forest(vasc_RR_meta, layout="meta")

# Cyst
cyst_RR_meta <- metabin(fep_cyst, fep_total, hc_cyst, hc_total,
                        studlab = author_year, 
                        label.e = "FEP", label.c = "Controls",
                        fixed = FALSE, random = TRUE,
                        method.tau = "REML", 
                        sm = "RR",
                        data = data)
cyst_RR_meta
#forest(cyst_RR_meta, layout="meta")

# Atrophy
atrophy_RR_meta <- metabin(fep_atrophy, fep_total, hc_atrophy, hc_total,
                        studlab = author_year, 
                        label.e = "FEP", label.c = "Controls",
                        fixed = FALSE, random = TRUE,
                        method.tau = "REML", 
                        sm = "RR",
                        data = data)
atrophy_RR_meta
#forest(atrophy_RR_meta, layout="meta")

# Tumour
tumour_RR_meta <- metabin(fep_tumour, fep_total, hc_tumour, hc_total,
                           studlab = author_year, 
                           label.e = "FEP", label.c = "Controls",
                           fixed = FALSE, random = TRUE,
                           method.tau = "REML", 
                           sm = "RR",
                           data = data)
tumour_RR_meta
#forest(tumour_RR_meta, layout="meta")

# Ventricular
ventricular_RR_meta <- metabin(fep_ventricular, fep_total, hc_ventricular, hc_total,
                          studlab = author_year, 
                          label.e = "FEP", label.c = "Controls",
                          fixed = FALSE, random = TRUE,
                          method.tau = "REML", 
                          sm = "RR",
                          data = data)
ventricular_RR_meta
#forest(ventricular_RR_meta, layout="meta")

# Pituitary
pituitary_RR_meta <- metabin(fep_pituitary, fep_total, hc_pituitary, hc_total,
                               studlab = author_year, 
                               label.e = "FEP", label.c = "Controls",
                               fixed = FALSE, random = TRUE,
                               method.tau = "REML", 
                               sm = "RR",
                               data = data)
pituitary_RR_meta
#forest(pituitary_RR_meta, layout="meta")

# Other
other_RR_meta <- metabin(fep_other, fep_total, hc_other, hc_total,
                             studlab = author_year, 
                             label.e = "FEP", label.c = "Controls",
                             fixed = FALSE, random = TRUE,
                             method.tau = "REML", 
                             sm = "RR",
                             data = data)
other_RR_meta
#forest(other_RR_meta, layout="meta")
