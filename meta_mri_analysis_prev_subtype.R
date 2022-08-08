########################################################################################
#                                                                                      #
# meta_mri_analysis_prev_subtype.R from Blackman et al                                         #
#                                                                                      #
# "How common are neuroradiological abnormalities in first-episode psychosis?          #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
########################################################################################

#
# This script calculates the meta analysis of proportions of neuroanatomical subtypes...
#
# Note: it uses both rma and metaprop because rma is needed for moderator and leave one out
#       sensitivity analyses 
#       
# ... and Eggers / trim and fill / funnel plot at the end of the script
#

# meta analysis of subtype of abnormalities

# NB:
# back transformation of Freeman-Tukey (double arcsine) does not appear to occur
#see archive for long hand version 
#notes made on back transformation
#see visualisation for figures
#-  Inverse of the Freeman-Tukey (double arcsine) transformation for proportions using the harmonic mean of the sample sizes for the back-transformation [transf.ipft.hm].
#-  link: https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2017-July/000028.html

# all abnormalities


# all abnormalities --------------------------------------------------------------------

#ventricular
pes_fep_ventricular_summary=meta::metaprop(fep_ventricular, fep_total, author_year, data=data_subtype, sm="PFT",
                                           method.tau="DL", method.ci="NAsm")
#white_matter [include Zannetti]
pes_fep_white_matter_summary<-meta::metaprop(fep_white_matter, fep_total, author_year, data=data_subtype_all, sm="PFT",
                                             method.tau="DL", method.ci="NAsm")
#other
pes_fep_other_summary=meta::metaprop(fep_other, fep_total, author_year, data=data_subtype, sm="PFT",
                                     method.tau="DL", method.ci="NAsm")
#vascular
pes_fep_vascular_summary=meta::metaprop(fep_vascular, fep_total, author_year, data=data_subtype, sm="PFT",
                                        method.tau="DL", method.ci="NAsm")
#atrophy
pes_fep_atrophy_summary=meta::metaprop(fep_atrophy, fep_total, author_year, data=data_subtype, sm="PFT",
                                       method.tau="DL", method.ci="NAsm")

#cyst
pes_fep_cyst_summary=meta::metaprop(fep_cyst, fep_total, author_year, data=data_subtype, sm="PFT",
                                    method.tau="DL", method.ci="NAsm")
#tumour
pes_fep_tumour_summary=meta::metaprop(fep_tumour, fep_total, author_year, data=data_subtype, sm="PFT",
                                      method.tau="DL", method.ci="NAsm")

#pituitary
pes_fep_pituitary_summary=meta::metaprop(fep_pituitary, fep_total, author_year, data=data_subtype, sm="PFT",
                                         method.tau="DL", method.ci="NAsm")


# clinically relevant abnormalities --------------------------------------------------------------------


#white_matter [check: inc Zannetti?]
pes_fep_cr_white_matter_summary<-meta::metaprop(fep_cr_white_matter, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                             method.tau="DL", method.ci="NAsm")
#vascular
pes_fep_cr_vascular_summary=meta::metaprop(fep_cr_vascular, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                        method.tau="DL", method.ci="NAsm")

#cyst
pes_fep_cr_cyst_summary=meta::metaprop(fep_cr_cyst, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                    method.tau="DL", method.ci="NAsm")
#tumour
pes_fep_cr_tumour_summary=meta::metaprop(fep_cr_tumour, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                      method.tau="DL", method.ci="NAsm")
#atrophy
pes_fep_cr_atrophy_summary=meta::metaprop(fep_cr_atrophy, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                       method.tau="DL", method.ci="NAsm")
#ventricular
pes_fep_cr_ventricular_summary=meta::metaprop(fep_cr_ventricular, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                           method.tau="DL", method.ci="NAsm")
#pituitary
pes_fep_cr_pituitary_summary=meta::metaprop(fep_cr_pituitary, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                         method.tau="DL", method.ci="NAsm")
#other
pes_fep_cr_other_summary=meta::metaprop(fep_cr_other, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                     method.tau="DL", method.ci="NAsm")
