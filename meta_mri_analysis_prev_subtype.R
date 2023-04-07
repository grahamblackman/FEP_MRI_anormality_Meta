########################################################################################
#                                                                                      #
# meta_mri_analysis_prev_subtype.R from Blackman et al                                 #
#                                                                                      #
# "Prevalence of neuroradiological abnormalities in First Episode Psychosis:           #
#  a Meta-Analysis"                                                                    #
#                                                                                      #
########################################################################################

#
# This script calculates the meta analysis of proportions of neuroanatomical subtypes...
#

# all abnormalities --------------------------------------------------------------------

# ventricular
data_subtype$fep_ventricular<-as.numeric(data_subtype$fep_ventricular)
(pes_fep_ventricular_summary=meta::metaprop(fep_ventricular, fep_total, author_year, data=data_subtype, sm="PFT",
                                           method.tau="DL", method.ci="NAsm"))

# white_matter [including Zannetti et al study]
data_subtype_all$fep_white_matter<-as.numeric(data_subtype_all$fep_white_matter)
(pes_fep_white_matter_summary<-meta::metaprop(fep_white_matter, fep_total, author_year, data=data_subtype_all, sm="PFT",
                                             method.tau="DL", method.ci="NAsm"))

# other
data_subtype$fep_other<-as.numeric(data_subtype$fep_other)
(pes_fep_other_summary=meta::metaprop(fep_other, fep_total, author_year, data=data_subtype, sm="PFT",
                                     method.tau="DL", method.ci="NAsm"))

# vascular
data_subtype$fep_vascular<-as.numeric(data_subtype$fep_vascular)
(pes_fep_vascular_summary=meta::metaprop(fep_vascular, fep_total, author_year, data=data_subtype, sm="PFT",
                                        method.tau="DL", method.ci="NAsm"))

# atrophy
data_subtype$fep_atrophy<-as.numeric(data_subtype$fep_atrophy)
(pes_fep_atrophy_summary=meta::metaprop(fep_atrophy, fep_total, author_year, data=data_subtype, sm="PFT",
                                       method.tau="DL", method.ci="NAsm"))

# cyst
data_subtype$fep_cyst<-as.numeric(data_subtype$fep_cyst)
(pes_fep_cyst_summary=meta::metaprop(fep_cyst, fep_total, author_year, data=data_subtype, sm="PFT",
                                    method.tau="DL", method.ci="NAsm"))

# tumour
data_subtype$fep_tumour<-as.numeric(data_subtype$fep_tumour)
(pes_fep_tumour_summary=meta::metaprop(fep_tumour, fep_total, author_year, data=data_subtype, sm="PFT",
                                      method.tau="DL", method.ci="NAsm"))

# pituitary
data_subtype$fep_pituitary<-as.numeric(data_subtype$fep_pituitary)
(pes_fep_pituitary_summary=meta::metaprop(fep_pituitary, fep_total, author_year, data=data_subtype, sm="PFT",
                                         method.tau="DL", method.ci="NAsm"))

# clinically relevant abnormalities --------------------------------------------------------------------

# white_matter [ Zannetti et al not included as does not report clinically relevant abnormalities]
data_cr_subtype$fep_cr_white_matter<-as.numeric(data_cr_subtype$fep_cr_white_matter)
(pes_fep_cr_white_matter_summary<-meta::metaprop(fep_cr_white_matter, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                             method.tau="DL", method.ci="NAsm"))

# vascular
data_cr_subtype$fep_cr_vascular<-as.numeric(data_cr_subtype$fep_cr_vascular)
(pes_fep_cr_vascular_summary=meta::metaprop(fep_cr_vascular, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                        method.tau="DL", method.ci="NAsm"))

# cyst
data_cr_subtype$fep_cr_cyst<-as.numeric(data_cr_subtype$fep_cr_cyst)
(pes_fep_cr_cyst_summary=meta::metaprop(fep_cr_cyst, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                    method.tau="DL", method.ci="NAsm"))

# tumour
data_cr_subtype$fep_cr_tumour<-as.numeric(data_cr_subtype$fep_cr_tumour)
(pes_fep_cr_tumour_summary=meta::metaprop(fep_cr_tumour, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                      method.tau="DL", method.ci="NAsm"))

# atrophy
data_cr_subtype$fep_cr_atrophy<-as.numeric(data_cr_subtype$fep_cr_atrophy)
(pes_fep_cr_atrophy_summary=meta::metaprop(fep_cr_atrophy, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                       method.tau="DL", method.ci="NAsm"))

# ventricular
data_cr_subtype$fep_cr_ventricular<-as.numeric(data_cr_subtype$fep_cr_ventricular)
(pes_fep_cr_ventricular_summary=meta::metaprop(fep_cr_ventricular, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                           method.tau="DL", method.ci="NAsm"))

# pituitary
data_cr_subtype$fep_cr_pituitary<-as.numeric(data_cr_subtype$fep_cr_pituitary)
(pes_fep_cr_pituitary_summary=meta::metaprop(fep_cr_pituitary, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                         method.tau="DL", method.ci="NAsm"))

# other
data_cr_subtype$fep_cr_other<-as.numeric(data_cr_subtype$fep_cr_other)
(pes_fep_cr_other_summary=meta::metaprop(fep_cr_other, fep_total, author_year, data=data_cr_subtype, sm="PFT",
                                     method.tau="DL", method.ci="NAsm"))
