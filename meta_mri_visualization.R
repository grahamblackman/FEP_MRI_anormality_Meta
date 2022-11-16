########################################################################################
#                                                                                      #
# meta_mri_visualization.R from Blackman et al                                         #
#                                                                                      #
# "How common are neuroradiological abnormalities in first-episode psychosis?          #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
########################################################################################

#
# This script visualizes the results of the meta-analysis presented in the paper and
# related analyses
#


############# Figure 1a. Forest plots: any abnormality ###############


# general comments:
##  see meta package to explore more useful arguments to customize forest plots.
## seems that double-arcsine transformed not performed using this approach 

# all abnormalities
pes_fep_ab_summary <- meta::metaprop(fep_abnormal, fep_total, author_year, data=data, sm="PFT",
                                  method.tau="DL", method.ci="WS")

# - Inverse variance method
# - DerSimonian-Laird estimator for tau^2
# - Jackson method for confidence interval of tau^2 and tau
# - Freeman-Tukey double arcsine transformation

# 
# summary plot measure (sm)= Freeman-Tukey Double arcsine transformation (PFT),
# estimate  between-study variance (tau)= DerSimonian-Laird  (DL)
# calculate confidence intervals for individual studies (method.ci)= Wilson Score (WS)

#order by precision
precision_ab <- sqrt(ies_da_fep_ab$vi) 

forest_fep_ab <- forest(pes_fep_ab_summary,
                        xlim=c(0,100),
                        pscale=100,
                        rightcols=FALSE,
                        leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                        fontsize = 10,
                        xlab="Any abnormality(%)", smlab="",
                        fixed=FALSE,
                        weight.study="fixed", #same size if random used
                        col.square="black", col.square.lines="black",
                        col.diamond="black", col.diamond.lines="black",
                        plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                        pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1, sortvar=precision_ab)


############# Figure 1b. Forest plots: clinically relevant abnormalities ###############

# general comments:
# # see meta package to explore more useful arguments to customize forest plots.

#order by precision [placed at top to guide exclusion]
precision_cr_ab <- sqrt(ies_da_fep_cr_ab$vi)  

# clinically relevant abnormalities
pes_fep_clin_ab_summary <- meta::metaprop(fep_cr_abnormal, fep_total, author_year, data=data_cr, sm="PFT", 
                                   method.tau="DL", method.ci="WS")

precision_cr_ab <- sqrt(ies_da_fep_cr_ab$vi) 

year_cr <- data_cr$year

forest_fep_cr_ab <- forest(pes_fep_cr_ab_summary,
                         subset=5,
                         xlim=c(0,40),
                         pscale=100,
                         rightcols=FALSE,
                         leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                         fontsize = 10,
                         xlab="Clinically relevant abnormality(%)", smlab="",
                         fixed=FALSE,
                         weight.study="fixed",
                         col.square="black", col.square.lines="black",
                         col.diamond="black", col.diamond.lines="black",
                         plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                         pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=year_cr)


############# Supplementary Figures Section  ###############

############# sFigures section 2: Funnel plot of studies 

# sFigure 2a - Funnel plot - All abnormalities
funnel(pes_fep_ab_summary,
       xlim=c(0.2,1),
       steps="10",
       pscale=100,
       fixed=FALSE,
       rightcols=FALSE,
       leftcols=c("studlab", "event", "n", "effect", "ci"), ylab="Standard Error", xlab="Any abnormality(%)", smlab="",
       col="black", 
       pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=2, sortvar=precision)

# sFigure 2b - Funnel plot - Clinically relevant abnormalities
funnel(pes_fep_cr_ab_summary,
       xlim=c(0,.6),
       steps="10",
       pscale=100,
       fixed=FALSE,
       rightcols=FALSE,
       leftcols=c("studlab", "event", "n", "effect", "ci"), ylab="Standard Error", xlab="Clinically Relevant Abnormality(%)", smlab="",
       col="black", 
       pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=2, sortvar=precision)


############# sFigures 3: Abnormality subtype forest plots

# sFigure 3a - Forest plot - abnormality subtype: white matter [includes Zanetti et al 2008]

options(digits=1)

# #order by precision
# precision=sqrt(ies_da_fep_ab$vi) 

# white matter
pes_fep_white_matter_summary <- meta::metaprop(fep_white_matter, fep_total, author_year, data=data_subtype_all, sm="PFT",
                                      method.tau="DL", method.ci="WS")

precision_wm <- pes_fep_white_matter_summary$TE

forest_white_matter_ab <- forest(pes_fep_white_matter_summary,
                                 subset=5,
                                 xlim=c(0,40),
                                 pscale=100,
                                 rightcols=FALSE,
                                 leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                                 fontsize = 10,
                                 xlab="white matter abnormality (%)", smlab="",
                                 fixed=FALSE,
                                 weight.study="fixed",
                                 col.square="black", col.square.lines="black",
                                 col.diamond="black", col.diamond.lines="black",
                                 plotwidth="5cm", colgap= "5mm", fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                                 pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_wm)

# sFigure 3b - Forest plot - abnormality subtype: vascular abnormality

pes_fep_vascular_summary <- meta::metaprop(fep_vascular, fep_total, author_year, data=data_subtype, sm="PFT",
                                            method.tau="DL", method.ci="WS")

precision_vascular <- pes_fep_vascular_summary$TE

forest_vascular_ab <- forest(pes_fep_vascular_summary,
                               subset=5,
                               xlim=c(0,40),
                               pscale=100,
                               rightcols=FALSE,
                               leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                               fontsize = 10,
                               xlab="vascular abnormality(%)", smlab="",
                               fixed=FALSE,
                               weight.study="fixed",
                               col.square="black", col.square.lines="black",
                               col.diamond="black", col.diamond.lines="black",
                               plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                               pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_vascular)

# sFigure 3c - Forest plot - abnormality subtype: cyst

pes_fep_cyst_summary <- meta::metaprop(fep_cyst, fep_total, author_year, data=data_subtype, sm="PFT",
                                        method.tau="DL", method.ci="WS")

precision_cyst <- pes_fep_cyst_summary$TE

forest_cyst_ab <- forest(pes_fep_cyst_summary,
                           subset=5,
                           xlim=c(0,40),
                           pscale=100,
                           rightcols=FALSE,
                           leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                           fontsize = 10,
                           xlab="cyst abnormality(%)", smlab="",
                           fixed=FALSE,
                           weight.study="fixed",
                           col.square="black", col.square.lines="black",
                           col.diamond="black", col.diamond.lines="black",
                           plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                           pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_cyst)

# sFigure 3d - Forest plot - abnormality subtype: tumour

pes_fep_tumour_summary <- meta::metaprop(fep_tumour, fep_total, author_year, data=data_subtype, sm="PFT",
                                      method.tau="DL", method.ci="WS")

precision_tumour <- pes_fep_tumour_summary$TE

forest_tumour_ab <- forest(pes_fep_tumour_summary,
                       subset=5,
                       xlim=c(0,40),
                       pscale=100,
                       rightcols=FALSE,
                       leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                       fontsize = 10,
                       xlab="tumour abnormality(%)", smlab="",
                       fixed=FALSE,
                       weight.study="fixed",
                       col.square="black", col.square.lines="black",
                       col.diamond="black", col.diamond.lines="black",
                       plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                       pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_tumour)

# sFigure 3e - Forest plot - abnormality subtype: atrophy

pes_fep_atrophy_summary <- meta::metaprop(fep_atrophy, fep_total, author_year, data=data_subtype, sm="PFT",
                                    method.tau="DL", method.ci="WS")

precision_atrophy <- pes_fep_atrophy_summary$TE

forest_atrophy_ab <- forest(pes_fep_atrophy_summary,
                         subset=5,
                         xlim=c(0,40),
                         pscale=100,
                         rightcols=FALSE,
                         leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                         fontsize = 10,
                         xlab="atrophy abnormality(%)", smlab="",
                         fixed=FALSE,
                         weight.study="fixed",
                         col.square="black", col.square.lines="black",
                         col.diamond="black", col.diamond.lines="black",
                         plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                         pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_atrophy)

# sFigure 3f - Forest plot - abnormality subtype: ventricular

pes_fep_ventricular_summary <- meta::metaprop(fep_ventricular, fep_total, author_year, data=data_subtype, sm="PFT",
                                    method.tau="DL", method.ci="WS")

precision_ventricular <- pes_fep_ventricular_summary$TE

forest_ventricular_ab <- forest(pes_fep_ventricular_summary,
                          subset=5,
                          xlim=c(0,40),
                          pscale=100,
                          rightcols=FALSE,
                          leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                          fontsize = 10,
                          xlab="ventricular abnormality(%)", smlab="",
                          fixed=FALSE,
                          weight.study="fixed",
                          col.square="black", col.square.lines="black",
                          col.diamond="black", col.diamond.lines="black",
                          plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                          pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_ventricular)

# sFigure 3g - Forest plot - abnormality subtype: pituitary

pes_fep_pituitary_summary <- meta::metaprop(fep_pituitary, fep_total, author_year, data=data_subtype, sm="PFT",
                                           method.tau="DL", method.ci="WS")

precision_pituitary <- pes_fep_pituitary_summary$TE

forest_pituitary_ab <- forest(pes_fep_pituitary_summary,
                              subset=5,
                              xlim=c(0,40),
                              pscale=100,
                              rightcols=FALSE,
                              leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                              fontsize = 10,
                              xlab="pituitary abnormality(%)", smlab="",
                              fixed=FALSE,
                              weight.study="fixed",
                              col.square="black", col.square.lines="black",
                              col.diamond="black", col.diamond.lines="black",
                              plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                              pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1, sortvar=precision_pituitary)

# sFigure 3h - Forest plot - abnormality subtype: other

pes_fep_other_summary <- meta::metaprop(fep_other, fep_total, author_year, data=data_subtype, sm="PFT",
                                         method.tau="DL", method.ci="WS")

precision_other <- pes_fep_other_summary$TE

forest_other_ab <- forest(pes_fep_other_summary,
                            subset=5,
                            xlim=c(0,40),
                            pscale=100,
                            rightcols=FALSE,
                            leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                            fontsize = 10,
                            xlab="other abnormality(%)", smlab="",
                            fixed=FALSE,
                            weight.study="fixed",
                            col.square="black", col.square.lines="black",
                            col.diamond="black", col.diamond.lines="black",
                            plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                            pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_other)


############# sFigures Section 4: Forest plots of clinically relevant MRI abnormalities by anatomical subtype

img_width = 7
img_height = 3.2
img_res = 120

# sFigure 4a - Forest plot - clinically relevant abnormality subtype: white matter

pes_fep_cr_white_matter_summary <- meta::metaprop(fep_cr_white_matter, fep_total, author_year, data=data_subtype_all, sm="PFT",
                                            method.tau="DL", method.ci="WS")

precision_wm <- pes_fep_cr_white_matter_summary$TE

forest_white_matter_ab <- forest(pes_fep_cr_white_matter_summary,
                               subset=5,
                               xlim=c(0,40),
                               pscale=100,
                               rightcols=FALSE,
                               leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                               fontsize = 10,
                               xlab="white matter abnormality (%)", smlab="",
                               fixed=FALSE,
                               weight.study="fixed",
                               col.square="black", col.square.lines="black",
                               col.diamond="black", col.diamond.lines="black",
                               plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                               pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_wm)


# sFigure 4b - Forest plot - clinically relevant abnormality subtype: vascular

pes_fep_cr_vascular_summary <- meta::metaprop(fep_cr_vascular, fep_total, author_year, data=data_subtype, sm="PFT",
                                        method.tau="DL", method.ci="WS")

precision_vascular <- pes_fep_cr_vascular_summary$TE

forest_vascular_ab <- forest(pes_fep_cr_vascular_summary,
                           subset=5,
                           xlim=c(0,40),
                           pscale=100,
                           rightcols=FALSE,
                           leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                           fontsize = 10,
                           xlab="vascular abnormality(%)", smlab="",
                           fixed=FALSE,
                           weight.study="fixed",
                           col.square="black", col.square.lines="black",
                           col.diamond="black", col.diamond.lines="black",
                           plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                           pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_vascular)

# sFigure 4c - Forest plot - clinically relevant abnormality subtype: cyst

pes_fep_cr_cyst_summary <- meta::metaprop(fep_cr_cyst, fep_total, author_year, data=data_subtype, sm="PFT",
                                    method.tau="DL", method.ci="WS")

precision_cyst <- pes_fep_cr_cyst_summary$TE

forest_cyst_ab <- forest(pes_fep_cr_cyst_summary,
                       subset=5,
                       xlim=c(0,40),
                       pscale=100,
                       rightcols=FALSE,
                       leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                       fontsize = 10,
                       xlab="cyst abnormality(%)", smlab="",
                       fixed=FALSE,
                       weight.study="fixed",
                       col.square="black", col.square.lines="black",
                       col.diamond="black", col.diamond.lines="black",
                       plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                       pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_cyst)

# sFigure 4d - Forest plot - clinically relevant abnormality subtype: tumour

pes_fep_cr_tumour_summary <- meta::metaprop(fep_cr_tumour, fep_total, author_year, data=data_subtype, sm="PFT",
                                      method.tau="DL", method.ci="WS")

precision_tumour <- pes_fep_cr_tumour_summary$TE

forest_tumour_ab <- forest(pes_fep_cr_tumour_summary,
                         subset=5,
                         xlim=c(0,40),
                         pscale=100,
                         rightcols=FALSE,
                         leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                         fontsize = 10,
                         xlab="tumour abnormality(%)", smlab="",
                         fixed=FALSE,
                         weight.study="fixed",
                         col.square="black", col.square.lines="black",
                         col.diamond="black", col.diamond.lines="black",
                         plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                         pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_tumour)


# sFigure 4e - Forest plot - clinically relevant abnormality subtype: atrophy

pes_fep_cr_atrophy_summary <- meta::metaprop(fep_cr_atrophy, fep_total, author_year, data=data_subtype, sm="PFT",
                                       method.tau="DL", method.ci="WS")

precision_atrophy <- pes_fep_cr_atrophy_summary$TE

forest_atrophy_ab <- forest(pes_fep_cr_atrophy_summary,
                          subset=5,
                          xlim=c(0,40),
                          pscale=100,
                          rightcols=FALSE,
                          leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                          fontsize = 10,
                          xlab="atrophy abnormality(%)", smlab="",
                          fixed=FALSE,
                          weight.study="fixed",
                          col.square="black", col.square.lines="black",
                          col.diamond="black", col.diamond.lines="black",
                          plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                          pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_atrophy)

# sFigure 4f - Forest plot - clinically relevant abnormality subtype: ventricular

pes_fep_cr_ventricular_summary <- meta::metaprop(fep_cr_ventricular, fep_total, author_year, data=data_subtype, sm="PFT",
                                           method.tau="DL", method.ci="WS")

precision_ventricular <- pes_fep_cr_ventricular_summary$TE

forest_ventricular_ab <- forest(pes_fep_cr_ventricular_summary,
                              subset=5,
                              xlim=c(0,40),
                              pscale=100,
                              rightcols=FALSE,
                              leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                              fontsize = 10,
                              xlab="ventricular abnormality(%)", smlab="",
                              fixed=FALSE,
                              weight.study="fixed",
                              col.square="black", col.square.lines="black",
                              col.diamond="black", col.diamond.lines="black",
                              plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                              pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1, sortvar=precision_ventricular)

# sFigure 4g - Forest plot - clinically relevant abnormality subtype: pituitary

pes_fep_cr_pituitary_summary <- meta::metaprop(fep_cr_pituitary, fep_total, author_year, data=data_subtype, sm="PFT",
                                         method.tau="DL", method.ci="WS")

precision_pituitary <- pes_fep_cr_pituitary_summary$TE

forest_pituitary_ab <- forest(pes_fep_cr_pituitary_summary,
                            subset=5,
                            xlim=c(0,40),
                            pscale=100,
                            rightcols=FALSE,
                            leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                            fontsize = 10,
                            xlab="pituitary abnormality(%)", smlab="",
                            fixed=FALSE,
                            weight.study="fixed",
                            col.square="black", col.square.lines="black",
                            col.diamond="black", col.diamond.lines="black",
                            plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                            pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1, sortvar=precision_pituitary)

# sFigure 4h - Forest plot - clinically relevant abnormality subtype: other

pes_fep_cr_other_summary <- meta::metaprop(fep_cr_other, fep_total, author_year, data=data_subtype, sm="PFT",
                                     method.tau="DL", method.ci="WS")

precision_other <- pes_fep_cr_other_summary$TE

forest_other_ab <- forest(pes_fep_cr_other_summary,
                        subset=5,
                        xlim=c(0,40),
                        pscale=100,
                        rightcols=FALSE,
                        leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                        fontsize = 10,
                        xlab="other abnormality(%)", smlab="",
                        fixed=FALSE,
                        weight.study="fixed",
                        col.square="black", col.square.lines="black",
                        col.diamond="black", col.diamond.lines="black",
                        plotwidth="5cm", colgap= "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                        pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_other)


############# Supplementary Figures Section 5. Forest plots of leave one out sensitivity analysis ###############

# Clinically relevant abnormalities

# L1O based on 'meta::metainf' function
precision_cr <- sqrt(ies_da_fep_cr_ab$vi) 

leave1out_fep_cr_ab <- metainf(pes_fep_cr_ab_summary, pooled="random", sortvar=precision_cr)
forest(leave1out_fep_cr_ab)
