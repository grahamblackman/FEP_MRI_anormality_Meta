############### visualisation script###################

## notes:

## forest plots:
#forest plot
#leave 1 out (to relocate)

#meta regression
# funnel plot
# Labbe plot (in progress)


############# Forest plots: all ###############
# general comments:
##  see meta package to explore more useful arguments to customize forest plots.
## seems that double-arcsine transformed not performed using this approach 

# all abnormalities
pes_fep_ab_summary=meta::metaprop(fep_abnormal, fep_total, author_year, data=data, sm="PFT",
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
precision_ab=sqrt(ies_da_fep_ab$vi) 

forest_fep_ab<-forest(pes_fep_ab_summary,
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


 ############# Forest plots: clinically  relevant ###############

# general comments:
# # see meta package to explore more useful arguments to customize forest plots.

#order by precision [placed at top to guide exclusion]
precision_cr_ab=sqrt(ies_da_fep_cr_ab$vi)  

# clinically relevant abnormalities
pes_fep_clin_ab_summary=meta::metaprop(fep_cr_abnormal, fep_total, author_year, data=data_cr, sm="PFT", 
                                   method.tau="DL", method.ci="WS")

precision_cr_ab=sqrt(ies_da_fep_cr_ab$vi) 

year_cr<-data_cr$year

forest_fep_cr_ab<-forest(pes_fep_cr_ab_summary,
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


################## abnormality subtype  ################## 
options(digits=1)

# #order by precision
# precision=sqrt(ies_da_fep_ab$vi) 


# white matter
pes_fep_white_matter_summary=meta::metaprop(fep_white_matter, fep_total, author_year, data=data_subtype_all, sm="PFT",
                                      method.tau="DL", method.ci="WS")

precision_wm= pes_fep_white_matter_summary$TE

forest_white_matter_ab<-forest(pes_fep_white_matter_summary,
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



# vascular
pes_fep_vascular_summary=meta::metaprop(fep_vascular, fep_total, author_year, data=data, sm="PFT",
                                            method.tau="DL", method.ci="WS")

precision_vascular= pes_fep_vascular_summary$TE

forest_vascular_ab<-forest(pes_fep_vascular_summary,
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

# cyst
pes_fep_cyst_summary=meta::metaprop(fep_cyst, fep_total, author_year, data=data, sm="PFT",
                                        method.tau="DL", method.ci="WS")

precision_cyst= pes_fep_cyst_summary$TE

forest_cyst_ab<-forest(pes_fep_cyst_summary,
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

# tumour
pes_fep_tumour_summary=meta::metaprop(fep_tumour, fep_total, author_year, data=data, sm="PFT",
                                      method.tau="DL", method.ci="WS")

precision_tumour= pes_fep_tumour_summary$TE

forest_tumour_ab<-forest(pes_fep_tumour_summary,
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


# atrophy
pes_fep_atrophy_summary=meta::metaprop(fep_atrophy, fep_total, author_year, data=data, sm="PFT",
                                    method.tau="DL", method.ci="WS")

precision_atrophy= pes_fep_atrophy_summary$TE

forest_atrophy_ab<-forest(pes_fep_atrophy_summary,
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

# ventricular
pes_fep_ventricular_summary=meta::metaprop(fep_ventricular, fep_total, author_year, data=data, sm="PFT",
                                    method.tau="DL", method.ci="WS")

precision_ventricular= pes_fep_ventricular_summary$TE

forest_ventricular_ab<-forest(pes_fep_ventricular_summary,
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

# pituitary
pes_fep_pituitary_summary=meta::metaprop(fep_pituitary, fep_total, author_year, data=data, sm="PFT",
                                           method.tau="DL", method.ci="WS")

precision_pituitary= pes_fep_pituitary_summary$TE

forest_pituitary_ab<-forest(pes_fep_pituitary_summary,
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
                              pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_pituitary)

# other
pes_fep_other_summary=meta::metaprop(fep_other, fep_total, author_year, data=data, sm="PFT",
                                         method.tau="DL", method.ci="WS")

precision_other= pes_fep_other_summary$TE

forest_other_ab<-forest(pes_fep_other_summary,
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

################## abnormality subtype: Clinically Relevant Abnormalities  ################## 

# #order by precision
# precision=sqrt(ies_da_fep_ab$vi) 

# white matter
pes_fep_cr_white_matter_summary=meta::metaprop(fep_cr_white_matter, fep_total, author_year, data=data_subtype_all, sm="PFT",
                                            method.tau="DL", method.ci="WS")

precision_wm= pes_fep_cr_white_matter_summary$TE

forest_white_matter_ab<-forest(pes_fep_cr_white_matter_summary,
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



# vascular
pes_fep_cr_vascular_summary=meta::metaprop(fep_cr_vascular, fep_total, author_year, data=data, sm="PFT",
                                        method.tau="DL", method.ci="WS")

precision_vascular= pes_fep_cr_vascular_summary$TE

forest_vascular_ab<-forest(pes_fep_cr_vascular_summary,
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

# cyst
pes_fep_cr_cyst_summary=meta::metaprop(fep_cr_cyst, fep_total, author_year, data=data, sm="PFT",
                                    method.tau="DL", method.ci="WS")

precision_cyst= pes_fep_cr_cyst_summary$TE

forest_cyst_ab<-forest(pes_fep_cr_cyst_summary,
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

# tumour
pes_fep_cr_tumour_summary=meta::metaprop(fep_cr_tumour, fep_total, author_year, data=data, sm="PFT",
                                      method.tau="DL", method.ci="WS")

precision_tumour= pes_fep_cr_tumour_summary$TE

forest_tumour_ab<-forest(pes_fep_cr_tumour_summary,
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


# atrophy
pes_fep_cr_atrophy_summary=meta::metaprop(fep_cr_atrophy, fep_total, author_year, data=data, sm="PFT",
                                       method.tau="DL", method.ci="WS")

precision_atrophy= pes_fep_cr_atrophy_summary$TE

forest_atrophy_ab<-forest(pes_fep_cr_atrophy_summary,
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

# ventricular
pes_fep_cr_ventricular_summary=meta::metaprop(fep_cr_ventricular, fep_total, author_year, data=data, sm="PFT",
                                           method.tau="DL", method.ci="WS")

precision_ventricular= pes_fep_cr_ventricular_summary$TE

forest_ventricular_ab<-forest(pes_fep_cr_ventricular_summary,
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

# pituitary
pes_fep_cr_pituitary_summary=meta::metaprop(fep_cr_pituitary, fep_total, author_year, data=data, sm="PFT",
                                         method.tau="DL", method.ci="WS")

precision_pituitary= pes_fep_cr_pituitary_summary$TE

forest_pituitary_ab<-forest(pes_fep_cr_pituitary_summary,
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
                            pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=1,sortvar=precision_pituitary)

# other
pes_fep_cr_other_summary=meta::metaprop(fep_cr_other, fep_total, author_year, data=data, sm="PFT",
                                     method.tau="DL", method.ci="WS")

precision_other= pes_fep_cr_other_summary$TE

forest_other_ab<-forest(pes_fep_cr_other_summary,
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

################## Forest plots: subgroup

#see example code: https://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups


# rater  ----------------------------------------------------------

forest_fep_ab_summary_by_neurorad<-forest(pes_fep_ab_summary_by_neurorad,
                         xlim=c(0,100),
                         fixed=FALSE,
                         pscale=100,
                         rightcols=FALSE,
                         leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prevalence(%)", "95% C.I."), xlab="Prevalence of abnormality(%)", smlab="",
                         weight.study="fixed", squaresize=0.3, col.square="black", col.square.lines="navy",
                         col.diamond="black", col.diamond.lines="black", 
                         pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, print.Q.subgroup = TRUE, test.subgroup.random= TRUE, digits=1)

forest_fep_ab_summary_by_neurorad


forest_fep_cr_ab_summary_by_neurorad<-forest(pes_fep_cr_ab_summary_by_recruitment,
                                             xlim=c(0,100),
                                             fixed=FALSE,
                                             pscale=100,
                                             rightcols=FALSE,
                                             leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prevalence(%)", "95% C.I."), xlab="Prevalence of abnormality(%)", smlab="",
                                             weight.study="fixed", squaresize=0.3, col.square="black", col.square.lines="navy",
                                             col.diamond="black", col.diamond.lines="black", 
                                             pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, print.Q.subgroup = TRUE, test.subgroup.random= TRUE, digits=1)

forest_fep_cr_ab_summary_by_neurorad

# field strength ----------------------------------------------------------

forest_fep_ab_summary_by_recruitment<-forest(ppes_fep_cr_ab_summary_by_recruitment,
                                             xlim=c(0,100),
                                             fixed=FALSE,
                                             pscale=100,
                                             rightcols=FALSE,
                                             leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prevalence(%)", "95% C.I."), xlab="Prevalence of abnormality(%)", smlab="",
                                             weight.study="fixed", squaresize=0.3, col.square="black", col.square.lines="navy",
                                             col.diamond="black", col.diamond.lines="black", 
                                             pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, print.Q.subgroup = TRUE, test.subgroup.random= TRUE, digits=1)

forest_fep_ab_summary_by_recruitment


# field strength ----------------------------------------------------------

forest_fep_cr_ab_summary_by_recruitment<-forest(pes_fep_cr_ab_summary_by_recruitment,
                                                xlim=c(0,100),
                                                fixed=FALSE,
                                                pscale=100,
                                                rightcols=FALSE,
                                                leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prevalence(%)", "95% C.I."), xlab="Prevalence of abnormality(%)", smlab="",
                                                weight.study="fixed", squaresize=0.3, col.square="black", col.square.lines="navy",
                                                col.diamond="black", col.diamond.lines="black", 
                                                pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, print.Q.subgroup = TRUE, test.subgroup.random= TRUE, digits=1)

forest_fep_cr_ab_summary_by_recruitment


################## leave-one-out sensitivity analysis [in progress] ##########


# leave-one-out: leave-one-out: Forest plot
# 
#L1O based on 'meta::metainf' function
precision_cr=sqrt(ies_da_fep_cr_ab$vi) # no precision estimate for Dazzan et al
leave1out_FEP_cr_ab<-metainf(pes_fep_cr_ab_summary,pooled="random") # works as of 21st Dec

leave1out_fep_cr_ab<-forest(metainf(pes_fep_cr_ab_summary,pooled="random",sortvar=precision_cr)) 

leave1out_fep_cr_ab<-metainf(pes_fep_cr_ab_summary,pooled="random",sortvar=precision_cr)

forest(leave1out_fep_cr_ab)



################## meta regression [in progress] ###################

# wi=1/sqrt(ies.da$vi)
# size=1+3*(wi-min(wi))/(max(wi)-min(wi))
# plot(ies.da$dummyvar, transf.ipft.hm(ies.da$yi, targ=list(ni=dat$total)), cex=size)
# plot(ies.da$dummyvar, ies.da$yi, cex=size) #y-axis unit: double-arcsine-transformed value


########### L'abbe plot #####################################################

# not performed. Not clear if possible to plot for proportion data

########## funnel plot [in progress: x not recognised as a meta object] ###########


# All abnormalities

funnel(pes_fep_ab_summary,
       xlim=c(0.2,1),
       steps="10",
       pscale=100,
       fixed=FALSE,
rightcols=FALSE,
leftcols=c("studlab", "event", "n", "effect", "ci"), ylab="Standard Error", xlab="Any abnormality(%)", smlab="",
col="black", 
pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=2, sortvar=precision)


# # Clinically relevant abnormalities

funnel(pes_fep_cr_ab_summary,
       xlim=c(0,.6),
       steps="10",
       pscale=100,
       fixed=FALSE,
       rightcols=FALSE,
       leftcols=c("studlab", "event", "n", "effect", "ci"), ylab="Standard Error", xlab="Clinically Relevant Abnormality(%)", smlab="",
       col="black", 
       pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, digits=2, sortvar=precision)

