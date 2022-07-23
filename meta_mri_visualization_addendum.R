########################################################################################
#                                                                                      #
# meta_mri_visualization_addendum.R from Blackman et al                                #
#                                                                                      #
# "How command are neuroradiological abnormalities in first-episode psychosis?         #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
########################################################################################

#
# This script includes visualizations not included in the publication
#

################## Forest plots: subgrouped by various factors

# see example code: https://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups

# by rater  ----------------------------------------------------------

# All abnormalities
forest_fep_ab_summary_by_neurorad <- forest(pes_fep_ab_summary_by_neurorad,
                         xlim=c(0,100),
                         fixed=FALSE,
                         pscale=100,
                         rightcols=FALSE,
                         leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prevalence(%)", "95% C.I."), xlab="Prevalence of abnormality(%)", smlab="",
                         weight.study="fixed", squaresize=0.3, col.square="black", col.square.lines="navy",
                         col.diamond="black", col.diamond.lines="black", 
                         pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, print.Q.subgroup = TRUE, test.subgroup.random= TRUE, digits=1)

forest_fep_ab_summary_by_neurorad

# Clinically relevant abnormalities
forest_fep_cr_ab_summary_by_neurorad <- forest(pes_fep_cr_ab_summary_by_recruitment,
                                             xlim=c(0,100),
                                             fixed=FALSE,
                                             pscale=100,
                                             rightcols=FALSE,
                                             leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prevalence(%)", "95% C.I."), xlab="Prevalence of abnormality(%)", smlab="",
                                             weight.study="fixed", squaresize=0.3, col.square="black", col.square.lines="navy",
                                             col.diamond="black", col.diamond.lines="black", 
                                             pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, print.Q.subgroup = TRUE, test.subgroup.random= TRUE, digits=1)

forest_fep_cr_ab_summary_by_neurorad

# by recruitment  ----------------------------------------------------------

# All abnormalities
forest_fep_ab_summary_by_recruitment <- forest(pes_fep_ab_summary_by_recruitment,
                                             xlim=c(0,100),
                                             fixed=FALSE,
                                             pscale=100,
                                             rightcols=FALSE,
                                             leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prevalence(%)", "95% C.I."), xlab="Prevalence of abnormality(%)", smlab="",
                                             weight.study="fixed", squaresize=0.3, col.square="black", col.square.lines="navy",
                                             col.diamond="black", col.diamond.lines="black", 
                                             pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, print.Q.subgroup = TRUE, test.subgroup.random= TRUE, digits=1)

forest_fep_ab_summary_by_recruitment

# Clinically relevant abnormalities
forest_fep_cr_ab_summary_by_recruitment <- forest(pes_fep_cr_ab_summary_by_recruitment,
                                                xlim=c(0,100),
                                                fixed=FALSE,
                                                pscale=100,
                                                rightcols=FALSE,
                                                leftcols=c("studlab", "event", "n", "effect", "ci"), leftlabs=c("Study", "Cases", "Total", "Prevalence(%)", "95% C.I."), xlab="Prevalence of abnormality(%)", smlab="",
                                                weight.study="fixed", squaresize=0.3, col.square="black", col.square.lines="navy",
                                                col.diamond="black", col.diamond.lines="black", 
                                                pooled.totals=FALSE, comb.fixed=FALSE, fs.hetstat=10, print.tau2=TRUE, print.Q=TRUE, print.pval.Q=TRUE, print.I2=TRUE, print.Q.subgroup = TRUE, test.subgroup.random= TRUE, digits=1)

forest_fep_cr_ab_summary_by_recruitment



################## meta regression [in progress] ###################

# wi=1/sqrt(ies.da$vi)
# size=1+3*(wi-min(wi))/(max(wi)-min(wi))
# plot(ies.da$dummyvar, transf.ipft.hm(ies.da$yi, targ=list(ni=dat$total)), cex=size)
# plot(ies.da$dummyvar, ies.da$yi, cex=size) #y-axis unit: double-arcsine-transformed value


########### L'abbe plot #####################################################

# not performed. Not clear if possible to plot for proportion data

