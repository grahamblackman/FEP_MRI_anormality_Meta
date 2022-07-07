

###################  calculating OR effect sizes (com FEP vs HC) ###################  

#  key reference: https://www.jstatsoft.org/article/view/v036i03

#  to do

# -finish off OR calc for subtypes (part complete)
# -fix forest plots
# -run subanalyses?
  
# queries:
# # how to add study label to forest plot?
#  check calc of OR correct (using formuala to calc non event cases, which will need double checking)

#key functions: 
#escalc()
#rma()= Function to fit the meta-analytic fixed- and random/mixed-effects models with or without moderators via linear (mixed-effects) models. See the documentation of the metafor-package for more details on these models.
#predict() functions.

# The escalc() function, individual effect sizes and their corresponding
# sampling variances are estimated by fitting a meta-analytic model. One can
# decide whether to transform these effect sizes with the measure= argument. 

#Input:
# ai= event in experimental group
# bi= non-event in experimental group
# ci= event in control group 
# di= non-event in control group
# NB The log odds ratio is equal to the log of (ai*di)/(bi*ci). log transformation performed to make outcome measures symmetric around 0 and helps to make the distribution of these outcome measure
# closer to normal

#output: 
#yi= effect size
#vi= sample variance

# then pool the individual effect sizes and their sampling variances based on
# the inverse variance method with the rma() function. To do this, we can
# execute:

# general structure or IEF of OR in metafor
# R> dat <- escalc(measure = "OR", ai = , bi = , ci = , di = , data = , append = TRUE)

#individual effect sizes and their sampling variances


# calc effect side (indivudual) -------------------------------------------


ies_or_fep_ab <- escalc(measure = "OR", ai = fep_abnormal, bi = fep_normal, ci = hc_abnormal, di = hc_normal, data = data, append = TRUE) #all abnormalities
# ies_or_fep_cr_ab <- escalc(measure = "OR", ai = fep_cr_abnormal, bi = fep_normal, ci = hc_cr_abnormal, di = hc_normal, data = data, append = TRUE) # CR abnormalities [need to impute number of CR abnormalities om healthy controls  - to add in once computed]

#subtypes
ies_or_fep_wm <- escalc(measure = "OR", ai = fep_white_matter, bi = fep_total-fep_white_matter, ci = hc_whitematter, di = hc_total-hc_normal, data = data, append = TRUE) # need to adjust odds (calc non WM)
ies_or_fep_vasc <- escalc(measure = "OR", ai = fep_vascular, bi = fep_total-fep_vascular, ci = hc_vascular, di = hc_total-hc_vascular, data = data, append = TRUE) # need to adjust odds (calc non WM)

[add in others...]

# print subset of vars [check step]
# view(print(ies_or_fep_ab[,c("author_year","yi","vi")], row.names = FALSE))
                                    


# Pooled summary effect size. (Random effects model) ----------------------


# queries:
#default method REML?  
#data already logged? Which should be reported?
pes_or_fep_ab<-rma(yi, vi, data=ies_or_fep_ab, level=95) # all abnormalities
# pes_or_fep_cr_ab<-rma(yi, vi, data=ies_or_fep_cr_ab, level=95) # CR abnormalities [to add in once computed]

#subtypes
pes_or_fep_wm<-rma(yi, vi, data=ies_or_fep_wm, level=95) # WM abnormalities
pes_or_fep_vasc<-rma(yi, vi, data=ies_or_fep_vasc, level=95) # vasc abnormalities
pes_or_fep_cyst<-rma(yi, vi, data=ies_or_fep_cyst, level=95) # cyst abnormalities
pes_or_fep_atro<-rma(yi, vi, data=ies_or_fep_atrophy, level=95) # atrophy abnormalities
pes_or_fep_tum<-rma(yi, vi, data=ies_or_fep_tumour, level=95) # tumour abnormalities
pes_or_fep_vent<-rma(yi, vi, data=ies_or_fep_ventricular, level=95) # ventricular abnormalities
pes_or_fep_pit<-rma(yi, vi, data=ies_or_fep_pituitary, level=95) # pituitary abnormalities
pes_or_fep_oth<-rma(yi, vi, data=ies_or_fep_other, level=95) # other abnormalities


summary(pes_or_fep_ab) # all abnormalities
# summary(pes_or_fep_cr_ab) # CR abnormalities [to add in once computed]

#subtypes
summary(pes_or_fep_vasc) 


# Case-control studies reporting clinical relevatn findings
# Falkenberg: yes [5.5 HC had abnormalities leading to referal. Need to double check]
# OPTiMiSE: yes [but not publically available]
# Khandapour: no [but double check]
# Liberman: no [but double check]
# Lubman: no
# Miller: no
# Symond: no
# Zanetti: no
# Any others studies? 

# once plotting finalised, move to visualization

# metagen package [offers preset formatting]. Inverse variance method is used for pooling.
settings.meta("jama")
forest(metagen(ies_or_fep_ab$yi, ies_or_fep_ab$vi, comb.fixed=FALSE, studlab = data$author_year, sm = "OR")) # find out how to omit missing studies from view


# metafor package [no preset formatting, presents log OR]
forest(pes_or_fep_ab, slab = paste(data$author_year)) 

#subtypes
forest(pes_or_fep_vasc)

, slab = paste(data$author_year)) 


# 
# # Studies with NAs omitted from model fitting automatically
# 
# #To convert to non-transformed measurement scale (i.e., proportion) and yield a true summary proportion
# 
# # all abnormalities
# pes_fep_ab= predict(pes_da_fep_ab, transf=transf.ipft.hm, targ=list(ni=data$fep_total)) #class: list.rma
# summary(pes_fep_ab)
# print(pes_fep_ab, digits=2)
# prop_fep_ab<-pes_fep_ab$pred
# prop_LCI_fep_ab<-pes_fep_ab$ci.lb
# prop_UCI_fep_ab<-pes_fep_ab$ci.ub
# nns_fep_ab<-(ceiling(1/pes_fep_ab$pred)) #number needed to scan
# nns_LCI_fep_ab<-(ceiling(1/pes_fep_ab$ci.ub)) #number needed to scan upper and lower estimate
# nns_UCI_fep_ab<-(ceiling(1/pes_fep_ab$ci.lb))
# isqu_fep_ab<-pes_da_fep_ab$I2 # NB based on transformed data [convert?]
# 
# 
# 
# # clinically relevant abnormalities
# pes_fep_cr_ab= predict(pes_da_fep_cr_ab, transf=transf.ipft.hm, targ=list(ni=data$fep_total)) #class: list.rma
# summary(pes_fep_cr_ab)
# print(pes_fep_cr_ab, digits=2)
# prop_fep_cr_ab<-pes_fep_cr_ab$pred
# prop_LCI_fep_cr_ab<-pes_fep_cr_ab$ci.lb
# prop_UCI_fep_cr_ab<-pes_fep_cr_ab$ci.ub
# nns_fep_cr_ab<-(ceiling(1/pes_fep_cr_ab$pred)) #number needed to scan
# nns_LCI_fep_cr_ab<-(ceiling(1/pes_fep_cr_ab$ci.ub)) #number needed to scan upper and lower estimate
# nns_UCI_fep_cr_ab<-(ceiling(1/pes_fep_cr_ab$ci.lb))
# isqu_fep_cr_ab<-pes_da_fep_cr_ab$I2 # NB based on transformed data [convert?]
# 
# #If we decide not to perform a transformation, this object is suggested to be
# #named pes, which stands for pooled effect size; if we decide to perform a
# #transformation, either the logit or the doublearcsine, it is suggested to be
# #named pes. logit or pes.da, which stands for logit and double-arcsin
# #transformed summary effect size, respectively. This object will store the
# #results generated by the rma() function. The function will yield a pooled
# #effect size based on the individual effect sizes and their sampling variances
# #contained in ies. method= argument  which of the following between-study
# #variance estimators will be used (the default method is REML): method="DL"
# ##random effects using the DerSimonian-Laird estimator method="REML" #random
# #effects using the restricted maximum-likelihood estimator
# 
# # When a study contains proportions equal to 0, R will automatically add 0.5 to the observed data (i.e., the number of event of interest, namely the cases variable). Since the double-arcsine transformation does not require any adjustments to be made to
# #the data in such a situation, we can explicitly switch add=0.5 to add=0 to stop the default adjustment. Returning to the running example, the summary proportion is generated using option 2
# #(i.e., the logit transformation) on the grounds that all of the observed proportions in the dataset are far below 0.2 and there are no zero events. Thus, we would execute: ies.logit=escalc(xi=cases, ni=total, measure="PLO", data=dat) pes.logit=rma(yi, vi, data=ies.logit, method="DL", level=95)
# #pes=predict(pes.logit, transf=transf.ilogit)
# 
# #digits=specifies the number of decimal places to which the printed results should be rounded (the default is 4). 
# 
# 
# ###################  subgroup analysis [does not run] ################### 
# 
# # Error comment: Error in x[xiTF] <- paste(xi, seq_along(xi), sep = "."):NAs are not allowed in subscripted assignments 
# 
# # group by study recruitment
# 
# ### routine clinical scans
# #### all
# # pes_da_fep_clinsubgp_ab<- rma(yi, vi, subset=(recruitment=="recruit_clin"), data=ies_da_fep_ab)
# # pes_fep_clinsubgp_ab= predict(pes_da_fep_clinsubgp_ab, transf=transf.ipft.hm, targ=list(ni=data$fep_total))
# # print(pes_fep_clinsubgp_ab)
# 
# #### clinically relevant
# # pes_da_fep_clinsubgp_cr_ab<- rma(yi, vi, subset=(recruitment=="recruit_clin"), data=ies_da_fep_cr_ab)
# # pes_fep_clinsubgp_cr_ab= predict(pes_da_fep_clinsubgp_cr_ab, transf=transf.ipft.hm, targ=list(ni=data$fep_total))
# # print(pes_fep_clinsubgp_cr_ab)
# 
# ### research scans
# ### all
# # pes_da_fep_ressubgp_ab<- rma(yi, vi, subset=(recruitment=="recruit_res"), data=ies_da_fep_ab)
# # pes_fep_ressubgp_ab= predict(pes_da_fep_ressubgp_ab, transf=transf.ipft.hm, targ=list(ni=data$fep_total))
# # print(pes_fep_ressubgp_ab)
# #### clinically relevant
# # pes_da_fep_ressubgp_cr_ab<- rma(yi, vi, subset=(recruitment=="recruit_res"), data=ies_da_fep_cr_ab)
# # pes_fep_ressubgp_cr_ab= predict(pes_da_fep_ressubgp_cr_ab, transf=transf.ipft.hm, targ=list(ni=data$fep_total))
# # print(pes_fep_ressubgp_cr_ab) 
# 
# ### mixture clinical and routine clinical scans
# ### all
# # pes_da_fep_mixsubgp_ab<- rma(yi, vi, subset=(recruitment=="recruit_mix"), data=ies_da_fep_ab)
# # pes_fep_mixsubgp_ab= predict(pes_da_fep_mixsubgp_ab, transf=transf.ipft.hm, targ=list(ni=data$fep_total))
# # print(pes_fep_mixsubgp_ab)
# # #### clinically relevant
# # pes_da_fep_mixsubgp_cr_ab<- rma(yi, vi, subset=(recruitment=="recruit_mix"), data=ies_da_fep_cr_ab)
# # pes_fep_mixsubgp_cr_ab= predict(pes_da_fep_mixsubgp_cr_ab, transf=transf.ipft.hm, targ=list(ni=data$fep_total))
# # print(pes_fep_mixsubgp_cr_ab)
# 
# 
# 
# #query: 
# # what does method CI refer to?
# # getting L1O ,ethod to work [reliant on rma data format]
# 
# pes_fep_ab_summary=meta::metaprop(fep_abnormal, fep_total, author_year, data=data, sm="PFT",
#                                   method.tau="DL", method.ci="NAsm") #class: chr [1:2] "metaprop" "meta"
# 
# pes_fep_cr_ab_summary=meta::metaprop(fep_cr_abnormal, fep_total, author_year, data=data, sm="PFT",
#                                      method.tau="DL", method.ci="NAsm") #class: chr [1:2] "metaprop" "meta"
# 
# #attributes 
# #attributes(pes_fep_ab_summary)
# #attributes(pes_fep_cr_ab_summary)
# 
# #options:
# # Inverse variance method
# # DerSimonian-Laird estimator for tau^2
# # Jackson method for confidence interval of tau^2 and tau
# # Freeman-Tukey double arcsine transformation
# # Normal approximation confidence interval for individual studies
# 
# # forest plot: see visualization
# 
# ###################  outlier study detection 
# 
# # identify studentized residuals larger than 2 (or 3 if enough studies) 
# # R comment: 'rstudent' function requires object of class "rma". multi-array average (RMA)
# 
# stud.res=rstudent(pes_da_fep_ab) # calc Residual Values
# abs.z=abs(stud.res$z) # standardized residuals (externally standardized for rstudent).
# stud.res[order(-abs.z)] # ordered
# 
# 
# ###################  influential study detection 
# 
# #L1O based on 'metafor::leave1out' function 
# leave1out_fep_ab<-leave1out(pes_da_fep_ab) # works nb based on DA transformation)
# print(leave1out_fep_ab)
# 
# infuence_fep_ab=influence(pes_da_fep_ab)
# print(infuence_fep_ab) #  asterix for influential cases -   based on 1 o 4 criteria (see help page for details)
# plot(infuence_fep_ab) # plot
# 
# 
# #L1O based on 'meta::metainf' function
# precision=sqrt(ies_da_fep_ab$vi) # no precision estimate for Dazzan et al
# leave1out_FEP_ab<-metainf(pes_fep_ab_summary,pooled="random") # works as of 21st Dec
# forest(metainf(pes_fep_ab_summary,pooled="random",sortvar=precision), layout="JAMA") #  works as of 21st Dec
# 
# 
# #################### Rerun meta analysis excluding outliers:  manually specify outlier
# 
# #all abnormalities
# pes_fep_ab_noutlier_summary=meta::metaprop(fep_abnormal, fep_total, author_year, data=data[-c(5),], sm="PFT",
#                                            method.tau="DL", method.ci="NAsm")
# forest(pes_fep_ab_noutlier_summary, layout="JAMA")
# 
# ## clinically relevant abnormalities 
# 
# 
# ########## Explaining heterogeneity [subgroup and meta regression]
# 
# ########## subgroup 
# # rater type: to do
# # scan resolution: to do
# # recruitment type (clin vs res):  [in progress]
# # whole vs restricted brain: works [for training, consider removing in due course 1/11/20]
# 
# # calculating subgroup summary proportions, conduct sub- group analysis, and
# # recalculate the overall summary proportion[see pg 40 of Conducting
# # Meta-Analyses of Proportions in R for guidance]
# 
# #brief summary [pg 39-40] " we assume a common between-study variance
# #component across subgroups and pool within-group estimates of τ^2 . In this
# #case, we can directly use the rma() com- mand to fit a mixed-effect model to
# #evaluate the moderating effect of a potential predictor. How- ever, to allow
# #us to calculate a new overall summary proportion using a pooled τ^2 across
# #all stud- ies, we still have to combine a new data frame containing
# #statistics estimated in two random- effects models. Once we have created the
# #new data frame, we can calculate a new overall summary effect based on the
# #data frame using a fixed-effect model or a random-effects model (based on the
# #various factors we have discussed above, e.g., the conclusion one wishes to
# #make, the true distribu- tion of effect sizes, etc.)."Assume a common
# #between-study variance component. Therefore pool within-group estimates of
# #between-study variance. This code is for data which has undergone
# #double-arcsine transformation
# 
# #objects created:
# # subg_moderator_fep_ab = subgroup moderator 
# # pes_da_all_brain_fep_ab = display subgroup 1 summary ES
# # pes_da_restricted_brain_fep_ab = display subgroup 2 summary 
# # subg_moderator_fep_ab =  subgroup analysis results
# # pes_whole_brain_fep = recomputed summary effect size
# 
# ############
# 
# # queries: 
# # set rma to random effects e.g DL?
# 
# ########### subgroup [works, 'all abnormalities' only at present whilst getting comfortbale with code]
# 
# # subgroup: all verses restricted brain regions
# # create moderator
# subg_brain_moderator_fep_ab=rma(yi, vi, data=ies_da_fep_ab, mods=~whole_brain) # works
# #create subgroup "all brain"
# pes_da_all_brain_fep_ab=rma(yi, vi, data=ies_da_fep_ab, mods=~whole_brain=="restricted_brain") 
# #create subgroup "restricted brain"
# pes_da_restricted_brain_fep_ab=rma(yi, vi, data=ies_da_fep_ab, mods=~whole_brain=="all_brain") 
# #create prediction model
# pes_subg_brain_moderator_fep_ab=predict(subg_brain_moderator_fep_ab,transf=transf.ipft.hm, targ=list(ni=data$fep_total))  #inverse of double-arcsine transformation
# #create pooled effect size and SE for moderator 
# data_brain_samevar=data.frame(estimate=c((pes_da_all_brain_fep_ab$b)[1], (pes_da_restricted_brain_fep_ab$b)[1]),
#                               stderror=c((pes_da_all_brain_fep_ab$se)[1], (pes_da_restricted_brain_fep_ab$se)[1]),
#                               tau2=subg_brain_moderator_fep_ab$tau2)
# #create pooled effect size and SE for moderator
# pes_da_whole_brain_fep=rma(estimate, sei=stderror,
#                            method="DL", data=data_brain_samevar)
# #back transform data 
# pes_whole_brain_fep=predict(pes_da_whole_brain_fep, transf=transf.ipft.hm, targ=list(ni=data$fep_total)) 
# # subgroup summary ES 
# print(pes_da_all_brain_fep_ab) #display subgroup 1 summary ES
# print(pes_da_restricted_brain_fep_ab) #display subgroup 2 summary 
# print(subg_brain_moderator_fep_ab) #display subgroup analysis results
# print(pes_whole_brain_fep) #display recomputed summary effect size
# 
# 
# # subgroup: research type: research, clinical, mixture populations [in progress]
# 
# # create moderator
# subg_recruit_moderator_fep_ab=rma(yi, vi, data=ies_da_fep_ab, mods=~recruitment) # works
# #create subgroup "recruit_clin"
# pes_da_recruit_clin_fep_ab=rma(yi, vi, data=ies_da_fep_ab, mods=~recruitment=="recruit_clin") 
# #create subgroup "recruit_res"
# pes_da_recruit_res_fep_ab=rma(yi, vi, data=ies_da_fep_ab, mods=~recruitment=="recruit_res") 
# #create prediction model
# pes_subg_recruit_moderator_fep_ab=predict(subg_recruit_moderator_fep_ab,transf=transf.ipft.hm, targ=list(ni=data$fep_total))  #inverse of double-arcsine transformation
# #create pooled effect size and SE for moderator 
# data_recruit_samevar=data.frame(estimate=c((pes_da_recruit_clin_fep_ab$b)[1], (pes_da_recruit_res_fep_ab$b)[1]),
#                                 stderror=c((pes_da_recruit_clin_fep_ab$se)[1], (pes_da_recruit_res_fep_ab$se)[1]),
#                                 tau2=subg_recruit_moderator_fep_ab$tau2)
# #create pooled effect size and SE for moderator
# pes_da_recruit_fep=rma(estimate, sei=stderror,
#                        method="DL", data= data_recruit_samevar)
# #back transform data 
# pes_recruit_fep=predict(pes_da_recruit_fep, transf=transf.ipft.hm, targ=list(ni=data$fep_total)) 
# # subgroup summary ES 
# print(pes_da_recruit_clin_fep_ab) #display subgroup 1 summary ES
# print(pes_da_recruit_res_fep_ab) #display subgroup 2 summary 
# print(subg_recruit_moderator_fep_ab) #display subgroup analysis results
# print(pes_recruit_fep) #display recomputed summary effect size
# 
# 
# ########## meta-regression: continuous variables [in progress: regression by year not working fully] ########## 
# #mean age 
# #duration of psychosis [requires data]
# 
# # querries 
# # Bonferroni correction [if multiple testing?]
# # use transformed or non transformed ies?
# 
# #metareg: mean age all abnormalities
# metareg_age_fep_ab=rma(yi, vi, data=ies_da_fep_ab, mods=~age_fep, method="DL")
# print(metareg_age_fep_ab)
# metareg_pval_age_fep_ab<-metareg_age_fep_ab$pval[2]
# 
# #metareg: mean age clinically relevant abnormalities
# metareg_age_fep_cr_ab=rma(yi, vi, data=ies_da_fep_cr_ab, mods=~age_fep, method="DL")
# print(metareg_age_fep_cr_ab)
# metareg_pval_age_fep_cr_ab<-metareg_age_fep_cr_ab$pval[2]
# 
# #metareg: year all abnormalities
# metareg_year_fep_ab=rma(yi, vi, data=ies_da_fep_ab, mods=~year, method="DL")
# print( metareg_year_fep_ab)
# 
# #metareg: year  clinically relevant abnormalities
# metareg_year_fep_cr_ab=rma(yi, vi, data=ies_da_fep_cr_ab, mods=~year, method="DL")
# print(metareg_year_fep_cr_ab)
# 
# #metareg: psychosis duration all abnormalities [treating psych duration as catergorical variable]
# metareg_psychdur_fep_ab=rma(yi, vi, data=ies_da_fep_ab, mods=~psychosis_duration_wks, method="DL")
# print(metareg_psychdur_fep_ab)
# 
# #metareg: psychosis duration  clinically relevant abnormalities [treating psych duration as catergorical variable]
# metareg_psychdur_fep_cr_ab=rma(yi, vi, data=ies_da_fep_cr_ab, mods=~year, method="DL")
# 
# # psych_dur_min<-min(data$psychosis_duration_wks,na.rm=TRUE)
# 
# ########## sensitivity analyses (robustness) [to complete] ########## 
# 
# #(i) studies of poor quality
# 
# #(ii) studies that performed radiological assessment on restricted brain regions
# 
# #(iii) studies that screened out patients with evidence of a secondary cause of psychosis (e.g. abnormal neurological exam)
# 
# #(iv) studies where MRI assessment was not performed by a radiologist
# 
# #(v) studies that were based on a research sample
# 
# # (i) Leave-one-out sensitivity analysis (determine whether the findings were driven by any single study)
# 
# # nb considered influential if the pooled estimate excluding the study was outside the 95% confidence interval for the overall estimate. 
# 
# ########## publication bias in meta-analyses of proportions [works, 'all
# #abnormalities' only at present]########## traditional publication bias
# #modelling tools developed for randomized con- trolled trials. Less useful in
# #the context of meta-analyses of observational studies but still reported by
# #convention
# 
# #Egger's regression test (Regression Test for Funnel Plot Asymmetry)
# 
# eggers_reg_fep_ab<-regtest(pes_da_fep_ab, model="rma", predictor="sei")
# print(eggers_reg_fep_ab)
# 
# eggers_reg_fep_cr_ab<-regtest(pes_da_fep_cr_ab, model="rma", predictor="sei")
# print( eggers_reg_fep_cr_ab)
# 
# ########### trim and fill ########## 
# # To correct for publication bias,(Duval and Tweedie, 2000)
# 
# # trim and fill based for CR abnormalities only
# 
# trimfill_fep_cr_ab<-trimfill(pes_da_fep_cr_ab)
# print(trimfill_fep_cr_ab) # estim prevalence
# funnel(trimfill_fep_cr_ab) # funnel plot
# 
# 
