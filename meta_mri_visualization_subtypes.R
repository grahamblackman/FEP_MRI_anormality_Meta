
########################################################################################
#                                                                                      #
# meta_mri_visualization_subtypes.R from Blackman et al                                #
#                                                                                      #
# "Prevalence of neuroradiological abnormalities in First Episode Psychosis:           #
#  a Meta-Analysis"                                                                    #
#                                                                                      #
########################################################################################

#
# This script visualizes the results of the meta-analysis presented in the paper and
# related analyses
#
# Function to extract raw proportion and upper and lower CI from meta-analytic objects
# Outputs vector with name, propotion, upper and lower CI

extract_proportions <- function(meta_obj, name){
  (TE_random_vals <- c(meta_obj$TE.random,
                       meta_obj$lower.random,
                       meta_obj$upper.random))
  vals <- unlist(lapply(TE_random_vals, meta:::backtransf, sm="PFT", n=1/mean(1/meta_obj$n)))
  append(name, vals)
}

############# Figure 2a. Forest plots: MRI abnormalities in first episode psychosis grouped by neuroanatomical type (all abnormalities)  ###############


# Extract raw proportions from meta-analytic objects using the extract_proportions function
# and add each one to the bottom of the summary_df dataframe

# Create empty dataframe to store results
summary_df <- data.frame(type=character(),
                         prop = numeric(),
                         prop_lower = numeric(),
                         prop_higher = numeric(),
                         stringsAsFactors=FALSE)

extractpr <- extract_proportions(pes_fep_ventricular_summary, "Ventricular")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_fep_pituitary_summary, "Pituitary")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_fep_tumour_summary, "Tumour")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_fep_other_summary, "Other")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr  <- extract_proportions(pes_fep_vascular_summary, "Vascular")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr  <- extract_proportions(pes_fep_cyst_summary, "Cyst")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr  <- extract_proportions(pes_fep_atrophy_summary, "Atrophy")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr  <- extract_proportions(pes_fep_white_matter_summary, "White Matter")
summary_df[nrow(summary_df)+1,] <- extractpr


# Change relevant columns to numeric)
summary_df$prop <- as.numeric(summary_df$prop)
summary_df$prop_lower <- as.numeric(summary_df$prop_lower)
summary_df$prop_higher <- as.numeric(summary_df$prop_higher)

# visualisation -----------------------------------------------------------

# global options

options(scipen=100, digits=6)
options(max.print = 500000000)

DV    <- c("","","","","","","","")
IV <- summary_df$type
ES <- summary_df$prop
LCI <- summary_df$prop_lower
UCI <- summary_df$prop_higher

subtype_ab <- data.frame(DV,IV,ES,LCI,UCI)

if (write_to_file == 1) {
  png(file=paste('output/', 'Figure_2a.png', sep=''), width = 10, height = 10, units = 'in', res = 200)
}

ggplot(data=subtype_ab, aes(x= reorder(IV,-ES), y=ES, ymin=LCI, ymax=UCI)) +
  geom_pointrange()+ # Makes range for ggplot values based on the data and AES specified in first line
  geom_hline(yintercept=0, lty=1, size =1) +  # add a  line at x=0 after flip
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=0.5, cex=1)+ # Makes whiskers on the range
  facet_wrap(~DV)+ # Makes DV header (Can handle multiple DVs)
  coord_flip() + # flip coordinates (puts labels on y axis)
  geom_point(shape = 15, size = 5) + # specifies the size and shape of the geompoint
  ggtitle("")+ # Blank Title for the Graph
  xlab("") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("Any abnormality (proportion)  ") + # Label on the X axis (flipped specification do to coord_flip)
  scale_y_continuous(limits = c(0,.15), breaks = c(0,.05,.10,.15))+ # limits and tic marks on X axis (flipped specification do to coord_flip)
  theme(line = element_line(colour = "white", size = 3),
        strip.background = element_rect(fill="white"),
        legend.position ="none",
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.border= element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(colour = "Black", margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text=element_text(size=24, color = "Black"),
        text=element_text(size=24), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"))

if (write_to_file == 1) {
  dev.off()
}

# 
# 
############# Figure 2b. Forest plots: MRI abnormalities in first episode psychosis grouped by neuroanatomical type (clinically relevant abnormalities)  ###############


# Extract raw proportions from meta-analytic objects using the extract_proportions function
# and add each one to the bottom of the summary_df_cr dataframe

# Create empty dataframe to store results
summary_df_cr <- data.frame(type=character(),
                         prop = numeric(),
                         prop_lower = numeric(),
                         prop_higher = numeric(),
                         stringsAsFactors=FALSE)

extractpr_cr <- extract_proportions(pes_fep_cr_ventricular_summary, "Ventricular")
summary_df_cr[nrow(summary_df_cr)+1,] <- extractpr_cr

extractpr_cr <- extract_proportions(pes_fep_cr_pituitary_summary, "Pituitary")
summary_df_cr[nrow(summary_df_cr)+1,] <- extractpr_cr

extractpr_cr <- extract_proportions(pes_fep_cr_tumour_summary, "Tumour")
summary_df_cr[nrow(summary_df_cr)+1,] <- extractpr_cr

extractpr_cr <- extract_proportions(pes_fep_cr_other_summary, "Other")
summary_df_cr[nrow(summary_df_cr)+1,] <- extractpr_cr

extractpr_cr  <- extract_proportions(pes_fep_cr_vascular_summary, "Vascular")
summary_df_cr[nrow(summary_df_cr)+1,] <- extractpr_cr

extractpr_cr  <- extract_proportions(pes_fep_cr_cyst_summary, "Cyst")
summary_df_cr[nrow(summary_df_cr)+1,] <- extractpr_cr

extractpr_cr  <- extract_proportions(pes_fep_cr_atrophy_summary, "Atrophy")
summary_df_cr[nrow(summary_df_cr)+1,] <- extractpr_cr

extractpr_cr  <- extract_proportions(pes_fep_cr_white_matter_summary, "White Matter")
summary_df_cr[nrow(summary_df_cr)+1,] <- extractpr_cr


# Change relevant columns to numeric)
summary_df_cr$prop <- as.numeric(summary_df_cr$prop)
summary_df_cr$prop_lower <- as.numeric(summary_df_cr$prop_lower)
summary_df_cr$prop_higher <- as.numeric(summary_df_cr$prop_higher)

# visualisation -----------------------------------------------------------

# global options

options(scipen=100, digits=6)
options(max.print = 500000000)

DV_cr    <- c("","","","","","","","")
IV_cr <- summary_df_cr$type
ES_cr <- summary_df_cr$prop
LCI_cr <- summary_df_cr$prop_lower
UCI_cr <- summary_df_cr$prop_higher

subtype_cr_ab <- data.frame(DV_cr,IV_cr,ES_cr,LCI_cr,UCI_cr)

if (write_to_file == 1) {
  png(file=paste('output/', 'Figure_2b.png', sep=''), width = 10, height = 10, units = 'in', res = 200)
}

ggplot(data=subtype_cr_ab, aes(x= reorder(IV_cr,-ES_cr), y=ES_cr, ymin=LCI_cr, ymax=UCI_cr)) +
  geom_pointrange()+ # Makes range for ggplot values based on the data and AES specified in first line
  geom_hline(yintercept=0, lty=1, size =1) +  # add a  line at x=0 after flip
  geom_errorbar(aes(ymin=LCI_cr, ymax=UCI_cr), width=0.5, cex=1)+ # Makes whiskers on the range
  facet_wrap(~DV_cr)+ # Makes DV header (Can handle multiple DVs)
  coord_flip() + # flip coordinates (puts labels on y axis)
  geom_point(shape = 15, size = 5) + # specifies the size and shape of the geompoint
  ggtitle("")+ # Blank Title for the Graph
  xlab("") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("Clinically relevant abnormality (proportion)  ") + # Label on the X axis (flipped specification do to coord_flip)
  scale_y_continuous(limits = c(0,.15), breaks = c(0,.05,.10,.15))+ # limits and tic marks on X axis (flipped specification do to coord_flip)
  theme(line = element_line(colour = "white", size = 3),
        strip.background = element_rect(fill="white"),
        legend.position ="none",
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.border= element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(colour = "Black", margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text=element_text(size=24, color = "Black"),
        text=element_text(size=24), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"))

if (write_to_file == 1) {
  dev.off()
}
