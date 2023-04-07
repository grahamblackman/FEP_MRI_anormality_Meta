Analysis code and results for the study

# Prevalence of Neuroradiological Abnormalities in First Episode Psychosis: a Meta-Analysis 

<p align="center">
	<a href="https://en.wikipedia.org/wiki/R_(programming_language)"><img
		alt="R Programming Language"
		src="https://img.shields.io/badge/Language-R-%232268BB.svg"></a>
	<a href="https://opensource.org/licenses/MIT"><img
		alt="MIT License"
		src="https://img.shields.io/badge/license-MIT-blue.svg"></a>
</p>

This archive contains the code and data for the analysis reported in the above study. The code is written in the [R](https://en.wikipedia.org/wiki/R_(programming_language)) programming language.

This repository contains the following files

0. [FEP_MRI_data.xlsx](https://github.com/vaughanbell/FEP_MRI_anormality_Meta/blob/main/FEP_MRI_data.xlsx) - data extracted for meta-analysis used for this analysis
1.  [Master_FEP_MRI_MetaAnal.R](https://github.com/grahamblackman/FEP_MRI_anormality_Meta/blob/main/Master_FEP_MRI_MetaAnal.R) - the master script that calls other scripts in order to reproduce the results reported in the manuscript
2.  [meta_mri_processing.R](https://github.com/grahamblackman/FEP_MRI_anormality_Meta/blob/main/meta_mri_processing.R) - pre-processing step that ensures key variables are set to the correct data types and subsets the main dataframes into dataframes that contain specific study types for later analysis
3. [meta_mri_descriptive.R](https://github.com/grahamblackman/FEP_MRI_anormality_Meta/blob/main/meta_mri_descriptive.R) - calculates and displays descriptive statistics
4. [meta_mri_analysis_prev.R](https://github.com/grahamblackman/FEP_MRI_anormality_Meta/blob/main/meta_mri_analysis_prev.R) - calculates the meta analysis of proportions and Eggers / trim and fill / funnel plots
5. [meta_mri_analysis_prev_subtype.R](https://github.com/grahamblackman/FEP_MRI_anormality_Meta/blob/main/meta_mri_analysis_prev_subtype.R) - calculates the meta analysis of proportions of neuroanatomical subtypes
6. [meta_mri_analysis_RR.R](https://github.com/grahamblackman/FEP_MRI_anormality_Meta/blob/main/meta_mri_analysis_RR.R) - calculates meta-analytic risk ratios for differences in abnormalities between FEP sample and healthy controls
7. [meta_mri_visualization.R](https://github.com/grahamblackman/FEP_MRI_anormality_Meta/blob/main/meta_mri_visualization.R) - visualizes the results of the meta-analysis presented in the paper and related analyses (Figures 1a / 1b and figures in the supplementary material)
8. [meta_mri_visualization_subtypes.R](https://github.com/grahamblackman/FEP_MRI_anormality_Meta/blob/main/meta_mri_visualization_subtypes.R) - visualizes the results of the subtype prevalence meta-analysis presented in the paper (Figures 2a / 2b)

---

### Platform and package versions

R language version, and package versions used to generate the results are:

R Version 4.2.1<br>
Package version for meta is 6.2.1<br>
Package version for metafor is 4.0.0<br>
Package version for readxl is 1.4.2<br>
Package version for skimr is 2.1.5<br>
Package version for tidyverse is 2.0.0<br>
Package version for lubridate is 1.9.2<br>
Package version for MASS is 7.3.58.3<br>
Package version for janitor is 2.2.0<br>
Package version for devtools is 2.4.5<br>
Package version for lme4 is 1.1.32<br>
Package version for expss is 0.11.4<br>
Package version for scales is 1.2.1<br>
Package version for dmetar is 0.0.9000<br>
