setwd("D:/Dropbox/Benchm_LGM/PMIP4_Benchm_proj")

print(paste("setup and configuration"))
source('init.R')

print(paste("convert bartlein moisture data from alpha to MI"))
source("Dat0_B_convert_splash.R")

print(paste("Compare cleator versions"))
source("Dat1_compare_CL_versions.R")

print(paste("Latitudinal boxplots with observations"))
source("Dat2_obs_boxplots.R")
print(paste("Done. See output at: "))

print(paste("Compare Bartlein with Cleator 244 dataset"))
source("Dat3_compare_B_CL.R")
print(paste("Done. See output at: "))

print(paste("extract PMIP4 model metadata"))
source("Mod0_metadata.R")
print(paste("Done. See output at: "))

print(paste("Correct netcdf issues and plot mon anomalies"))
source("Mod1_homog_nc_plot_mon.R")
print(paste("Done. See output at: "))


print(paste("compute model variables for DM comparison"))
source("Mod2_create_vars_nc.R")
print(paste("Done. See output at: "))


print(paste("data-model latitudinal boxplots"))
source("DM1_boxplots.R.R")
print(paste("Done. See output at: "))


print(paste("run DM comparison and produce benchmarking scores"))
source("DM2_run_comparison_scores.R")
print(paste("Done. See output at: "))


print(paste("produce DM scatterplots"))
source("DM3_scatterplots.R")
print(paste("Done. See output at: "))


print(paste("create traffic light plot with scores"))
source("DM4_plot_scores.R")
print(paste("Done. See output at: "))












