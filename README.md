# PMIP4_Benchm_proj

README file for PMIP4_Benchm_proj R project.
Created by Laia Comas-Bru in February 2021.

---

`init.R`
Initialise run and setp up paths, functions, etc. 
It calls functions_source.R to load a set of functions needed to plot or transform the data.
Make sure that the folders' structure is ok

---

## LOAD, TRANSFORM, CHECK AND PLOT OBSERVATIONAL DATA 

`Dat0_B_convert_splash.R`
Converts Bartlein moisture data from ALPHA to MI. It saves a csv file with bartlein converted data in /input_data_obs. It requires the SPLASH code by Prentice. See README file in "splash_r_prentice" folder. Some files in that folder have been slightly modified to remove unneeded steps. The code of the original v.1.0 (Davis et a., 2017) is hosted here: https://bitbucket.org/labprentice/splash/src/master/. All subsequent "Dat" scripts require the output of this one saved as "Bartlein_converted.csv" (without SysDate()).Output: \splash_data_temp and \input_data_obs

`Dat1_compare_CL_versions.R`
This script produces a set of plots comparing CL229 and CL244. Output: output_plots\dat_CL_versions

`Dat2_obs_boxplots.R`
It requires to have Bartlein moisture data converted to MI. Create data boxplots (with Bartlein and Cleator for overlapping sites). Create maps of Bartlein sites with/without inversions and maps with data available for each variable. Output: output_plots\dat_boxplots_maps

`Dat3_compare_B_CL.R`
Creates some plots comparing coverage (and magnitudes) in B and CL(244). Output: 

---

## LOAD AND PREPARE MODEL DATA

`Mod0_metadata.R`
script to output a text file with the main metadata fields of the raw PMIP4 netcdf files. Useful to look for inconsistencies. Output: \output

`Mod1_homog_nc_plot_mon.R`
Load PMIP4 model data, fix inconsistencies, apply land/ice masks, plot monthly data and save RDS data file to calculate anomalies in a subsequent step. Outputs: \output_plots\mod_LGM_maps, \output_plots\mod_PI_maps, \output_plots\mod_anom_maps and output_rds_temp

`Mod2_create_vars_nc.R`
Load PMIP4 model data saved as RDS (output of Mod1 script), calculate MAP, MAT, MTCO, MTWA and GDD5 mean annual anomalies. Save anomalies as clean, homogeneised netcdf files (one nc file per model).Output: \output_netcdf

`Mod2a_clt_to_mi_NOTWORKING.R`
This script attempts at converting clt model data to MI using SPLASH but something is not working. When fixed, it should be embedded to Mod2_create_vars_nc.R

---

## DATA MODEL COMPARISONS

`DM1_boxplots.R`
Produces data model boxplots. Output: \output_plots\DM_boxplots

`DM2_run_comparison_scores.R`
Produces csv files with the benchmarking scores (with observational uncertainties). Output: \output_scores

`DM3_scatterplots.R`
Produces data-model scatterplots (same extraction as for scores in DM2).Make sure that L84 is fixed according to your own wd path (in this case, 55 is triming the entire path until the first letter of the model and "17" is removing "_LGM_anomalies.nc"
Output: \output_plots\DM_scatterplots

`DM4_plot_scores.R`
Produces plot with scores (as exported in DM2) taking into account the observational uncertainties. Traffic light plot. Output: \output_plots\DM_scores

---

People involved in creating these scripts/workflow: 

- L.Comas-Bru (l.comasbru@reading.ac.uk; laia.comas-bru@ucdconnect.ie; owner). Orcid: 0000-0002-7882-4996
- K. Atsawawaranunt (produced Spline interpolation with conservation of mean function). Orcid: 0000-0003-3995-5448
- D. Kelley (produced the original scripts to output the scores). Orcid:0000-0003-1413-4969
- M. Noguera-Julian (provided help with R scripting and plotting). Orcid: 0000-0002-6194-1395
