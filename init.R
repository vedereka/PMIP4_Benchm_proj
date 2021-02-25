# This file is to setup the LGM benchmarking scripts in the repository
# created by Laia Comas-Bru in Feb 2021

rm (list = ls())
graphics.off()

# set wd (revise as necessary)
 # setwd("D:/Dropbox/Benchm_LGM/PMIP4_Benchm_proj")

options( scipen = 0, digits=3 )

# load contributed packages
if(!require("pacman")) install.packages ("pacman")
pacman::p_load (zoo, ncdf4, facetscales, ggpubr, tidyverse, ggExtra, reshape2,
                naniar, RColorBrewer, plyr, dplyr, ggplot2, xts, DescTools,
                sjmisc, raster, lattice, pracma)


# if(!require("rgdal")) install.packages ("rgdal")
# options("rgdal_show_exportToProj4_warnings"="none")
# library(rgdal)

# load functions 
source('functions_source.R')#load functions and coast shapefile

# setup paths
# data paths, in main directory, now in gitignore (=not recorded in Git)
dataobspath <- paste(getwd(), "/input_data_obs/", sep="") 
pmip_ncpath <- paste(getwd(), "/input_pmip4_nc/", sep="")
rdspath <- paste(getwd(), "/output_rds_temp/", sep="") 
ncpath <- paste(getwd(), "/output_netcdf/", sep="")

#within working directory
plotpath <- paste(getwd(), "/output_plots/", sep="") 
outputpath <- paste(getwd(), "/splash_data_input/", sep="") 
datapath <- paste(getwd(), "/output/", sep="") 

# map background 
mp <- ggplot() +
  coord_fixed(xlim = c(-180, 180),
              ylim = c(-60, 80),
              expand = F) +
  theme_bw() +
  xlab('Longitude [deg]') +
  ylab('Latitude [deg]') +
  theme(axis.title.x = element_text(size = rel(0.8))) +
  theme(axis.text.x = element_text(size = rel(0.8))) +
  theme(axis.title.y = element_text(size = rel(0.8))) +
  theme(axis.text.y = element_text(size = rel(0.8))) +
  mapWorld_grey

# # clear packages
# pacman::p_unload(all)
# detach("package:datasets", unload=T)
# 
# # Clear packages
# p_unload(all)  # Remove all add-ons
# 
# # Clear plots
# graphics.off()
# 
# # Clear environment
# rm(list = ls())
# 
# # # clear console
# cat("\014") #ctrl+L

# clean environment
# rm(list=ls(pattern="_def$"))
# rm(list="ncname", "ncfname")
# rm(list=setdiff(ls(), c("model", "model_ls", "variab_ls"))) # remove all variables except
# source ("init.R")