### produce csv file with benchmarking scores. 
# Use Kageyama 2020 (cp-2019-169) regions:
# ### definition of the regions: latitude range, longitude range
#   'Globe':[(-90,90,'cc'),(-180,180,'cc')],
#   Tropics':[(-30,30,'cc'),(-180,180,'cc')],
#   NAtlEurope':[(30,50,'cc'),(-45,45,'cc')],
#   NorthAtlantic':[(30,50,'cc'),(-60,10,'cc')],
#   Europe':[(35,70,'cc'),(-10,60,'cc')],
#   WesternEurope':[(35,70,'cc'),(-10,30,'cc')],
#   NWAmerica':[(20,50,'cc'),(-125,-105,'cc')],
#   NEAmerica':[(20,50,'cc'),(-105,-50,'cc')],
#   Africa':[(-35,35,'cc'),(-10,50,'cc'),],
#   WestAfrica':[(5,30,'cc'),(-17,30,'cc'),],
#   NAmerica':[(20,50,'cc'),(-140,-60,'cc'),],
#   SHextratropics':[(-90,-30,'cc'),(-180,180,'cc')],
#   NHextratropics':[(30,90,'cc'),(-180,180,'cc')],
#   NTropics':[(0,30,'cc'),(-180,180,'cc')],
#   ExtratropicalAsia':[(30,75,'cc'),(60,135,'cc')],
#   TropicalAsia':[(8,30,'cc'),(60,120,'cc')],
#   TropicalAmericas':[(-30,30,'cc'),(-120,-35,'cc')],
# -----------------------------------------------------------------
#  My own ocean basin definitions
#  Check selections that go across 'dateline' (-180 to 180, as selection may go wrong way) #   and may need to be split to W. and E. versions across Pacific

#   Tropics':[(-30,30,'cc'),(-180,180,'cc')],
#   NAtlEurope':[(30,50,'cc'),(-45,45,'cc')],
#   NorthAtlantic':[(30,50,'cc'),(-60,-10,'cc')],
#   ArcticOcean':[(75,90,'cc'),(-180,180,'cc')],
#   SAtlantic':[(10,-50,'cc'),(15,-70,'cc')],
#   MidAtlantic':[(10,30,'cc'),(-90,25,'cc')],
#   IndianOcean':[(0,-30,'cc'),(40,100,'cc'),],
#   WPacNExtraTropics':[(30,60,'cc'),(-180, -120,'cc')],
#   EPacNExtraTropics':[(30,60,'cc'),(130, 180,'cc')],
#   WPacTropics':[(-30,30,'cc'),(-180,-80,'cc'),],
#   EPacTropics':[(-30,30,'cc'),(130,180,'cc'),],
#   WPacSExtraTropics':[(-60,-30,'cc'),(-180,-70,'cc'),],
#   EPacSExtraTropics':[(-60,-30,'cc'),(120,180,'cc'),],
#  
#  
#-------------------------------------------------------------

# These scores are based on Douglas' Kelley work on FireMIP benchmarking

# Created by Laia Comas-Bru in October 2020
# Last modified: February 2021

##### SET STUFF ################################################################################
source('region_def.R')
source(paste(getwd(),"/LGM_Benchmarking/cfg.r", sep=""))

#load observations 
#obs_file <- paste (dataobspath,'ocean_data/SST_Masa/ocean_obs_Margo.csv',sep="")
#obs_file <- paste (dataobspath,'ocean_data/SST_Masa/ocean_obs_Tierney.csv',sep="")
#obs_file <- paste (dataobspath,'ocean_data/SST_Masa/ocean_obs_T_Grid.csv',sep="")
obs_file <- paste (dataobspath,'ocean_data/SST_Masa/ocean_obs_glomap.csv',sep="")
#obs_file <- paste (dataobspath,'ocean_data/SST_Masa/ocean_obs_AH.csv',sep="")

#obs_file <- paste (dataobspath,'ocean_data/SST_Masa/ocean_obs_kn.csv',sep="")


obsraw <- read.csv(obs_file)   

#modify ref names (to short comb)
obsraw <- obsraw %>% mutate(ref = case_when(
  ref == "Margo_min" ~ "Margo_min",
  ref == "Margo_max" ~ "Margo_max",
  ref == "Margo" ~ "Margo",
  ref == "Tierney_min" ~ "Tierney_min",
  ref == "Tierney_max" ~ "Tierney_max",
  ref == "Tierney"~ "Tierney",
  ref == "AH_min"~ "AH_min",
  ref == "AH_max" ~ "AH_max",
  ref == "AH" ~ "AH",
  ref == "glomap" ~ "glomap",
  ref == "glomap_min" ~ "glomap_min",
  ref == "glomap_max" ~ "glomap_max",
  ref == "kn" ~ "kn",                                                               
  ref == "kn_min" ~ "kn_min",
  ref == "kn_max" ~ "kn_max",
  ref == "T_Grid" ~ "T_Grid",       
  ref == "T_Grid_min" ~ "T_Grid_min",
  ref == "T_Grid_max" ~ "T_Grid_max",
  ref == 'other' ~ 'other'
))


#obsraw$LAT <- obsraw$LAT+0.001 # I have to do this, otherwise one of the models does not work. No idea why!!?
#obsraw$LON <- obsraw$LON+0.001


# location of model output
mod_dir <- ncpath_ocean
mod_files <- list.files(mod_dir, pattern = "anomalies", full.names = TRUE)

# create list of model names for output
mod_files_lab <- lapply(list.files(mod_dir, pattern = "anomalies", full.names = F), FUN = my_name_trim)

# variable name in model nc files
#mod_variable_ls <- c('ocean_tas_anom', 'ocean_mtco_anom', 'ocean_mtwa_anom')
mod_variable_ls <- c('ocean_tas_anom')

# use model grid or site locations as basis of comparison
# (basically, Hantson et al 2020 = FALSE; Forrest et al in prep = TRUE)
modgrid = FALSE

# ------------------------------------------------------------
#------------------------------------------------------------------
# Use regions as defined in region_def.R
region_ls <- region_ls_zonal

# define source of data
source_ls <- unique (obsraw$ref)

##### RUN COMPARISON -> SAVE SCORES ################################################################################

for (source in source_ls) {
  print(source)
  for (region in region_ls$reg_name) {
    
    out_filename <- paste (source, "_", region, ".csv", sep = "")
    
    for (mod_varname in mod_variable_ls) {
      ## filename with output ##
      scores_output_filename <- paste(getwd(),"/output_scores/Ocean/",mod_varname, "_NME_", out_filename, sep = "")
      
      ## open stuff ## s
      mods <-
        lapply(mod_files, raster, varname = mod_varname) # for 2D netCDF files
      
      ## filter obsraw by ref and region ##
      obs <- obsraw %>% filter (ref == source)
      obs <- obs %>% filter (lat >= region_ls %>% filter (reg_name == region) %>% dplyr::select(min_lat) %>% as.numeric() &
                             lat <= region_ls %>% filter (reg_name == region) %>% dplyr::select(max_lat) %>% as.numeric() &
                             lon >= region_ls %>% filter (reg_name == region) %>% dplyr::select(min_lon) %>% as.numeric() &
                             lon <= region_ls %>% filter (reg_name == region) %>% dplyr::select(max_lon) %>% as.numeric())
      
      obs <-
        obs[, c(1, 2, which(colnames(obs) == mod_varname))] # select relevant variable
      #obs <-
        #obs[, c(1, 2, which(colnames(obs) == "SST_anom_ann"))] # select relevant variable
      obs <- na.omit(obs)
      #print(paste(region, mod_varname, dim(obs)))
      #Should check for 0 data points, and skip over is no data)
      print(paste(region,dim(obs)[1]))
      if (dim(obs)[1] == 0) {
        print(paste(region, "No obs", sep =" "))
       next
      }
           
      #print(paste(region, length(obs$ocean_tas_anom)))
      ## save output ##
      comp_output = matrix(
        data = NA,
        nrow = length(mod_variable_ls),
        ncol = length(mods) + 5
      )
      
      ## compare ##
      scores = sapply(mods, makeComparison)
      null_scores = summary(null.NME(obs[, 3]))
      #print(null_scores)
      ## output ##
      out = cbind(mod_varname, null_scores[1], null_scores[2], scores)
      colnames(out) = c("varname", "mean_null", "random_null", mod_files_lab)
      write.csv(file = scores_output_filename, out)
    }
  }
}
graphics.off()