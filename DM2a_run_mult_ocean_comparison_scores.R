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

# These scores are based on Douglas' Kelley work on FireMIP benchmarking

# Created by Laia Comas-Bru in October 2020
# Last modified: February 2021

##### SET STUFF ################################################################################
source(paste(getwd(),"/LGM_Benchmarking/cfg.r", sep=""))

#load observations 
#obs_file <- paste (dataobspath,'ocean_data/SST_Masa/ocean_obs_Margo.csv',sep="")
obs_file <- paste (dataobspath,'ocean_data/SST_Masa/ocean_obs_Tierney.csv',sep="")
#obs_file <- paste (dataobspath,'ocean_data/SST_Masa/ocean_obs_AH.csv',sep="")
#obs_file <- paste (dataobspath,'ocean_data/SST_Masa/ocean_obs_glomap.csv',sep="")
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
  ref == "kn" ~ "kn",                                                                        ref == "kn_min" ~ "kn_min",
  ref == "kn_max" ~ "kn_max",
  ref == 'other' ~ 'other'
))


#obsraw$LAT <- obsraw$LAT+0.001 # I have to do this, otherwise one of the models does not work. No idea why!!?
#obsraw$LON <- obsraw$LON+0.001


# location of model output
mod_dir <- ncpath_ocean
mod_files <- list.files(mod_dir, full.names = TRUE)

# create list of model names for output
mod_files_lab <- lapply(list.files(mod_dir, full.names = F), FUN = my_name_trim)

# variable name in model nc files
#mod_variable_ls <- c('ocean_tas_anom', 'ocean_mtco_anom', 'ocean_mtwa_anom')
mod_variable_ls <- c('ocean_tas_anom')

# use model grid or site locations as basis of comparison
# (basically, Hantson et al 2020 = FALSE; Forrest et al in prep = TRUE)
modgrid = FALSE


# define regions (as in Kageyama et al., 2020 CP in review)

  # #uncomment below to run just one region (in this case N America)
  # region_ls <- rbind(c("NAmerica", 20,50,-140,-60)) %>%
  #   as.data.frame (.) %>%
  #   dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)
  
  
  # #uncomment below to run all regions at once
  # region_ls <- rbind( c("global", -90,90,-180,180),c("NH", 0,90,-180,180),c("NHextratropics", 30,90,-180,180),
  #        c("NTropics", 0,30,-180,180),c("NAmerica", 20,50,-140,-60),
  #        c("TropicalAmericas", -30,30,-120,-35), c("WesternEurope", 35,70,-10,30),#c("TropicalAsia",8,30,60,120),
  #        c("ExtratropicalAsia", 30,75,60,135), c("Africa",-35,35,-10,50)) %>%
  #   as.data.frame (.) %>%
  #   dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)


# Removed ExtraTropical Asia from the set below, as it causes an error (no data points?) 
region_ls <- rbind(c("global", -90,90,-180,180),c("NH", 0,90,-180,180),c("NHextratropics", 30,90,-180,180),
                   c("NTropics", 0,30,-180,180),c("NAmerica", 20,50,-140,-60),
                   c("TropicalAmericas", -30,30,-120,-35), c("WesternEurope", 35,70,-10,30), c("TropicalAsia",8,30,60,120), c("Africa",-35,35,-10,50), c("TropicalOceans", -30,30,-180, 180),c("NorthAtlantic",30,50,-60,-10)) %>%
  as.data.frame (.) %>%
  dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)

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
      print(paste(region, mod_varname, dim(obs)))
            
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