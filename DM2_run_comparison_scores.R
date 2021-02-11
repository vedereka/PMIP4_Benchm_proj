### produce csv file with benchmarking scores. 
# Use Kageyama 2020 (cp-2019-169) regions:
# ### definition of the regions: latitude range, longitude range
#   'Globe':[(-90,90,'cc'),(-180,180,'cc')],
#   Tropics':[(-30,30,'cc'),(-180,180,'cc')],
#   NAtlEurope':[(30,50,'cc'),(-45,45,'cc')],
#   NorthAtlantic':[(30,50,'cc'),(-60,-10,'cc')],
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
obs_file <- paste (dataobspath,'/data_obs_all_wf.csv',sep="")
obsraw <- read.csv(obs_file)  

#modify ref names (to short comb)
obsraw <- obsraw %>% mutate(ref = ifelse(ref == "bartlein_min" , "B_min",
                                                   ifelse( ref == "bartlein_max", "B_max",
                                                          ifelse(ref == "bartlein", "B",
                                                                 ifelse( ref == "prentice", "P", 
                                                                        ifelse( ref == "cleator_min", "CL_min",
                                                                                ifelse(ref == "cleator_max", "CL_max",
                                                                                       ifelse(ref == "cleator", "CL", "other"))))))))

obsraw <- obsraw %>% filter (ref != "P") # no uncertainties in Prentice et al data -> remove completely
obsraw$lat <- obsraw$lat+0.001 # I have to do this, otherwise one of the models does not work. No idea why!!?
obsraw$lon <- obsraw$lon+0.001

obsraw <- obsraw %>% filter (obsraw$ref != "B", obsraw$ref != "B_min", obsraw$ref != "B_max") # no B data in the tropics.

# location of model output
mod_dir <- ncpath
mod_files <- list.files(mod_dir, full.names = TRUE)

# create list of model names for output
mod_files_lab <- lapply(list.files(mod_dir, full.names = F), FUN = my_name_trim)
mod_files_lab [[5]] <- "iLOVECLIM-GLAC" # names too long otherwise (awkward in plot)
mod_files_lab [[6]] <- "iLOVECLIM-ICE"

# variable name in model nc files
mod_variable_ls <- c('tas_anom', 'mtco_anom','mtwa_anom','pre_anom','gdd5_anom')

# use model grid or site locations as basis of comparison
# (basically, Hantson et al 2020 = FALSE; Forrest et al in prep = TRUE)
modgrid = FALSE


# define regions (as in Kageyama et al., 2020 CP in review)

  # #uncomment below to run just one region (in this case N America)
  # region_ls <- rbind(c("NAmerica", 20,50,-140,-60)) %>%
  #   as.data.frame (.) %>%
  #   dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)
  
  
  # #uncomment below to run all regions at once
  region_ls <- rbind( c("global", -90,90,-180,180),c("NH", 0,90,-180,180),c("NHextratropics", 30,90,-180,180),
         c("NTropics", 0,30,-180,180),c("NAmerica", 20,50,-140,-60),
         c("TropicalAmericas", -30,30,-120,-35), c("WesternEurope", 35,70,-10,30),#c("TropicalAsia",8,30,60,120),
         c("ExtratropicalAsia", 30,75,60,135), c("Africa",-35,35,-10,50)) %>%
    as.data.frame (.) %>%
    dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)


# define source of data
source_ls <- unique (obsraw$ref)
  
##### RUN COMPARISON -> SAVE SCORES ################################################################################

for (source in source_ls) {
  for (region in region_ls$reg_name) {
    
    out_filename <- paste (source, "_", region, ".csv", sep = "")
    
    for (mod_varname in mod_variable_ls) {
      ## filename with output ##
      scores_output_filename <- paste(getwd(),"/output_scores/", mod_varname, "_NME_", out_filename, sep = "")
      
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
      obs <- na.omit(obs)
      
      ## save output ##
      comp_output = matrix(
        data = NA,
        nrow = length(mod_variable_ls),
        ncol = length(mods) + 5
      )
      
      ## compare ##
      scores = sapply(mods, makeComparison)
      null_scores = summary(null.NME(obs[, 3]))
      
      ## output ##
      out = cbind(mod_varname, null_scores[1], null_scores[2], scores)
      colnames(out) = c("varname", "mean_null", "random_null", mod_files_lab)
      write.csv(file = scores_output_filename, out)
    }
  }
}
