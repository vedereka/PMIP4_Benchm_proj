# Produces scatter plots of the data/model extracted to produce the benchmarking scores in DM2
# 
# The definition of mod_lab will need to be adjusted to match the length of the path name 
#   - mod_lab <- substr(mod_name, A, nchar(mod_name) - B) -> A and B are numbers
#     that will depend on the length of the working path
# If models are updated -> make sure that the model labels manually modifed are ok
# 
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
# 
# This script requires "my_scatterplot" function defined in functions_source.R 
# and also "extractComparison" (sourced in DM2 with:
# source(paste(getwd(),"/LGM_Benchmarking/cfg.r", sep="")) and "modgrid" 
# (modgrid = FALSE)
# 
# Created by Laia Comas-Bru in October 2020
# Last modified: February 2021
# 
##### SET STUFF ################################################################################

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
mod_files_lab [[6]] <- "HadCM3-GLAC" # names too long
mod_files_lab [[7]] <- "HadCM3-ICE"
mod_files_lab [[8]] <- "iLOVECLIM-GLAC" # names too long
mod_files_lab [[9]] <- "iLOVECLIM-ICE"

# variable name in model nc files
mod_variable_ls <- c('tas_anom', 'mtco_anom','mtwa_anom','pre_anom','gdd5_anom')

# define regions (as in Kageyama et al., 2020 CP in review)

  ## uncomment below to run just one region (in this case N America)
  # region_ls <- rbind(c("NAmerica", 20,50,-140,-60)) %>%
  #   as.data.frame (.) %>%
  #   dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)
  
  
  ## uncomment below to run all regions at once
  region_ls <- rbind( c("global", -90,90,-180,180),c("NH", 0,90,-180,180),c("NHextratropics", 30,90,-180,180),
         c("NTropics", 0,30,-180,180),c("NAmerica", 20,50,-140,-60),
         c("TropicalAmericas", -30,30,-120,-35), c("WesternEurope", 35,70,-10,30),#c("TropicalAsia",8,30,60,120),
         c("ExtratropicalAsia", 30,75,60,135), c("Africa",-35,35,-10,50)) %>%
    as.data.frame (.) %>%
    dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)

# define source of data (CL/B)  
source_ls <- unique (obsraw$ref)

##### EXTRACT DATA -> CREATE AND SAVE SCATTERPLOTS ################################################################################

for (source in source_ls) {
  for (region in region_ls$reg_name) {
    for (mod_name in mod_files) {
      
      mod_lab <- substr(mod_name, 55, nchar(mod_name) - 17) # 44 will change if using a different path name
      
      if (mod_name == mod_files[[6]]) {mod_lab <- "Had-GLAC"}
      if (mod_name == mod_files[[7]]) {mod_lab <- "Had-ICE"}
      if (mod_name == mod_files[[8]]) {mod_lab <- "iLOVE-GLAC"}
      if (mod_name == mod_files[[9]]) {mod_lab <- "iLOVE-ICE"}
      
      for (mod_varname in mod_variable_ls) {
        mods <- lapply(mod_name, raster, varname = mod_varname) # for 2D netCDF files
        
        ## filter obsraw by ref and region ##
        obs <- obsraw %>% filter (ref == source)
        
        obs <- obs %>% filter (lat >= region_ls %>% filter (reg_name == region) %>% dplyr::select(min_lat) %>% as.numeric() &
                                 lat <= region_ls %>% filter (reg_name == region) %>% dplyr::select(max_lat) %>% as.numeric() &
                                 lon >= region_ls %>% filter (reg_name == region) %>% dplyr::select(min_lon) %>% as.numeric() &
                                 lon <= region_ls %>% filter (reg_name == region) %>% dplyr::select(max_lon) %>% as.numeric())
        
        ## select relevant variable ##
        obs <- obs[, c(1, 2, which(colnames(obs) == mod_varname))] 
        obs <- na.omit(obs)
        
        obsSims <- extractComparison(mods[[1]])
        
        plot_data <- na.omit(as.data.frame.array(t(obsSims)))
        
        ## save data file for plotting (otherwise, ggarrange loses variables that are modif in loop)
        assign(paste("plot_data", mod_varname, sep = "_"), plot_data)
        rm(ls = "plot_data", "obs", "mods", "obsSims")
      }
      
      fig <- ggarrange(
        my_scatterplot(plot_data_tas_anom),
        my_scatterplot(plot_data_mtco_anom),
        my_scatterplot(plot_data_mtwa_anom),
        my_scatterplot(plot_data_pre_anom),
        my_scatterplot(plot_data_gdd5_anom),
        labels = c("tas", "mtco", "mtwa", "pre", "gdd5"),
        # font.label = list (size=12, face="bold"),
        label.x = 0,
        label.y = 1.028,
        align = "hv",
        ncol = 3,
        nrow = 2
      )
      
      fig <- annotate_figure(
        fig,
        right = text_grob(paste (mod_lab),x = -1,y = 0.4,face = "bold",size = 14),
        bottom = text_grob(
          paste ("Obs vs Sim (prior to scores) \n Source: ", source, "\n Region: ",region, sep = ""),
          y = 3.5,x = 0.88,face = "italic",size = 12),
        top = text_grob(" ", color = "green", rot = 90),
      )
      
      ggsave(
        fig,
        file = paste(plotpath,"DM_scatterplots/",source,"_",region,"_",mod_lab,
                     "_scatterplot.jpg", sep = ""),
        width = 11.69,height = 8.27)
    }
  }
}

graphics.off()
