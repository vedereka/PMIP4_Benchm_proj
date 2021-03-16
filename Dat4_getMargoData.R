### Read MARGO SST data for LGM
# 
# Created by Gill Thornhill in Feb2021
# Last modified: February 2021
# Note: Africa plots commented (not needed)

#### LOAD ALL DATA AND ARRANGE TO COMMON VARIABLES #### 

margoALL <- read.csv(paste (dataobspath,"/ocean_data/margodatagridded_edit.csv",sep="")) %>% 
  dplyr::select(LON, LAT,SST_ANN, SST_JAS, SST_JFM, ANOM_ANN,ANOM_JAS,ANOM_JFM, ANOM_SD_ANN,ANOM_SD_JAS, ANOM_SD_JFM,ANOM_MIN_ANN,	ANOM_MIN_JAS,	ANOM_MIN_JFM,	ANOM_MAX_ANN,	ANOM_MAX_JAS,	ANOM_MIN_JFM) %>%
  dplyr::mutate(LON = ((LON+ 180) %% 360) - 180)
margoALL[margoALL == -99.99] <- NA 

#
# Create a .csv file that is in same format as the Bartlein data (Same column names etc, to simply scores calculation. Later can combine other ocean datasets.
margoOut <- margoALL %>% 
  dplyr::select(LON, LAT,SST_ANN, ANOM_ANN, ANOM_SD_ANN) %>% 
  dplyr::rename (lon=LON, lat=LAT, SST_ann=SST_ANN, SST_anom_ann=ANOM_ANN, SST_anom_ann_sd=ANOM_SD_ANN)
  

data_margo_clean <- cbind(margoOut, "Margo") %>% 
  `colnames<-`(c("lon", "lat", "SST_ann", "SST_anom_ann", "SST_anom_ann_sd", "REF"))
write.csv(data_margo_clean, paste(dataobspath, "ocean_data/data_margo_clean.csv", sep=""), row.names=FALSE)

#df <- melt(margoALL, na.rm = FALSE, id = c('LAT','LON'))
#grid <- expand.grid(lon = lon, lat = lat)
#grid$lat_min <- grid$lat - mean(diff(lat)) / 2
#grid$lat_max <- grid$lat + mean(diff(lat)) / 2
#grid$lon_min <- grid$lon - mean(diff(lon)) / 2
#grid$lon_max <- grid$lon + mean(diff(lon)) / 2
#grid$count_n <- NA

#### CREATE GRIDDED DATASETS ####
#requirement: user-defined "data_obs_raw" function (see in functions_source.R)

#BP_CL_244_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
#  mutate (REF = "CL_wof") %>%  rbind(., BP_wof) %>% mutate (REF = as.factor (REF))
# BP_CL_wof %>%  write.csv(., file.path(dataobspath, "BP_CL_wof.csv"),  na = "NA",  row.names = FALSE)

#rm(ls="grid")


###--------------------------------------------------------------------------------
var_ls <- c("SST_ANN", "SST_JAS", "SST_JFM", "ANOM_ANN","ANOM_JAS",	"ANOM_JFM")
#var_ls <- c("SST_ANN", "SST_JAS")

refdata1 = "LAT"
num = ncol(margoALL)
n = 3

for (varname in var_ls){
  print(paste(varname))
  n=n+1
#---------------------
# scatterplot
  p <- ggplot(na.omit(margoALL), aes_string(x = 'LAT', varname)) +
    geom_point(alpha = 1,color = "darkred", size = 2) +
    theme(plot.caption = element_text(size=10, vjust = -0.18)) +
    labs(title = paste ("Plot of Margo Data",varname, sep = " "),
         x = refdata1,
         y = varname)
  
  assign(paste("plot",varname,sep="_"),p)
  rm(ls="p")
  fig <- ggarrange(get(paste("plot", varname, sep="_")),   ncol = 1, nrow = 1)
  fig
  
  ggsave(fig,file=paste(plotpath, "MargoOceanplots/",varname,"MargoData.jpg", sep = ""),width = 11.69,height = 8.27)
 
#--------------------- 
# Map plots
  rng <- range (margoALL[n], na.rm = TRUE)
  #a range to have the same min and max for both plots
  #breakcol <- seq(from=round(rng[1]), to=round(rng[2]), length.out = 6)
  breakcol <- c(-12, -9, -6, -3, 0, 3, 6, 9)
  lim_colbar <- c(-12, 12)
  #lim_colbar <- c(floor(rng[1]), ceiling(rng[2]))
  
  p_map <- mp +
    geom_point(data=na.omit(margoALL), aes_string(x='LON', y='LAT', colour = varname),
               alpha = 0.8, size = 1.75, shape = 15, show.legend = T) +
    #scale_fill_gradient2(low = "#e41a1c", mid = "#377eb8", high = "#4daf4a",space = "Lab",
    scale_fill_gradient2(low = "#4daf4a", mid = "#377eb8", high = "#e41a1c",space = "Lab",
                         na.value = "grey50", guide = "colourbar",
                         aesthetics = "colour",breaks=breakcol,
                         limits=lim_colbar) +
    labs(title = paste("Margo", varname, sep=" "), color = varname)
  

  
  assign(paste("map_plot",varname,sep="_"),p_map)
  rm(ls="p_map")
  
  fig <- ggarrange(get(paste("map_plot", varname, sep="_")),   ncol = 1, nrow = 1)
  fig
  
  ggsave(fig,file=paste(plotpath, "MargoOceanplots/",varname,"Map_MargoData.jpg", sep = ""),width = 11.69,height = 8.27)
}

# 
fig <- ggarrange(get(paste("plot_SST_ANN", sep="_")),
get(paste("plot_SST_JAS", sep="_")),
get(paste("plot_SST_JFM", sep="_")),
get(paste("plot_ANOM_ANN", sep="_")),
get(paste("plot_ANOM_JAS", sep="_")),
get(paste("plot_ANOM_JFM", sep="_")),
labels = c("A", "B", "C", "D","E", "F"),
ncol = 3, nrow = 2)

fig

ggsave(fig,file=paste(plotpath,"MargoOceanplots/","MargoData.jpg", sep = ""),width = 11.69,height = 8.27)
 
graphics.off()