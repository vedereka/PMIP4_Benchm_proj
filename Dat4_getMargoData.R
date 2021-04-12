### Read MARGO SST data for LGM
# 
# Created by Gill Thornhill in Feb2021
# Last modified: February 2021
# Note: Africa plots commented (not needed)

#### LOAD ALL DATA AND ARRANGE TO COMMON VARIABLES #### 
# This gets the original Margo csv data
margoALL <- read.csv(paste (dataobspath,"/ocean_data/margodatagridded_edit.csv",sep="")) %>% 
  dplyr::select(LON, LAT,SST_ANN, SST_JAS, SST_JFM, ANOM_ANN,ANOM_JAS,ANOM_JFM, ANOM_SD_ANN,ANOM_SD_JAS, ANOM_SD_JFM,ANOM_MIN_ANN,ANOM_MIN_JAS,	ANOM_MIN_JFM,	ANOM_MAX_ANN,	ANOM_MAX_JAS,	ANOM_MAX_JFM) %>% 
  dplyr::mutate(LON = ((LON+ 180) %% 360) - 180)

# Set missing data values to NA 
margoALL[margoALL == -99.99] <- NA 
# Set max and min values to NA if they are the same as the Mean
# Must be a cleaner way to do this
margoALL$ANOM_MIN_ANN[margoALL$ANOM_MIN_ANN == margoALL$ANOM_ANN] <- NA 
margoALL$ANOM_MAX_ANN[margoALL$ANOM_MAX_ANN == margoALL$ANOM_ANN] <- NA 
margoALL$ANOM_MIN_JAS[margoALL$ANOM_MIN_JAS == margoALL$ANOM_JAS] <- NA 
margoALL$ANOM_MAX_JAS[margoALL$ANOM_MAX_JAS == margoALL$ANOM_JAS] <- NA 
margoALL$ANOM_MIN_JFM[margoALL$ANOM_MIN_JFM == margoALL$ANOM_JFM] <- NA 
margoALL$ANOM_MAX_JFM[margoALL$ANOM_MAX_JFM == margoALL$ANOM_JFM] <- NA 

# Create a .csv file that is in same format as the Bartlein data (Same column names etc, to simply scores calculation. Later can combine other ocean datasets.
data_tas_margo_clean <- margoALL %>% mutate(REF = "Margo") %>%
  dplyr::select(LON, LAT,SST_ANN, SST_JAS, SST_JFM, ANOM_ANN, ANOM_JAS,ANOM_JFM, ANOM_MIN_ANN, ANOM_MIN_JAS,	ANOM_MIN_JFM,	ANOM_MAX_ANN,	ANOM_MAX_JAS,	ANOM_MAX_JFM, REF) %>%
  dplyr::rename (lon=LON, lat=LAT, MAT=ANOM_ANN, MTWA=ANOM_JAS, MTCO=ANOM_JFM)

write.csv(data_tas_margo_clean, paste(dataobspath, "ocean_data/data_margo_tas_clean.csv", sep=""), row.names=FALSE)

margo_base <- data_tas_margo_clean %>% 
  dplyr::select(lon, lat, MAT, MTWA, MTCO, REF) %>% 
`colnames<-`(c("lon", "lat", "MAT", "MTWA", "MTCO","REF"))

margo_min <- data_tas_margo_clean %>% 
  dplyr::select(lon, lat, ANOM_MIN_ANN,ANOM_MIN_JAS, ANOM_MIN_JFM, REF) %>% 
  
  dplyr::rename (MAT=ANOM_MIN_ANN, MTWA=ANOM_MIN_JAS, MTCO=ANOM_MIN_JFM) %>%
 `colnames<-`(c("lon", "lat", "MAT", "MTWA", "MTCO","REF"))
 margo_min$REF = "Margo_min"
 
 
margo_max <- data_tas_margo_clean %>%
  dplyr::select(lon, lat, ANOM_MAX_ANN,ANOM_MAX_JAS,	ANOM_MAX_JFM, REF) %>%
  dplyr::rename (MAT=ANOM_MAX_ANN, MTWA=ANOM_MAX_JAS, MTCO=ANOM_MAX_JFM) %>%
`colnames<-`(c("lon", "lat", "MAT", "MTWA", "MTCO","REF"))
 margo_max$REF = "Margo_max"

# margo_bench_ready <- rbind(margo_base, margo_min, margo_max) %>% mutate(PRE = NA, MI = NA, GDD5 = NA) %>%
#   `colnames<-`(c("lon","lat", "ocean_tas_anom", "ocean_mtco_anom", "ocean_mtwa_anom", "ocean_pre_anom", "ocean_mi_anom", "ocean_gdd5_anom", "ref"))
 
 margo_tas_bench_ready <- rbind(margo_base, margo_min, margo_max) %>%
 `colnames<-`(c("lon","lat", "ocean_tas_anom", "ocean_tas_mtco_anom", "ocean_tas_mtwa_anom", "ref"))

 margo_tas_bench_ready <- margo_tas_bench_ready[c("lat","lon", "ocean_tas_anom", "ocean_tas_mtco_anom", "ocean_tas_mtwa_anom", "ref")]

margo_tas_bench_ready %>% write.csv(., file.path(dataobspath, "/ocean_data/margo_bench_ready.csv"),  na = "NA",  row.names = FALSE)

###--------------------------------------------------------------------------------
var_ls <- c("SST_ANN", "SST_JAS", "SST_JFM", "ANOM_ANN","ANOM_JAS",	"ANOM_JFM")
#var_ls <- c("SST_ANN", "SST_JAS")

refdata1 = "LAT"
num = ncol(margoALL)
n = 3

for (varname in var_ls){
  #print(paste(varname))
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
  breakcol <- (seq(from = -25, to = 15,length.out =10))
  lim_colbar <- c(-25, 15)
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