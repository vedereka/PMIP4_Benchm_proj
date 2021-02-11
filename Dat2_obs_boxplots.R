### create data boxplots (with Bartlein and Cleator for overlapping sites)
# create maps of Bartlein sites with/without inversions as well as maps with data available for each variable
# Created by Laia Comas-Bru in September 2020
# Last modified: February 2021


#### LOAD OBSERVATIONS AND ORGANISE DATA #### 
bart <- read.csv(paste (dataobspath,"bartlein_converted.csv",sep="")) %>% 
  mutate (INVERSION = as.factor(INVERSION), MAT_max = MAT + MAT_SD , MTCO_max = MTCO + MTCO_SD, 
          MTWA_max = MTWA + MTWA_SD, MAP_max = MAP + MAP_SD, MI_max = MI_converted + MI_converted_SD,
          GDD5_max = GDD5 + GDD5_SD, MI_max = MI_converted + MI_converted_SD,
          MAT_min = MAT - MAT_SD, MTCO_min = MTCO - MTCO_SD, MTWA_min = MTWA - MTWA_SD, 
          MAP_min = MAP - MAP_SD, MI_min = MI_converted - MI_converted_SD, GDD5_min = GDD5 - GDD5_SD)

#numb of sites with no inv
# bart %>% filter (INVERSION == "NO") %>% select (SAMPLE_ID) %>% dim(.)

#map with/without inversions
p1 <- mp + geom_point(data = bart %>% arrange(desc(INVERSION)), aes (x = LON, y = LAT, color = as.factor(INVERSION), 
                      shape = as.factor(INVERSION),size = ordered(INVERSION)), alpha = 0.6, stroke = 1,
                      show.legend = T, position = "jitter") +
  theme_bw() + # apply bw them
  labs(title = "Bartlein sites (raw)", color = "Inversion", shape = "Inversion", size = "Inversion") +
  theme(legend.position = c(0.063, 0.18), legend.box = "horizontal") +
  theme(legend.title = element_text(colour = "black", size = 8, face = "bold" )) + # legend title
  theme(legend.text = element_text(colour = "black", size = 8)) + # legend labels
  theme(legend.background = element_rect(fill = "white",size = 0.4,linetype = "solid",colour = "black"))

ggsave(plot=p1,file=paste(plotpath,"dat_boxplots_maps/map_Bartlein_inversions.pdf", sep=""),width=10,height=4.5)
#ggsave(plot=p1,file=paste(plotpath,"dat_boxplots_maps/map_Bartlein_inversions.jpg", sep=""),width=10,height=4.5)


#load all data and arrange to common variables

bart_max <- bart %>% filter (INVERSION == "NO") %>% 
  mutate (REF = "bartlein_max") %>% 
  dplyr::select(LAT,LON,MAT_max,MTCO_max,MTWA_max,MAP_max,MI_max,GDD5_max,REF) %>% 
  dplyr::rename (MAT = MAT_max, MTCO = MTCO_max, MTWA = MTWA_max, MAP = MAP_max, MI = MI_max, GDD5 = GDD5_max)

bart_min <- bart %>% filter (INVERSION == "NO") %>% 
  mutate (REF = "bartlein_min") %>% 
  dplyr::select(LAT,LON,MAT_min,MTCO_min,MTWA_min,MAP_min,MI_min,GDD5_min,REF) %>% 
  dplyr::rename (MAT = MAT_min, MTCO = MTCO_min, MTWA = MTWA_min, MAP = MAP_min, MI = MI_min, GDD5 = GDD5_min)

bart <- bart %>% filter (INVERSION == "NO") %>% 
  mutate (REF = "bartlein") %>% 
  dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI_converted,GDD5,REF) %>% 
  dplyr::rename (MI = MI_converted)

clea <- read.csv(file.path(dataobspath, "cleator244.csv"),na.strings = "NA",strip.white = TRUE) %>% 
  mutate (REF = "cleator") %>% dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI,GDD5,REF)

clea_max <- read.csv(file.path(dataobspath, "cleator244.csv"),na.strings = "NA",strip.white = TRUE) %>% 
  mutate (REF = "cleator_max", MAT_max = MAT + MAT_SD , MTCO_max = MTCO + MTCO_SD, MTWA_max = MTWA + MTWA_SD,
          MAP_max = MAP + MAP_SD, MI_max = MI + MI_SD, GDD5_max = GDD5 + GDD5_SD, MI_max = MI + MI_SD) %>% 
  dplyr::select(LAT,LON,MAT_max,MTCO_max,MTWA_max,MAP_max,MI_max,GDD5_max,REF) %>% 
  dplyr::rename (MAT = MAT_max, MTCO = MTCO_max, MTWA = MTWA_max, MAP = MAP_max, MI = MI_max, GDD5 = GDD5_max)

clea_min <- read.csv(file.path(dataobspath, "cleator244.csv"),na.strings = "NA",strip.white = TRUE) %>% 
  mutate (REF = "cleator_min", MAT_min = MAT - MAT_SD, MTCO_min = MTCO - MTCO_SD, MTWA_min = MTWA - MTWA_SD,
          MAP_min = MAP - MAP_SD, MI_min = MI - MI_SD, GDD5_min = GDD5 - GDD5_SD) %>% 
  dplyr::select(LAT,LON,MAT_min,MTCO_min,MTWA_min,MAP_min,MI_min,GDD5_min,REF) %>% 
  dplyr::rename (MAT = MAT_min, MTCO = MTCO_min, MTWA = MTWA_min, MAP = MAP_min, MI = MI_min, GDD5 = GDD5_min)

prenti <-read.csv(file.path(dataobspath, "prentice.csv"), strip.white = TRUE) %>% 
  mutate (REF = "prentice", MTCO = NA, GDD5 = NA, MAP = NA, MTWA = NA) %>% 
  dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI,GDD5,REF)

# merge the datasets together
data_obs <- rbind(prenti, clea, clea_min, clea_max, bart, bart_min, bart_max) %>% 
  `colnames<-`(c("lat", "lon", "tas_anom", "mtco_anom", "mtwa_anom", "pre_anom","mi_anom", "gdd5_anom", "ref"))
data_obs %>% write.csv(., file.path(dataobspath, "data_obs_all_wf.csv"),  na = "NA",  row.names = FALSE)
  
data_BarPre <- rbind(prenti, bart) 
data_BarPre %>%  write.csv(., file.path(dataobspath, "data_obs_BarPre.csv"),  na = "NA",  row.names = FALSE)

data_Cle <- clea 
data_Cle %>%  write.csv(., file.path(dataobspath, "data_obs_Clea.csv"),  na = "NA",  row.names = FALSE)

## load and save a data_obs_no_filter (without Inversion filtering)
bart <- read.csv(paste (dataobspath,"bartlein_converted.csv",sep="")) %>% 
  mutate (INVERSION = as.factor(INVERSION)) %>% mutate (REF = "bartlein") %>% 
  dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI_converted,GDD5,REF) %>% 
  dplyr::rename (MI = MI_converted)

data_obs_all_nofilt <- rbind(prenti, clea, bart) %>% 
  `colnames<-`(c("lat", "lon", "tas_anom", "mtco_anom", "mtwa_anom", "pre_anom","mi_anom", "gdd5_anom", "ref"))
data_obs_all_nofilt %>% write.csv(., file.path(dataobspath, "data_obs_all_nofilt.csv"),  na = "NA",  row.names = FALSE)

rm(ls="prenti","clea", "bart")

# load Bartlein's gridded data
variab_name_ls <- c('mat', 'mtco', 'mtwa', 'map', 'alpha' ,'gdd5')

for (variab_name in variab_name_ls){

ncfname <-  paste (dataobspath, "raw_data/",variab_name,"_delta_21ka_ALL_grid_2x2.nc",sep="")
ncin <- nc_open(ncfname)

x <- ncvar_get(ncin, paste(variab_name,"_anm_mean", sep="")) %>% 
  `colnames<-` (c(ncin[["dim"]][["lat"]][["vals"]])) %>% 
  `rownames<-`(c(ncin[["dim"]][["lon"]][["vals"]])) %>% 
  reshape2::melt(., na.rm=F, value.name = variab_name) %>% 
  dplyr::rename (lon = Var1, lat=Var2)
  

if (variab_name ==variab_name_ls[1]) {
  Bart_grid_raw <- x 
} else {
  Bart_grid_raw <- cbind(Bart_grid_raw, x[,3])
  colnames(Bart_grid_raw)[ncol(Bart_grid_raw)]<-variab_name
}
}

names(Bart_grid_raw) <- toupper(names(Bart_grid_raw))

ggsave(plot(Bart_grid_raw[,c(-1,-2)]) , file=paste(plotpath, "dat_boxplots_maps/scatter_Bart_grid_data_raw_sites.jpg", sep = ""),width = 11.69,height = 8.27)
ggsave(plot(data_BarPre[,c(-1,-2,-9)]), file=paste(plotpath, "dat_boxplots_maps/scatter_Bart_Pre_point_data_raw_sites.jpg", sep = ""),width = 11.69,height = 8.27)


#### PLOT MAPS WITH raw SITES (one per variable) #### 

var_ls <- c("MAT","MTCO","MTWA","MAP","MI","GDD5")

for (var in var_ls){

p1 <- mp + geom_point(data = data_BarPre %>% dplyr::select(LAT,LON,var,REF) %>%
                        na.omit(), aes (x = LON, y = LAT, color = as.factor(REF)),
                      alpha = 1, stroke = 1, show.legend = F, size = 0.3) +
      theme_bw() +
      labs(title = paste("Raw data: ",var,sep=""), color = "Data source") +
      theme(legend.position = c(0.095, 0.24), legend.box = "horizontal") +
      theme(legend.title = element_text(colour = "black", size = 8, face = "bold" )) +
      theme(legend.text = element_text(colour = "black", size = 8)) +
      theme(legend.background = element_rect(fill = "white",size = 0.4,linetype = "solid",colour = "black"))

p2 <- mp + geom_point(data = data_Cle %>% dplyr::select(LAT,LON,var,REF) %>% 
                        na.omit(), aes (x = LON, y = LAT, color = as.factor(REF)),
                      alpha = 1, stroke = 1, show.legend = F, size = 0.1) +
      theme_bw() +
      labs(title = paste("Raw data: ",var,sep=""), color = "Data source") +
      theme(legend.position = c(0.095, 0.24),legend.box = "horizontal") +
      theme(legend.title = element_text(colour = "black",size = 8,face = "bold")) +
      theme(legend.text = element_text(colour = "black", size = 8)) +
      theme(legend.background = element_rect(fill = "white",size = 0.4,linetype = "solid",colour = "black"))


fig <- ggarrange(p1, p2,
                 labels = c("BP", "CL"),
                 ncol = 1, nrow = 2)
fig

ggsave(fig,file=paste(plotpath, "dat_boxplots_maps/map_raw_sites_",var,".jpg", sep = ""),width = 11.69,height = 8.27)
#ggsave(fig,file=paste(plotpath, "dat_boxplots_maps/map_raw_sites_",var,".pdf", sep = ""),width = 11.69,height = 8.27)

rm(ls="p1","p2","fig")
}
#dev.off()

#### SELECT OVERLAPPING SITES BETWEEN BARTLEIN GRIDS AND CLEATOR #### 

# 1. load gridcells from Bartlein's gridded data
# 2. identify how many Bart/Prent sites are in each gridcell
# 3. filter Cleator data to just that spread of data

ncfname <- paste (getwd(), "/input_data_obs/raw_data/mat_delta_21ka_ALL_grid_2x2.nc", sep="")
ncin <- nc_open(ncfname)
lat <- ncin[["dim"]][["lat"]][["vals"]]
lon <- ncin[["dim"]][["lon"]][["vals"]]
rm(ls="ncfname","ncin")

grid <- expand.grid(lon = lon, lat = lat)
grid$lat_min <- grid$lat - mean(diff(lat)) / 2
grid$lat_max <- grid$lat + mean(diff(lat)) / 2
grid$lon_min <- grid$lon - mean(diff(lon)) / 2
grid$lon_max <- grid$lon + mean(diff(lon)) / 2

grid$count_n <- NA

for (n in 1:dim(grid)[1]) {
  newx  <-
    data_BarPre %>% filter (
      data_BarPre$LAT >= grid$lat_min [n] &
        data_BarPre$LAT < grid$lat_max[n] &
        data_BarPre$LON >= grid$lon_min[n] &
        data_BarPre$LON < grid$lon_max[n]
    )
  if (dim(newx)[1] == 0) {
    grid$count_n[n] = NA
  } else {
    grid$count_n[n] <- dim(newx)[1] # how many data points per gridcell?
  }
  if (n == 1) {
    x_temp <- newx [, 3:8] %>% summarise_if(is.numeric, mean, na.rm = T)
  } else {
    x_temp[n,] <- newx [, 3:8] %>% summarise_if(is.numeric, mean, na.rm = T)
  }
}

grid_BartPren <- cbind (grid, x_temp) %>% filter (grid$count_n >= 0) %>% 
  mutate (REF = "BartPren")

rm(ls="n","x_temp","newx","grid")

# select grid lat/lons for which we have BArt/Pren data and filter Cleator's to that geographical range (with averaged values for all variables)
grid <- grid_BartPren [, 1:6]  %>% mutate (count_n = NA)

for (n in 1:dim(grid)[1]) {
  newx  <-
    data_Cle %>% filter (
      data_Cle$LAT >= grid$lat_min [n] &
        data_Cle$LAT < grid$lat_max[n] &
        data_Cle$LON >= grid$lon_min[n] &
        data_Cle$LON < grid$lon_max[n]
    )
  if (dim(newx)[1] == 0) {
    grid$count_n[n] = NA
  } else {
    grid$count_n[n] <- dim(newx)[1] # how many data points per gridcell?
  }
  if (n == 1) {
    x_temp <-
      newx [, 3:8] %>% summarise_if(is.numeric, mean, na.rm = T)
  } else {
    x_temp[n,] <-
      newx [, 3:8] %>% summarise_if(is.numeric, mean, na.rm = T)
  }
}

grid_Cle <- cbind (grid, x_temp) %>% filter (grid$count_n >= 0) %>% 
  mutate (REF = "Cleator")

ggsave(plot(grid_Cle[,8:13]) , file=paste(plotpath, "dat_boxplots_maps/scatter_Clea_selected_grid_sites.jpg", sep = ""),width = 11.69,height = 8.27)
ggsave(plot(data_Cle[,3:8]) , file=paste(plotpath, "dat_boxplots_maps/scatter_Clea_all_grid_sites.jpg", sep = ""),width = 11.69,height = 8.27)

rm(ls="n","x_temp","newx","grid")

# produce maps with the location of overlapping sites (purpose: checking that everything is alright)

# cairo_pdf(paste(plotpath, Sys.Date(), '_sites_overlap_grids_per_variable.pdf', sep = ""),
#           width = 11.69,height = 8.27,onefile = T)

for (var in var_ls){

  data_plot <- grid_BartPren %>% 
    dplyr::select(lat,lon,var,count_n,REF) %>%
    na.omit() %>%
    semi_join(.,grid_Cle %>% dplyr::select(lat,lon,var,count_n,REF)%>% na.omit(),
              by =c("lon","lat"))
  
p <- mp +
  geom_point(data = data_plot ,aes (x = lon,y = lat),alpha = 1, #,color = as.factor(count_n) # removed this from aes
             stroke = 0.5,show.legend = T,size = 0.5, shape = 23,
             color= "darkorchid4", fill="darkorchid1") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = rel(0.6)),
        axis.text.y = element_text(size = rel(0.6)),
        axis.title.y = element_text(size = rel(0.6)),
        axis.title.x = element_text(size = rel(0.6)),
        plot.title = element_text(size = 8, face = "bold"))+
  labs(title = var)#,color = "counts (FYI, data averaged)")

assign(paste(var,"_plot", sep=""),p)

rm(ls="p","data_plot")
}

 fig <- ggarrange(MAT_plot, MTCO_plot, MTWA_plot, MAP_plot, MI_plot, GDD5_plot,
                 #labels = c("A", "B", "C", "D","E", "F"),
                 ncol = 3, nrow = 2)
 fig

ggsave(fig,file=paste(plotpath, "dat_boxplots_maps/map_overlap_sites.jpg", sep=""),width = 11.69,height = 4)
#ggsave(fig,file=paste(plotpath, "dat_boxplots_maps/map_overlap_sites.pdf", sep = ""),width = 11.69,height = 4)

# dev.off()

#save Cle_overlap dataset for scoring (to compare with Cl_all)
grid_Cle %>% mutate (REF = "cleator") %>%
  dplyr::select (lat, lon, MAT, MTCO, MTWA, MAP, MI, GDD5, REF) %>% 
  `colnames<-`(c("lat", "lon", "tas_anom", "mtco_anom", "mtwa_anom", "pre_anom","mi_anom", "gdd5_anom", "ref")) %>% 
  write.csv(., file.path(dataobspath, "data_obs_Clea_overlap.csv"),  na = "NA",  row.names = FALSE)


#### BOXPLOTS WITH LATITUDINAL BANDS ####
# Group the data by latitudinal bands
brkpnt <- seq(-80, 80, by = 20)
startpnt <- brkpnt[1:length(brkpnt) - 1]
endpnt <- brkpnt[2:length(brkpnt)]
brk_lab <- paste(startpnt, '° to ', endpnt, '°', sep = '')

## comparisons for gridded overlapping data sources
# can't apply semijoin here as not all data points have the same site locations
dtBP <- grid_BartPren %>% mutate (source = "BP")
dtCL <- grid_Cle %>% mutate (source = "CL")
dtCL_all <- data_Cle  %>% mutate (source = "CL_all") %>% dplyr::rename (lat= LAT, lon = LON)

dtBP$lat_band <- cut(dtBP$lat, breaks = brkpnt,labels = brk_lab)
dtCL$lat_band <- cut(dtCL$lat, breaks = brkpnt,labels = brk_lab)
dtCL_all$lat_band <- cut(dtCL_all$lat, breaks = brkpnt,labels = brk_lab)

rm(ls="brkpnt","startpnt","endpnt","brk_lab")

# Do for each variable with overlapping sites.

cairo_pdf(paste(plotpath, "dat_boxplots_maps/boxplot_20deg_latbands_pervar.pdf", sep = ""),onefile = T,width = 8.27 ,height = 11.69)
plotlist = list()

for (var in var_ls){

# prepare dataframe with chosen variable and no NAs and rename variable to VAR
  data_ls1 <- dtBP %>% dplyr::select(lat,lon,var,lat_band,source) %>% na.omit() 
  data_ls2 <- dtCL %>% dplyr::select(lat,lon,var,lat_band,source) %>% na.omit() 
  
  data_ls1 <- semi_join(data_ls1,data_ls2, by =c("lon","lat"))# select the intersect between the two dataframes
  data_ls2 <- semi_join(data_ls2,data_ls1, by =c("lon","lat"))# select the intersect between the two dataframes
  
  data_ls3<- dtCL_all %>% dplyr::select(lat,lon,var,lat_band,source) %>% na.omit() 
  
  dtcomb <- rbind(data_ls1, data_ls2, data_ls3)
  dtcomb$source <- factor(dtcomb$source)

  yrange <- range(dtcomb[,3])
  (ymin <- yrange[1])
  (ymax <- yrange[2] + diff(yrange) / 10)

  counts <- ddply(dtcomb, .(dtcomb$lat_band, dtcomb$source), nrow)
  names(counts) <- c("lat_band", "source", "no_sample")

  dtsum1 <- subset(counts, source == 'BP')
  dtsum2 <- subset(counts, source == 'CL')
  dtsum3 <- subset(counts, source == 'CL_all')
  
  assign(paste(var,"_dataplot", sep=""),dtcomb)
  
  p <- ggplot(data = get(paste(var,"_dataplot", sep="")),
              aes(x = lat_band, y = get(paste(var,"_dataplot", sep=""))[,var],
                  fill = source)) +
    # stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outlier.alpha = 0.7, width = 0.8,position = position_dodge2(preserve = "single")) +
      theme_bw() +
    xlab('Latitude band') +
    ylab(var) +
    scale_fill_manual(
      name = 'source',
      breaks = c('BP', 'CL', 'CL_all'),
      labels = c(expression(bold('BP')),
                 expression(bold('CL')),
                 expression(bold('CL all'))),
      values = c('orange', 'steelblue4', 'cyan3')) +
    annotate(geom = 'text',x = dtsum2$lat_band,y = yrange[2] + diff(yrange) / 10,label = dtsum2$no_sample,
             col = 'steelblue4',fontface = 2,size = 4) +
    annotate(geom = 'text',x = dtsum3$lat_band,y = yrange[2] + diff(yrange) / 20,label = dtsum3$no_sample,
             col = 'cyan3',fontface = 2,size = 4) +
    theme(axis.title = element_text(size = 12, face = 'bold'),
      panel.grid = element_blank(),
      axis.text = element_text(colour = 'black', size = 10),# face = 'bold'),
      legend.position = c(0.01, 0.01),
      legend.justification = c(0, 0),  # Set the "anchoring point" of the legend (bottom-left is 0,0; top-right is 1,1)
      legend.text.align = 0,
      legend.background = element_rect(),
      legend.title = element_blank(),
      legend.text = element_text(colour = 'black', size = 11)#, face = 'bold')
    ) +
    geom_hline(yintercept = 0, linetype="dotted", color = "grey60", size=1) +
    coord_flip()
  
  print(p)
  #rm(ls="p","data_ls1","data_ls2","data_ls3","yrange","ymin","ymax","counts","dtsum1","dtsum2","dtsum3")
  ggsave(p,file=paste(plotpath, "dat_boxplots_maps/data_boxplot_",var,".jpg", sep=""),width = 6.5,height = 8.27)
  
}

#### REGIONAL BOXPLOTS ####

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

# define region boundaries and names
dtBP <-
  dtBP %>% mutate(region = ifelse(
    lon >= -140 & lon <= -60 & lat >= 20 & lat <= 50,"N America",
    ifelse( lon >= -10 & lon <= 30 & lat >= 35 & lat <= 70,"W Europe",
            ifelse(lon >= 50 & lon <= 150 & lat >= 35 & lat <= 75,"Extratropical Asia",
                   ifelse(lon >= -120 & lon <= -60 & lat >= -30 & lat <= 30,"Tropical Americas",
                          ifelse(lon >= -10 & lon <= 50 & lat >= -35 & lat <= 35, "Africa",
                                 "other"))))))
dtCL <-
  dtCL %>% mutate(region = ifelse(
    lon >= -140 & lon <= -60 & lat >= 20 & lat <= 50,"N America",
    ifelse( lon >= -10 & lon <= 30 & lat >= 35 & lat <= 70,"W Europe",
            ifelse(lon >= 50 & lon <= 150 & lat >= 35 & lat <= 75,"Extratropical Asia",
                   ifelse(lon >= -120 & lon <= -60 & lat >= -30 & lat <= 30,"Tropical Americas",
                          ifelse(lon >= -10 & lon <= 50 & lat >= -35 & lat <= 35, "Africa",
                                        "other"))))))
dtCL_all <-
  dtCL_all %>% mutate(region = ifelse(
    lon >= -140 & lon <= -60 & lat >= 20 & lat <= 50,"N America",
    ifelse( lon >= -10 & lon <= 30 & lat >= 35 & lat <= 70,"W Europe",
            ifelse(lon >= 50 & lon <= 150 & lat >= 35 & lat <= 75,"Extratropical Asia",
                   ifelse(lon >= -120 & lon <= -60 & lat >= -30 & lat <= 30,"Tropical Americas",
                          ifelse(lon >= -10 & lon <= 50 & lat >= -35 & lat <= 35, "Africa",
                                 "other"))))))

cairo_pdf(paste(plotpath, "dat_boxplots_maps/boxplot_regions.pdf", sep = ""),onefile = T,width = 8.27 ,height = 11.69)

for (var in var_ls){
  
  # prepare dataframe with chosen variable and no NAs and rename variable to VAR
  data_ls1 <- dtBP %>% filter(region != "other") %>% dplyr::select(lat,lon,var,region,source) %>% na.omit() 
  data_ls2 <- dtCL %>% filter(region != "other") %>% dplyr::select(lat,lon,var,region,source) %>% na.omit() 
  
  data_ls1 <- semi_join(data_ls1,data_ls2, by =c("lon","lat"))# select the intersect between the two dataframes
  data_ls2 <- semi_join(data_ls2,data_ls1, by =c("lon","lat"))# select the intersect between the two dataframes
  
  data_ls3<- dtCL_all %>% filter(region != "other") %>% dplyr::select(lat,lon,var,region,source) %>% na.omit(data_ls3) 
  
  dtcomb <- rbind(data_ls1, data_ls2, data_ls3) %>% mutate(source = as.factor(source))
  
  yrange <- range(dtcomb[,3])
  ymin <- yrange[1]
  ymax <- yrange[2] + diff(yrange) / 10
  
  counts <- ddply(dtcomb, .(dtcomb$region, dtcomb$source), nrow)
  names(counts) <- c("region", "source", "no_sample")
  
  dtsum1 <- subset(counts, source == 'BP')
  dtsum2 <- subset(counts, source == 'CL')
  dtsum3 <- subset(counts, source == 'CL_all')

    pcomb <- ggplot(data = dtcomb, aes(x = region, y = dtcomb[,3], fill = source)) +
    stat_boxplot(geom = 'errorbar') +
    geom_boxplot(outlier.alpha = 0.7, width = 0.8, position = position_dodge2(preserve = "single")) +
    theme_bw() +
    xlab('Region') +
    ylab(var) +
    scale_fill_manual(
      name = 'source',
      breaks = c('CL', 'BP', 'CL_all'),
      labels = c(expression(bold('Cleator')), expression(bold('Bartlein+Prentice')), 
                 expression(bold('Cleator all'))),#expression(bold('Bartlein all'))),
      values = c('cyan3', 'steelblue4', 'orange'))+#,'red')) +
    annotate(geom = 'text',x = dtsum1$region,y = yrange[2] + diff(yrange) / 10,label = dtsum1$no_sample,
             col = 'black',fontface = 2,size = 4) +
      annotate(geom = 'text',x = dtsum3$region,y = yrange[2] + diff(yrange) / 20,label = dtsum3$no_sample,
               col = 'orange',fontface = 2,size = 4) +
      theme(axis.title = element_text(size = 12, face = 'bold'),
          panel.grid = element_blank(),
          axis.text = element_text(colour = 'black', size = 10),
          legend.position = c(0.01, 0.01),
          legend.justification = c(0, 0),
          legend.text.align = 0,
          legend.background = element_rect(),
          legend.title = element_blank(),
          legend.text = element_text(colour = 'black', size = 11)#, face = 'bold')
    ) +
    geom_hline(yintercept = 0, linetype="dotted", color = "grey60", size=1) +
    coord_flip()
  
  print(pcomb)
  
  rm(ls="pcomb","data_ls1","data_ls2","yrange","ymin","ymax","counts","dtsum1","dtsum2")
  ggsave(p,file=paste(plotpath, "dat_boxplots_maps/data_boxplot_region_",var,".jpg", sep=""),width = 6.5,height = 8.27)
  
}


#### SCATTERPLOTS #### 

# rearrange
dBP_g <- gather (dtBP, key="variable",value="value", MAT, MTCO, MTWA, MAP, MI, GDD5)
dCL_g <- gather (dtCL, key="variable",value="value", MAT, MTCO, MTWA, MAP, MI, GDD5)


for (var in var_ls){
  
  # filter by variable
  dBP_gat <- dBP_g %>% dplyr::select(lat,lon,variable,value) %>% filter(variable==var) %>% 
    dplyr::rename(var_BP = variable, val_BP = value) 
  
  dCL_gat <- dCL_g %>% dplyr::select(lat,lon,variable,value) %>% filter(variable==var) %>% 
    dplyr::rename(var_CL = variable, val_CL = value) 
  
  
# keep overlaping lat/long
  dBP_gat <- semi_join(dBP_gat,dCL_gat, by =c("lon","lat"))# select the intersect between the two dataframes
  dCL_gat <- semi_join(dCL_gat,dBP_gat, by =c("lon","lat"))# select the intersect between the two dataframes

  data <- join(dBP_gat, dCL_gat, by = c("lat","lon") , type = "left", match = "all")
  
  # scatterplot
  
  p <- ggplot(na.omit(data), aes(x = val_BP, y = val_CL)) +
    geom_point(alpha = 1,color = "darkred", size = 2) +
    geom_smooth(method='lm', show.legend = NA,inherit.aes = TRUE,formula = y ~x, se = TRUE,
                weight=0.5, color = "darkred", size = 0.5) +
    theme_bw()+
    labs(title = paste ("",var,".",sep=""),
         x = "Bartlein + Prentice",
         y = "Cleator")+
    geom_abline(intercept = 0,slope=1, color = "black", linetype="dotted", size=1) +
    stat_cor()       # Add correlation coefficient
  
  assign(paste(var,"_plot", sep=""),p)
  
  #print(p)
  
  rm(ls="p","dBP_gat","dCL_gat","data")
  
}

fig <- ggarrange(MAT_plot, MTCO_plot, MTWA_plot, MAP_plot, MI_plot, GDD5_plot,
                    labels = c("A", "B", "C", "D","E", "F"),
                    ncol = 3, nrow = 2)
fig

ggsave(fig,file=paste(plotpath, "dat_boxplots_maps/scatterplots_per_variable.jpg", sep = ""),width = 11.69,height = 8.27)
ggsave(fig,file=paste(plotpath, "dat_boxplots_maps/scatterplots_per_variable.pdf", sep = ""),width = 11.69,height = 8.27)

graphics.off()

# Clear packages
# p_unload(all)  # Remove all add-ons

# Clear environment
#rm(list = ls()) 

