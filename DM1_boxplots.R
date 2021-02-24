### produce data-model latitudinal boxplots
# Created by Laia Comas-Bru in October 2020
# Last modified: February 2021

#### LOAD OBSERVATIONS AND ORGANISE DATA ####  files produced in Step0 extract site data
data_obs <- read.csv(file.path(dataobspath, "data_obs_raw.csv"), na.strings = "NA",strip.white = TRUE, blank.lines.skip = T) %>% 
  dplyr::rename (LAT = lat, LON = lon) %>%  dplyr::select (LAT, LON, MAT, MTCO, MTWA, MAP, REF)
                     
data_BarPre <- data_obs %>%  filter (REF == "B_wf" | REF == "PR_all") 
  
data_Cle <- data_obs %>%  filter (REF == "CL_all_229")

#### SELECT OVERLAPPING SITES BETWEEN BARTLEIN GRIDS AND CLEATOR #### 

## load gridcells from Bartlein's gridded data and filter Cleator to just that spread of data
ncfname <-  paste (dataobspath, "raw_data/mat_delta_21ka_ALL_grid_2x2.nc",sep="")
ncin <- nc_open(ncfname)
lat <- ncin[["dim"]][["lat"]][["vals"]]
lon <- ncin[["dim"]][["lon"]][["vals"]]
rm(ls="ncfname","ncin")
grid <- expand.grid(lon = lon, lat = lat)
#ranges
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
    x_temp <- newx [, 3:6] %>% summarise_if(is.numeric, mean, na.rm = T)
  } else {
    x_temp[n,] <- newx [, 3:6] %>% summarise_if(is.numeric, mean, na.rm = T)
  }
}

grid <- cbind (grid, x_temp)
grid <- grid %>% filter (grid$count_n >= 0)

grid_BartPren <- grid

rm(ls="n","x_temp","newx","grid")

# select grid lat/lons for which we have BArt/Pren data and filter Cleator's to that geographical range (with averaged values for all variables)
grid <- grid_BartPren [, 1:6]
grid$count_n <- NA

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
      newx [, 3:6] %>% summarise_if(is.numeric, mean, na.rm = T)
  } else {
    x_temp[n,] <-
      newx [, 3:6] %>% summarise_if(is.numeric, mean, na.rm = T)
  }
}

grid <- cbind (grid, x_temp)
grid <- grid %>% filter (grid$count_n >= 0)
grid_Cle <- grid

rm(ls="n","x_temp","newx","grid")

grid_Cle$REF <- "CL"
grid_BartPren$REF <- "BP"

#### end of data manipulation #### 

#### BOXPLOTS WITH LATITUDINAL BANDS ####
## comparisons for gridded overlapping data sources
dtBP <- grid_BartPren [, -c(3:7)]
dtCL <- grid_Cle  [, -c(3:7)]
dtCL_all <- data_Cle
colnames(dtCL_all) [1] <- "lat"
colnames(dtCL_all) [2] <- "lon"
dtCL_all$REF <- "CL_all"
obs <- rbind(dtBP, dtCL, dtCL_all)

# Group the data by latitudinal bands
brkpnt <- seq(-80, 80, by = 20)
startpnt <- brkpnt[1:length(brkpnt) - 1]
endpnt <- brkpnt[2:length(brkpnt)]
brk_lab <- paste(startpnt, '° to ', endpnt, '°', sep = '')

obs$lat_band <- cut(obs$lat, breaks = brkpnt,labels = brk_lab)
obs = obs[!is.na(obs$lat_band),] #remove lats outside of range

# select chosen variables, in this case, MAP, MTCO and MTWA
#obs <- obs [,-c(3,7:8)]

sum_obs = summary(obs %>% filter (obs$REF == "BP"))
write.csv(sum_obs, paste(datapath, "summary_BP.csv", sep=""))
sum_obs = summary(obs %>% filter (obs$REF == "CL_all"))
write.csv(sum_obs, paste(datapath, "summary_CL_all.csv", sep=""))
sum_obs = summary(obs %>% filter (obs$REF == "CL"))
write.csv(sum_obs, paste(datapath, "summary_CL_overlap.csv", sep=""))
obs2 = obs

obs <- reshape2::melt(obs, na.rm=F, id.vars = c("lat","lon","REF", "lat_band"), variable.name = "var")
# undo with: dcast(obs, lat + lon + REF + lat_band ~ var, value.var = "value")
obs$REF <- factor(obs$REF , levels=c("CL_all", "CL", "BP")) # reorder boxplots bottom to top


bp <- ggplot(na.omit(obs), aes(x=lat_band, y=value, fill=REF)) + 
  geom_boxplot(aes(fill=REF),outlier.alpha = 0.5, outlier.size = 0.5, outlier.colour = "grey86",
               width = 0.8, varwidth=F, lwd=0.01,position = position_dodge2(preserve = "single")) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.9,face="bold"),
        axis.text.y = element_text(angle = 0, vjust = -0.1, hjust=0.5,face="bold"),
        legend.position="top",
        legend.box = "horizontal", legend.text.align=0)+
  scale_fill_manual(name = element_blank(),
                    breaks = c('BP', 'CL', 'CL_all'),
                    labels = c(expression('Bartlein + Prentice'), expression('Cleator'), 
                               expression('Cleator all')),
                    values = c('orange', 'steelblue4', 'cyan3')) +
  facet_grid(.~ var,scales='free') +
  coord_flip()

print(bp)

ggsave(bp,file=paste(plotpath,"DM_boxplots/boxplot_data_CL229.jpg", sep=""),width=12,height=7)

#model data

mod_variable_ls <- c('tas_anom','mtco_anom','mtwa_anom','pre_anom', 'gdd5_anom')
model_ls <-c('AWIESM1','AWIESM2','CCSM4-UofT','CESM1-2',"iLOVECLIM1-1-1-GLAC-1D","iLOVECLIM1-1-1-ICE-6G-C",
             'INM-CM4-8',"IPSLCM5A2",'MIROC-ES2L','MPI-ESM1-2')

obs_coord = unique(obs[,1:2])
for (mod_name in model_ls){
  
  ncname <- paste(ncpath,mod_name, "_LGM_anomalies.nc",sep="") # adjust file name if updated CRU version
  ncin <- nc_open(ncname) 
  lat <- ncin[["dim"]][["lat"]][["vals"]]; nlat <- length(lat)
  lon <- ncin[["dim"]][["lon"]][["vals"]];nlon <- length(lon)
  grid <- expand.grid(lon=lon, lat=lat)
  
  
  for (mod_varname in mod_variable_ls) {
    var <- ncvar_get(ncin, mod_varname)
    var[var=="NaN"]=NA
    
    # extract indices of closest gridcells
    j <- sapply(obs_coord$lon, function(x)
      which.min(abs(lon - x)))
    k <- sapply(obs_coord$lat, function(x)
      which.min(abs(lat - x)))
    
    var_vec <- as.vector(var) 
    
    # extract data for all locations
    jk <- (k - 1) * nlon + j  #jk <- (j-1)*nlat + k
    var_extr <- var_vec[jk]
    
    var_extr_df <- data.frame (var_extr)
    colnames(var_extr_df)[1] = "value"
    var_extr_df$REF = mod_name
    var_extr_df$var = mod_varname
    var_extr_df = cbind (obs_coord, var_extr_df)
    
    if (mod_varname == mod_variable_ls[1] & mod_name == model_ls[1]) {
      pts <- var_extr_df
    } else {
      pts <- rbind (pts, var_extr_df)
    }
  }
}
nc_close(ncin)

pts$lat_band <- cut(pts$lat, breaks = brkpnt,labels = brk_lab)

data_all = rbind(obs, pts)

data_all <- data.frame(lapply(data_all, function(x) {gsub("tas_anom", "MAT", x)}))
data_all <- data.frame(lapply(data_all, function(x) {gsub("mtco_anom", "MTCO", x)}))
data_all <- data.frame(lapply(data_all, function(x) {gsub("mtwa_anom", "MTWA", x)}))
data_all <- data.frame(lapply(data_all, function(x) {gsub("pre_anom", "MAP", x)}))
data_all <- data.frame(lapply(data_all, function(x) {gsub("gdd5_anom", "GDD5", x)}))

#remove data_all => CL_all
data_all <- data_all %>% filter(REF != "CL")
data_all <- data_all %>% filter(var != "MI")

data_all$lat <- as.numeric(data_all$lat)
data_all$lon <- as.numeric(data_all$lon)
data_all$value <- as.numeric(data_all$value)
data_all$var <- as.factor(data_all$var)
data_all$REF <- factor(data_all$REF , 
                       levels=c("MPI-ESM1-2", "MIROC-ES2L", "IPSLCM5A2", "INM-CM4-8",
                                "iLOVECLIM1-1-1-ICE-6G-C", "iLOVECLIM1-1-1-GLAC-1D",
                                "CESM1-2", "CCSM4-UofT", "AWIESM2", "AWIESM1", "CL_all", "BP"))
data_all$lat_band <- factor(data_all$lat_band, levels = brk_lab[2:8])


saveRDS(data_all, file = paste(datapath,"obs_mod.RDS", sep=""))

#colorSet = c('grey75', 'grey40', brewer.pal(10,"Paired"))
colorSet= c("#A6CEE3","#1F78B4","grey75","grey40","#B2DF8A","#33A02C","#FB9A99","#E31A1C",
            "#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A")


bp <- ggplot(na.omit(data_all), aes(x=lat_band, y=value, fill=REF)) + 
  geom_boxplot(aes(fill=REF),outlier.shape = NA, 
               #outlier.alpha = 0.5, outlier.size = 0.5, outlier.colour = "grey86",
               width = 0.8, varwidth=F, lwd=0.01,position = position_dodge2(preserve = "single")) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.9,face="bold"),
        axis.text.y = element_text(angle = 0, vjust = -0.1, hjust=0.5,face="bold"),
        legend.position="right",
        legend.box = "horizontal", legend.text.align=0)+
  scale_fill_manual(name = element_blank(),
                    breaks = c("AWIESM2", "AWIESM1", "CL_all", "BP", "iLOVECLIM1-1-1-ICE-6G-C", 
                               "iLOVECLIM1-1-1-GLAC-1D", "CESM1-2", "CCSM4-UofT", "MPI-ESM1-2", "MIROC-ES2L", "IPSLCM5A2", "INM-CM4-8"),
                    labels = c("AWIESM2", "AWIESM1", "CL_all", "BP", "iLOVECLIM-ICE", 
                               "iLOVECLIM-GLAC", "CESM1-2", "CCSM4-UofT", "MPI-ESM1-2", "MIROC-ES2L", "IPSLCM5A2", "INM-CM4-8"),
                    values = colorSet) +
  facet_grid(.~ var,scales='free') +
  #facet_grid_sc(rows=vars(var), scales = list(y = scales_y)) +
  coord_flip()

print(bp)

require(facetscales) # install with devtools::install_github("zeehio/facetscales")

scales_y <- list(
    GDD5 = scale_y_continuous(breaks=scales::extended_breaks(n=3),limits=c(1500,-4000)),
    MAP = scale_y_continuous(breaks=scales::extended_breaks(n=5),limits=c(1500,-1500)),
    MAT = scale_y_continuous(breaks=scales::extended_breaks(n=4),limits=c(10,-20)),
    MTWA = scale_y_continuous(breaks=scales::extended_breaks(n=4),limits=c(10,-20)),
    MTCO = scale_y_continuous(breaks=scales::extended_breaks(n=4),limits=c(10,-30))
  )

scales_x <- list(
  name = scale_x_discrete()
)


bp <-ggplot(na.omit(data_all), aes(x=lat_band, y=value, fill=var)) + 
  geom_hline(yintercept = 0, linetype="solid", color = "black", size=0.5) +
  geom_boxplot(aes(fill=REF),outlier.alpha = 0.8, outlier.size = 0.5, outlier.colour = "grey86", 
               width = 0.8, varwidth=F,lwd=0.2,fatten=1,position = position_dodge2(preserve = "single")) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0, hjust=0.9,size=13,face="bold"),
        axis.text.y = element_text(angle = -90, vjust = -0.1, hjust=0.5,size=13,face="bold"),
        legend.position="left") +
  guides(fill = guide_legend(reverse = TRUE,
                             direction = "vertical", 
                             nrow = 4,
                             ncol = 3,
                             label.position = "bottom", 
                             legend.box.just = "right",
                             #legend.text.align=0,
                             label.theme = element_text(angle = -90, vjust = 0.5, hjust=0,size=10), 
                             title.position = "bottom", title.theme = element_text(angle = 90)))+
  
  scale_x_discrete(position = "top") +
  scale_fill_manual(name = element_blank(),
                    breaks = c("AWIESM2", "AWIESM1", "CL_all", "BP", "iLOVECLIM1-1-1-ICE-6G-C", 
                               "iLOVECLIM1-1-1-GLAC-1D", "CESM1-2", "CCSM4-UofT", "MPI-ESM1-2", "MIROC-ES2L", "IPSLCM5A2", "INM-CM4-8"),
                    labels = c("AWIESM2", "AWIESM1", "CL_all", "BP", "iLOVECLIM-ICE", 
                                    "iLOVECLIM-GLAC", "CESM1-2", "CCSM4-UofT", "MPI-ESM1-2", "MIROC-ES2L", "IPSLCM5A2", "INM-CM4-8"),
                    values = colorSet) +
  facet_grid_sc(rows=vars(var), scales = list(y = scales_y))+
  theme(strip.text.y = element_text(
    size = 14, color = "black", face = "bold"
  ))

bp

ggsave(bp,file=paste(plotpath,"DM_boxplots/boxplot_data_model.jpg", sep=""),width=11,height=14)
#ggsave(bp,file=paste(plotpath,"DM_boxplots/boxplot_data_model.pdf", sep=""),width=11,height=14)


br = c("AWIESM2", "AWIESM1", "CL_all", "BP", "iLOVECLIM1-1-1-ICE-6G-C", 
       "iLOVECLIM1-1-1-GLAC-1D", "CESM1-2", "CCSM4-UofT", "MPI-ESM1-2", "MIROC-ES2L", "IPSLCM5A2", "INM-CM4-8")
for (i in br){
x1 <- data_all %>% filter (data_all$REF == i)
sum_obs = summary(dcast(x1, lat + lon + lat_band ~ var, value.var = "value"))
write.csv(sum_obs, paste(datapath, "summary_mod_boxplot_",i,".csv", sep=""))
}



