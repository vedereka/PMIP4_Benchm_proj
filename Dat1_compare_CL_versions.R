### compare Cleator versions
# cleator229: Pre-acceptance version (https://doi.org/10.17864/1947.229)
# cleator244: Peer-reviewed version (https://doi.org/10.17864/1947.244)
# Created by Laia Comas-Bru in December 2020
# Last modified: February 2021

#load all data and arrange to common variables

bart_wf <- read.csv(paste (dataobspath,"bartlein_converted.csv",sep="")) %>% #update this to latest conversion
  filter (INVERSION == "NO") %>% 
  dplyr::mutate (REF = "B_wf") %>% 
  dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI_converted,GDD5,REF) %>% 
  dplyr::rename (MI = MI_converted)

bart_wof <- read.csv(paste (dataobspath,"bartlein_converted.csv",sep="")) %>% #update this to latest conversion
  mutate (INVERSION = as.factor(INVERSION)) %>% mutate (REF = "B_wof") %>% 
  dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI_converted,GDD5,REF) %>% 
  dplyr::rename (MI = MI_converted)

clea_all_244 <- read.csv(file.path(dataobspath, "cleator244.csv"),na.strings = "NA",strip.white = TRUE) %>% 
  mutate (REF = "CL_all_244") %>% 
  dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI,GDD5,REF)


clea_all_229 <- read.csv(file.path(dataobspath, "cleator229.csv"),na.strings = "NA",strip.white = TRUE) %>% 
  mutate (REF = "CL_all_229") %>% filter (MAP <10000) %>% 
  dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI,GDD5,REF)


prent_all <-read.csv(file.path(dataobspath, "prentice.csv"), strip.white = TRUE) %>% 
  mutate (REF = "PR_all", MTCO = NA, GDD5 = NA, MAP = NA, MTWA = NA) %>% 
  dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI,GDD5,REF)

# merge the datasets together
data_obs_raw <- rbind(bart_wf, bart_wof, clea_all_229, clea_all_244, prent_all) %>% 
  `colnames<-`(c("lat", "lon", "MAT", "MTCO", "MTWA", "MAP","MI", "GDD5", "REF"))

write.csv(data_obs_raw, paste(dataobspath, "data_obs_raw.csv", sep=""))

rm(ls="bart_wf", "bart_wof", "prent_all")

#### SELECT OVERLAPPING SITES BETWEEN BARTLEIN GRIDS AND CLEATOR #### 

# 1. load gridcells from Bartlein's gridded data
# 2. identify how many Bart sites are in each gridcell
# 3. filter Cleator data to just that spread of data

ncfname <- paste(dataobspath, "raw_data/mat_delta_21ka_ALL_grid_2x2.nc", sep="") 
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

#### CREATE GRIDDED DATASETS ####
#requirement: user-defined "data_obs_raw" function (see in functions_source.R)

# Bwofilt -----------------------------------------------------------------------
data <- data_obs_raw %>%  filter (REF == "B_wof")
B_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "B_wof")

# Bwofilt + PR_all --------------------------------------------------------------
data <- data_obs_raw %>%  filter (REF == "B_wof" | REF == "PR_all")
BP_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "BP_wof")

# Bwfilt -----------------------------------------------------------------------
data <- data_obs_raw %>%  filter (REF == "B_wf")
B_wf <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "B_wf")

# Bwfilt + PR_all --------------------------------------------------------------
data <- data_obs_raw %>%  filter (REF == "B_wf" | REF == "PR_all")
BP_wf <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "BP_wf")

#### OVERLAP GRIDDED DATASETS WITH CLEATOR ####

# cleator 229
data <- data_obs_raw %>%  filter (REF == "CL_all_229")

# grid_B_CL_wof ----------------------------------------------------------------
grid <-B_wof [, 1:6]  %>% mutate (count_n = NA)

B_CL_229_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "CL_wof") %>%  rbind(., B_wof) %>% mutate (REF = as.factor (REF))
# B_CL_wof %>%  write.csv(., file.path(dataobspath, "B_CL_wof.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid")

# grid_B_CL_wf ----------------------------------------------------------------
grid <- B_wf [, 1:6]  %>% mutate (count_n = NA)

B_CL_229_wf <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "CL_wf") %>%  rbind(., B_wf) %>% mutate (REF = as.factor (REF))
# B_CL_wf %>%  write.csv(., file.path(dataobspath, "B_CL_wf.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid")

# grid_BP_CL_wof ----------------------------------------------------------------
grid <- BP_wof [, 1:6]  %>% mutate (count_n = NA)

BP_CL_229_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "CL_wof") %>%  rbind(., BP_wof) %>% mutate (REF = as.factor (REF))
# BP_CL_wof %>%  write.csv(., file.path(dataobspath, "BP_CL_wof.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid")

# grid_BP_CL_wf ----------------------------------------------------------------
grid <- BP_wf [, 1:6]  %>% mutate (count_n = NA)

BP_CL_229_wf <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "CL_wf") %>%  rbind(., BP_wf) %>% mutate (REF = as.factor (REF))
# BP_CL_wf %>%  write.csv(., file.path(dataobspath, "BP_CL_wf.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid", "data")

##cleator 244

data <- data_obs_raw %>%  filter (REF == "CL_all_244")

# grid_B_CL_wof ----------------------------------------------------------------
grid <-B_wof [, 1:6]  %>% mutate (count_n = NA)

B_CL_244_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "CL_wof") %>%  rbind(., B_wof) %>% mutate (REF = as.factor (REF))
# B_CL_wof %>%  write.csv(., file.path(dataobspath, "B_CL_wof.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid")

# grid_B_CL_wf ----------------------------------------------------------------
grid <- B_wf [, 1:6]  %>% mutate (count_n = NA)

B_CL_244_wf <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "CL_wf") %>%  rbind(., B_wf) %>% mutate (REF = as.factor (REF))
# B_CL_wf %>%  write.csv(., file.path(dataobspath, "B_CL_wf.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid")

# grid_BP_CL_wof ----------------------------------------------------------------
grid <- BP_wof [, 1:6]  %>% mutate (count_n = NA)

BP_CL_244_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "CL_wof") %>%  rbind(., BP_wof) %>% mutate (REF = as.factor (REF))
# BP_CL_wof %>%  write.csv(., file.path(dataobspath, "BP_CL_wof.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid")

# grid_BP_CL_wf ----------------------------------------------------------------
grid <- BP_wf [, 1:6]  %>% mutate (count_n = NA)

BP_CL_244_wf <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "CL_wf") %>%  rbind(., BP_wf) %>% mutate (REF = as.factor (REF))
# BP_CL_wf %>%  write.csv(., file.path(dataobspath, "BP_CL_wf.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid", "data")


#load gridded Bart data --------------------------------------------------------
#source('Bartlein_gridded_convert_splash_20201118.R') # need to convert grid_B_nc$ALPHA to MI!!!!

variab_name_ls <- c('mat', 'mtco', 'mtwa', 'map', 'alpha' ,'gdd5')

for (variab_name in variab_name_ls){
  
  ncfname <-  paste (dataobspath, "raw_data/",variab_name,"_delta_21ka_ALL_grid_2x2.nc",sep="")
  ncin <- nc_open(ncfname)
  
  x <- ncvar_get(ncin, paste(variab_name,"_anm_mean", sep="")) %>% 
    `colnames<-` (c(ncin[["dim"]][["lat"]][["vals"]])) %>% 
    `rownames<-`(c(ncin[["dim"]][["lon"]][["vals"]])) %>% 
    reshape2::melt(., na.rm=F, value.name = variab_name) %>% 
    dplyr::rename (lon = Var1, lat=Var2)
  
  
  if (variab_name == variab_name_ls[1]) {
    B_nc <- x 
  } else {
    B_nc <- cbind(B_nc, x[,3])
    colnames(B_nc)[ncol(B_nc)]<-variab_name
  }
}

names(B_nc) <- toupper(names(B_nc))
B_nc <- B_nc %>% dplyr::rename (MI = ALPHA, lat = LAT, lon= LON) # MI NOT CONVERTED!!!!

###--------------------------------------------------------------------------------
# save variables
CL_all <- clea_all_229
#CL_all <- clea_all_244

a <- list("B_nc","B_wf","B_wof","BP_wf", "BP_wof",
          "B_CL_229_wf","B_CL_229_wof","BP_CL_229_wf", "BP_CL_229_wof",
          "B_CL_244_wf","B_CL_244_wof","BP_CL_244_wf", "BP_CL_244_wof",
          "CL_all_244","CL_all_229","data_obs_raw")
save(a, file = paste(dataobspath,"obs_data_CL2versions.RData", sep=""))
save.image() # creating ".RData" in current working directory
unlink("obs_data_CL2versions.RData")

rm(ls="x", "ncin", "ncfname", "variab_name","a")

###--------------------------------------------------------------------------------
var_ls <- c("MAT","MTCO","MTWA","MAP","MI","GDD5")

b <- list("B_CL_wf","B_CL_wof", "BP_CL_wf", "BP_CL_wof")

for (j in b){
  for (variabl in var_ls){
    
    if (j=="B_CL_wf"){
      data <- B_CL_229_wf
      refdata1 <- "B_wf"
      refdata2 <- "CL_wf"
    }else if (j == "B_CL_wof") {
      data <- B_CL_229_wof
      refdata1 <- "B_wof"
      refdata2 <- "CL_wof"
    }else if (j=="BP_CL_wf") {
      data <- BP_CL_229_wf
      refdata1 <- "BP_wf"
      refdata2 <- "CL_wf"
    }else if (j=="BP_CL_wof") {
      data <- BP_CL_229_wof
      refdata1 <- "BP_wof"
      refdata2 <- "CL_wof"
    }
    
    
    # filter by variable
    dBP_gat <- data %>% filter (REF == refdata1) %>% dplyr::select(lat, lon, all_of(variab)) %>% 
      dplyr::rename(val_BP = variabl) %>%  na.omit()
    
    dCL_gat <- data %>% filter (REF == refdata2) %>%  dplyr::select(lat, lon, all_of(variab)) %>% 
      dplyr::rename(val_CL = variabl) %>%  na.omit()
    
    data_merge <- join(dBP_gat, dCL_gat, by = c("lat","lon") , type = "inner", match = "all")
    
    # scatterplot
    
    p <- ggplot(na.omit(data_merge), aes(x = val_BP, y = val_CL)) +
      geom_point(alpha = 1,color = "darkred", size = 2) +
      geom_smooth(method='lm', show.legend = NA,inherit.aes = TRUE,formula = y ~x, se = TRUE,
                  weight=0.5, color = "darkred", size = 0.5) +
      theme_bw()+
      theme(plot.caption = element_text(size=10, vjust = -0.18)) +
      labs(title = paste ("",variabl,".",sep=""),
           x = refdata1,
           y = refdata2)+
      labs(caption = paste ("n=",dim(data_merge)[1],sep=" ")) +
      geom_abline(intercept = 0,slope=1, color = "black", linetype="dotted", size=1) +
      stat_cor()     
    
    assign(paste("plot",variabl,j,sep="_"),p)
    
    rm(ls="p","dBP_gat","dCL_gat","data","data_merge", "refdata1", "refdata2")
    
  }
  
  fig <- ggarrange(get(paste("plot_MAT",j, sep="_")),
                   get(paste("plot_MTCO",j, sep="_")),
                   get(paste("plot_MTWA",j, sep="_")),
                   get(paste("plot_MAP",j, sep="_")),
                   get(paste("plot_MI",j, sep="_")),
                   get(paste("plot_GDD5",j, sep="_")),
                   labels = c("A", "B", "C", "D","E", "F"),
                   ncol = 3, nrow = 2)
  
  fig
  
  ggsave(fig,file=paste(plotpath,"dat_CL_versions/", j,"_229_scatterplots.jpg", sep = ""),width = 11.69,height = 8.27)
  #ggsave(fig,file=paste(plotpath,"", j,"_229_scatterplots.pdf", sep = ""),width = 11.69,height = 8.27)
  
}

## AFRICA ONLY -------------------------
#   Africa':[(-35,35,'cc'),(-10,50,'cc'),],
#   WestAfrica':[(5,30,'cc'),(-17,30,'cc'),],

for (j in b[1:2]){
  for (variabl in var_ls){
    
    if (j=="B_CL_wf"){
      data <- B_CL_229_wf 
      refdata1 <- "B_wf"
      refdata2 <- "CL_wf"
    }else if (j == "B_CL_wof") {
      data <- B_CL_229_wof
      refdata1 <- "B_wof"
      refdata2 <- "CL_wof"
    }
    
    data <- data %>% 
      mutate(region = ifelse(lon >= -10 & lon <= 50 & lat >= -35 & lat <= 35, "Africa", "other")) %>% 
      filter (region != "other")
    
    # filter by variable
    dBP_gat <- data %>% filter (REF == refdata1) %>% dplyr::select(lat, lon, all_of(variab)) %>% 
      dplyr::rename(val_BP = variabl) %>%  na.omit()
    
    dCL_gat <- data %>% filter (REF == refdata2) %>%  dplyr::select(lat, lon, all_of(variab)) %>% 
      dplyr::rename(val_CL = variabl) %>%  na.omit()
    
    data_merge <- join(dBP_gat, dCL_gat, by = c("lat","lon") , type = "inner", match = "all")
    
    # scatterplot
    
    p <- ggplot(na.omit(data_merge), aes(x = val_BP, y = val_CL)) +
      geom_point(alpha = 1,color = "darkred", size = 2) +
      geom_smooth(method='lm', show.legend = NA,inherit.aes = TRUE,formula = y ~x, se = TRUE,
                  weight=0.5, color = "darkred", size = 0.5) +
      theme_bw()+
      theme(plot.caption = element_text(size=10, vjust = -0.18)) +
      labs(title = paste ("",variab,".",sep=""),
           x = refdata1,
           y = refdata2)+
      labs(caption = paste ("n=",dim(data_merge)[1],sep=" ")) +
      geom_abline(intercept = 0,slope=1, color = "black", linetype="dotted", size=1) +
      stat_cor()     
    
    assign(paste("plot",variabl,j,sep="_"),p)
    
    rm(ls="p","dBP_gat","dCL_gat","data","data_merge", "refdata1", "refdata2")
    
  }
  
  fig <- ggarrange(get(paste("plot_MAT",j, sep="_")),
                   get(paste("plot_MTCO",j, sep="_")),
                   get(paste("plot_MTWA",j, sep="_")),
                   get(paste("plot_MAP",j, sep="_")),
                   get(paste("plot_MI",j, sep="_")),
                   get(paste("plot_GDD5",j, sep="_")),
                   labels = c("A", "B", "C", "D","E", "F"),
                   ncol = 3, nrow = 2)
  
  fig
  
  ggsave(fig,file=paste(plotpath, "dat_CL_versions/", j,"_229_scatterplots_Africa.jpg", sep = ""),width = 11.69,height = 8.27)
  ggsave(fig,file=paste(plotpath, "dat_CL_versions/", j,"_229_scatterplots_Africa.pdf", sep = ""),width = 11.69,height = 8.27)
  
}


## map overlapping sites ####

rm(list=ls(pattern="^plot_")) # remove all variables starting with "plot_"
rm(ls="fig", "j", "variabl","data")

for (j in b[3:4]){
  for (variabl in var_ls){
    if (j=="BP_CL_wf"){
      data <- BP_CL_229_wf 
      refdata1 <- "BP_wf"
      refdata2 <- "CL_wf"
    }else if (j == "BP_CL_wof") {
      data <- BP_CL_229_wof
      refdata1 <- "BP_wof"
      refdata2 <- "CL_wof"
    }
    
    # filter by variable
    dBP_gat <- data %>% filter (REF == refdata1) %>% dplyr::select(lat, lon, all_of(variab)) %>% 
      dplyr::rename(val_BP = variabl) %>%  na.omit()
    
    dCL_gat <- data %>% filter (REF == refdata2) %>%  dplyr::select(lat, lon, all_of(variab)) %>% 
      dplyr::rename(val_CL = variabl) %>%  na.omit()
    
    data_merge <- join(dBP_gat, dCL_gat, by = c("lat","lon") , type = "inner", match = "all")
    
  p <- mp +
    geom_point(data = data_merge ,aes (x = lon,y = lat),alpha = 1, #,color = as.factor(count_n) # removed this from aes
               stroke = 0.5,show.legend = T,size = 0.5, shape = 23,
               color= "darkorchid4", fill="darkorchid1") +
    theme_bw() + 
    theme(axis.text.x = element_text(size = rel(0.6)),
          axis.text.y = element_text(size = rel(0.6)),
          axis.title.y = element_text(size = rel(0.6)),
          axis.title.x = element_text(size = rel(0.6)),
          plot.title = element_text(size = 8, face = "bold"))+
    labs(title = variabl)#,color = "counts (FYI, data averaged)")
  
  assign(paste("plot",variabl,j,sep="_"),p)
  
  rm(ls="p","data_merge","dBP_gat","dCL_gat")

  }

  fig <- ggarrange(get(paste("plot_MAT",j, sep="_")),
                   get(paste("plot_MTCO",j, sep="_")),
                   get(paste("plot_MTWA",j, sep="_")),
                   get(paste("plot_MAP",j, sep="_")),
                   get(paste("plot_MI",j, sep="_")),
                   get(paste("plot_GDD5",j, sep="_")),
                   # labels = c("A", "B", "C", "D","E", "F"),
                   ncol = 3, nrow = 2)
  fig

ggsave(fig,file=paste(plotpath,"dat_CL_versions/",j, "229_map_overlap_sites.jpg", sep=""),width = 11.69,height = 4)
#ggsave(fig,file=paste(plotpath,"dat_CL_versions/",j, "229_map_overlap_sites.pdf", sep = ""),width = 11.69,height = 4)
}

rm(list=ls(pattern="^plot_")) # remove all variables starting with "plot_"


## maps/scatterplots comparing sites/values in cleator versions ####

rm(list=ls(pattern="^plot_")) # remove all variables starting with "plot_"
rm(ls="fig", "j", "variabl","data")

  for (variabl in var_ls){
  
    data229 <-  clea_all_229 %>% dplyr::select(LAT,LON,all_of(variabl)) %>%
      dplyr::rename(val = variabl) %>%  na.omit()
    
    data244 <-  clea_all_244 %>% dplyr::select(LAT,LON,all_of(variabl)) %>%
      dplyr::rename(val = variabl) %>%  na.omit()

    
    rng <- range (data229$val, data244$val)#a range to have the same min and max for both plots
    breakcol <- seq(from=round(rng[1]), to=round(rng[2]), length.out = 6)
    lim_colbar <- c(floor(rng[1]), ceiling(rng[2]))
    
    
     g1 <- mp +
       geom_point(data=data229, aes(x=LON, y=LAT, colour = val),
                  alpha = 0.8, size = 1.75, shape = 15, show.legend = F) +
       scale_fill_gradient2(low = "#e41a1c", mid = "#377eb8", high = "#4daf4a",space = "Lab",
                           na.value = "grey50",guide = "colourbar",
                           aesthetics = "colour", breaks=breakcol,
                           limits=lim_colbar)+
       labs(title = paste("v229: ",variabl,". n=", dim (data229)[1], sep=""), color = variabl) 

     
     g2 <- mp +
       geom_point(data=data244, aes(x=LON, y=LAT, colour = val),
                  alpha = 0.8, size = 1.75, shape = 15) +
       scale_fill_gradient2(low = "#e41a1c", mid = "#377eb8", high = "#4daf4a",space = "Lab",
                            na.value = "grey50",guide = "colourbar",
                            aesthetics = "colour", breaks=breakcol,
                            limits=lim_colbar)+
       labs(title = paste("v244: ",variabl,". n=", dim (data244)[1], sep=""), color = variabl) +
       theme(legend.position = c(0.065, 0.27), legend.box = "horizontal")+
       theme(legend.title = element_text(colour = "black", size = 8, face = "bold" )) +
       theme(legend.text = element_text(colour = "black", size = 8)) +
       theme(legend.background = element_rect(fill = "white",size = 0.4,linetype = "solid",colour = "black"))

    
    fig <- ggarrange(g1, g2,
                     #labels = c("BP", "CL"),
                     ncol = 1, nrow = 2)
    fig
    
    ggsave(fig,file=paste(plotpath, "dat_CL_versions/CL_map_raw_",variabl,".jpg", sep = ""),width = 8,height = 8.27)
    #ggsave(fig,file=paste(plotpath, "dat_CL_versions/CL_map_raw_",variabl,".pdf", sep = ""),width = 8,height = 8.27)
    
    
    data_merge <-
      join(data229 %>% dplyr::rename (val_229 = val), 
           data244 %>% dplyr::rename (val_244 = val),
           by = c("LAT","LON") , type = "inner", match = "all") %>% na.omit()

    gg <- ggplot(data_merge, aes(x = val_229, y = val_244, color= LAT), show.legend = F) +
      geom_point(alpha = 1, size = 2) +
      geom_vline(xintercept = 0, color = "grey70", size=0.5) +
      geom_hline(yintercept = 0,color = "grey70", size=0.5) +
      lims (x=rng, y=rng) +
      scale_color_gradientn (colors= rainbow(5),na.value = "grey50",guide = "colourbar",aesthetics = "colour") +
      geom_smooth(method='lm', show.legend = F,inherit.aes = TRUE,formula = y ~x, se = TRUE,
                  weight=1, color = "darkred", size = 1) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            legend.position=c(0.9, 0.15),
            panel.background = element_rect(colour = "black", size=0.5, fill=NA),#element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      labs(title = paste ("",variabl,".","n=",dim(data_merge)[1],". 1:1 line in black", sep=" "),
           x = "version 229", y = "version 244")+
      geom_abline(intercept = 0,slope=1, color = "black", linetype="dotted", size=1) +
      stat_cor()

    p <- ggMarginal(gg, type = "histogram", fill="grey90", size = 8)
    
    ggsave(p,file=paste(plotpath,"dat_CL_versions/scatter_CL_229_244_",variabl,".jpg", sep = ""),width = 8,height = 8.27)
    
    rm(ls="data229", "data244", "rng","breakcol","lim_colbar", "g1", "g2", "fig", "p", "data_merge")
    
    }

rm(list=ls(pattern="^plot_")) # remove all variables starting with "plot_"

## Africa scatterplots comparing sites/values in cleator versions ####

rm(ls="fig", "j", "variabl","data")

for (variabl in var_ls){
  
  data229 <-  clea_all_229 %>% dplyr::select(LAT,LON,all_of(variabl)) %>%
    dplyr::rename(val_229 = variabl) %>%  na.omit() %>% 
    mutate(region = ifelse(LON >= -10 & LON <= 50 &
                             LAT >= -35 & LAT <= 35, "Africa", "other")) %>% 
    filter (region != "other") %>% dplyr::select (c(-ncol(.)))
  
  data244 <-  clea_all_244 %>% dplyr::select(LAT,LON,all_of(variabl)) %>%
    dplyr::rename(val_244 = variabl) %>%  na.omit() %>% 
    mutate(region = ifelse(LON >= -10 & LON <= 50 &
                             LAT >= -35 & LAT <= 35, "Africa", "other")) %>% 
    filter (region != "other") %>% dplyr::select (c(-ncol(.)))
  
  rng <- range (data229$val_229, data244$val_244)
  breakcol <- seq(from=-35, to=35, length.out = 3)
  lim_colbar <- c(-35, 35)
  
  
  data_merge <- join(data229, data244, by = c("LAT","LON") , type = "inner",
                     match = "all") %>% na.omit()
  
  gg <- ggplot(data_merge, aes(x = val_229, y = val_244, color= LAT), show.legend = F) +
    geom_point(alpha = 1, size = 2) +
    geom_vline(xintercept = 0, color = "grey70", size=0.5) +
    geom_hline(yintercept = 0,color = "grey70", size=0.5) +
    lims (x=rng, y=rng) +
    scale_color_gradientn (colors= rainbow(5),na.value = "grey50",
                           guide = "colourbar",aesthetics = "colour", breaks=breakcol,
                           limits=lim_colbar) +
    geom_smooth(method='lm', show.legend = F,inherit.aes = TRUE,formula = y ~x, se = TRUE,
                weight=1, color = "darkred", size = 1) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          legend.position = c(0.25, 0.78),
          legend.direction = "horizontal",
          legend.key.width = unit(0.7, "cm"),
          legend.key.height = unit(0.25, "cm"),
          plot.caption = element_text(size=12, vjust = 22, hjust = 0.99),
          panel.background = element_rect(colour = "black", size=0.5, fill=NA),#element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    labs(caption = paste ("n=",dim(data_merge)[1], sep=" "),
         x = "version 229", y = "version 244", color = "")+
    geom_abline(intercept = 0,slope=1, color = "black", linetype="dotted", size=1) +
    stat_cor()
  
  gg
  p <- ggMarginal(gg, type = "histogram", fill="grey90", size = 8)
  p
  assign(paste("plot",variabl,sep="_"),p)
  
  rm(ls="data229", "data244", "rng", "p", "data_merge")
  
}

fig <- ggarrange(get(paste("plot_MAT", sep="_")),
                 get(paste("plot_MTCO", sep="_")),
                 get(paste("plot_MTWA", sep="_")),
                 get(paste("plot_MAP", sep="_")),
                 get(paste("plot_MI", sep="_")),
                 get(paste("plot_GDD5", sep="_")),
                 labels = c("MAT", "MTCO", "MTWA", "MAP","MI", "GDD5"),
                 ncol = 3, nrow = 2)  
fig
ggsave(fig,file=paste(plotpath, "dat_CL_versions/Africa_scatter_CL_229_244.jpg", sep = ""),width = 14,height = 8)

rm(list=ls(pattern="^plot_")) # remove all variables starting with "plot_"


graphics.off()

