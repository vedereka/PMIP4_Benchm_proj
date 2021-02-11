# Script comparing Bartlein and Cleators datasets. Coverage (maps) and magnitudes/stdev (scatterplots)
# Created by Laia Comas-Bru in September 2020
# Last modified: February 2021

#load all data and arrange to common variables

bart_wf <- read.csv(paste (dataobspath,"bartlein_converted.csv",sep="")) %>% 
  filter (INVERSION == "NO") %>% 
  mutate (REF = "B_wf") %>% 
  dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI_converted,GDD5,REF) %>% 
  dplyr::rename (MI = MI_converted)

bart_wof <- read.csv(paste (dataobspath,"bartlein_converted.csv",sep="")) %>% 
  mutate (INVERSION = as.factor(INVERSION)) %>% mutate (REF = "B_wof") %>% 
  dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI_converted,GDD5,REF) %>% 
  dplyr::rename (MI = MI_converted)

clea_all <- read.csv(file.path(dataobspath, "cleator244.csv"),na.strings = "NA",strip.white = TRUE) %>% 
  mutate (REF = "CL_all") %>% 
  dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI,GDD5,REF)

prent_all <-read.csv(file.path(dataobspath, "prentice.csv"), strip.white = TRUE) %>% 
  mutate (REF = "PR_all", MTCO = NA, GDD5 = NA, MAP = NA, MTWA = NA) %>% 
  dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI,GDD5,REF)

# merge the datasets together
data_obs_raw <- rbind(bart_wf, bart_wof, clea_all, prent_all) %>% 
  `colnames<-`(c("lat", "lon", "MAT", "MTCO", "MTWA", "MAP","MI", "GDD5", "REF"))

rm(ls="bart_wf", "bart_wof", "prent_all")

#### SELECT OVERLAPPING SITES BETWEEN BARTLEIN GRIDS AND CLEATOR #### 

# 1. load gridcells from Bartlein's gridded data
# 2. identify how many Bart sites are in each gridcell
# 3. filter Cleator data to just that spread of data

ncfname <-  paste (dataobspath, "raw_data/mat_delta_21ka_ALL_grid_2x2.nc",sep="")
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
data <- data_obs_raw %>%  filter (REF == "CL_all")

# grid_B_CL_wof ----------------------------------------------------------------
grid <-B_wof [, 1:6]  %>% mutate (count_n = NA)

B_CL_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "CL_wof") %>%  rbind(., B_wof) %>% mutate (REF = as.factor (REF))
# B_CL_wof %>%  write.csv(., file.path(dataobspath, "B_CL_wof.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid")

# grid_B_CL_wf ----------------------------------------------------------------
grid <- B_wf [, 1:6]  %>% mutate (count_n = NA)

B_CL_wf <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "CL_wf") %>%  rbind(., B_wf) %>% mutate (REF = as.factor (REF))
# B_CL_wf %>%  write.csv(., file.path(dataobspath, "B_CL_wf.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid")

# grid_BP_CL_wof ----------------------------------------------------------------
grid <- BP_wof [, 1:6]  %>% mutate (count_n = NA)

BP_CL_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "CL_wof") %>%  rbind(., BP_wof) %>% mutate (REF = as.factor (REF))
# BP_CL_wof %>%  write.csv(., file.path(dataobspath, "BP_CL_wof.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid")

# grid_BP_CL_wf ----------------------------------------------------------------
grid <- BP_wf [, 1:6]  %>% mutate (count_n = NA)

BP_CL_wf <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
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
  
  
  if (variab_name ==variab_name_ls[1]) {
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
CL_all <- clea_all

a <- list("B_nc","B_wf","B_wof","B_CL_wf","B_CL_wof", "BP_wf", "BP_wof","BP_CL_wf", "BP_CL_wof", "CL_all","data_obs_raw")
save(a, file = paste(dataobspath,"obs_data_reformatted.RData", sep=""))
save.image() # creating ".RData" in current working directory
unlink("obs_data_reform.RData")

rm(ls="x", "ncin", "ncfname", "variab_name","a")

###--------------------------------------------------------------------------------
var_ls <- c("MAT","MTCO","MTWA","MAP","MI","GDD5")

b <- list("B_CL_wf","B_CL_wof", "BP_CL_wf", "BP_CL_wof")

for (j in b){
for (variab in var_ls){
  
  if (j=="B_CL_wf"){
    data <- B_CL_wf
    refdata1 <- "B_wf"
    refdata2 <- "CL_wf"
  }else if (j == "B_CL_wof") {
    data <- B_CL_wof
    refdata1 <- "B_wof"
    refdata2 <- "CL_wof"
  }else if (j=="BP_CL_wf") {
    data <- BP_CL_wf
    refdata1 <- "BP_wf"
    refdata2 <- "CL_wf"
    
  }else if (j=="BP_CL_wof") {
    data <- BP_CL_wof
    refdata1 <- "BP_wof"
    refdata2 <- "CL_wof"
  }
    
  
  # filter by variable
  dBP_gat <- data %>% filter (REF == refdata1) %>% dplyr::select(lat, lon, all_of(variab)) %>% 
    dplyr::rename(val_BP = variab) %>%  na.omit()
  
  dCL_gat <- data %>% filter (REF == refdata2) %>%  dplyr::select(lat, lon, all_of(variab)) %>% 
    dplyr::rename(val_CL = variab) %>%  na.omit()
  
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
  
  assign(paste("plot",variab,j,sep="_"),p)
  
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

ggsave(fig,file=paste(plotpath, j,"_scatterplots.jpg", sep = ""),width = 11.69,height = 8.27)
#ggsave(fig,file=paste(plotpath, j,"_scatterplots.pdf", sep = ""),width = 11.69,height = 8.27)

}
graphics.off()
