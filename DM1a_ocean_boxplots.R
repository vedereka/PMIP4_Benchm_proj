### produce data only and data-model latitudinal boxplots
# Boxplot #1: Bartlein (B), Cleator at the Bartlein sites (CL) and all Cleator data (CL_all)
# Boxplot #2: Bartlein (B), all Cleator data (CL_all) and model data
# Statistical summaries of all variables are saved in output/
# These are the things that will require checking if the models are updated:
# - model_ls: Are model names correctly trimmed?
# - scales_y: are limits still valid?
# - guide_legend nrow and ncol: do they need to be updated?
# - breaks and levels in scale_fill_manual (note that the order is strange)
# - colorSet to match the number of models (and the order)

# Created by Laia Comas-Bru in October 2020
# Last modified: February 2021

# Still to-do: Haven't been able to keep empty spaces for missing data in the 
# DM boxplots. This is a known issue of ggplot2. See:
# https://github.com/tidyverse/ggplot2/issues/3345

#--------------------------------------------------------------
#### LOAD OBSERVATIONS AND ORGANISE DATA ####  
# files produced in Step0 extract site data

# This Margo data is already gridded
data_obs <- read.csv(file.path(dataobspath, "/ocean_data/SST_Masa/ocean_obs_Margo.csv"), na.strings = "NA") %>% 
  dplyr::select (lat, lon, ocean_tas_anom, ref)
grid <- data_obs %>% dplyr::select (lat, lon)

data_Margo <- data_obs %>%  filter(ref == "Margo") 

grid_Margo <- grid
grid_Margo$ref <- "Margo"
#------------------------

# Get Tierney data (also gridded, same grid as Margo)
data_Tierney <- read.csv(file.path(dataobspath, "/ocean_data/SST_Masa/ocean_obs_Tierney.csv"), na.strings = "NA") %>%
  dplyr::select (lat, lon, ocean_tas_anom, ref)
grid <- data_Tierney %>% dplyr::select (lat, lon)

grid_Tierney <- grid
grid_Tierney$ref <- "Tierney"
data_Tierney <- data_Tierney %>%  filter(ref == "Tierney") 
# #------------------------
# 
# # Get AH data 
data_AH <- read.csv(file.path(dataobspath, "/ocean_data/SST_Masa/ocean_obs_AH.csv"), na.strings = "NA") %>%
  dplyr::select (lat, lon, ocean_tas_anom, ref)
grid <- data_AH %>% dplyr::select (lat, lon)


grid_AH <- grid
grid_AH$ref <- "AH"
data_AH <- data_AH %>%  filter(ref == "AH")
# #------------------------
# 
# # Get glomap data 
data_glomap <- read.csv(file.path(dataobspath, "/ocean_data/SST_Masa/ocean_obs_glomap.csv"), na.strings = "NA") %>%
  dplyr::select (lat, lon, ocean_tas_anom, ref)
grid <- data_glomap %>% dplyr::select (lat, lon)


grid_glomap <- grid
grid_glomap$ref <- "glomap"
data_glomap <- data_glomap %>%  filter(ref == "glomap")

# #------------------------
# # Get kn data (also gridded)
data_kn <- read.csv(file.path(dataobspath, "/ocean_data/SST_Masa/ocean_obs_kn.csv"), na.strings = "NA") %>%
  dplyr::select (lat, lon, ocean_tas_anom, ref)
grid <- data_kn %>% dplyr::select (lat, lon)


grid_kn <- grid
grid_kn$ref <- "kn"
data_kn <- data_kn %>%  filter(ref == "kn")
#---------------------------------------------------------
# Get Tierney gridded  data (also gridded)
data_Tgrid <- read.csv(file.path(dataobspath, "/ocean_data/SST_Masa/ocean_obs_T_Grid.csv"), na.strings = "NA") %>%
  dplyr::select (lat, lon, ocean_tas_anom, ref)
grid <- data_Tgrid %>% dplyr::select (lat, lon)


grid_Tgrid <- grid
grid_Tgrid$ref <- "T_grid"
data_Tgrid <- data_Tgrid %>%  filter(ref == "T_Grid")

#-----------------------------------------
# end of data manipulation # 
#-----------------------------------------------------------------
#### BOXPLOT #1: only data ####
obs <- rbind(data_Margo, data_Tierney, data_AH, data_glomap, data_kn, data_Tgrid)
# 

#  Group the data by latitudinal bands
brkpnt <- seq(-90, 90, by = 60)
startpnt <- brkpnt[1:length(brkpnt) - 1]
endpnt <- brkpnt[2:length(brkpnt)]
brk_lab <- paste(startpnt,   ' to ', endpnt, sep = '')

obs$lat_band <- cut(obs$lat, breaks = brkpnt,labels = brk_lab)
obs = obs[!is.na(obs$lat_band),] #remove lats outside of range
latband_ls = (unique(obs$lat_band))

#save statistical summary of each variable for each lat band
for (band in latband_ls) {
  obs_latband <- obs %>% filter(obs$lat_band == band)
  sum_obs = summary(obs_latband %>% filter(obs_latband$ref == "Margo"))
  write.csv(sum_obs, paste(datapath,"/ocean_output/", band,"_summary_Margo.csv", sep=""))
  sum_obs = summary(obs_latband %>% filter (obs_latband$ref == "Tierney"))
  write.csv(sum_obs, paste(datapath,"/ocean_output/", band,"_summary_Tierney.csv", sep=""))
  sum_obs = summary(obs_latband %>% filter (obs_latband$ref == "AH"))
  write.csv(sum_obs, paste(datapath,"/ocean_output/", band,"_summary_AH.csv", sep=""))
  sum_obs = summary(obs_latband %>% filter (obs_latband$ref == "glomap"))
  write.csv(sum_obs, paste(datapath,"/ocean_output/", band,"_summary_glomap.csv", sep=""))
  sum_obs = summary(obs_latband %>% filter (obs_latband$ref == "kn"))
  write.csv(sum_obs, paste(datapath,"/ocean_output/", band,"_summary_kn.csv", sep=""))
  sum_obs = summary(obs_latband %>% filter (obs_latband$ref == "T_Grid"))
  write.csv(sum_obs, paste(datapath,"/ocean_output/", band,"_summary_T_Grid.csv", sep=""))
}
# 
obs2 = obs
# 
obs <- reshape2::melt(obs, na.rm=F, id.vars = c("lat","lon","ref", "lat_band"), variable.name = "var")


# # undo with: dcast(obs, lat + lon + ref + lat_band ~ var, value.var = "value")
obs$ref <- factor(obs$ref , levels=c("Margo", "Tierney", "AH", "glomap","kn", "T_Grid")) # reorder boxplots bottom to top

scales_y <- scale_y_continuous(breaks=scales::extended_breaks(n=10),limits=c(5,-5))


bplot <- ggplot(na.omit(obs), aes(x=lat_band, y=value, fill=ref)) + 
  geom_boxplot(aes(fill=ref),outlier.alpha = 0.5, outlier.size = 0.5, outlier.colour = "grey86", width = 0.8, varwidth=F, lwd=0.01,position = position_dodge2(preserve = "single")) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.9,face="bold"),
        axis.text.y = element_text(angle = 0, vjust = -0.1, hjust=0.5,face="bold"),
        legend.position="top",
        legend.box = "vertical", legend.text.align=0)+
  scale_fill_manual(name = element_blank(), 
                    breaks = c('Margo', 'Tierney', 'AH', 'glomap','kn', 'T_Grid'), 
                    labels = c(expression('Margo', 'Tierney', 'AH', 'glomap','kn','T_Grid')),  
                    values = c('orange', 'steelblue4', 'cyan3', 'brown4','springgreen', 'red3')) +
  facet_grid(.~ var,scales='fixed')
#coord_flip()

#print(bp)


ggsave(bplot,file=paste(plotpath,"DM_boxplots/boxplot60_data_All.jpg", sep=""),width=12,height=7)

##################################################
#### BOXPLOT #2: observations and model data ####

mod_variable_ls <- c('ocean_tas_anom')

# location of model output
mod_dir <- ncpath_ocean
mod_files <- list.files(mod_dir, pattern = "anomalies", full.names = TRUE)

# create list of model names for output
model_ls <- lapply(list.files(mod_dir, pattern="anomalies", full.names = F), FUN = my_name_trim) %>% as.character (.)

# Remove 'ocean' from model names so they are readable
modNames_ls <- lapply(model_ls, FUN = ocean_name_trim) %>% as.character (.)

obs_coord = unique(obs[,1:2])

for (mod_name in model_ls){
  ncname <- paste(ncpath_ocean, mod_name, "_LGM_anomalies.nc",sep="")
  ncin <- nc_open(ncname)
  lat <- ncin[["dim"]][["lat"]][["vals"]]; nlat <- length(lat)
  lon <- ncin[["dim"]][["lon"]][["vals"]];nlon <- length(lon)
  grid <- expand.grid(lon=lon, lat=lat)
  
  # Takes only model output with data output? But multiple datasets, so not clear how this is compared in final plots
  for (mod_varname in mod_variable_ls) {
    var <- ncvar_get(ncin, mod_varname)
    var[var=="NaN"]=NA
    # extract indices of closest gridcells
    j <- sapply(obs_coord$lon, function(x) which.min(abs(lon - x)))
    k <- sapply(obs_coord$lat, function(x) which.min(abs(lat - x)))
    
    var_vec <- as.vector(var)
    
    # extract data for all locations
    jk <- (k - 1) * nlon + j  #jk <- (j-1)*nlat + k
    var_extr <- var_vec[jk]
    
    var_extr_df <- data.frame (var_extr)
    colnames(var_extr_df)[1] = "value"
    var_extr_df$ref = mod_name
    var_extr_df$var = mod_varname
    var_extr_df = cbind (obs_coord, var_extr_df)
    
    #var_extra_df[ , c(var_extr_df$ref, "x1", "x3")]
    if (mod_varname == mod_variable_ls[1] & mod_name == model_ls[1]) {
      pts <- var_extr_df
    } else {
      pts <- rbind (pts, var_extr_df)
    }
  }
  
}
nc_close(ncin)

pts$lat_band <- cut(pts$lat, breaks = brkpnt,labels = brk_lab)
#print(colnames(pts))

# rename vars
pts <- data.frame(lapply(pts, function(x) {gsub("ocean_tas_anom", "ocean_tas_anom", x)}))
#pts <- data.frame(lapply(pts, function(x) {gsub("ocean_mtco_anom", "ocean_mtco_anom", x)}))
#pts <- data.frame(lapply(pts, function(x) {gsub("ocean_mtwa_anom", "ocean_mtwa_anom", x)}))


#print(dim(pts))
#print(dim(obs))
data_all = rbind(obs, pts)


#data_all <- data_all %>% filter(ref == "Margo")
data_all$lat <- as.numeric(data_all$lat)
data_all$lon <- as.numeric(data_all$lon)
data_all$value <- as.numeric(data_all$value)
data_all$var <- as.factor(data_all$var)
data_all$ref <- factor(data_all$ref ,
                       levels= c(rev(as.character(model_ls)), "Margo", "Tierney", "AH", "glomap", "kn", "T_Grid"))
data_all$lat_band <- factor(data_all$lat_band, levels = brk_lab)

saveRDS(data_all, file = paste(datapath,"obs_mod.RDS", sep=""))

require (randomcoloR) # ColorBrewer max length is 12, we need 13 + 2 grey
# color palette in the right order
n <- length(unique(data_all$ref)) %>%  distinctColorPalette(.)
colorSet <- rev(c(n[1:2],'grey75', 'grey40',n[3:length(n)]))
# pie(rep(1, length(colorSet), col=colorSet)) # to see colours in a pie chart (diff each time)

require(facetscales) # install with devtools::install_github("zeehio/facetscales")
#set limits for each variable (only possible with facetscales)
scales_y <- list(
  ocean_tas_anom = scale_y_continuous(breaks=scales::extended_breaks(n=4),limits=c(5,-30))
  # MAP = scale_y_continuous(breaks=scales::extended_breaks(n=5),limits=c(1500,-1500)),
  #ocean_mtco_anom = scale_y_continuous(breaks=scales::extended_breaks(n=4),limits=c(10,-20)),
  #ocean_mtwa_anom = scale_y_continuous(breaks=scales::extended_breaks(n=4),limits=c(10,-30))
)

scales_x <- list(
  name = scale_x_discrete()
)

bpMod <-ggplot(na.omit(data_all), aes(x=lat_band, y=value, fill=var)) +
  geom_hline(yintercept = 0, linetype="solid", color = "black", size=0.5) +
  geom_boxplot(aes(fill=ref),outlier.alpha = 0.8, outlier.size = 0.5, outlier.colour = "grey86",
               width = 0.8, varwidth=F,lwd=0.2,fatten=1,position = position_dodge2(preserve = "single")) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0, hjust=0.9,size=16,face="bold"),
        axis.text.y = element_text(angle = -90, vjust = -0.1, hjust=0.5,size=16,face="bold"),
        legend.position="left") +
  guides(fill = guide_legend(reverse = TRUE,
                             direction = "vertical",
                             nrow = 4,
                             ncol = 5,
                             label.position = "bottom",
                             legend.box.just = "right",
                             #legend.text.align=0,
                             label.theme = element_text(angle = -90, vjust = 0.1, hjust=0,size=10),
                             title.position = "bottom", title.theme = element_text(angle = 90)))+
  
  scale_x_discrete(position = "top") +
  scale_fill_manual(name = element_blank(),
                    breaks = c("Margo","Tierney","AH", "glomap", "kn","T_Grid",model_ls[3], model_ls[2], model_ls[1],
                               model_ls[8],model_ls[7],model_ls[6],model_ls[5],model_ls[4],
                               model_ls[13],model_ls[12],model_ls[11],model_ls[10],model_ls[9]),
                    labels = c("Margo","Tierney","AH", "glomap", "kn","T_Grid",model_ls[3], model_ls[2], model_ls[1],
                               model_ls[8],model_ls[7],model_ls[6],model_ls[5],model_ls[4],
                               model_ls[13],model_ls[12],model_ls[11],model_ls[10],model_ls[9]),
                    values = colorSet) + #strange order
  facet_grid_sc(rows=vars(var), scales = list(y = scales_y))+
  theme(strip.text.y = element_text(
    size = 10, color = "black", face = "bold"
  ))

#print(bpMod)

ggsave(bpMod,file=paste(plotpath,"DM_boxplots/boxplot60_ocean_dataAll_model.jpg", sep=""),width=14,height=14)

# ----------- Plot the means ----------------# 

# Plot the means by region as simple scatterplot ---------------------------  
select_val_ls = c(rev(as.character(model_ls)), "Margo", "Tierney", "AH", "glomap", "kn", "T_Grid")


df_means = data.frame()
df_means <- df_means %>% dplyr::mutate (means = NA, Source = "null", Type="data")

for (band in latband_ls) {
  df_band <- data_all[data_all$lat_band == band, ]
  for (meanSource in select_val_ls) {
    
    #print(meanSource)
    
    df_part <- df_band[df_band$ref == meanSource, ]
    #df_part <- df_source[df_source$lat_band == band, ]
    
    meanVal <- mean(df_part$value, na.rm=TRUE)
    #print(meanVal)
    if (meanSource == "Margo" ||  meanSource ==  "Tierney" || meanSource ==  "AH" || meanSource ==  "glomap" ||  meanSource ==  "kn" || meanSource == "T_Grid") {
      typeVal = "data" }
    else {
      typeVal = "model"  
    }
    df_new <- data.frame(meanSource, meanVal, typeVal) 
    
    df_means <- rbind(df_means, df_new)
    #print(df_means)
    title = paste("Means of Ocean Datasets", band, sep=" ")
    mp <- ggplot(df_means, aes(meanVal, meanSource)) +
      geom_point(size = 10, aes(colour = factor(meanSource), shape = factor(typeVal))) + 
      labs(title=title, y="Source", x="Mean Value") +
      scale_color_discrete(name="Source") + scale_shape_discrete(name="") +
      theme(plot.title=element_text(size=40,  face="bold"), axis.title.x=element_text(size=25), axis.title.y=element_text(size=25), axis.text.x=element_text(size = 15),
            axis.text.y=element_text(size = 15),
            legend.text = element_text(size=25),legend.title = element_text(size=30) )
    
    ggsave(mp,file=paste(plotpath,"/oceanplots/mean_plots/meanLatBand_",band,"_ocean_data.jpg", sep=""),width=14,height=11)
    
    write.csv(df_means,row.names=FALSE, paste(datapath,"/ocean_output/mean_",band,"_ocean_data.csv", sep=""))
  }
}

# extract statistical summary of all variables used in the boxplot
# dlist <- c("Margo", "Tierney", as.character(model_ls))
# for (i in dlist){
# x1 <- data_all %>% filter (data_all$ref == i)
# sum_obs = summary(dcast(x1, lat + lon + lat_band ~ var, value.var = "value"))
# write.csv(sum_obs, paste(datapath, "summary_mod_boxplot_",i,".csv", sep=""))
#}


graphics.off()