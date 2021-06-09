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


#---------------------------------------------------------

# To do this by region - These are not ocean regions
source('region_def.R')

# Select the regions from the region_def routine
region_ls <- region_ls_zonal

#------------------------------------------------------
#### LOAD OBSERVATIONS AND ORGANISE DATA ####
# files produced in Step0 extract site data
data_obs <- read.csv(file.path(dataobspath, "data_obs_raw.csv"), na.strings = "NA",strip.white = TRUE, blank.lines.skip = T) %>%
  dplyr::rename (LAT = lat, LON = lon) %>%  dplyr::select (LAT, LON, MAT, MTCO, MTWA, MAP, REF)

data_BarPre <- data_obs %>%  filter (REF == "B_wf" | REF == "PR_all")

data_Cle <- data_obs %>%  filter (REF == "CL_all_244") # use most recent Cleator dataset

#### SELECT OVERLAPPING SITES BETWEEN BARTLEIN GRIDS AND CLEATOR ####
# load gridcells from Bartlein's gridded data and filter Cleator to just that spread of data
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

# end of data manipulation #
#### BOXPLOT #1: only data ####
## comparisons for gridded overlapping data sources
dtBP <- grid_BartPren [, -c(3:7)]
dtCL <- grid_Cle  [, -c(3:7)]
dtCL_all <- data_Cle
colnames(dtCL_all) [1] <- "lat"
colnames(dtCL_all) [2] <- "lon"
dtCL_all$REF <- "CL_all"
obs <- rbind(dtBP, dtCL, dtCL_all)

# Set the default region for the datasets
obs <- obs %>% dplyr::mutate (Region = "global")

for (region in region_ls$reg_name) {
  print(paste(region, "obs num ", dim(obs[1]), sep=" "))
  obs_reg <- obs %>% filter (lat >= region_ls %>% filter (reg_name == region) %>% dplyr::select(min_lat) %>% as.numeric() &
                               lat <= region_ls %>% filter (reg_name == region) %>% dplyr::select(max_lat) %>% as.numeric() &
                               lon >= region_ls %>% filter (reg_name == region) %>% dplyr::select(min_lon) %>% as.numeric() &
                               lon <= region_ls %>% filter (reg_name == region) %>% dplyr::select(max_lon) %>% as.numeric()) %>% mutate(Region = region)
  
  
  obs_reg = obs_reg[!is.na(obs$Region),] #remove lats outside of range
  
  
  # # Group the data by latitudinal bands
  # brkpnt <- seq(-90, 90, by = 60)
  # startpnt <- brkpnt[1:length(brkpnt) - 1]
  # endpnt <- brkpnt[2:length(brkpnt)]
  # brk_lab <- paste(startpnt, '° to ', endpnt, '°', sep = '')
  # 
  # obs$lat_band <- cut(obs$lat, breaks = brkpnt,labels = brk_lab)
  # obs = obs[!is.na(obs$lat_band),] #remove lats outside of range
  
  # select chosen variables, in this case, MAP, MTCO and MTWA
  #obs <- obs [,-c(3,7:8)]
  
  #save statistical summary of each variable
  sum_obs = summary(obs_reg %>% filter (obs_reg$REF == "BP"))
  write.csv(sum_obs, paste(datapath, "summary_BP.csv", sep=""))
  sum_obs = summary(obs_reg %>% filter (obs_reg$REF == "CL_all"))
  write.csv(sum_obs, paste(datapath, "summary_CL_all.csv", sep=""))
  sum_obs = summary(obs_reg %>% filter (obs_reg$REF == "CL"))
  write.csv(sum_obs, paste(datapath, "summary_CL_overlap.csv", sep=""))
  obs2 = obs
  
  obs_reg_reshape <- reshape2::melt(obs_reg, na.rm=F, id.vars = c("lat","lon","REF", "Region"), variable.name = "var")
  # undo with: dcast(obs, lat + lon + REF + region ~ var, value.var = "value")
  obs_reg_reshape$REF <- factor(obs$REF , levels=c("CL_all", "CL", "BP")) # reorder boxplots bottom to top
  
  bp <- ggplot(na.omit(obs_reg_reshape), aes(x=region, y=value, fill=REF)) +
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
  
  #print(bp)
  
  ggsave(bp,file=paste(plotpath,"DM_boxplots/Landboxplot_",region,"_data_B_CL244.jpg", sep=""),width=12,height=7)
  ##-------------------------------------------------------------------
  #### BOXPLOT #2: observations and model data ####
  
  mod_variable_ls <- c('tas_anom','mtco_anom','mtwa_anom','pre_anom', 'gdd5_anom')
  
  # location of model output
  mod_dir <- ncpath
  mod_files <- list.files(mod_dir, pattern = "anomalies", full.names = TRUE)
  
  # create list of model names for output
  model_ls <- lapply(list.files(mod_dir, pattern="anomalies", full.names = F), FUN = my_name_trim) %>% as.character (.)
  
  
  #print(model_ls)
  obs_coord = unique(obs[,1:2])
  
  for (mod_name in model_ls){
    
    ncname <- paste(ncpath, mod_name, "_LGM_anomalies.nc",sep="")
    ncin <- nc_open(ncname)
    lat <- ncin[["dim"]][["lat"]][["vals"]]; nlat <- length(lat)
    lon <- ncin[["dim"]][["lon"]][["vals"]];nlon <- length(lon)
    grid <- expand.grid(lon=lon, lat=lat)
    
    
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
  
  pts <- pts %>% dplyr::mutate (Region = "global")
  pts <- pts %>% filter (lat >= region_ls %>% filter (reg_name == region) %>% dplyr::select(min_lat) %>% as.numeric() &
                           lat <= region_ls %>% filter (reg_name == region) %>% dplyr::select(max_lat) %>% as.numeric() &
                           lon >= region_ls %>% filter (reg_name == region) %>% dplyr::select(min_lon) %>% as.numeric() &
                           lon <= region_ls %>% filter (reg_name == region) %>% dplyr::select(max_lon) %>% as.numeric()) %>% mutate(Region = region)
  
  
  # rename vars
  pts <- data.frame(lapply(pts, function(x) {gsub("tas_anom", "MAT", x)}))
  pts <- data.frame(lapply(pts, function(x) {gsub("mtco_anom", "MTCO", x)}))
  pts <- data.frame(lapply(pts, function(x) {gsub("mtwa_anom", "MTWA", x)}))
  pts <- data.frame(lapply(pts, function(x) {gsub("pre_anom", "MAP", x)}))
  pts <- data.frame(lapply(pts, function(x) {gsub("gdd5_anom", "GDD5", x)}))
  
  #print(pts)
  #print(obs)
  data_all = rbind(obs_reg_reshape, pts)
  
  #remove => CL (=Cleator at Bartlein sites)
  data_all <- data_all %>% filter(REF != "CL")
  data_all$lat <- as.numeric(data_all$lat)
  data_all$lon <- as.numeric(data_all$lon)
  data_all$value <- as.numeric(data_all$value)
  data_all$var <- as.factor(data_all$var)
  data_all$REF <- factor(data_all$REF ,
                         levels= c(rev(as.character(model_ls)), "CL_all", "BP"))
  
  
  saveRDS(data_all, file = paste(datapath,"obs_mod.RDS", sep=""))
  
  require (randomcoloR) # ColorBrewer max length is 12, we need 13 + 2 grey
  # color palette in the right order
  n <- length(unique(data_all$REF)) %>%  distinctColorPalette(.)
  colorSet <- rev(c(n[1:2],'grey75', 'grey40',n[3:length(n)]))
  # pie(rep(1, length(colorSet), col=colorSet)) # to see colours in a pie chart (diff each time)
  
  require(facetscales) # install with devtools::install_github("zeehio/facetscales")
  #set limits for each variable (only possible with facetscales)
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
  
  bpmodel <-ggplot(na.omit(data_all), aes(x=region, y=value, fill=var)) +
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
                               nrow = 5,
                               ncol = 3,
                               label.position = "bottom",
                               legend.box.just = "right",
                               #legend.text.align=0,
                               label.theme = element_text(angle = -90, vjust = 0.5, hjust=0,size=10),
                               title.position = "bottom", title.theme = element_text(angle = 90)))+
    
    scale_x_discrete(position = "top") +
    scale_fill_manual(name = element_blank(),
                      breaks = c(model_ls[3], model_ls[2], model_ls[1],"CL_all", "BP",
                                 model_ls[8],model_ls[7],model_ls[6],model_ls[5],model_ls[4],
                                 model_ls[13],model_ls[12],model_ls[11],model_ls[10],model_ls[9]),
                      labels = c(model_ls[3], model_ls[2], model_ls[1],"CL_all", "BP",
                                 model_ls[8],model_ls[7],model_ls[6],model_ls[5],model_ls[4],
                                 model_ls[13],model_ls[12],model_ls[11],model_ls[10],model_ls[9]),
                      values = colorSet) + #strange order
    facet_grid_sc(rows=vars(var), scales = list(y = scales_y))+
    theme(strip.text.y = element_text(
      size = 14, color = "black", face = "bold"
    ))
  
  
  ggsave(bpmodel,file=paste(plotpath,"DM_boxplots/Landboxplot_",region,"_dataModel_All.jpg", sep=""),width=11,height=14)
  
  
  
  # extract statistical summary of all variables used in the boxplot
  # br <- c("CL_all", "BP", as.character(model_ls))
  # for (i in br){
  #   x1 <- data_all %>% filter (data_all$REF == i)
  #   sum_obs = summary(dcast(x1, lat + lon + lat_band ~ var, value.var = "value"))
  #   write.csv(sum_obs, paste(datapath, "summary_mod_boxplot_",i,".csv", sep=""))
  # }
  
  
  # ----------- Plot the means ----------------#
  
  #Plot the means by region as simple scatterplot ---------------------------
  
  select_val_ls = c(rev(as.character(model_ls)), "CL_all", "BP")
  #select_val_ls = c("CL_all", "BP")
  varList_ls = c("MAT", "MTCO", "MTWA", "MAP", "GDD5")
  
  #for (varname in varList_ls) {
    
    df_means = data.frame()
    df_means <- df_means %>% dplyr::mutate (means = NA, Source = "null", Type="data")
    for (meanSource in select_val_ls) {
      varname= "GDD5"
      #data_all[is.na(data_all)] <- NA
      df_var <- data_all[data_all$var == varname, ]
      print(meanSource)
      #print(df_var)
      df_part <- df_var[df_var$REF == meanSource, ] 
     
      #print(colnames(data_all))
      meanVal <- mean(df_part$value, na.rm=TRUE)
      print(meanVal)
      if (meanSource == "CL_all" ||  meanSource ==  "BP") {
        typeVal = "data" }
      else {
        typeVal = "model"
      }
      df_new <- data.frame(meanSource, meanVal, typeVal)
      
      df_means <- rbind(df_means, df_new)
      #print(df_means)
      title = paste("Means of Land Datasets", region, varname, sep=" ")
      mp <- ggplot(df_means, aes(meanVal, meanSource)) +
        geom_point(size = 10, aes(colour = factor(meanSource), shape = factor(typeVal))) +
        labs(title=title, y="Source", x="Mean Value") +
        scale_color_discrete(name="Source") + scale_shape_discrete(name="") +
        theme(plot.title=element_text(size=40,  face="bold"), axis.title.x=element_text(size=25), axis.title.y=element_text(size=25), axis.text.x=element_text(size = 15),
              axis.text.y=element_text(size = 15),
              legend.text = element_text(size=25),legend.title = element_text(size=30) )
      
      
      ggsave(mp,file=paste(plotpath,"/mean_plots/mean_land_",region,"_",varname,"data.jpg", sep=""),width=14,height=11)
    }
    
  }
  
#}

graphics.off()