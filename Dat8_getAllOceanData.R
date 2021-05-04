### Read Ocean SST data for LGM from Masa
# 
# Created by Gill Thornhill in Feb2021
# Last modified: February 2021
# Note: Africa plots commented (not needed)

#### LOAD ALL DATA AND ARRANGE TO COMMON VARIABLES #### 

# ---- Get  data from Masa ) ----------

# create txt file to save logs
st = format(Sys.time(), "%Y%m%d_%H%M%S")
filename <- paste("output/", st, "ocean_obsdataMasa_metadata.txt", sep = "")
sink(filename, split = TRUE, append = TRUE) # divert all text outputs to a file
paste("Obs metadata. Created on ", Sys.Date(), sep = '')


ncfname_Margo <-
  paste(dataobspath, "ocean_data/SST_Masa/MARGO_data.nc", sep = '')
fname_ls <- ncfname_Margo
#print(ncfname_Margo)

ncfname_Tierney <-
  paste(dataobspath, "ocean_data/SST_Masa/Tierney2020_ProxyData_5x5_deltaSST.nc", sep = '')
fname_ls <- append(fname_ls, ncfname_Tierney)

ncfname_AH <-
  paste(dataobspath, "ocean_data/SST_Masa/sst_AH2013.nc", sep = '')
fname_ls <- append(fname_ls,ncfname_AH)

ncfname_glomap <-
  paste(dataobspath, "ocean_data/SST_Masa/glomap_sst_anomaly.nc", sep = '')
fname_ls <- append(fname_ls,ncfname_glomap)

ncfname_kn <-
  paste(dataobspath, "ocean_data/SST_Masa/kurahashi-nakamura_sst_anomaly.nc", sep = '')

fname_ls <- append(fname_ls,ncfname_kn)

#---------------------------------------------------

data_source_ls <- list("Margo", "Tierney", "AH", "glomap", "kn")
varname_ls <- c("lgmanomannsst","deltaSST", "SST", "sst_anomaly", "sst_anomaly" )

names(varname_ls)  <- data_source_ls
names(fname_ls)  <- data_source_ls

for (source in data_source_ls) { 
  
  fname = (fname_ls[source])
  
  ncin <- nc_open(fname)
  var_name <- (varname_ls[source])
  print(var_name)
  SST <- ncvar_get(ncin, var_name)
  atts <- ncatt_get(ncin, var_name)
  #print(ncin)
  #print(atts)
  #print(dims)
  miss_value = ncin[["var"]][[var_name]][["missval"]]
  SST <- as.data.frame (SST) # na_if works with df
  SST <- SST %>% dplyr::na_if(miss_value)
  
  #------------------------------------  
  if(source == "Tierney") {
    lon <- "lon"
    lat <- "lat"
  }
  else {
    lon <- "longitude"
    lat = "latitude"
  }
  
  
  # Make it ready to plot
  if (is.null(ncin$dim$axis_3$len)) {
    targetSize <-c(ncin[["dim"]][[lon]][["len"]], ncin[["dim"]][[lat]][["len"]])#lon*lat
  } else{
    targetSize <- c(ncin$dim$axis_3$len, ncin$dim$axis_2$len) #lon*lat
  }
  
  #add column (lat) and row names (lon)
  colnames(SST) <- ncin[["dim"]][[lat]][["vals"]]
  
  
  # make sure lat order is correct in all models
  SST <- SST[,order(-as.numeric(names(SST)))] %>% as.matrix(.)
  
  # adjust longitudes so that they're -180 to 180 and ensure they're right
  #adjust lon names to -180 to 180 and rearrange matrix for plotting
  lon_names <-as.data.frame (ncin[["dim"]][[lon]][["vals"]])
  index <- lon_names > 180
  lon_names[index, 1] <- (lon_names[index] - 360)
  colnames(lon_names) <- "lon_180"
  rownames(SST) <- as.array(lon_names$lon_180)
  SST <- SST[order(as.numeric(row.names(SST))),]
  
  #-------------------------------------------
  
  # Produce output for use in benchmark code (csv)
  print("Get the max and min values")
  
  # Convert to CSV
  fout = paste("ocean_obs_", source, ".csv", sep="")
  
  r<-brick(fname, varname=var_name)
  nc.df <- as.data.frame(r[[1]], xy=T) %>% mutate(ref = source) %>% `colnames<-`(c('lon','lat','ocean_tas_anom', 'ref'))
  
  
  # lons should all be from -180 to 180. They need revision if 0-360.
  if (max(nc.df['lon'] > 181)) {
    index <- nc.df['lon'] > 180; nc.df['lon'][index] <- nc.df['lon'][index] - 360 
  }
  #nc.df <- nc.df[order(nc.df$lon),]
  # Swap order of lat and lon
  nc.df <- nc.df[c("lat","lon", "ocean_tas_anom", "ref")]
  head(nc.df)
  
  print("Before max and min")
  
  ######## Get values for Margo min and max #######
  # Need to set those values where the min and max = mean to NA 
  if (source == "Margo") {
    print("Margo max and min")
    print(fname)
    r_max <- brick(fname, varname="lgmanomannmax")
    refmax = "Margo_max"
    nc_max.df <- as.data.frame(r_max[[1]], xy=T) %>% mutate(ref = refmax) %>% `colnames<-`(c('lon','lat','ocean_tas_anom', 'ref'))
    if (max(nc_max.df['lon'] > 181)) {
      index <- nc_max.df['lon'] > 180; nc_max.df['lon'][index] <- nc_max.df['lon'][index] - 360 
    }
    # nc_max.df <- nc_max.df[order(nc_max.df$lon),]
    nc_max.df <- nc_max.df[c("lat","lon", "ocean_tas_anom","ref")]
    
    
    r_min <- brick(fname, varname="lgmanomannmin")
    refmin = "Margo_min"
    nc_min.df <- as.data.frame(r_min[[1]], xy=T) %>% mutate(ref = refmin) %>% `colnames<-`(c('lon','lat','ocean_tas_anom', 'ref'))
    if (max(nc_min.df['lon'] > 181)) {
      index <- nc_min.df['lon'] > 180; nc_min.df['lon'][index] <- nc_min.df['lon'][index] - 360 
    }
    #nc_min.df <- nc_min.df[order(nc_min.df$lon),]
    nc_min.df <- nc_min.df[c("lat","lon", "ocean_tas_anom","ref")]
    
    # Check where max and min = mean
    #out <- ifelse(nc_min.df$ocean_tas_anom == nc.df$ocean_tas_anom, "equal", "different")
    #print(out)
    
    nc_min.df$ocean_tas_anom[nc_min.df$ocean_tas_anom == nc.df$ocean_tas_anom] <- NA 
    nc_max.df$ocean_tas_anom[nc_max.df$ocean_tas_anom == nc.df$ocean_tas_anom] <- NA
  } 
  #----------------------------------------------------------------
  ###### Fix values for Tierney min and max (use std) #######
  else if (source == "Tierney") {
      print("Tierney max and min")
      r_std <- brick(fname, varname="std")
      refstd = "std"
      nc_std.df <- as.data.frame(r_std[[1]], xy=T) %>% mutate(ref = refstd) %>% `colnames<-`(c('lon','lat','ocean_tas_anom_std', 'ref'))
      nc_std.df <- nc_std.df[c("lat","lon", "ocean_tas_anom_std","ref")]
      
      # if nc_std.df == 0 set max and min to NA
      nc_min.df <- nc_std.df %>% mutate(ref = "Tierney_min", ocean_tas_anom_std = nc.df$ocean_tas_anom - nc_std.df$ocean_tas_anom_std) %>%
        dplyr::rename (ocean_tas_anom = ocean_tas_anom_std)
      
      nc_max.df <- nc_std.df %>% mutate(ref = "Tierney_max", ocean_tas_anom_std = nc.df$ocean_tas_anom + nc_std.df$ocean_tas_anom_std) %>%
        dplyr::rename (ocean_tas_anom = ocean_tas_anom_std)
      
      
    } 
    #----------------------------------------------------------
    # Default min and max values to standard where they are not available, and add to file so plot works properly
    else {
      refVal = paste(source, "_min", sep = "")
      nc_min.df <- nc.df %>% mutate(ref = refVal) %>% `colnames<-`(c('lat','lon','ocean_tas_anom', 'ref'))
      refVal = paste(source, "_max", sep = "")
      nc_max.df <- nc.df %>% mutate(ref = refVal) %>% `colnames<-`(c('lat','lon','ocean_tas_anom', 'ref'))
      
    }
    #---------------------------------------------------------------------
    nc.df <- rbind(nc.df, nc_min.df, nc_max.df) %>%
    `colnames<-`(c("lat","lon", "ocean_tas_anom", "ref"))
    
    write.csv(nc.df,row.names=FALSE, paste(dataobspath, "/ocean_data/SST_Masa/",fout, sep=""))
    
    
    # ---------------------------------------------------------------------  
    #print("Mapping plots")
    # MAP 1: ANOMALIES
    
    cols <- (rev(brewer.pal(9, "RdBu")))
    varunits <- "K"
    
    #cairo_pdf(
      #paste(plotpath, 'oceanplots/MasaData/', source, '_SST.pdf', sep = ""),width = 11.69,
      #height = 8.27, onefile = T)
    
    var_title <-paste(source, "SST anomaly",sep = "")
    
    #colbreaks
    colbreaks <- c(seq(from = -10, to = 2, length.out = 10))
    #colbreaks <- c(seq(from = min(SST, na.rm = TRUE),to = max(SST, na.rm = TRUE),length.out = 11
    #))
    
    p <- plot_mtco_eg_disc(
      mat_withlatlon = SST,
      cols = cols,
      brkpnt = colbreaks,
      title_name = var_title,
      varunits = varunits,
      shapefile_df = shapefile_df_180
    )
    
    assign(paste("map_plot_SST",var_title,sep="_"),p)
    #print(p)
    #dev.off()
    
    fig <- ggarrange(get(paste("map_plot_SST", var_title, sep="_")),   ncol = 1, nrow = 1)
    fig
    
    ggsave(fig,file=paste(plotpath, 'oceanplots/MasaData/', source, '_SST.jpg', sep = ""),width = 11.69, height = 8.27)
    print(p)
    dev.off()
    rm(ls="p")
    
    nc_close(ncin)
    
  }  
  
  
  graphics.off()
  sink()