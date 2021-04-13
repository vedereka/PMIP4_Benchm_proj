### Read Ocean SST data for LGM from Masa
# 
# Created by Gill Thornhill in Feb2021
# Last modified: February 2021
# Note: Africa plots commented (not needed)

#### LOAD ALL DATA AND ARRANGE TO COMMON VARIABLES #### 

# ---- Get  data from Masa ) ----------

# create txt file to save logs
st = format(Sys.time(), "%Y%m%d_%H%M%S")
filename <- paste("output/", "ocean_obsdata_metadata.txt", sep = "")
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

  print(source)
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
  print("Before csv write")
  # Convert to CSV
  fout = paste("ocean_obs_", source, ".csv", sep="")
  
  if(source == "Tierney") {
    lon <- ncvar_get(ncin, "lon")
    nlon <- dim(lon)
    head(lon)
    lat <- ncvar_get(ncin, "lat", verbose = F)
    nlat <- dim(lat)
    head(lat)
  }
  else {
    lon <- ncvar_get(ncin, "longitude")
    nlon <- dim(lon)
    head(lon)
    lat <- ncvar_get(ncin, "latitude", verbose = F)
    nlat <- dim(lat)
    head(lat)
  }
  
  
  r<-brick(fname)
  nc.df <- as.data.frame(r[[1]], xy=T)
  head(nc.df)
  names(nc.df)<-c('longitude','latitude','temp')
  write.csv(nc.df,paste(dataobspath, "/ocean_data/SST_Masa/",fout, sep=""))
  
  
 # fout = paste("ocean_obs_", source, ".csv", sep="")
  #data_obs <- SST_df %>% `colnames<-`(c("lat", "lon", "SST"))
  
  #write.csv(SST_df, paste(dataobspath, "/ocean_data/SST_Masa/",fout, sep=""))
  
  # Or produce it as .nc file?
  
 
  # ---------------------------------------------------------------------  
  #print("Mapping plots")
  # MAP 1: ANOMALIES
  
  cols <- (rev(brewer.pal(11, "RdBu")))
  varunits <- "K"
  
  cairo_pdf(
    paste(plotpath, 'oceanplots/MasaData/', source, '_SST.pdf', sep = ""),width = 11.69,
    height = 8.27, onefile = T)
  
  var_title <-paste(source, "SST anomaly",sep = "")
  
  #colbreaks
    colbreaks <- c(seq(from = -10, to = 2, length.out = 11))
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
  print(p)
  dev.off()
  
  nc_close(ncin)

}  
 
 
graphics.off()
sink()