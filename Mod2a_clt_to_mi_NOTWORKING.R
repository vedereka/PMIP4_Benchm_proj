# Objective is to concert clt data to MI but something isn't working. Needs to be fixed! 
# This variable is currently being omitted  in analyses.
# Created by Laia Comas-Bru in November 2020
# Last modified: February 2021


if(!require("pacman")) install.packages ("pacman")
pacman::p_load (pacman, ncdf4, xts, tidyverse)#load contributed packages

#paths/functions
datapath <- paste(getwd(), "/data_output/", sep="") 
CRUncpath <- "D:/Dropbox/CRU_data/CRU_CL_2.0/" # set wd and paths to CRU and site data (to be changed in other PCs)
outputtemppath <- paste(getwd(), "/splash_data_input/temp_files/", sep="") 

# load functions 
source('functions_source.R')#load functions and coast shapefile
source(paste(getwd(), "/splash_r_prentice/data.R", sep=""))
source(paste(getwd(), "/splash_r_prentice/splash.R", sep=""))

fillvalue <- NA

# load data and convert to vector to create a 2D array with all the months 

model_ls <-c('AWIESM1','AWIESM2','CCSM4-UofT','CESM1-2','INM-CM4-8','MIROC-ES2L','MPI-ESM1-2',
             'iLOVECLIM1-1-1-GLAC-1D','iLOVECLIM1-1-1-ICE-6G-C','IPSLCM5A2')
variab_ls <- c('tas', 'pr','clt')
per_ls <- c("LGM", "PI")


for (model in model_ls){
  
  # load lat lon for each model
  lat <- readRDS(paste(datapath,model,"_lat.RDS", sep=""))
  lon <- readRDS(paste(datapath,model,"_lon.RDS", sep=""))
  grid_coords <- expand.grid(lon = lon, lat = lat)

  # Elevation in data (from CRU) - should be done with orography from the model outputs!?
  elevdata <-  read.delim(paste(CRUncpath, "grid_10min_elv.dat", sep = ""),header = F,
                      sep = "",dec = ".") %>% 
    `colnames<-`(c("lat", "lon", "elev")) 
  
  j <- sapply(grid_coords$lon, function(x) which.min(abs(elevdata$lon-x)))
  k <- sapply(grid_coords$lat, function(x) which.min(abs(elevdata$lat-x)))
  
  grid_coords$newLat<-elevdata$lat[k]
  grid_coords$newLon<-elevdata$lon[j]
  grid_coords$newElev <- NA
  
  for (i in 1:dim(grid_coords)[1]) {
    myLat <- grid_coords[i, "newLat"]
    myLon <- grid_coords[i, "newLon"]
    
    a = FindGridCoords(myLat, myLon, elevdata)[, 3] # function in functions_source
    
    if (pracma::isempty(a))
      grid_coords$newElev [i] <- 0
    else {
      grid_coords$newElev [i] <- a
    }
  }
  
  grid_coords$id <- seq(from=1, to=nrow(grid_coords), by =1)
  
  for (per in per_ls) {
    for (variab in variab_ls) {
      for (mon in 1:12) {
        per_vec <-
          readRDS(paste(datapath,model,"_",variab,"_",per,"_",mon,".RDS",
                        sep = "")) %>% as.vector()
        
        if (mon == 1) {
          per_data_vec <- data.frame(cbind (grid_coords, per_vec))
        } else {
          per_data_vec <- data.frame(cbind (per_data_vec, per_vec))
        }
      }
      
      colnames(per_data_vec)[1] <- "lon"
      colnames(per_data_vec)[2] <- "lat"
      colnames(per_data_vec)[7:dim(per_data_vec)[2]] <-
        seq(from = 1, to = 12, by = 1) #rename columns to months
      
      if (variab == "tas") {
        # convert to degC
        per_data_vec[, 3:dim(per_data_vec)[2]] <-
          per_data_vec[, 3:dim(per_data_vec)[2]] - 273.15
      }
      
      if (variab == "clt") {
        # convert to hours of sunshine: 1-(cloud fraction/100)
        per_data_vec[, 3:dim(per_data_vec)[2]] <-
          1 - (per_data_vec[, 3:dim(per_data_vec)[2]] / 100)
      }
      
      # now, *_data_Vec has all months in columns. one row per latlon combination and one col per month
      assign(paste ("data_mon", model, variab, per, sep = "_"),
             per_data_vec)
      
      # interpolate all three variables to daily, as we need it to calc evap and ultimately calc MI
      
      # Use function interpol_spline_cons_mean (y_points, month_len, max_iter, tol)
      #   y_points = c(12, 13, 14, 29, 32, 35, 33, 24, 18, 10, 8, 7) # data
      #   month_len = c(31, 28 ,31, 30, 31, 30, 31, 31, 30, 31, 30, 31) # month length the data represents
      #   y_interpolated = interpol_spline_cons_mean(y_points, month_len, 100, 0.01) # interpolate
      #   unlist(lapply(unname(split(y_interpolated, rep(1:length(month_len), month_len))), mean)) # see whether the means are the same with the threshold defined
      month_len = c(31, 28 , 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      max_iter = 100 # max num of iterations to achieve conversion
      tol = 0.01 #tolerance threshold
      dates <-
        seq(as.Date("2001-01-01"), length = 365, by = "days") # need to add a year or it does not work
      x3 <- matrix(data = NA,
                   nrow = dim(grid_coords)[1],
                   ncol = 365) %>%
        as.data.frame() %>% cbind(grid_coords,.)
      
      quiet(for (i in 1:dim(grid_coords)[1]) {
        x1 <-
          get (paste ("data_mon", model, variab, per, sep = "_"))[i, 7:18]
        
        if (sum(is.na(x1)) != 0)
          next #ocean gridcell, ignore
        else {
          y_points <- as.numeric(x1)
          y_interp <-
            interpol_spline_cons_mean(y_points, month_len, max_iter, tol)
          x2 <- xts(x = y_interp, order.by = dates)
          x3 [i, 7:ncol(x3)] <- t(x2)
        }
      })
      
      colnames(x3)[7:ncol(x3)] <- seq(from = 1, to = 365, by = 1)
      x3 <- x3[complete.cases(x3),]
      assign(paste(variab, per, "daily", sep = "_"), x3)
      rm(list = c("x2", "x1"))
    }
  }


 
  
  
  
  
  
  ##------------------------------------ he arribat fins aqui calc ETP to then calc MI
  
  # 1. Calc ETP with SPLASH
  # 2. Use that to calc MI in PI and in LGM
  # 3. Calculate MI LGM-PI anomaly and merge it with the data outuput in Step3 so 
  # that it is included in the netcdf conversion
  # 4. double check values are real with panoply
  # 5. ensure MI is included in all other steps (scores in ubunti, matrix of scores,
  #                                              scatterplots, etc)
  # 6. re-run everything
  
  for (per in per_ls) {}
  
  for (i in 1:dim(x3)[1]){
    
    data=list(sf = t(get(paste("clt",per,"daily", sep="_")) [i, 7:ncol(get(paste("clt",per,"daily", sep="_")))]))
    data$tair <- t(get(paste("tas",per,"daily", sep="_")) [i, 7:ncol(get(paste("tas",per,"daily", sep="_")))])
    data$pn <- t(get(paste("pr",per,"daily", sep="_")) [i, 7:ncol(get(paste("pr",per,"daily", sep="_")))])
    data$lat = x3$lat[i]
    data$elv = x3$newElev[i]
    data$id = x3$id[i]
    
    if (!is.na(data$sf[1])) {
      for (n in seq(1, 365, by = 1)) {
        xx <- calc_daily_evap(data$lat, n, data$elv, y = 0, data$sf[n], 
                              data$tair[n],sw = 1.0)
        data$eet[n] = xx$eet_mm
        data$aet[n] = xx$aet_mm
      }
    }
    
    data$eet_mon <-as.numeric(lapply(split(xts(x = data$eet, order.by = dates), f = "months"), mean, na.rm=T))
    data$aet_mon <-as.numeric(lapply(split(xts(x = data$aet, order.by = dates), f = "months"), mean, na.rm=T))
    
    data$sf_mon <-as.numeric(lapply(split( xts(x = data$sf, order.by = dates), f = "months"), mean, na.rm=T))
    data$tair_mon <-as.numeric(lapply(split( xts(x = data$tair, order.by = dates), f = "months"), mean, na.rm=T))
    data$pn_mon <-as.numeric(lapply(split( xts(x = data$pn, order.by = dates), f = "months"), mean, na.rm=T))
    
    data$mi = sum(data$pn_mon) / sum (data$eet_mon)
    
    grid_coords[which(grid_coords$id==x3$id[i]),"EqEvapo_CRU"] <- sum(data$eet_mon)
    grid_coords[which(grid_coords$id==x3$id[i]),"MAP_CRU"] <- sum(data$pn_mon)
    grid_coords[which(grid_coords$id==x3$id[i]),"MI"] <- data$mi
    
    #saveRDS(data, file = paste(outputtemppath, id_ls[i],"_mon.RDS",sep=""))
  }
  
  
  
  
  
  
  # compute GDD
  #This is wrong. GGDD5 is monthly!!!!
  GDD5_anom <- LGM_data_vec_gdd5[,1:2]
  GDD5_anom$GDD5 <- LGM_data_vec_gdd5$GDD5 - PI_data_vec_gdd5$GDD5
  
  # convert data_vec back into an array and then reshape the array
  GDD5_array <- array(as.matrix(GDD5_anom[,3]), dim=c(nlon,nlat)); 
  assign("data_array_gdd5",GDD5_array)
  
  ### Create and write netCDF files -- ncdf4 versions ####
  # here I have one data array for each variable in the model
  # First, create the netCDF filename:
  # path and file name, set dname
  ncpath <- paste(getwd(), "/netcdf_output/", sep="") 
  ncname <- paste(model,"_LGM_anomalies",sep="")
  ncfname <- paste(ncpath, ncname, ".nc", sep="")
  
  # define dimensions
  londim <- ncdim_def("lon","degrees",as.double(lon)) 
  latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
  #timedim <- ncdim_def("time","months",as.double(time)) # annual anomalies, no need for time dimension
  
  # define variables
  fillvalue <- NA
  tas_def <- ncvar_def("tas_anom","anomalies from PI", list(londim,latdim),fillvalue,"tas",prec="single")
  pre_def <- ncvar_def("pre_anom","anomalies from PI",list(londim,latdim),fillvalue,"pre",prec="single")
  mtco_def <- ncvar_def("mtco_anom","anomalies from PI",list(londim,latdim),fillvalue,"mtco",prec="single")
  mtwa_def <- ncvar_def("mtwa_anom","anomalies from PI",list(londim,latdim),fillvalue,"mtwa",prec="single")
  gdd5_def <- ncvar_def("gdd5_anom","anomalies from PI",list(londim,latdim),fillvalue,"gdd5",prec="single")
  
  # Next, create the file, and put the variables into it, along with additional variable and “global” attributes
  # (those that apply to the whole file). Note that the attributes are of key importance to the self-documenting 
  # properties of netCDF files.
  
  # create netCDF file and put arrays
  if(variab =="clt" & model=="iLOVECLIM1-1-1-ICE-6G-C"){
    ncout <- nc_create(ncfname,list(tas_def, pre_def, mtco_def, mtwa_def, gdd5_def),force_v4=TRUE)
  } else if(variab =="clt" & model=="iLOVECLIM1-1-1-GLAC-1D") {
    ncout <- nc_create(ncfname,list(tas_def, pre_def, mtco_def, mtwa_def, gdd5_def),force_v4=TRUE)
  } else { # all models with clt data
    clt_def <- ncvar_def("clt_anom","anomalies from PI",list(londim,latdim),fillvalue,"clt",prec="single")
    ncout <- nc_create(ncfname,list(tas_def, pre_def, clt_def, mtco_def, mtwa_def, gdd5_def),force_v4=TRUE)
    ncvar_put(ncout,clt_def,data_array_clt)
  }
  
  # put variables #clt is already there for models were it exists
  ncvar_put(ncout,tas_def,data_array_tas)
  ncvar_put(ncout,pre_def,data_array_pr)
  ncvar_put(ncout,mtco_def,data_array_mtco)
  ncvar_put(ncout,mtwa_def,data_array_mtwa)
  ncvar_put(ncout,gdd5_def,data_array_gdd5)
  
  # put additional attributes into dimension and data variables
  ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,"lat","axis","Y")
  #ncatt_put(ncout,"time","axis","T")
  
  # add global attributes
  # ncatt_put(ncout,0,"title",title$value)
  # ncatt_put(ncout,0,"institution",institution$value)
  # ncatt_put(ncout,0,"source",datasource$value)
  # ncatt_put(ncout,0,"references",references$value)
  history <- paste("created by L. Comas-Bru on", date(), sep=", ")
  ncatt_put(ncout,0,"history",history, prec="text")
  # ncatt_put(ncout,0,"Conventions",Conventions$value)
  
  # Get a summary of the created file:
  #ncout
  nc_close(ncout)# close the file, writing data to disk
  
}

