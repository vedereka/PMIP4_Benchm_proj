# Load PMIP4 model data saved as RDS (output of Mod1 script), calculate MAP, 
# MAT, MTCO, MTWA and GDD5 mean annual anomalies,
# save anomalies as clean, homogeneised netcdf files (one nc file per model)
# Created by Laia Comas-Bru in June 2020
# Last modified: February 2021

# Note: "Error in R_nc4_create: Permission denied (creation mode was 4096)"
# appears when netcdf is not properly closed (nc_close) as when the script is 
# manually stopped midway. Once it shows up, you'll need to restart R

# load data and convert to vector to create a 2D array with all the months 
model_ls <-c('AWIESM1','AWIESM2','CCSM4-UofT','CESM1-2','CESM2-1','HadCM3-GLAC1D',
             'HadCM3-ICE6GC','iLOVECLIM1-1-1-GLAC-1D','iLOVECLIM1-1-1-ICE-6G-C',
             'INM-CM4-8','IPSLCM5A2','MIROC-ES2L','MPI-ESM1-2')
variab_ls <- c('tas', 'pr','clt')#, 'mtco','mtwa')


for (model in model_ls){
  # load lat lon for each model
  lat <- readRDS(paste(rdspath,model,"_lat.RDS", sep=""))
  lon <- readRDS(paste(rdspath,model,"_lon.RDS", sep=""))
  lonlat <- as.matrix(expand.grid(lon,lat))
  #dim(lonlat)
  nlon <- length(lon)
  nlat <- length(lat)
  time <- seq(from=1, to=12, by=1)
  nt <- 12
  
  #extract and rearrange tas, pre, clt anomalies
  for (variab in variab_ls) {
    if(variab =="clt" & model=="iLOVECLIM1-1-1-ICE-6G-C") next
    if(variab =="clt" & model=="iLOVECLIM1-1-1-GLAC-1D") next
    if(variab =="clt" & model=="CESM2-1") next
    else {
      for (mon in 1:12) {
        data.series <-
          readRDS(paste(rdspath, model, "_", variab, "_anom_", mon, ".RDS", sep =
                          ""))
        temp_vec <-
          as.vector(data.series) # check length with length(data_vec)
        
        if (mon == 1) {
          data_vec <- data.frame(cbind (lonlat, temp_vec))
        } else {
          data_vec <- data.frame(cbind (data_vec, temp_vec))
        }
      }
      colnames(data_vec)[1] <- "lon"
      colnames(data_vec)[2] <- "lat"
      colnames(data_vec)[3:dim(data_vec)[2]] <- seq(from=1, to=12, by=1) #rename columns to months
      
      # now, data_Vec has all months in columns. one row per latlon combination
      
      # reshaping the "full" data frame to an array. If ocean gridcells have been removed, you need to reshape an incomplete data
      # frame (more difficult). Check instructions here: http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html#create-and-write-a-netcdf-file
      # This is a two-step conversion:
      # 1) converting that part of the the data frame containing the 12 monthly values into a 2-d matrix, and then 
      # 2) reshaping the 2-d matrix into a 3-d array.
      
      # convert data_vec back into an array
      data_vec2 <- as.matrix(data_vec[,3:(3+12-1)])
      #dim(data_vec2)
      # then reshape the array
      data_array <- array(data_vec2, dim=c(nlon,nlat,nt))
      dim(data_array)

      if (variab == "pr") {
        data_array <- apply (data_array, c(1:2), sum, na.rm = T)
      } else {
        data_array <- apply (data_array, c(1:2), mean, na.rm = T)
      }
      
      assign(paste ("data_array_",variab,sep=""),data_array)
      
    }
  }
  
  #check whether arrays Ok
  # cutpts <-  c(seq(from = min(data_array_tas, na.rm=TRUE), to = max(data_array_tas, na.rm=TRUE),length.out =10))
  # grid <- expand.grid(lon=lon, lat=lat)
  # levelplot(data_array_tas ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, col.regions=(rev(brewer.pal(10,"RdBu"))), main="testing array")
  
  ### MTCO and MTWA ####
  for (mon in 1:12) {
    
    PI.series <- readRDS(paste(rdspath,model,"_tas_PI_",mon,".RDS", sep=""))
    LGM.series <- readRDS(paste(rdspath,model,"_tas_LGM_",mon,".RDS", sep=""))
    
    PI_vec <- as.vector(PI.series) # check length with length(data_vec)
    LGM_vec <- as.vector(LGM.series) # check length with length(data_vec)
    
    if (mon==1){
      PI_data_vec <- data.frame(cbind (lonlat,PI_vec))
      LGM_data_vec <- data.frame(cbind (lonlat,LGM_vec))
    }else {
      PI_data_vec <- data.frame(cbind (PI_data_vec, PI_vec))
      LGM_data_vec <- data.frame(cbind (LGM_data_vec, LGM_vec))
    }
    
  }
  
  rm(PI_vec); rm(LGM_vec)
  
  colnames(PI_data_vec)[1] <- "lon"
  colnames(PI_data_vec)[2] <- "lat"
  colnames(PI_data_vec)[3:dim(PI_data_vec)[2]] <- seq(from=1, to=12, by=1) #rename columns to months
  PI_data_vec[,3:dim(PI_data_vec)[2]] <- PI_data_vec[,3:dim(PI_data_vec)[2]]-273.15 # convert to degC
  
  colnames(LGM_data_vec)[1] <- "lon"
  colnames(LGM_data_vec)[2] <- "lat"
  colnames(LGM_data_vec)[3:dim(LGM_data_vec)[2]] <- seq(from=1, to=12, by=1) #rename columns to months
  LGM_data_vec [,3:dim(LGM_data_vec)[2]] <- LGM_data_vec[,3:dim(LGM_data_vec)[2]]-273.15 # convert to degC
  # now, *_data_Vec has all months in columns. one row per latlon combination and one col per month
  
  PI_vec <- PI_data_vec
  LGM_vec <- LGM_data_vec
  
  # compute MTCO and MTWA
  LGM_data_vec$MTWA <- apply(LGM_data_vec[,3:14], 1, max)
  LGM_data_vec$MTCO <- apply(LGM_data_vec[,3:14], 1, min)
  
  PI_data_vec$MTWA <- apply(PI_data_vec[,3:14], 1, max)
  PI_data_vec$MTCO <- apply(PI_data_vec[,3:14], 1, min)
  
  MTWA_anom <- LGM_data_vec[,1:2]
  MTWA_anom$MTWA <- LGM_data_vec$MTWA - PI_data_vec$MTWA
  
  MTCO_anom <- LGM_data_vec[,1:2]
  MTCO_anom$MTCO <- LGM_data_vec$MTCO - PI_data_vec$MTCO
  
  # convert data_vec back into an array and then reshape the array
  MTWA_array <- array(as.matrix(MTWA_anom[,3]), dim=c(nlon,nlat))
  MTCO_array <- array(as.matrix(MTCO_anom[,3]), dim=c(nlon,nlat)); #dim(MTCO_array)
  
  assign("data_array_mtwa",MTWA_array)
  assign("data_array_mtco",MTCO_array)
  
  ### GDD5 ####
  
  # 1. loop through each row and convert it to ls
  # 2. interpolate it daily
  # 3. select number of days above 5 degrees for each month and add up their temperature
  # 4. add a column PI/LGM_data_vec$GDD5 with that value
  
  # Use function interpol_spline_cons_mean (y_points, month_len, max_iter, tol)
  #   y_points = c(12, 13, 14, 29, 32, 35, 33, 24, 18, 10, 8, 7) # data
  #   month_len = c(31, 28 ,31, 30, 31, 30, 31, 31, 30, 31, 30, 31) # month length the data represents
  #   y_interpolated = interpol_spline_cons_mean(y_points, month_len, 100, 0.01) # interpolate
  #   unlist(lapply(unname(split(y_interpolated, rep(1:length(month_len), month_len))), mean)) # see whether the means are the same with the threshold defined
  #
  month_len = c(31, 28 ,31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  max_iter= 100 # max num of iterations to achieve conversion
  tol = 0.01 #tolerance threshold
  dates <- seq(as.Date("2001-01-01"), length = 365, by = "days") # need to add a year or it does not work
  gdd5_func <- function (x) {sum(x[which( x >5)])} # function to sum temp of days over 5 deg  
  x3 <- matrix(data=NA, nrow = dim(PI_vec)[1], ncol = dim(PI_vec)[2]) # prealocate answer
  x3 <- as.data.frame(x3)
  x3[,1:2] <- PI_vec [,1:2]
  colnames(x3) <- colnames (PI_vec)  
  
  per_ls <- c("LGM", "PI")
  
  for (per in per_ls) {
    if (per == "LGM") {
      data = LGM_vec
    } else {
      data = PI_vec
    }
    for (i in 1:dim(PI_vec)[1]) {
      x1 <- data[i, 3:14]
      if (sum(is.na(x1)) != 0)
        next #ocean gridcell, ignore
      else {
        y_points <- as.numeric(x1)
        y_interp = quiet(interpol_spline_cons_mean(y_points, month_len, max_iter, tol))
        x2 <- xts(x = y_interp, order.by = dates)
        x2_mon <- split(x2, f = "months")
        temps_avg <-as.numeric(lapply(x2_mon, gdd5_func))# values to add to new netcdf
        x3 [i, 3:14] <- t(temps_avg)
      }
    }
    
    x3$GDD5 <- apply(x3[, 3:14],MARGIN = 1,FUN = sum,na.rm = F)
    
    if (per == "LGM") {
      LGM_data_vec_gdd5 = x3
    } else {
      PI_data_vec_gdd5 = x3
    }
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
  
  # create netCDF file and put arrays (no clt in iLOVEs)
  if(variab =="clt" & model=="iLOVECLIM1-1-1-ICE-6G-C"){
    ncout <- nc_create(ncfname,list(tas_def, pre_def, mtco_def, mtwa_def, gdd5_def),force_v4=FALSE)
    } else if(variab =="clt" & model=="iLOVECLIM1-1-1-GLAC-1D") {
    ncout <- nc_create(ncfname,list(tas_def, pre_def, mtco_def, mtwa_def, gdd5_def),force_v4=FALSE)
    } else if(variab =="clt" & model=="CESM2-1") {
    ncout <- nc_create(ncfname,list(tas_def, pre_def, mtco_def, mtwa_def, gdd5_def),force_v4=FALSE)
    } else { # all models with clt data
      clt_def <- ncvar_def("clt_anom","anomalies from PI",list(londim,latdim),fillvalue,"clt",prec="single")
      ncout <- nc_create(ncfname,list(tas_def, pre_def, clt_def, mtco_def, mtwa_def, gdd5_def),force_v4=FALSE)
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
  #ncatt_put(ncout,0,"PMIP4 LGM anomalies",title$value)
  #ncatt_put(ncout,0,"Original netcdf files from IPSL (Masa Kageyama). LGM anomalies calculated at University of Reading (Laia Comas-Bru).",institution$value)
  #ncatt_put(ncout,0,"source",datasource$value)
  #ncatt_put(ncout,0,"See references in original netcdf files.",references$value)
  #history <- paste("created by L. Comas-Bru on", date(), sep=", ")
  #ncatt_put(ncout,0,"Scripts and versions available at: https://github.com/vedereka ",history, prec="text")
  
  # Get a summary of the created file:
  #ncout
  nc_close(ncout)# close the file, writing data to disk
}

graphics.off()