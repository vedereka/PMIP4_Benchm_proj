#### RUNNING SPLASH Model Inputs #### 

# For radiation, the basic geographic coordinates and time parameters needed are:
# - year (y)
# - day of year (n)
# - latitude (φ), radians: rad = deg⋅π/180.
# - elevation (z), meters
# 
# For evapotranspiration, the basic meteorological variables needed are:
#   
# - daily air temperature (Tair), °C
# - daily precipitation (Pn), mm
# - daily fraction of bright sunshine hours (Sf), %

# Data from CRU CL 2.0 dataset (as used in Bartlein)
# grid_10min_pre.dat.gz -> Precipitation; mm/month
# grid_10min_tmp.dat.gz	-> Mean temperature; degC
# grid_10min_sunp.dat.gz -> Sunshine; percent of maximum possible (percent of daylength)
# grid_10min_elv.dat.gz -> Elevation; km

# Gonna be using monthly data so need to interpolate with the mean-conserving spline 

# Script created by Laia Comas-Bru in October 2020.
# Last modified: February 2021.


# set paths not in init.R and load site data
outputtemppath <- paste(getwd(), "/splash_data_temp/", sep="") 
CRUncpath <- paste(getwd(), "/input_CRU_CL_2.0/", sep="")

# load splash functions (the order is important)
source(paste(getwd(), "/splash_r_prentice/data.R", sep=""))
source(paste(getwd(), "/splash_r_prentice/const.R", sep=""))
source(paste(getwd(), "/splash_r_prentice/evap.R", sep=""))
source(paste(getwd(), "/splash_r_prentice/splash.R", sep=""))

# load data
site_data <- read.csv(paste(dataobspath,"bartlein.csv",sep="")) # raw Bartelin data file
site_data <- site_data[,1:5]

data <-  read.delim(paste(CRUncpath, "grid_10min_elv.dat", sep = ""),header = F,sep = "",dec = ".")%>% 
  `colnames<-`(c("lat", "lon", "elev")) 


j <- sapply(site_data$LON, function(x) which.min(abs(data$lon-x)))
k <- sapply(site_data$LAT, function(x) which.min(abs(data$lat-x)))
site_data$newLat<-data$lat[k]
site_data$newLon<-data$lon[j]


for (i in 1:dim(site_data)[1]) {
  myLat <- site_data[i, "newLat"]
  myLon <- site_data[i, "newLon"]
  
  a = FindGridCoords(myLat, myLon, data)[, 3] # function in functions_source
  
  if (!pracma::isempty(a)) {
    site_data$newElev [i] <- a #[,3]
  }
}


# list CRU variables
var_ls <- c('pre', 'tmp', 'sunp')

for (var in var_ls) {
  #var <- "tmp"
  nm <- 12 # months
  
  data <-  read.delim(paste(CRUncpath, "grid_10min_", var, ".dat", sep = ""),header = F,sep = "",dec = ".")
  colnames(data) <- c("lat", "lon", paste(var, seq(1:nm), sep = ""))
  
  pts_ltm <- cbind (site_data[i, ], matrix(data=NA, nrow = 1, ncol = 12))
  colnames (pts_ltm) [(ncol(pts_ltm)-11):ncol(pts_ltm)] = paste(var, seq(1:nm), sep = "")
  
  for (i in 1:dim(site_data)[1]) {
    myLat <- site_data[i, "newLat"]
    myLon <- site_data[i, "newLon"]
    
    a = FindGridCoords(myLat, myLon, data)
    
    if (nrow(a) == 0){
      pts_ltm [i,] <- as.numeric(cbind (site_data[i, ], matrix(data=NA, nrow = 1, ncol = 12)))
    } else {
      pts_ltm [i,] <- cbind (site_data[i,], a[, 3:ncol(a)])
    }
  }
  
  # delete unwanted rows (once it's check that all good)
  
  pts_ltm <- pts_ltm[,c(-7:-5)]
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
  x3 <- matrix(data=NA, nrow = dim(pts_ltm)[1], ncol = 365+5) # prealocate answer
  x3 <- as.data.frame(x3)
  
  x3[,1:5] <- pts_ltm [,1:5]

  colnames(x3)[1:5] <- colnames (pts_ltm)[1:5]
  
  quiet(
    for (i in 1:dim(pts_ltm)[1]) {
      x1 <- pts_ltm[i, 6:17]
      if (sum(is.na(x1)) != 0)
        next #ocean gridcell, ignore
      else {
        y_points <- as.numeric(x1)
        y_interp = interpol_spline_cons_mean(y_points, month_len, max_iter, tol)
        x2 <- xts(x = y_interp, order.by = dates)
        x3 [i, 6:ncol(x3)] <- t(x2)
      }
      colnames(x3)[6:ncol(x3)] <- seq(from=1, to=365, by=1)
      assign(paste(var,"_daily",sep=""),x3)
    }
  )
}


site_data <- merge (site_data[,c(1:4,8)], read.csv(paste(dataobspath,"bartlein.csv",sep="")) [,-c(5,6,21)],
             by=c("SAMPLE_ID")) %>% 
  dplyr::rename(ELEVATION = newElev, SITE_ID = SITE_ID.x, LAT = LAT.x, LON = LON.x) %>%
  .[,-c(6:8)]

id_ls <- site_data$SAMPLE_ID


for (i in 1:length(id_ls)){
  
  sf <- t(sunp_daily [i, 6:ncol(sunp_daily)])
  tair <- t(tmp_daily [i, 6:ncol(tmp_daily)])
  pn <- t(pre_daily [i, 6:ncol(pre_daily)])
  
  cbind (sf,tair,pn) %>% 
    `colnames<-`(c("sf", "tair", "pn")) %>% 
    write.csv(., paste(outputtemppath, id_ls[i],".csv", sep=""), na = "NA",  row.names = FALSE)
  
  data <- read_csv(paste(outputtemppath, id_ls[i],".csv", sep=""),y=-1)
  data$lat <- site_data$LAT[i]
  data$elv <- site_data$ELEVATION[i]
  data$sample_id <- id_ls[i]
  
  if (!is.na(data$sf[1])){
    for (n in seq(1,365,by=1)){
      xx <- calc_daily_evap(data$lat, n, data$elv, y=0, data$sf[n], data$tair[n], sw=1.0)
      data$eet[n] = xx$eet_mm
      data$aet[n] = xx$aet_mm
    }
  }
  
  data$eet_mon <-as.numeric(lapply(split( xts(x = data$eet, order.by = dates), f = "months"), mean, na.rm=T))
  data$aet_mon <-as.numeric(lapply(split( xts(x = data$aet, order.by = dates), f = "months"), mean, na.rm=T))
  
  data$sf_mon <-as.numeric(lapply(split( xts(x = data$sf, order.by = dates), f = "months"), mean, na.rm=T))
  data$tair_mon <-as.numeric(lapply(split( xts(x = data$tair, order.by = dates), f = "months"), mean, na.rm=T))
  data$pn_mon <-as.numeric(lapply(split( xts(x = data$pn, order.by = dates), f = "months"), mean, na.rm=T))
  
  data$mi = sum(data$pn_mon) / sum (data$eet_mon)
  
  site_data[which(site_data$SAMPLE_ID==id_ls[i]),"EqEvapo_CRU"] <- sum(data$eet_mon)
  site_data[which(site_data$SAMPLE_ID==id_ls[i]),"MAP_CRU"] <- sum(data$pn_mon)
  alpha <- site_data[which(site_data$SAMPLE_ID==id_ls[i]),"ALPHA"]
  
  site_data[which(site_data$SAMPLE_ID==id_ls[i]),"MI_converted"] <- delta_alpha_to_MI(data$mi,alpha) # see functions_source
  
  saveRDS(data, file = paste(outputtemppath, id_ls[i],"_mon.RDS",sep=""))
}

write.csv(site_data, paste (dataobspath,"bartlein_converted_",Sys.Date(),".csv",sep=""), row.names = F)
#write.csv(site_data, paste (dataobspath,"bartlein_converted.csv",sep=""), row.names = F)

