# Script with all the functions needed in the PMIP4 benchmarking project. 
# It must be called by init.R
# Created by Laia Comas-Bru in October 2020
# last modified: February 2021
# This script includes interpolate_spline_conserve_mean.R by Author: Kamolphat Atsawawaranunt

#### function to supressing automatic output from cat(). Use it as: y <- quiet(FUNCTION) ####
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 
#### function for converting gregorian days to julian calendar  #### 
### FROM: https://rdrr.io/rforge/ncdf.tools/src/R/convertDateNcdf2R.R ###

convertDateNcdf2R  =  function(
  ##title<< Convert netCDF time vector to POSIXct R date object
  time.source ##<< numeric vector or netCDF connection: either a number of time units since
  ##   origin or a netCDF file connection, In the latter case, the time 
  ##   vector is extracted from the netCDF file, This file, and especially the 
  ##   time variable, has to follow the CF netCDF conventions.
  , units = 'days' ##<< character string: units of the time source. If the source
  ##   is a netCDF file, this value is ignored and is read from that file.
  , origin = as.POSIXct('1800-01-01', tz = 'UTC') ##<< POSIXct object:
  ##   Origin or day/hour zero of the time source. If the source
  ##   is a netCDF file, this value is ignored and is read from that file.
  , time.format =  c('%Y-%m-%d', '%Y-%m-%d %H:%M:%S', '%Y-%m-%d %H:%M', '%Y-%m-%d %Z %H:%M', '%Y-%m-%d %Z %H:%M:%S')
)
##description<< This function converts a time vector from a netCDF file or a vector of Julian days (or seconds, minutes, hours)
##              since a specified origin into a POSIXct R vector.
{
  close.file =  FALSE
  if (class(time.source) ==  'character') {
    if (file.exists(time.source)) {
      time.source = open.nc(time.source)
    } else {
      stop(paste('File ', time.source, ' is not existent!', sep = ''))
    }
  }
  if (class(time.source) == 'NetCDF') {
    attget.result <- try({
      units.file      <- infoNcdfAtts(time.source, 'time')[, 'value'][infoNcdfAtts(time.source, 'time')[, 'name'] == 'units']
      origin.char     <- sub('^.*since ', '', units.file)
      units <-  sub(' since.*', '', units.file)
    }, silent = TRUE)
    for (formatT in time.format) {
      origin <- strptime(origin.char, format = formatT,  tz =  'UTC')
      if (!is.na(origin))
        break
    }
    if (is.na(origin))
      stop('Not possible to determine origin. Wrong format supplied?')
    
    date.vec     <- as.numeric(var.get.nc(time.source, 'time')) 
  } else {
    if (!is.numeric(time.source))
      stop('time.source needs to be numeric if not a netCDF file connection!')
    date.vec  <- time.source
  }
  
  
  if (!is.element(units, c('seconds', 'minutes', 'hours', 'days')))
    stop(paste('Unit ', units, ' is not implemented.', sep  =  ''))
  multiplicator      <- switch(units, days = 60 * 60 * 24, hours = 60 * 60, minutes = 60, seconds = 1)
  time.out <- origin + date.vec * multiplicator
  if (origin <  as.POSIXct('1582-10-30', tz = 'UTC')) 
    time.out <- time.out + 10 * 24 * 60 * 60
  if (close.file)
    close.nc(time.source)
  ##value<<
  ## POSIXct vector: time vector in native R format
  return(time.out)
}

#### Read in shape file for plotting #### 
options("rgdal_show_exportToProj4_warnings"="none") #-> to mute the text output'
library(rgdal)

quiet(shapefile <- readOGR(dsn=paste(getwd(), "/input_coast_shapefile/ne_110m_land/ne_110m_land.shp", sep=""), layer = 'ne_110m_land')) #laptop
shapefile_df_360 <- fortify(shapefile)

# #arrange longitudes to be from 0 to 360 instead of -180 to 180
index <- shapefile_df_360$long <0
shapefile_df_360$long[index] <- (shapefile_df_360$long[index] +360)
rm(index)
rm(shapefile)

quiet(shapefile <- readOGR(dsn=paste(getwd(), "/input_coast_shapefile/ne_110m_land_360/ne_110m_land.shp", sep=""), layer = 'ne_110m_land')) #laptop
shapefile_df_180 <- fortify(shapefile)

rm(shapefile)

mapWorld_grey <- geom_polygon(data= shapefile_df_180, aes(x = long, y = lat, group = group), fill = 'grey87', colour = 'grey22', size = 0.05, alpha=0.7)

#### function for plotting maps #### 

plot_mtco_eg_disc <- function(mat_withlatlon, cols, brkpnt, title_name, varunits, shapefile_df){
  mat <- mat_withlatlon
  startpnt <- brkpnt[1:length(brkpnt)-1]
  endpnt <- brkpnt[2:length(brkpnt)]
    brk_lab <- paste(signif(startpnt, digits = 3), signif(endpnt, digits = 3), sep = ' to ')
    mapWorld <- geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group), fill = NA, colour = 'black', size = 0.05)
  dt <- melt(mat)
  colnames(dt) <- c('lon', 'lat', 'val')
  dt <- subset(dt, !is.na(val))
  dt$breaks <- cut(dt$val, breaks = brkpnt,
                   labels = brk_lab)
  mp <- ggplot() +
    geom_tile(data = dt, aes(x=lon, y=lat, colour = breaks, fill = breaks)) +
    coord_fixed(xlim = c(-180, 180), ylim = c(-90,90), expand = F) +
    theme_bw() +
    xlab('Longitude [deg]') +
    ylab('Latitude [deg]') +
    # scale_fill_manual(name = varunits,
    #                   breaks = brk_lab[brk_lab %in% sort(unique(dt$breaks))],
    #                   values = cols[brk_lab %in% sort(unique(dt$breaks))]) +
    scale_fill_manual(name = varunits,
                      breaks = brk_lab[brk_lab %in% sort(unique(dt$breaks))],
                      values = cols[brk_lab %in% sort(unique(dt$breaks))]) +
    scale_colour_manual(name = varunits,
                        breaks = brk_lab[brk_lab %in% sort(unique(dt$breaks))],
                        values = cols[brk_lab %in% sort(unique(dt$breaks))]) +
    theme(legend.position = 'left',
          legend.key.width = unit(0.5, 'cm')
    ) + ggtitle(paste(title_name, ' min/max = ', round(min(mat, na.rm = T), 2), "/", round(max(mat, na.rm = T), 2), sep = '')) + mapWorld
  return(mp)
}


#### Spline interpolation with conservation of mean ####

# interpolate_spline_conserve_mean.R
# Author: Kamolphat Atsawawaranunt
# This file contains a snippet of code which I have used for mean-preserving
# spline interpolation. The user may alter this in whichever way they want to
# suit their own need.
#
# The idea of how this was done was taken from:
#   https://stats.stackexchange.com/questions/59418/interpolation-of-influenza-data-that-conserves-weekly-mean
#   which follows methods described in:
#     Harzallah, A., 1995. The interpolation of data series using a constrained iterating technique. Monthly weather review, 123(7), pp.2251-2254.
#     DOI/URL: https://doi.org/10.1175/1520-0493(1995)123%3C2251:TIODSU%3E2.0.CO;2

# The function follows an iterative process:
#   1. Interpolate the data (in this case, using spline interpolation)
#   2. Calculate the mean of each of the time periods defined
#   3. Calculate the differences between the means of the timeperiods (from 2.) with the previous time period (original or the previous iteration)
#   4. If the differences between the means (step 3.) is more than the threshold value:
#       i. store the interpolated values.
#       ii. store the residuals, and interpolate the residuals (go back to step 1., and repeat)
#   5. If the differences between the means (step 4.) is less than the threshold value; or the maximum number of iterations are met:
#       i. store the interpolated values
#       ii. sum up the interpolated values, and return these values.
# 
# The fucntion describe at https://stats.stackexchange.com/questions/59418/interpolation-of-influenza-data-that-conserves-weekly-mean
# follows the same logic, but only works with equally spaced sampled data (i.e. weekly)
# and therefore was edited.

# Define functions
interpol_spline_cons_mean <- function(y_points, month_len, max_iter, tol){
  # Interpolate monthly timeseries data to daily.
  #
  # Args:
  # y_points: iterable(list or array), mean values at each timestep
  # month_len: iterable(list or array), number of days at which each timestep represents
  # max_iter: integer, maximum iterations if convegence is never met
  # tol: numeric, the tolerance threshold for indicating convergence
  #
  # Returns:
  # An array of interpolated values as the same length as the total sum of month_len
  #
  #
  # Example:
  #   y_points = c(12, 13, 14, 29, 32, 35, 33, 24, 18, 10, 8, 7) # data
  #   month_len = c(31, 28 ,31, 30, 31, 30, 31, 31, 30, 31, 30, 31) # month length the data represents
  #   y_interpolated = interpol_spline_cons_mean(y_points, month_len, 100, 0.01) # interpolate
  #   unlist(lapply(unname(split(y_interpolated, rep(1:length(month_len), month_len))), mean)) # see whether the means are the same with the threshold defined
  #
  # Check if month_len and y_points have the same dimension
  if (length(month_len) != length(y_points)){
    print('Lengths of month_len and y_points are not the same.')
  } else {
    x_points <- (cumsum(month_len) + 1) - ((month_len + 1)/2)
    y_points_mat <- matrix(NA, ncol = length(y_points), nrow = max_iter+1)
    # input starting value as x_points
    y_points_mat[1,] <- y_points
    # create cumulative number of days from start from month_len
    max_x <- max(cumsum(month_len))
    # create an array of the interpolated points
    y_points_interp_mat <- matrix(NA, ncol = max_x, nrow = max_iter)
    x_points_out <- 1:max_x
    for (i in 1:max_iter){
      splineoutpt <- spline(x_points, y_points_mat[i,], method = 'fmm', xout = x_points_out)
      y_points_interp_mat[i,] <- splineoutpt$y
      new_mean <- unlist(lapply(unname(split(splineoutpt$y, rep(1:length(month_len), month_len))), mean))
      resid <- y_points_mat[i,] - new_mean
      if (max(resid) < tol){
        cat("Converged after", i, "iterations with tolerance of", tol, sep=" ")
        break
      }
      y_points_mat[i+1,] =resid
    }
    return(colSums(y_points_interp_mat, na.rm = T))
  }
}

#### define function to find the closest lat/lon values ####
FindGridCoords <- function (myLat, myLon, data){
  # data has at least two columns $lat and $lon
  outCoords <- data[which(data$lat==myLat)[which(data$lat==myLat) %in% which(data$lon==myLon)],]
  return (outCoords)
}

#### function opposite to "%in%" to be used in fi lters ####
`%notin%` <- Negate(`%in%`)


##### create function to grid datasets ####
# input: 
# - data file with lat and lon
# - grid data file with cells lat/lon ranges as columns
# output:
# - cbind of input grid with count(n) column and averaged observations
obs_data_to_grid <- function(grid, data) { 
  
  for (n in 1:dim(grid)[1]) {
    newx  <- data %>% filter (lat >= grid$lat_min [n] & lat < grid$lat_max[n] &
                                lon >= grid$lon_min[n] & lon < grid$lon_max[n])
    
    if (dim(newx)[1] == 0) {
      grid$count_n[n] = NA
    } else {
      grid$count_n[n] <- dim(newx)[1] # how many data points per gridcell?
    }
    if (n == 1) {
      x_temp <- newx [, 3:8] %>% summarise_if(is.numeric, mean, na.rm = T)
    } else {
      x_temp[n,] <- newx [, 3:8] %>% summarise_if(is.numeric, mean, na.rm = T)
    }
  }
  
  return (cbind (grid, x_temp))
  
} 

##### define function to find the closest lat/lon values #####
FindGridCoords <- function (myLat, myLon, data){
  # data has at least two columns $lat and $lon
  outCoords <- data[which(data$lat==myLat)[which(data$lat==myLat) %in% which(data$lon==myLon)],]
  return (outCoords)
}


##### function to trim model names #####
trim_mode_name = function (x) {
    name_trim <- substr(x, 1, nchar(x) - 5)
    return (name_trim)
}

my_name_trim = function (x) { # used in DM3
  name_trim <- substr(x, 1, nchar(x) - 17)
  return (name_trim)
}

##### matrixplot function for the score plotting (DM4 script)#####
matrixplot <- function (st) {
  
  ramp = c("darkolivegreen4", "palegreen1","lemonchiffon1", "sandybrown", "white")
  
  bp <- ggplot(data = data %>% filter (data$step == st), aes(x=model, y=var, fill= factor(z_val)))  +
    geom_tile(width=1, height = 0.97, color="black", show.legend = T) +
    scale_fill_manual(name = element_blank(),
                      labels = c("25% better than mean", "better than mean",
                                 "worse than mean but better than random", "worse than random", "no significant improvement"),
                      limits = factor(c(1,2,3,4,5)),
                      values = ramp,
                      drop = F) +
    guides(fill=guide_legend(nrow=3, byrow=F, x.intersp = 3, label.hjust =0, label.vjust = 0.5)) +
    
    geom_text(aes(label=paste(val,"\n [",min,"-",max,"]",sep=""),
                  color = "black"), size=4) +
    guides(color = FALSE) +
    scale_color_manual(values =c("black")) +
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 0.40, hjust=0.5,size=12,face="bold"),
          axis.text.y = element_text(angle = 0, vjust = 0.25, hjust=0.5,size=12,face="bold"),
          legend.position="top",
          legend.text = element_text(size=13),
          plot.caption = element_text(size=12, vjust = 2)) +
    labs(caption = paste ("Step:", st,sep=""))
  
  return(bp)
}

##### function to convert moisture from alpha to MI units #####
# MI=MAP/equilibrium evapotranspiration
# alpha = actual/equilibrium evapotranspiration

# actual evapotranspiration is given by:
# Ea = Eq [1 + m - (1 + mω)1/ω]   where ω = 3
# alpha = 1+m - (1 + mω)1/ω
# if we re-express the budyko formulation: 
# (1+mi**3)**(1/3) = z
# then α = 1 + mi - z
# dm/dα = (z/z-mi)2
# Δm = Δα dm/dα

delta_alpha_to_MI <- function (mi, alpha) {
  z <- (1+mi^3)^(1/3)
  MI <- alpha * (z^2/(z^2-mi^2))
  #MI = alpha * (z/(mi-z))^2 # incorrect (Sandy's)
  #Δm = Δα dm/dα
  return (round(MI,3))
}