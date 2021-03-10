### Read Tierney SST data for LGM
# 
# Created by Gill Thornhill in Feb2021
# Last modified: February 2021
#
#### LOAD ALL DATA AND ARRANGE TO COMMON VARIABLES #### 

ncfname <-
  paste(dataobspath,'Nakamura_last10yrAVERAGE_LGM400.nc',sep = '')

# open NetCDF files
ncin <- nc_open(ncfname)
# Put the netcdf structure into text file for reference
out_filename <- paste(dataobspath, "Nakamura_StructOut.txt", sep = "")
sink(out_filename, split = TRUE, append = FALSE)
print(ncin)


# Get the variable names and the lat lon names
varnames <- attributes(ncin$var)$names
latlon <- attributes(ncin$dim)$names

#print the varnames and lat lon names
print(varnames)
print(latlon)

# get the variable (SSTLGM) and set the dimensions as lat and lon
theta <- (ncvar_get(ncin, "THETA"))
print(paste("Theta has shape",dim(theta)))

#print(theta)
#minVal = min(theta, na.rm = TRUE)
#maxVal = max(theta, na.rm = TRUE)

#print(minVal)
#print(maxVal)
#

#nc_lat <- (ncvar_get(ncin, 'latitude'))
#nc_lon <- (ncvar_get(ncin, 'longitude'))
# 
# # Convert lon to -80 to 180
#nc_lon[nc_lon > 180] <- nc_lon[nc_lon > 180] - 360
# print(paste(nc_lon))
# 

# 
miss_value = 'NaN'
theta_df <- as.data.frame(theta) # na_if works with df
theta_df <- theta_df %>% dplyr::na_if(miss_value)
#dimnames(theta) <- list(lon=nc_lon, lat=nc_lat) 
# 
# 
# plot set up
cairo_pdf(
  paste(plotpath, 'NakamuraOceanplots/THETA_LGM.pdf', sep = ""),width = 11.69,
  height = 8.27, onefile = T)

cols <- (rev(brewer.pal(11, "RdBu")))
title_name = 'Theta'
varunits = 'K'

#colbreaks
colbreaks <- c(seq(from = round(min(theta, na.rm = TRUE)),to = round(max(theta, na.rm = TRUE)),length.out = 11))

print(colbreaks)

p <- plot_mtco_eg_disc(
  mat_withlatlon = theta,
  cols = cols,
  brkpnt = colbreaks,
  title_name,
  varunits = varunits,
  shapefile_df = shapefile_df_180
)
print(p)
dev.off()

#----------------

# ncfname <-
#   paste(dataobspath,'AnnanHargreaves_sat.nc',sep = '')
# 
# # open NetCDF files
# ncinsat <- nc_open(ncfname)
# # # get the variable (SSTLGM) and set the dimensions as lat and lon
# SAT_anom <- (ncvar_get(ncinsat, "SAT"))
# 
# print(ncinsat)
# sink()
# 
# nc_lat <- (ncvar_get(ncinsat, 'latitude'))
# nc_lon <- (ncvar_get(ncinsat, 'longitude'))
# # 
# # # Convert lon to -80 to 180
# nc_lon[nc_lon > 180] <- nc_lon[nc_lon > 180] - 360
# # print(paste(nc_lon))
# # 
# 
# miss_value = 'NaN'
# SATanom_df <- as.data.frame (SAT_anom) # na_if works with df
# SATanom_df <- SSTanom_df %>% dplyr::na_if(miss_value)
# dimnames(SAT_anom) <- list(lon=nc_lon, lat=nc_lat) 
# # 
# # 
# # plot set up
# cairo_pdf(
#   paste(plotpath, 'AnnanHargreavesOceanplots/SAT_ANOM_LGM.pdf', sep = ""),width = 11.69,
#   height = 8.27, onefile = T)
# 
# cols <- (rev(brewer.pal(11, "RdBu")))
# title_name = 'SAT ANOM LGM'
# varunits = 'K'
# 
# #colbreaks
# colbreaks <- c(-40, -20, -12, -8, -4, -2, -1, 0, 1, 2, 3)
# #colbreaks <- c(seq(from = round(min(SAT_anom, na.rm = TRUE)),to = round(max(SAT_anom, na.rm = TRUE)),length.out = 11))
# print(colbreaks)
# 
# p <- plot_mtco_eg_disc(
#   mat_withlatlon = SAT_anom,
#   cols = cols,
#   brkpnt = colbreaks,
#   title_name,
#   varunits = varunits,
#   shapefile_df = shapefile_df_180
# )
# print(p)
# dev.off()
# 
# # 
# # # 
# # fig <- ggarrange(get(paste("plot_SST_ANN", sep="_")),
# # get(paste("plot_SST_JAS", sep="_")),
# # get(paste("plot_SST_JFM", sep="_")),
# # get(paste("plot_ANOM_ANN", sep="_")),
# # get(paste("plot_ANOM_JAS", sep="_")),
# # get(paste("plot_ANOM_JFM", sep="_")),
# # labels = c("A", "B", "C", "D","E", "F"),
# # ncol = 3, nrow = 2)
# # 
# # fig
# # 
# # ggsave(fig,file=paste(plotpath,"MargoOceanplots/","MargoData.jpg", sep = ""),width = 11.69,height = 8.27)
#  
graphics.off()