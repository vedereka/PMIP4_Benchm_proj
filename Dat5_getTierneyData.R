### Read Tierney SST data for LGM
# 
# Created by Gill Thornhill in Feb2021
# Last modified: February 2021
#
#### LOAD ALL DATA AND ARRANGE TO COMMON VARIABLES #### 

ncfname <-
  paste(dataobspath,'/Tierney2020_DA_ocn_regrid.nc',sep = '')

# open NetCDF files
ncin <- nc_open(ncfname)
# Put the netcdf structure into text file for reference
out_filename <- paste(dataobspath, "Tierney2020_DA_ocn_regrid_StructOut.txt", sep = "")
sink(out_filename, split = TRUE, append = TRUE)
print(ncin)
sink()

print(ncin)
# Get the variable names and the lat lon names
varnames <- attributes(ncin$var)$names
latlon <- attributes(ncin$dim)$names

#print the varnames and lat lon names
varnames
latlon

# get the variable (SSTLGM) and set the dimensions as lat and lon
SST_all <- (ncvar_get(ncin, "SSTLGM"))
nc_lat <- (ncvar_get(ncin, 'lat'))
nc_lon <- (ncvar_get(ncin, 'lon'))

# Convert lon to -80 to 180
nc_lon[nc_lon > 180] <- nc_lon[nc_lon > 180] - 360
#print(paste(nc_lon))


miss_value = 'NaN'
SSTall_df <- as.data.frame (SST_all) # na_if works with df
SSTall_df <- SSTall_df %>% dplyr::na_if(miss_value)
dimnames(SST_all) <- list(lon=nc_lon, lat=nc_lat) 


# plot set up
cairo_pdf(
  paste(plotpath, 'TierneyOceanplots/Tierney_SST_LGM.pdf', sep = ""),width = 11.69,
  height = 8.27, onefile = T)

cols <- (rev(brewer.pal(11, "RdBu")))
title_name = 'SST_LGM'
varunits = 'K'

#colbreaks
colbreaks <- c(-3, 0, 3, 6, 9, 12, 15, 18, 20, 24, 27)
#colbreaks <- c(seq(from = min(SST_all, na.rm = TRUE),to = max(SST_all, na.rm = TRUE),length.out = 11))


p <- plot_mtco_eg_disc(
  mat_withlatlon = SST_all,
  cols = cols,
  brkpnt = colbreaks,
  title_name,
  varunits = varunits,
  shapefile_df = shapefile_df_180
)
print(p)
dev.off()

# get the variable (SSTLGM) and set the dimensions as lat and lon
SST_anom <- (ncvar_get(ncin, "deltaSST"))
miss_value = 'NaN'
SSTanom_df <- as.data.frame (SST_anom) # na_if works with df
SSTanom_df <- SSTanom_df %>% dplyr::na_if(miss_value)
dimnames(SST_anom) <- list(lon=nc_lon, lat=nc_lat) 


# plot set up
cairo_pdf(
  paste(plotpath, 'TierneyOceanplots/Tierney_SST_ANOM_LGM.pdf', sep = ""),width = 11.69,
  height = 8.27, onefile = T)

cols <- (rev(brewer.pal(11, "RdBu")))
title_name = 'SST_ANOM_LGM'
varunits = 'K'

#colbreaks
colbreaks <- c(-40, -20, -12, -8, -4, -2, -1, 0, 1, 2, 3)
#colbreaks <- c(seq(from = min(SST_anom, na.rm = TRUE),to = max(SST_anom, na.rm = TRUE),length.out = 11))
print(colbreaks)

p <- plot_mtco_eg_disc(
  mat_withlatlon = SST_anom,
  cols = cols,
  brkpnt = colbreaks,
  title_name,
  varunits = varunits,
  shapefile_df = shapefile_df_180
)
print(p)
dev.off()

# 
# # 
# fig <- ggarrange(get(paste("plot_SST_ANN", sep="_")),
# get(paste("plot_SST_JAS", sep="_")),
# get(paste("plot_SST_JFM", sep="_")),
# get(paste("plot_ANOM_ANN", sep="_")),
# get(paste("plot_ANOM_JAS", sep="_")),
# get(paste("plot_ANOM_JFM", sep="_")),
# labels = c("A", "B", "C", "D","E", "F"),
# ncol = 3, nrow = 2)
# 
# fig
# 
# ggsave(fig,file=paste(plotpath,"MargoOceanplots/","MargoData.jpg", sep = ""),width = 11.69,height = 8.27)
 
graphics.off()