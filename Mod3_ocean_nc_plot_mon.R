# Load PMIP4 model data, fix inconsistencies, apply land/ice masks, plot monthly
# data and save RDS data file to calculate anomalies in Mod2 script
# Created by Laia Comas-Bru in June 2020
# Last modified: February 2021

# To-do list:
# - Merge plots so that 1 file has the 12 months in pages (instead of 1 month =
# 1 file). Plots are produced in a loop so this isn't straightforward'

# create txt file to save logs
st = format(Sys.time(), "%Y%m%d_%H%M%S")
filename <- paste("output/", st, "_models_metadata.txt", sep = "")
sink(filename, split = TRUE, append = TRUE) # divert all text outputs to a file
paste("Model metadata. Created on ", Sys.Date(), sep = '')


#list models, variables and time-slices (months)
#model_ls <-c('AWIESM1','AWIESM2','CCSM4-UofT','CESM1-2','CESM2-1','HadCM3-GLAC1D',
            # 'HadCM3-ICE6GC','iLOVECLIM1-1-1-GLAC-1D','iLOVECLIM1-1-1-ICE-6G-C',
            # 'INM-CM4-8','IPSLCM5A2','MIROC-ES2L','MPI-ESM1-2')

model_ls <-c('MIROC-ES2L')
var_ls <- c('tas', 'pr', 'clt')
mon_ls <- seq(1,12,1)
period_sel <- c("LGM", "PI")
#period_sel <- c("LGM")
#variant <- 'r1i1p1f2'
#yr_range <- '320001-329912'

  for (model in model_ls) {
    print(paste(
      "============================================================"))
    print(paste("Model used:", model, sep = ' '))

    for (variab in var_ls) {
      print(paste("Variable:", variab, sep = ' '))
      
      # #iLOVECLIM and CESM2-1 have no clt data -> ignore
      if(variab =="clt" & model=="iLOVECLIM1-1-1-ICE-6G-C") next
      if(variab =="clt" & model=="iLOVECLIM1-1-1-GLAC-1D") next
      if(variab =="clt" & model=="CESM2-1") next
      
      # obtain that variable for both time periods with land/ice mask applied (m_LGM and m_PI)
      for (per in period_sel) {
        print(per)
        if (per == 'LGM') {
          ncfname <-
            paste(pmip_ncpath,model,'/',model,'_LGM_moclim_',variab,'.nc',sep = '')
          ncfname_sftlf <-
            paste(pmip_ncpath, model, '/', model, '_LGM_sftlf.nc', sep = '')
          
          
          if (model == "CESM2-1") {
            ncfname_sftgif <-
              paste(pmip_ncpath, 'CCSM4-UofT/CCSM4-UofT_LGM_sftgif.nc', sep = '')
          }
          else {
            ncfname_sftgif <-
              paste(pmip_ncpath, model, '/', model, '_LGM_sftgif.nc', sep = '')
          }
          
        } else {
          ncfname <-
            paste(pmip_ncpath,model,'/',model,'_PI_moclim_',variab,'.nc',sep = '')
          ncfname_sftlf <-
            paste(pmip_ncpath, model, '/', model, '_PI_sftlf.nc', sep = '')
         
           if (model == "CESM2-1") {
            ncfname_sftgif <-
              paste(pmip_ncpath, 'CCSM4-UofT/CCSM4-UofT_PI_sftgif.nc', sep = '')
          }
          else {
            ncfname_sftgif <-
              paste(pmip_ncpath, model, '/', model, '_PI_sftgif.nc', sep = '')
          }
        }
        
      # open NetCDF files
      ncin <- nc_open(ncfname)
      ncin_sftgif <- nc_open(ncfname_sftgif)
      ncin_sftlf <- nc_open(ncfname_sftlf)
      
      # ncatt_get(ncin_sftlf,'sftlf') # variable details
      # print(ncin_sftlf) #netcdf details
      #
      # ncatt_get(ncin_sftgif,'sftgif') # variable details
      # print(ncin_sftgif) #netcdf details
      #
      ## sftlf: Point-value Landmask, % values are 0 (not land) or 1 (land). Does not include floating ice
      ## sftgif: Point-value Icemask, % values are 0 (not ice) or 1 (ice). Floating ice is included
      
      sftlf <- ncvar_get(ncin_sftlf, "sftlf") 
      
      if (length(dim(sftlf)) == 3){ # CESM models have a third dimension (???)
        sftlf <- sftlf [,,1]
      }

      sftgif <- ncvar_get(ncin_sftgif, "sftgif")
      
      #print(sftgif)
      
      # Some model outputs are ratios instead of percentage
      # Decide what constitutes an ocean cell (< 20% land?)
   
      # Land area fraction inludes the ice areas, so can exclude this from the ocean squares
      
     #  # This keeps the areas which are not ice (whether % or fraction)
     if (max(sftgif,na.rm=T)==100){
         sftgif[sftgif > 20] <- NA
         sftgif[sftgif <= 20] <- 1 ## keep if no ice
     } else {
         sftgif[sftgif > 0.2] <- NA # keep if less than 80% ice
         sftgif[sftgif <= 0.2] <- 1
    }
      
      
      # This keeps squares that are < 20% land, as ocean squares
      if (max(sftlf,na.rm=T)==100){
        sftlf[sftlf > 20] <- NA
        sftlf[sftlf <= 20] <- 1 # keep less than 20% land = ocean
      } else {
        sftlf[sftlf > 0.2] <- NA
        sftlf[sftlf <= 0.2] <- 1 # keep less than 20% land = ocean
      }
      
      
      # adjust longitudes so that they're -180 to 180 and ensure they're right
      lon_names <-as.numeric(ncin_sftlf[["dim"]][["lon"]][["vals"]])
      # lons should all be from -180 to 180. They need revision if 0-360.
      if (max(lon_names) > 181) {
        index <- lon_names > 180; lon_names[index] <- lon_names[index] - 360  
      }
      rownames(sftlf) <- lon_names
      sftlf <- sftlf[order(as.numeric(row.names(sftlf))),]
      colnames(sftlf) <- ncin_sftlf[["dim"]][["lat"]][["vals"]]

      # This is for CESM only
      #glacier mask without lat/lon values in CESM, using those of the land mask
      if (is.null(ncin_sftgif[["dim"]][["lon"]][["vals"]])) {
        lon_names <- as.numeric(ncin_sftlf[["dim"]][["lon"]][["vals"]])
      } else {
        lon_names <- as.numeric(ncin_sftgif[["dim"]][["lon"]][["vals"]])
      }
      
      # lons should all be from -180 to 180. They need revision if 0-360.
      if (max(lon_names) > 181) {
        index <- lon_names > 180; lon_names[index] <- lon_names[index] - 360  
      }
      rownames(sftgif) <- lon_names
      sftgif <- sftgif[order(as.numeric(row.names(sftgif))),]
      
      # This is for CESM only
      #glacier mask without lat/lon values in CESM, using those of the land mask
      if (!is.null(ncin_sftgif[["dim"]][["lat"]][["vals"]])){
        colnames(sftgif) <- ncin_sftgif[["dim"]][["lat"]][["vals"]]
      } else {
        colnames(sftgif) <- ncin_sftlf [["dim"]][["lat"]][["vals"]]
      }
      
      # Here we need to select only the ocean data -
      # The sftgif is ice fraction, sftlf is land fraction
      # But this should leave us only the ocean squares now
      land_mask <- sftgif * sftlf # use this to multiply it by the variable and remove ocean gridcells
      
      land_mask <- land_mask[,order(-as.numeric(colnames(land_mask)))] %>% as.matrix()
      #print(land_mask)
      print('val')
      
#--------Plot the land mask ------------------
      # colbreaks <- c(0, 1)
      # var_title = 'Ocean mask'
      # varunits = "none"
      # cairo_pdf(
      #   paste(plotpath, '/LGM_Landmask_', model,per, '_landmask.pdf', sep = ""),width = 11.69,
      #   height = 8.27, onefile = T)

      # p <- plot_mtco_eg_disc(
      #   mat_withlatlon = land_mask,
      #   cols = cols,
      #   brkpnt = colbreaks,
      #   title_name = var_title,
      #   varunits = varunits,
      #   shapefile_df = shapefile_df_180
      # )
      # print(p)
      # dev.off()

# -------------------------------------------------------      
      if (is.null(ncin$dim$axis_3$len)) {
        targetSize <-c(ncin[["dim"]][["lon"]][["len"]], ncin[["dim"]][["lat"]][["len"]])#lon*lat
      } else{
        targetSize <- c(ncin$dim$axis_3$len, ncin$dim$axis_2$len) #lon*lat
      }
      
      # get variable
      if (per == 'LGM') {
        m_LGM <- ncvar_get(ncin, variab)
        ncin_LGM <- ncin
      } else {
        m_PI <- ncvar_get(ncin, variab)
        ncin_PI <- ncin
      }
      
      
      } 

      for (mon in mon_ls) {
        m_mon_LGM <- m_LGM[, , mon]
        m_mon_PI <- m_PI[, , mon]
        
        miss_value = ncin_LGM[["var"]][[variab]][["missval"]]
        m_mon_LGM <- as.data.frame (m_mon_LGM) # na_if works with df
        m_mon_LGM <- m_mon_LGM %>% dplyr::na_if(miss_value)
        
        miss_value = ncin_PI[["var"]][[variab]][["missval"]]
        m_mon_PI <- as.data.frame (m_mon_PI) # na_if works with df
        m_mon_PI <- m_mon_PI %>% dplyr::na_if(miss_value)
        
        #add column (lat) and row names (lon)
        colnames(m_mon_LGM) <- ncin_LGM[["dim"]][["lat"]][["vals"]]
        colnames(m_mon_PI) <- ncin_PI[["dim"]][["lat"]][["vals"]]
        
        # make sure lat order is correct in all models
        m_mon_LGM <- m_mon_LGM[,order(-as.numeric(names(m_mon_LGM)))] %>% as.matrix(.)
        m_mon_PI <- m_mon_PI[,order(-as.numeric(names(m_mon_PI)))] %>% as.matrix(.)
        
        #adjust lon names to -180 to 180 and rearrange matrix for plotting
        lon_names <-as.data.frame (ncin_LGM[["dim"]][["lon"]][["vals"]])
        index <- lon_names > 180
        lon_names[index, 1] <- (lon_names[index] - 360)
        colnames(lon_names) <- "lon_180"
        rownames(m_mon_LGM) <- as.array(lon_names$lon_180)
        m_mon_LGM <- m_mon_LGM[order(as.numeric(row.names(m_mon_LGM))),]
        
        lon_names <-as.data.frame (ncin_PI[["dim"]][["lon"]][["vals"]])
        index <- lon_names > 180
        lon_names[index, 1] <- (lon_names[index] - 360)
        colnames(lon_names) <- "lon_180"
        rownames(m_mon_PI) <- as.array(lon_names$lon_180)
        m_mon_PI <- m_mon_PI[order(as.numeric(row.names(m_mon_PI))),]
        
        #apply land_mask
        m_mon_LGM <- as.matrix(m_mon_LGM)
        x_df <- m_mon_LGM * land_mask
        x_df <- fortify (as.data.frame(x_df))
        m_mon_LGM_df <- as.matrix(x_df)
        
        m_mon_PI <- as.matrix(m_mon_PI)
        x_df <- m_mon_PI * land_mask
        x_df <- fortify (as.data.frame(x_df))
        m_mon_PI_df <- as.matrix(x_df)
        
        # create anomalies
        m_mon_anom <- m_mon_LGM_df- m_mon_PI_df
        
        # print(paste('var: ',variab,' / month: ',mon,' ->  min = ',min(anom_df, na.rm = TRUE),
        #     ' / max = ',max(anom_df, na.rm = TRUE),sep = ""))
        
        # plot on a map (3 maps: PI, LGM and anom)


        cols <- (rev(brewer.pal(11, "RdBu")))
        varunits <- paste (variab)
        
        
        # MAP 1: ANOMALIES
        cairo_pdf(
          paste(plotpath, 'mod_anom_maps/Ocean_LGM_PI_Anom_', model, '_', variab, '_', mon, '.pdf', sep = ""),width = 11.69,
          height = 8.27, onefile = T)

        var_title <-paste("LGM_PI_anom. Model: ",model,". Variable: ",variab,". Month: ",mon,".",sep = "")
        
        #colbreaks
        if (variab == 'pre') {
          colbreaks <- c(seq(from = max(m_mon_anom, na.rm = TRUE),to = min(m_mon_anom, na.rm = TRUE),length.out = 11))
        } else {
          colbreaks <- c(seq(from = min(m_mon_anom, na.rm = TRUE),to = max(m_mon_anom, na.rm = TRUE),length.out = 11
          ))
        }
        
        p <- plot_mtco_eg_disc(
          mat_withlatlon = m_mon_anom,
          cols = cols,
          brkpnt = colbreaks,
          title_name = var_title,
          varunits = varunits,
          shapefile_df = shapefile_df_180
        )
        print(p)
        dev.off()
        
        # MAP 2: LGM
        cairo_pdf(
          paste(plotpath, 'mod_LGM_maps/Ocean_LGM_data_', model, '_', variab, '_', mon, '.pdf', sep = ""),width = 11.69,
        height = 8.27, onefile = T)

        var_title <-paste("LGM_data. Model: ",model,". Variable: ",variab,". Month: ",mon,".",sep = "")

        #colbreaks
        if (variab == 'pre') {
          colbreaks <- c(seq(from = max(m_mon_LGM_df, na.rm = TRUE),to = min(m_mon_LGM_df, na.rm = TRUE),length.out = 11))
        } else {
          colbreaks <- c(seq(from = min(m_mon_LGM_df, na.rm = TRUE),to = max(m_mon_LGM_df, na.rm = TRUE),length.out = 11
          ))
        }

        p <- plot_mtco_eg_disc(
          mat_withlatlon = m_mon_LGM_df,
          cols = cols,
          brkpnt = colbreaks,
          title_name = var_title,
          varunits = varunits,
          shapefile_df = shapefile_df_180
        )
        print(p)
        dev.off()

        # MAP 3: PI
        cairo_pdf(
          paste(plotpath, 'mod_PI_maps/Ocean_PI_data_', model, '_', variab, '_', mon, '.pdf', sep = ""),width = 11.69,
          height = 8.27, onefile = T)

        var_title <-paste("PI_data. Model: ",model,". Variable: ",variab,". Month: ",mon,".",sep = "")

        #colbreaks
        if (variab == 'pre') {
          colbreaks <- c(seq(from = max(m_mon_PI_df, na.rm = TRUE),to = min(m_mon_PI_df, na.rm = TRUE),length.out = 11))
        } else {
          colbreaks <- c(seq(from = min(m_mon_PI_df, na.rm = TRUE),to = max(m_mon_PI_df, na.rm = TRUE),length.out = 11
          ))
        }

        p <- plot_mtco_eg_disc(
          mat_withlatlon = m_mon_PI_df,
          cols = cols,
          brkpnt = colbreaks,
          title_name = var_title,
          varunits = varunits,
          shapefile_df = shapefile_df_180
        )
        print(p)
        dev.off()

        # save anomaly files that month/model/variable
        saveRDS(m_mon_anom, file = paste(rdspath,model,"_",variab,"_anom_",mon,".RDS", sep=""))
        saveRDS(m_mon_PI_df, file = paste(rdspath,model,"_",variab,"_PI_",mon,".RDS", sep=""))
        saveRDS(m_mon_LGM_df, file = paste(rdspath,model,"_",variab,"_LGM_",mon,".RDS", sep=""))

        # save lat/lon values for that model (just once per model)

        if (variab =="tas" & (mon==1)) {
        lon <- as.numeric(row.names(m_mon_anom))
        lat <- as.numeric(colnames(m_mon_anom))
        saveRDS(lat, file = paste(rdspath,model,"_lat.RDS", sep=""))
        saveRDS(lon, file = paste(rdspath,model,"_lon.RDS", sep=""))
        }
      }
    }
  }
graphics.off()
sink()

# Short lines to plot the map arrays for checking purposes:
# x_trial <- m_mon_anom #land_mask
# cutpts <-  c(seq(from = min(x_trial, na.rm=TRUE), to = max(x_trial, na.rm=TRUE),length.out =10))
# lat <- ncin_LGM[["dim"]][["lat"]][["vals"]]
# lon <- ncin_LGM[["dim"]][["lon"]][["vals"]]
# grid <- expand.grid(lon=lon, lat=lat)
# levelplot(x_trial ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, col.regions=(rev(brewer.pal(10,"RdBu"))), main="testing array")

