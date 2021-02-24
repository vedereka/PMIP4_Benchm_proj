# Extract metadata in original netcdfs
# Created by Laia Comas-Bru on October 2020
# last modified: February 2021
# Script will need to be adjusted when ice mask for CESM2-1 becomes available
# 
##### SET STUFF ################################################################################

# create txt file to save logs
st = format(Sys.time(), "%Y%m%d_%H%M%S")
filename <- paste("output/Models_netcdf_metadata_", st, ".txt", sep = "")
sink(filename, split = TRUE, append = TRUE) # divert all text outputs to a file
paste("Created on ", Sys.Date(), sep = '')


#list models, variables and time-slices (months)
model_ls <-c('AWIESM1','AWIESM2','CCSM4-UofT','CESM1-2','CESM2-1','HadCM3-GLAC1D',
             'HadCM3-ICE6GC','iLOVECLIM1-1-1-GLAC-1D','iLOVECLIM1-1-1-ICE-6G-C',
             'INM-CM4-8','IPSLCM5A2','MIROC-ES2L','MPI-ESM1-2')
var_ls <- c('tas', 'pr', 'clt')
period_sel <- c("LGM", "PI")


for (model in model_ls) {
  print(paste("============================================================"))
  
  for (variab in var_ls) {
    
    if(variab =="clt" & model=="iLOVECLIM1-1-1-ICE-6G-C") next
    if(variab =="clt" & model=="iLOVECLIM1-1-1-GLAC-1D") next
    if(variab =="clt" & model=="CESM2-1") next
    
    # obtain that variable for both time periods with land/ice mask applied (m_LGM and m_PI)
    for (per in period_sel) {
      
      print(paste("Model:", model, ". Var:", variab,". Period:", per,  sep = ' '))
      
      if (per == 'LGM') {
        ncfname <-
          paste(pmip_ncpath,model,'/',model,'_LGM_moclim_',variab,'.nc',sep = '')
        if (model == "CESM2-1") { # noi sftfig in CESM2-1
          ncfname_sftgif <-
            paste(pmip_ncpath, model, '/', model, '_LGM_sftlf.nc', sep = '')
        } else {
          ncfname_sftgif <-
            paste(pmip_ncpath, model, '/', model, '_LGM_sftgif.nc', sep = '')
        }
        ncfname_sftlf <-
          paste(pmip_ncpath, model, '/', model, '_LGM_sftlf.nc', sep = '')
      } else {
        
        ncfname <-paste(pmip_ncpath,model,'/',model,'_PI_moclim_',variab,'.nc',sep = '')
        if (model == "CESM2-1") {
          ncfname_sftgif <-
            paste(pmip_ncpath, model, '/', model, '_PI_sftlf.nc', sep = '')
        } else {
          ncfname_sftgif <-
            paste(pmip_ncpath, model, '/', model, '_PI_sftgif.nc', sep = '')
        }
        ncfname_sftlf <-paste(pmip_ncpath, model, '/', model, '_PI_sftlf.nc', sep = '')
      }
      # open NetCDF files
      ncin <- nc_open(ncfname)
      ncin_sftgif <- nc_open(ncfname_sftgif)
      ncin_sftlf <- nc_open(ncfname_sftlf)
      
      ncatt_get(ncin_sftlf,'sftlf') # variable details
      print(ncin_sftlf) #netcdf details
      if (model == "CESM2-1") {
        print(paste("No sftfig metadata. File does not exist"))
      } else {
        ncatt_get(ncin_sftgif,'sftgif') # variable details
        print(ncin_sftgif) #netcdf details
        
      }
      
      ncatt_get(ncin,variab) # variable details
      print(ncin) #netcdf details

      print(paste("============================================================"))
    }
  }
}

sink()
