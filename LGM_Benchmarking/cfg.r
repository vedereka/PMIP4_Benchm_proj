
source(paste(getwd(),"/packages_downloaded/gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r", sep=""))
sourceAllLibs(paste(getwd(),"/packages_downloaded/gitProjectExtras/gitBasedProjects/R/", sep=""))
sourceAllLibs(paste(getwd(),"/packages_downloaded/benchmarkMetrics/", sep=""))

## uncomment on windows
#source('../gitProjectExtras/package_git2r.r')
#config(repository(), user.name="Douglas Kelley", user.email="douglas.i.kelley@gmail.com")

if(!require("pacman")) install.packages ("pacman")
pacman::p_load (raster,ncdf4,mapdata,plotrix,MASS,mapplots,truncdist,weights)

sourceAllLibs(paste(getwd(),"/LGM_Benchmarking/src/", sep=""))
sourceAllLibs(paste(getwd(),"/LGM_Benchmarking/src/dataProcessing/", sep=""))
sourceAllLibs(paste(getwd(),"/LGM_Benchmarking/src/libs/", sep=""))
sourceAllLibs(paste(getwd(),"/LGM_Benchmarking/src/metrics/", sep=""))
sourceAllLibs(paste(getwd(),"/LGM_Benchmarking/src/plotting/", sep=""))
sourceAllLibs(paste(getwd(),"/LGM_Benchmarking/src/postProcessing/", sep=""))
sourceAllLibs(paste(getwd(),"/LGM_Benchmarking/src/processModelOutputs/", sep=""))
sourceAllLibs(paste(getwd(),"/packages_downloaded/rasterextrafuns/rasterPlotFunctions/R/", sep=""))


# Source functions needed to create the scores in XXXx.R file
#function to get scores
makeComparison <- function(mod) {
  ## finds location of obs in model cells
  xy = xyFromCell(mod, c(1:length(mod))) # it works if using mods[[1]], which is a RasterLayer
  
  xstep = min(abs(diff(xy[,1])))
  ystep = min(abs(diff(xy[,1])))
  idIze <- function(cords) {        
    xid = which(abs(cords['lon'] -  xy[,1]) < ystep/2)
    yid = which(abs(cords['lat'] -  xy[,2]) < ystep/2)
    intersect(xid, yid)[1]
  }
  xyid = apply(obs[,1:2], 1, idIze)
  if (modgrid) xyidU = unique(xyid) else xyidU = xyid
  
  ## pairs obs and simulation
  vmod = values(mod)
  amod = raster::area(mod)
  obsSim <- function(xyidU) {
    out = c(mean(obs[which(xyid == xyidU),3]), vmod[xyidU]) 
    if (modgrid) out = c(out, amod[xyidU]) else out = c(out, 1)
    return(out)
  }
  
  ## runs the comparisons
  obsSims = sapply(xyidU, obsSim) #observation, model and null data (n=number of data points in obs)
  
  # The score function provides 3 measures: 
  # 1) straight NME/NMSE comparisons;
  # 2) comparisons with the influance of the mean removed (i.e testing pattern and variablity);
  # 3) comparisons with the influance of the mean and variance removed (i.e testing the pattern only).
  
  score(NME(obsSims[1,], obsSims[2,], w =  obsSims[3,]))
  
}

# function to extract obs and model data at each site
extractComparison <- function(mod) {
  ## finds location of obs in model cells
  xy = xyFromCell(mod, c(1:length(mod))) # it works if using mods[[1]], which is a RasterLayer
  
  xstep = min(abs(diff(xy[,1])))
  ystep = min(abs(diff(xy[,1])))
  idIze <- function(cords) {        
    xid = which(abs(cords['lon'] -  xy[,1]) < ystep/2)
    yid = which(abs(cords['lat'] -  xy[,2]) < ystep/2)
    intersect(xid, yid)[1]
  }
  xyid = apply(obs[,1:2], 1, idIze)
  if (modgrid) xyidU = unique(xyid) else xyidU = xyid
  
  ## pairs obs and simulation
  vmod = values(mod)
  amod = raster::area(mod)
  obsSim <- function(xyidU) {
    out = c(mean(obs[which(xyid == xyidU),3]), vmod[xyidU]) 
    out = c(out, 1)
    return(out)
  }
  
  ## runs the comparisons
  obsSims = sapply(xyidU, obsSim) #observation, model and null data (n=number of data points in obs)
  return(obsSims)
  
}

# function for model vs obs scatterplot
my_scatterplot <- function (dataplot){
  
  p <- ggplot(data=dataplot, aes(x = dataplot[,1], y = dataplot[,2])) +
    geom_point(alpha = 1,color = "darkred", size = 1.5) +
    geom_smooth(method='lm', show.legend = NA,inherit.aes = TRUE,formula = y ~x, se = TRUE,
                weight=0.5, color = "darkred", size = 0.5) +
    theme_bw()+
    theme(plot.caption = element_text(size=10, vjust = -0.18)) +
    labs(x = "Observations", y = "Model")+
    #labs(caption = paste ("n=",dim(obsSims)[2],sep=" ")) +
    geom_abline(intercept = 0,slope=1, color = "black", linetype="dotted", size=1) +
    stat_cor()     
  
  p <- ggMarginal(p, type = "histogram", fill="grey90", size = 8)
  
  return (p)
}

# function to trim model names
my_name_trim <- function (mod_ls){
  name_trim <- substr(mod_ls, 1, nchar(mod_ls)-17)
  return (name_trim)
}
