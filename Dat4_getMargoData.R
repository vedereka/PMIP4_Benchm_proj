### compare Cleator versions
# cleator229: Pre-acceptance version (https://doi.org/10.17864/1947.229)
# cleator244: Peer-reviewed version (https://doi.org/10.17864/1947.244)
# Created by Laia Comas-Bru in December 2020
# Last modified: February 2021
# Note: Africa plots commented (not needed)

#### LOAD ALL DATA AND ARRANGE TO COMMON VARIABLES #### 

margoALL <- read.csv(paste (dataobspath,"margodatagridded_edit.csv",sep="")) %>% 
 # dplyr::select(LAT,LON,MAT,MTCO,MTWA,MAP,MI_converted,GDD5,REF) %>% 
 # dplyr::rename (MI = MI_converted)
  

# merge the datasets together
#data_obs_raw <- rbind(bart_wf, bart_wof, clea_all_229, clea_all_244, prent_all) %>% 
  `colnames<-`(c("lat", "lon", "MAT", "MTCO", "MTWA", "MAP","MI", "GDD5", "REF"))

#write.csv(data_obs_raw, paste(dataobspath, "data_obs_raw.csv", sep=""))



#grid <- expand.grid(lon = lon, lat = lat)
#grid$lat_min <- grid$lat - mean(diff(lat)) / 2
#grid$lat_max <- grid$lat + mean(diff(lat)) / 2
#grid$lon_min <- grid$lon - mean(diff(lon)) / 2
#grid$lon_max <- grid$lon + mean(diff(lon)) / 2
#grid$count_n <- NA

#### CREATE GRIDDED DATASETS ####
#requirement: user-defined "data_obs_raw" function (see in functions_source.R)

# Bwofilt -----------------------------------------------------------------------
data <- data_obs_raw %>%  filter (REF == "B_wof")
B_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "B_wof")

# Bwofilt + PR_all --------------------------------------------------------------
data <- data_obs_raw %>%  filter (REF == "B_wof" | REF == "PR_all")
BP_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "BP_wof")

# Bwfilt -----------------------------------------------------------------------
data <- data_obs_raw %>%  filter (REF == "B_wf")
B_wf <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "B_wf")

# Bwfilt + PR_all --------------------------------------------------------------
data <- data_obs_raw %>%  filter (REF == "B_wf" | REF == "PR_all")
BP_wf <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "BP_wf")

#### OVERLAP GRIDDED DATASETS WITH CLEATOR ####

# cleator 229
data <- data_obs_raw %>%  filter (REF == "CL_all_229")


B_CL_229_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
  mutate (REF = "CL_wof") %>%  rbind(., B_wof) %>% mutate (REF = as.factor (REF))
# B_CL_wof %>%  write.csv(., file.path(dataobspath, "B_CL_wof.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid")



#BP_CL_244_wof <- obs_data_to_grid (grid, data) %>% filter (count_n >= 0) %>% 
#  mutate (REF = "CL_wof") %>%  rbind(., BP_wof) %>% mutate (REF = as.factor (REF))
# BP_CL_wof %>%  write.csv(., file.path(dataobspath, "BP_CL_wof.csv"),  na = "NA",  row.names = FALSE)

rm(ls="grid")



###--------------------------------------------------------------------------------
# save variables

#CL_all <- clea_all_244

#a <- list("B_nc","B_wf","B_wof","BP_wf", "BP_wof",
          "B_CL_229_wf","B_CL_229_wof","BP_CL_229_wf", "BP_CL_229_wof",
          "B_CL_244_wf","B_CL_244_wof","BP_CL_244_wf", "BP_CL_244_wof",
          "CL_all_244","CL_all_229","data_obs_raw")
save(a, file = paste(dataobspath,"obs_data_CL2versions.RData", sep=""))
save.image() # creating ".RData" in current working directory
unlink("obs_data_CL2versions.RData")

rm(ls="x", "ncin", "ncfname", "variab_name","a")

###--------------------------------------------------------------------------------
#var_ls <- c("MAT","MTCO","MTWA","MAP","MI","GDD5")

#b <- list("B_CL_wf","B_CL_wof", "BP_CL_wf", "BP_CL_wof")


# scatterplot

p <- ggplot(na.omit(data_merge), aes(x = val_BP, y = val_CL)) +
  geom_point(alpha = 1,color = "darkred", size = 2) +
  geom_smooth(method='lm', show.legend = NA,inherit.aes = TRUE,formula = y ~x, se = TRUE,
              weight=0.5, color = "darkred", size = 0.5) +
  theme_bw()+
  theme(plot.caption = element_text(size=10, vjust = -0.18)) +
  labs(title = paste ("",variabl,".",sep=""),
       x = refdata1,
       y = refdata2)+
  labs(caption = paste ("n=",dim(data_merge)[1],sep=" ")) +
  geom_abline(intercept = 0,slope=1, color = "black", linetype="dotted", size=1) +
  stat_cor()     

assign(paste("plot",variabl,j,sep="_"),p)

rm(ls="p","dBP_gat","dCL_gat","data","data_merge", "refdata1", "refdata2")



fig <- ggarrange(get(paste("plot_MAT",j, sep="_")),
                 get(paste("plot_MTCO",j, sep="_")),
                 get(paste("plot_MTWA",j, sep="_")),
                 get(paste("plot_MAP",j, sep="_")),
                 get(paste("plot_MI",j, sep="_")),
                 get(paste("plot_GDD5",j, sep="_")),
                 labels = c("A", "B", "C", "D","E", "F"),
                 ncol = 3, nrow = 2)

fig

ggsave(fig,file=paste(plotpath,"dat_CL_versions/", j,"_229_scatterplots.jpg", sep = ""),width = 11.69,height = 8.27)
#ggsave(fig,file=paste(plotpath,"", j,"_229_scatterplots.pdf", sep = ""),width = 11.69,height = 8.27)
  


# ## AFRICA ONLY -------------------------
# #   Africa':[(-35,35,'cc'),(-10,50,'cc'),],
# #   WestAfrica':[(5,30,'cc'),(-17,30,'cc'),],
# 
# for (j in b[1:2]){
#   for (variabl in var_ls){
#     
#     if (j=="B_CL_wf"){
#       data <- B_CL_229_wf 
#       refdata1 <- "B_wf"
#       refdata2 <- "CL_wf"
#     }else if (j == "B_CL_wof") {
#       data <- B_CL_229_wof
#       refdata1 <- "B_wof"
#       refdata2 <- "CL_wof"
#     }
#     
#     data <- data %>% 
#       mutate(region = ifelse(lon >= -10 & lon <= 50 & lat >= -35 & lat <= 35, "Africa", "other")) %>% 
#       filter (region != "other")
#     
#     # filter by variable
#     dBP_gat <- data %>% filter (REF == refdata1) %>% dplyr::select(lat, lon, variabl) %>% 
#       dplyr::rename(val_BP = variabl) %>%  na.omit()
#     
#     dCL_gat <- data %>% filter (REF == refdata2) %>%  dplyr::select(lat, lon, variabl) %>% 
#       dplyr::rename(val_CL = variabl) %>%  na.omit()
#     
#     data_merge <- join(dBP_gat, dCL_gat, by = c("lat","lon") , type = "inner", match = "all")
#     
#     # scatterplot
#     
#     p <- ggplot(na.omit(data_merge), aes(x = val_BP, y = val_CL)) +
#       geom_point(alpha = 1,color = "darkred", size = 2) +
#       geom_smooth(method='lm', show.legend = NA,inherit.aes = TRUE,formula = y ~x, se = TRUE,
#                   weight=0.5, color = "darkred", size = 0.5) +
#       theme_bw()+
#       theme(plot.caption = element_text(size=10, vjust = -0.18)) +
#       labs(title = paste ("",variabl,".",sep=""),
#            x = refdata1,
#            y = refdata2)+
#       labs(caption = paste ("n=",dim(data_merge)[1],sep=" ")) +
#       geom_abline(intercept = 0,slope=1, color = "black", linetype="dotted", size=1) +
#       stat_cor()     
#     
#     assign(paste("plot",variabl,j,sep="_"),p)
#     
#     rm(ls="p","dBP_gat","dCL_gat","data","data_merge", "refdata1", "refdata2")
#     
#   }
#   
#   fig <- ggarrange(get(paste("plot_MAT",j, sep="_")),
#                    get(paste("plot_MTCO",j, sep="_")),
#                    get(paste("plot_MTWA",j, sep="_")),
#                    get(paste("plot_MAP",j, sep="_")),
#                    get(paste("plot_MI",j, sep="_")),
#                    get(paste("plot_GDD5",j, sep="_")),
#                    labels = c("A", "B", "C", "D","E", "F"),
#                    ncol = 3, nrow = 2)
#   
#   fig
#   
#   ggsave(fig,file=paste(plotpath, "dat_CL_versions/", j,"_229_scatterplots_Africa.jpg", sep = ""),width = 11.69,height = 8.27)
#   ggsave(fig,file=paste(plotpath, "dat_CL_versions/", j,"_229_scatterplots_Africa.pdf", sep = ""),width = 11.69,height = 8.27)
#   
# }
# 
# 
## map overlapping sites ####

rm(list=ls(pattern="^plot_")) # remove all variables starting with "plot_"
rm(ls="fig", "j", "variabl")

for (j in b[3:4]){
  for (variabl in var_ls){
    if (j=="BP_CL_wf"){
      data <- BP_CL_229_wf 
      refdata1 <- "BP_wf"
      refdata2 <- "CL_wf"
    }else if (j == "BP_CL_wof") {
      data <- BP_CL_229_wof
      refdata1 <- "BP_wof"
      refdata2 <- "CL_wof"
    }
    
    # filter by variable
    dBP_gat <- data %>% filter (REF == refdata1) %>% dplyr::select(lat, lon, variabl) %>% 
      dplyr::rename(val_BP = variabl) %>%  na.omit()
    
    dCL_gat <- data %>% filter (REF == refdata2) %>%  dplyr::select(lat, lon, variabl) %>% 
      dplyr::rename(val_CL = variabl) %>%  na.omit()
    
    data_merge <- join(dBP_gat, dCL_gat, by = c("lat","lon") , type = "inner", match = "all")
    
  p <- mp +
    geom_point(data = data_merge ,aes (x = lon,y = lat),alpha = 1, #,color = as.factor(count_n) # removed this from aes
               stroke = 0.5,show.legend = T,size = 0.5, shape = 23,
               color= "darkorchid4", fill="darkorchid1") +
    theme_bw() + 
    theme(axis.text.x = element_text(size = rel(0.6)),
          axis.text.y = element_text(size = rel(0.6)),
          axis.title.y = element_text(size = rel(0.6)),
          axis.title.x = element_text(size = rel(0.6)),
          plot.title = element_text(size = 8, face = "bold"))+
    labs(title = variabl)#,color = "counts (FYI, data averaged)")
  
  assign(paste("plot",variabl,j,sep="_"),p)
  
  rm(ls="p","data_merge","dBP_gat","dCL_gat")

  }

  fig <- ggarrange(get(paste("plot_MAT",j, sep="_")),
                   get(paste("plot_MTCO",j, sep="_")),
                   get(paste("plot_MTWA",j, sep="_")),
                   get(paste("plot_MAP",j, sep="_")),
                   get(paste("plot_MI",j, sep="_")),
                   get(paste("plot_GDD5",j, sep="_")),
                   # labels = c("A", "B", "C", "D","E", "F"),
                   ncol = 3, nrow = 2)
  fig

ggsave(fig,file=paste(plotpath,"dat_CL_versions/",j, "229_map_overlap_sites.jpg", sep=""),width = 11.69,height = 4)
#ggsave(fig,file=paste(plotpath,"dat_CL_versions/",j, "229_map_overlap_sites.pdf", sep = ""),width = 11.69,height = 4)
}

rm(list=ls(pattern="^plot_")) # remove all variables starting with "plot_"


  

rm(list=ls(pattern="^plot_")) # remove all variables starting with "plot_"

# ## Africa scatterplots comparing sites/values in cleator versions ####
# 
# rm(ls="fig", "j", "variabl","data")
# 
# for (variabl in var_ls){
#   
#   data229 <-  clea_all_229 %>% dplyr::select(LAT,LON,variabl) %>%
#     dplyr::rename(val_229 = variabl) %>%  na.omit() %>% 
#     mutate(region = ifelse(LON >= -10 & LON <= 50 &
#                              LAT >= -35 & LAT <= 35, "Africa", "other")) %>% 
#     filter (region != "other") %>% dplyr::select (c(-ncol(.)))
#   
#   data244 <-  clea_all_244 %>% dplyr::select(LAT,LON,variabl) %>%
#     dplyr::rename(val_244 = variabl) %>%  na.omit() %>% 
#     mutate(region = ifelse(LON >= -10 & LON <= 50 &
#                              LAT >= -35 & LAT <= 35, "Africa", "other")) %>% 
#     filter (region != "other") %>% dplyr::select (c(-ncol(.)))
#   
#   rng <- range (data229$val_229, data244$val_244)
#   breakcol <- seq(from=-35, to=35, length.out = 3)
#   lim_colbar <- c(-35, 35)
#   
#   
#   data_merge <- join(data229, data244, by = c("LAT","LON") , type = "inner",
#                      match = "all") %>% na.omit()
#   
#   gg <- ggplot(data_merge, aes(x = val_229, y = val_244, color= LAT), show.legend = F) +
#     geom_point(alpha = 1, size = 2) +
#     geom_vline(xintercept = 0, color = "grey70", size=0.5) +
#     geom_hline(yintercept = 0,color = "grey70", size=0.5) +
#     lims (x=rng, y=rng) +
#     scale_color_gradientn (colors= rainbow(5),na.value = "grey50",
#                            guide = "colourbar",aesthetics = "colour", breaks=breakcol,
#                            limits=lim_colbar) +
#     geom_smooth(method='lm', show.legend = F,inherit.aes = TRUE,formula = y ~x, se = TRUE,
#                 weight=1, color = "darkred", size = 1) +
#     theme_bw() +
#     theme(axis.line = element_line(colour = "black"),
#           legend.position = c(0.25, 0.78),
#           legend.direction = "horizontal",
#           legend.key.width = unit(0.7, "cm"),
#           legend.key.height = unit(0.25, "cm"),
#           plot.caption = element_text(size=12, vjust = 22, hjust = 0.99),
#           panel.background = element_rect(colour = "black", size=0.5, fill=NA),#element_blank(),
#           panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank()) +
#     labs(caption = paste ("n=",dim(data_merge)[1], sep=" "),
#          x = "version 229", y = "version 244", color = "")+
#     geom_abline(intercept = 0,slope=1, color = "black", linetype="dotted", size=1) +
#     stat_cor()
#   
#   gg
#   p <- ggMarginal(gg, type = "histogram", fill="grey90", size = 8)
#   p
#   assign(paste("plot",variabl,sep="_"),p)
#   
#   rm(ls="data229", "data244", "rng", "p", "data_merge")
#   
# }
# 
# fig <- ggarrange(get(paste("plot_MAT", sep="_")),
#                  get(paste("plot_MTCO", sep="_")),
#                  get(paste("plot_MTWA", sep="_")),
#                  get(paste("plot_MAP", sep="_")),
#                  get(paste("plot_MI", sep="_")),
#                  get(paste("plot_GDD5", sep="_")),
#                  labels = c("MAT", "MTCO", "MTWA", "MAP","MI", "GDD5"),
#                  ncol = 3, nrow = 2)  
# fig
# ggsave(fig,file=paste(plotpath, "dat_CL_versions/Africa_scatter_CL_229_244.jpg", sep = ""),width = 14,height = 8)
# 
# rm(list=ls(pattern="^plot_")) # remove all variables starting with "plot_"
# 
# 
graphics.off()