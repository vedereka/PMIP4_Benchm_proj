# This script produces the matrix plots with the scores, coloured according to the model's 
# performance as well as their statistical significance.
# See "functions_source.R" for the plotting function (to change colours, thresholds, labels, etc)

# Significance testing/colouring:

#   1- If Sig=TRUE: A significant change can be either towards better/worse
# Two things can happen: Only the score changes (no change in category) or the score changes so much that there's also a change in category
# -> colour only the steps sig improving from step 1 PLUS with a change in category towards better
# if sig=T, check for categories in st1,2,3 -> only keep colour categ if they change towards better (4 to 1)

#   2- No significant change from st1 to st2
# sig=FALSE & step=2 -> convert z_val in step 2 to NA

#   3- No significant change from st2 to st3
# sig=FALSE & step=3 -> convert z_val in step 3 to NA

# Use Kageyama 2020 (cp-2019-169) regions:
# ### definition of the regions: latitude range, longitude range
#   'Globe':[(-90,90,'cc'),(-180,180,'cc')],
#   Tropics':[(-30,30,'cc'),(-180,180,'cc')],
#   NAtlEurope':[(30,50,'cc'),(-45,45,'cc')],
#   NorthAtlantic':[(30,50,'cc'),(-60,-10,'cc')],
#   Europe':[(35,70,'cc'),(-10,60,'cc')],
#   WesternEurope':[(35,70,'cc'),(-10,30,'cc')],
#   NWAmerica':[(20,50,'cc'),(-125,-105,'cc')],
#   NEAmerica':[(20,50,'cc'),(-105,-50,'cc')],
#   Africa':[(-35,35,'cc'),(-10,50,'cc'),],
#   WestAfrica':[(5,30,'cc'),(-17,30,'cc'),],
#   NAmerica':[(20,50,'cc'),(-140,-60,'cc'),],
#   SHextratropics':[(-90,-30,'cc'),(-180,180,'cc')],
#   NHextratropics':[(30,90,'cc'),(-180,180,'cc')],
#   NTropics':[(0,30,'cc'),(-180,180,'cc')],
#   ExtratropicalAsia':[(30,75,'cc'),(60,135,'cc')],
#   TropicalAsia':[(8,30,'cc'),(60,120,'cc')],
#   TropicalAmericas':[(-30,30,'cc'),(-120,-35,'cc')],

# Created by Laia Comas-Bru in November 2020
# Last modified: February 2021

##### SET STUFF ################################################################################
source('region_def.R')


#-----------------------------------------------
# Use regions as defined in region_def.R
region_ls <- region_ls_ocean

 
#source_ls <- c("Margo", "Margo_min", "Margo_max")
#source_ls <- c("Tierney", "Tierney_min", "Tierney_max")
#source_ls <- c("T_Grid", "T_Grid_min", "T_Grid_max") 
#source_ls <- c("glomap", "glomap_min", "glomap_max") 
source_ls <- c("AH", "AH_min", "AH_max") 
#  "kn")

#source_ls <- c("kn", "kn_min", "kn_max")
steps = c(1, 2, 3)

refs <- "AH"

##### OPEN AND MANIPULATE SCORES DATA ###########################################################

for (source in source_ls) {
  for (region in region_ls$reg_name) {
    # print(list.files(
    #    "output_scores/Ocean",
    #    pattern = paste ("*", source, "_", region, ".csv", sep = ""),
    #    full.names = TRUE))

    df_tbl <- #load all csv files in directory and rbind them
      list.files(
        "output_scores/Ocean/",
        pattern = paste ("*", source, "_", region, ".csv", sep = ""),
        full.names = TRUE
      ) %>%
      map_df( ~ read.csv(.)) %>% `colnames<-`(
        c(
          "X1","varname","mean_null","random_null","AWI1","AWI2","CCSM4",
          "CESM12","Had-GL","Had-IC","iLOVE-GL","iLOVE-IC",
          "INM","IPSL","MIROC","MPI"
        )
      )
    
    # choose step and prepare data
    for (st in steps) {
      #process data
      df <- as.data.frame (df_tbl)
      df <- df %>% filter (df$X1 == paste("step", st, sep = ""))
      df_data <- as.data.frame(df[, 5:ncol(df)])
      #df_data <- df[, 5:ncol(df)]
      #print(unique(df$varname))
      rownames(df_data) <- lapply(unique(df$varname), FUN = trim_mode_name)
      df$varname <- lapply(df$varname, FUN = trim_mode_name)
      
      
      # rename variables according to what they are prior to rbind
      if (sjmisc::str_contains(source, "min", ignore.case = T)) {
        df_models <-
          round(df[, 3:4], 2) %>% dplyr::rename (mean_min = mean_null, rand_min = random_null)
        df_models$var = rownames(df_data)
        
        df2 <- round(df_data, 2) %>% as.matrix() %>%
          melt(.,
               value.name = "score_min",
               varnames = c("var", "model")) %>%
          mutate (step = st)
        
      } else if (sjmisc::str_contains(source, "max", ignore.case = T)) {
        df_models <-
          round(df[, 3:4], 2) %>% dplyr::rename (mean_max = mean_null, rand_max = random_null)
        df_models$var = rownames(df_data)
        
        df2 <- round(df_data, 2) %>% as.matrix() %>%
          melt(.,
               value.name = "score_max",
               varnames = c("var", "model")) %>%
          mutate (step = st)
      }
      else {
        df_models <-
          round(df[, 3:4], 2) %>% dplyr::rename (mean_raw = mean_null, rand_raw = random_null)
        df_models$var = rownames(df_data)
        
        df2 <- round(df_data, 2) %>% as.matrix() %>%
          melt(.,
               value.name = "score_raw",
               varnames = c("var", "model")) %>%
          mutate (step = st)
      }
      
      
      # merge all steps/sources with rbind
      if (st == 1) {
        data <- df2
        df_mod <- df_models %>% mutate (step = st)
        
      } else {
        data <- rbind (data, df2)
        df_mod <- rbind(df_mod, df_models %>% mutate (step = st))
      }
      
    }
    
    data <- join(
      data,
      df_mod,
      by = c("var", "step") ,
      type = "left",
      match = "all"
    )
    assign(paste("data", source, region, sep = "_"), data)
  }
}

rm(list=ls(pattern="^df")) # clean environment

##### ASSIGN COLOURS FOR EACH SCORE (see plot legend) ################################################################################

refs_ls <- c("Margo", "Tierney") 
#, "AH", "glomap", "kn") # min/max already used. 


#for (refs in refs_ls) { # loop needed if more than one source (ie B and CL)
print(refs)

for (region in region_ls$reg_name) {
  print(region)
  #print(paste ("data", refs, "max", region, sep = "_"))
  
  data <- join(
  get(paste ("data", refs, "max", region, sep = "_")) %>% dplyr::select (var, model, step, score_max),
  get(paste ("data", refs, "min", region, sep = "_")) %>% dplyr::select (var, model, step, score_min),
  by = c("var", "model", "step") ,type = "left",match = "all") %>%
  join (., get(paste ("data", refs, region, sep = "_")) %>% dplyr::select (var, model, step, mean_raw, rand_raw, score_raw),
  by = c("var", "model", "step") ,
  type = "left",match = "all") %>%
  mutate (min = pmin(score_max, score_raw, score_min),
  max = pmax(score_max, score_raw, score_min)) %>%
  dplyr::select (var, model, step, mean_raw, rand_raw, score_raw, min, max) %>%
  dplyr::rename (mean_null = mean_raw,
  rand_null = rand_raw,
  val = score_raw) %>%
  mutate (z_val = NA,z_min = NA,z_max = NA)

  #assign values for colours
  for (k in 1:dim(data)[1]) {
    rand <- data$rand_null[k]
    mn <- data$mean_null[k]
    y <- data[k, (ncol(data) - 5):(ncol(data) - 3)]
    x <- data[k, (ncol(data) - 2):ncol(data)]
    
    # x [(condition == TRUE),] <- 1
    x [, ((y <= mn - ((25 / 100) * mn)) == TRUE)] <- 1
    x [, ((y > mn - ((25 / 100) * mn) & y <= mn) == TRUE)] <- 2
    x [, ((y > mn & y < rand) == TRUE)] <- 3
    x [, ((y >= rand) == TRUE)] <- 4
    
    data[k, (ncol(data) - 2):ncol(data)] <- x
  }
  
  rm(ls="x","y", "mn", "k", "rand")
  
  # are ranges overlappin -> not significant???? if so, convert z back to NA
  # Use DescTools::Overlap
  # steps are cumulative (1 to 2 and then 2 to 3)
  
  data <- data %>% mutate (sig = NA)
  for (k in 1:dim(data)[1]) {
    if (data[k, "step"] == 1) {
      int2 <-
        data %>% filter (step == 2, var == data[k, "var"], model == data[k, "model"])
      
      data[which(data$step == 2)[which(data$step == 2) %in% which(data$var == data[k, "var"] &
                                                                    data$model == data[k, "model"])], "sig"] <-
        c(data[k, "min"], data[k, "max"]) %overlaps% c(int2[, "min"], int2[, "max"])
      
      rm(ls = "int2")
      
    } else if (data[k, "step"] == 2) {
      int3 <-
        data %>% filter (step == 3, var == data[k, "var"], model == data[k, "model"])
      data[which(data$step == 3)[which(data$step == 3) %in% which(data$var == data[k, "var"] &
                                                                    data$model == data[k, "model"])], "sig"] <-
        c(data[k, "min"], data[k, "max"]) %overlaps% c(int3[, "min"], int3[, "max"])
      
      rm(ls = "int3")
    }
  }
  
  rm(ls="k")
  
  assign(paste ("df", refs, region, sep = "_"), data)
}


rm(ls="st", "source", "region")

variab_ls <- as.character(unique(data$var))
model_ls <- as.character(unique (data$model))

# rm(list=ls(pattern="^data")) # clean environment
#}

##### APPLY SIGNIFICANCE TO THE COLOURING,  CREATE AND SAVE PLOT  ##############
# choose wich scores to colour and scores to remain white acc to significance

#for (refs in refs_ls){ # loop needed only if more than one source (ie B and CL)

for (region in region_ls$reg_name) {

  data <- get (paste ("df", refs, region, sep = "_"))
  data[which(data$sig == FALSE), "sig"] <- 999
  data[which(data$sig == 1), "sig"] <- 0
  data[which(data$sig == 999), "sig"] <- 1
  data$sig <- as.logical (data$sig)
  
  filename_output_jpeg <-
    paste (plotpath,"DM_scores/", refs, "_", region, "_scores_plot.jpg", sep = "")
  
  for (mod in model_ls) {
    for (variab in variab_ls) {
      x <- data[which(data$var == variab & data$model == mod),]
      
      # 1a. signif change in scores from st2 to st3
      if (x[which(x$step == 3), "sig"]) {
        # different category towards better -> pass1 = TRUE
        if (x[which(x$step == 3), "z_val"] >= x[which(x$step == 2), "z_val"]) {
          # remove colour for scores not changing categ significantly
          data[which(data$var == variab &
                       data$model == mod &
                       data$step == 3), "z_val"] <- 5
        }
      }
      
      # 1b. signif change in scores from st1 to st2
      if (x[which(x$step == 2), "sig"]) {
        # different category towards better -> pass1 = TRUE
        if (x[which(x$step == 2), "z_val"] >= x[which(x$step == 1), "z_val"]) {
          # remove colour for scores not changing categ significantly
          data[which(data$var == variab &
                       data$model == mod &
                       data$step == 2), "z_val"] <- 5
        }
      }
      
      # 2.No significant change from st1 to st2
      if (x[which(x$step == 2), "sig"] == FALSE) {
        # -> convert z_val in step 2 to NA
        data[which(data$var == variab &
                     data$model == mod &
                     data$step == 2), "z_val"] <- 5
      }
      
      # 3.No significant change from st2 to st3
      if (x[which(x$step == 3), "sig"] == FALSE) {
        # -> convert z_val in step 2 to NA
        data[which(data$var == variab &
                     data$model == mod &
                     data$step == 3), "z_val"] <- 5
      }
      
      rm ("x")
    }
  }
  #here whe have a data file with NA in z_val for changes that are not significant
  # matrixplot is a function in functions_source.R
  
  fig <- ggarrange (
    matrixplot(1) + rremove("x.text"),
    matrixplot(2) + rremove("x.text"),
    matrixplot(3),
    ncol = 1,
    common.legend = TRUE,
    legend = "top"
  )
  
  fig <- annotate_figure(fig,
                         top = text_grob(
                           paste ("Target: ", refs, ". Region: ", region, sep = ""),
                           color = "black",
                           face = "bold",
                           size = 16
                         ))
  ggsave(fig,
         file = filename_output_jpeg,
         width = 12,
         height = 13)
  # }
  
}

graphics.off()
