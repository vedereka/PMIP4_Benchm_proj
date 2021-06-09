# Varios region definitions - ideally could be selected and included on the fly, when I figure out how to do that



# Use Kageyama 2020 (cp-2019-169) regions:
# ### definition of the regions: latitude range, longitude range
#   'Globe':[(-90,90,'cc'),(-180,180,'cc')],
#   Tropics':[(-30,30,'cc'),(-180,180,'cc')],
#   NAtlEurope':[(30,50,'cc'),(-45,45,'cc')],
#   NorthAtlantic':[(30,50,'cc'),(-60,10,'cc')],
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
# -----------------------------------------------------------------
#  My own ocean basin definitions
#  Check selections that go across 'dateline' (-180 to 180, as selection may go wrong way) #   and may need to be split to W. and E. versions across Pacific

#   Tropics':[(-30,30,'cc'),(-180,180,'cc')],
#   NAtlEurope':[(30,50,'cc'),(-45,45,'cc')],
#   NorthAtlantic':[(30,50,'cc'),(-60,-10,'cc')],
#   ArcticOcean':[(75,90,'cc'),(-180,180,'cc')],
#   SAtlantic':[(10,-50,'cc'),(15,-70,'cc')],
#   MidAtlantic':[(10,30,'cc'),(-90,25,'cc')],
#   IndianOcean':[(0,-30,'cc'),(40,100,'cc'),],
#   WPacNExtraTropics':[(30,60,'cc'),(-180, -120,'cc')],
#   EPacNExtraTropics':[(30,60,'cc'),(130, 180,'cc')],
#   WPacTropics':[(-30,30,'cc'),(-180,-80,'cc'),],
#   EPacTropics':[(-30,30,'cc'),(130,180,'cc'),],
#   WPacSExtraTropics':[(-30,-60,'cc'),(-180,-70,'cc'),],
#   EPacSExtraTropics':[(-30,-60,'cc'),(120,180,'cc'),],
#
# -----------------------------------------------------------------
#  Masa ocean basin definitions
#   Tropics':[(-30,30,'cc'),(-180,180,'cc')],
#   NAtlEurope':[(30,50,'cc'),(-45,45,'cc')],
#   NorthAtlantic':[(30,50,'cc'),(-60,-10,'cc')],
#   ArcticOcean':[(75,90,'cc'),(-180,180,'cc')],
#   SAtlantic':[(10,-50,'cc'),(15,-70,'cc')],
#   MidAtlantic':[(10,30,'cc'),(-90,25,'cc')],
#   IndianOcean':[(30,-60,'cc'),(20,150,'cc'),],       
#   WPacNExtraTropics':[(30,60,'cc'),(-180, -130,'cc')],
#   EPacNExtraTropics':[(30,60,'cc'),(110, 180,'cc')], 
#   WPacTropics':[(-30,30,'cc'),(-180,-130,'cc'),],
#   EPacTropics':[(-30,30,'cc'),(80,180,'cc'),],
#   WPacSExtraTropics':[(-30,-60,'cc'),(-180,-150,'cc'),], 
#   EPacSExtraTropics':[(-30,-60,'cc'),(70,180,'cc'),]
#
print("Loading region definitions")
#-------------------------------------------------------------
# Just N America for testing
region_ls_NAm <- rbind(c("NAmerica", 20,50,-140,-60)) %>%
  as.data.frame (.) %>%
  dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)

#-------------------------------------------
#All Land regions from Kagemaya 2020

region_ls_Landall <- rbind( c("global", -90,90,-180,180),c("NH", 0,90,-180,180),c("NHextratropics", 30,90,-180,180),
       c("NTropics", 0,30,-180,180),c("NAmerica", 20,50,-140,-60),
       c("TropicalAmericas", -30,30,-120,-35), c("WesternEurope", 35,70,-10,30),c("TropicalAsia",8,30,60,120),
       c("ExtratropicalAsia", 30,75,60,135), c("Africa",-35,35,-10,50)) %>%
  as.data.frame (.) %>%
  dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)

#-------------------------------------------
#Ocean basin regions updated from Masa
region_ls_ocean <- rbind(c("Arctic", 75,90,-180,180),
                   c("NatlEurope", 30,50,-45,45),
                   c("NorthAtlantic",30,50,-60,-10), 
                   c("MidAtlantic",10,30,-90,25),
                   c("SAtlantic",-50,10,-70,15),
                   c("EPacNextratropics", 30,60,-180,-110),
                   c("WPacNextratropics", 30,60,130,180),
                   c("EPacSextratropics", -60,-30,-180,-120),
                   c("WPacSextratropics", -60,-30,150,180), 
                   c("EPacTropics", -30,30,-180,-80),
                   c("WPacTropics", -30,30,130,180),
                   c("IndianOcean", -30,0,40,100)) %>% 
  as.data.frame (.) %>%
  dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)

#--------------------------------------------------------
#My Ocean basin regions, with one or more regions removed where no results available for the scores
region_ls_oceanLight <- rbind(c("Arctic", 75,90,-180,180),
                               c("NatlEurope", 30,50,-45,45),
                               c("NorthAtlantic",30,50,-60,-10), 
                               c("MidAtlantic",10,30,-90,25),
                               c("SAtlantic",-50,10,-70,15),
                               c("EPacNextratropics", 30,60,-180,-110),
                               c("WPacNextratropics", 30,60,130,180),
                               #c("EPacSextratropics", -60,-30,-180,-120),
                               c("WPacSextratropics", -60,-30,150,180), 
                               c("EPacTropics", -30,30,-180,-80),
                               c("WPacTropics", -30,30,130,180), 
                              c("IndianOcean", -30,0,40,100)) %>% 
  as.data.frame (.) %>%
  dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)

#------------------------------------------                     
# Zonal bands
region_ls_zonal <- rbind(c("Global", -90,90,-180,180),
                         c("NH", 0,90,-180,180),
                         c("SH", -90,0,-180,180),
                         c("NHextratropics", 30,90,-180,180),
                         c("SHextratropics", -30,-90,-180,180), 
                         c("STropics", -30,0, -180,180),
                         c("NTropics", 0,30,-180,180),
                         c("STropics23", -23,0, -180,180),
                         c("NTropics23", 0,23,-180,180)) %>%
  as.data.frame (.) %>%
  dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)

#------------------------------------------                     
# 60 degree bands
region_ls_zonal60 <- rbind(c("NExtraTrops60", 30,90,-180,180),
                         c("Tropics60", -30,30,-180,180),
                         c("SExtraTropics60", -90,-30,-180,180)) %>%
  as.data.frame (.) %>%
  dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)
#-------------------------------------------------
# Three region test bands
region_ls_Masa <- rbind(c("NorthAtlantic",30,50,-60,-10), 
                        c("TropicalOceans",-30,30,-180,180)) %>%
  as.data.frame (.) %>%
  dplyr::rename (reg_name = V1, min_lat = V2, max_lat = V3, min_lon = V4, max_lon = V5)


#------------------------------------------   