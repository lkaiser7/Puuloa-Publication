### Puuloa Environmental Justice ###
### Sea Grant (Dingy and Elieen)
### https://seagrant.soest.hawaii.edu/puuloalanduse/ 

##############
### SET UP ###
##############

# set working directory
rootDir<-"C:/Users/lkais/Dropbox/PacIOOS/Projects/other_projects/puuloa_analysis/"
setwd(rootDir)
list.files()
# dataset outputs
outDir<-paste0(rootDir, "output_datasets/")

# load packages
library(sf)
library(dplyr)
library(mapview)
library(readr)
library(tmap)

# "#154360" Unclassified "#8DA0CB"
# "#FF5733" Urban "#E78AC3"
# "#FFC300" Agriculture "#66C2A5"
# "#1ABC9C" Conservation "#FC8D62"

mapDir<-paste0(outDir, "LU_cat_maps/")
list.files(mapDir)
shp_files<-list.files(mapDir, pattern = "\\.shp$", full.names = T)

# 1820s
shp_map<-read_sf(shp_files[1])
shp_map$LU_Type<-as.factor(shp_map$LU_Type)
map1<-tm_shape(shp_map) +
  tm_polygons("LU_Type", 
              #fill.scale = tm_scale(values = c('#FFC300', '#154360', '#FF5733', '#1ABC9C')),
              fill.scale = tm_scale(values = c('#66C2A5', '#8DA0CB', '#E78AC3', '#FC8D62')),
              fill.legend = tm_legend("Land Use")) +
  tm_title_in(str_sub(shp_files[1], -9, -5), size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
map1

# 1870s
shp_map<-read_sf(shp_files[2])
shp_map$LU_Type<-as.factor(shp_map$LU_Type)
map2<-tm_shape(shp_map) +
  tm_polygons("LU_Type", 
              #fill.scale = tm_scale(values = c('#FFC300', '#154360', '#FF5733', '#1ABC9C')),
              fill.scale = tm_scale(values = c('#66C2A5', '#8DA0CB', '#E78AC3', '#FC8D62')),
              fill.legend = tm_legend("Land Use")) +
  tm_title_in(str_sub(shp_files[2], -9, -5), size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
map2

# 1890s
shp_map<-read_sf(shp_files[3])
shp_map$LU_Type<-as.factor(shp_map$LU_Type)
map3<-tm_shape(shp_map) +
  tm_polygons("LU_Type", 
              #fill.scale = tm_scale(values = c('#FFC300', '#154360', '#FF5733', '#1ABC9C')),
              fill.scale = tm_scale(values = c('#66C2A5', '#8DA0CB', '#E78AC3', '#FC8D62')),
              fill.legend = tm_legend("Land Use")) +
  tm_title_in(str_sub(shp_files[3], -9, -5), size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
map3

# 1900s
shp_map<-read_sf(shp_files[4])
shp_map$LU_Type<-as.factor(shp_map$LU_Type)
map4<-tm_shape(shp_map) +
  tm_polygons("LU_Type", 
              #fill.scale = tm_scale(values = c('#FFC300', '#154360', '#FF5733', '#1ABC9C')),
              fill.scale = tm_scale(values = c('#66C2A5', '#8DA0CB', '#E78AC3', '#FC8D62')),
              fill.legend = tm_legend("Land Use")) +
  tm_title_in(str_sub(shp_files[4], -9, -5), size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
map4

# 1950s
shp_map<-read_sf(shp_files[5])
shp_map$LU_Type<-as.factor(shp_map$LU_Type)
map5<-tm_shape(shp_map) +
  tm_polygons("LU_Type", 
              #fill.scale = tm_scale(values = c('#FFC300', '#1ABC9C', '#154360', '#FF5733')),
              fill.scale = tm_scale(values = c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3')),
              fill.legend = tm_legend("Land Use")) +
  tm_title_in(str_sub(shp_files[5], -9, -5), size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
map5

# 1960s
shp_map<-read_sf(shp_files[6])
shp_map$LU_Type<-as.factor(shp_map$LU_Type)
map6<-tm_shape(shp_map) +
  tm_polygons("LU_Type", 
              #fill.scale = tm_scale(values = c('#FFC300', '#1ABC9C', '#FF5733', '#154360')),
              fill.scale = tm_scale(values = c('#66C2A5', '#FC8D62', '#E78AC3', '#8DA0CB')),
              fill.legend = tm_legend("Land Use")) +
  tm_title_in(str_sub(shp_files[6], -9, -5), size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
map6

# 1970s
shp_map<-read_sf(shp_files[7])
shp_map$Name<-as.factor(shp_map$Name)
map7<-tm_shape(shp_map) +
  tm_polygons("Name", 
              #fill.scale = tm_scale(values = c('#FFC300', '#1ABC9C', '#FF5733', '#154360')),
              fill.scale = tm_scale(values = c('#66C2A5', '#FC8D62', '#E78AC3', '#8DA0CB')),
              fill.legend = tm_legend("Land Use")) +
  tm_title_in(str_sub(shp_files[7], -9, -5), size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
map7

# 1980s
shp_map<-read_sf(shp_files[8])
shp_map$LU_Type<-as.factor(shp_map$LU_Type)
map8<-tm_shape(shp_map) +
  tm_polygons("LU_Type", 
              #fill.scale = tm_scale(values = c('#FFC300', '#1ABC9C', '#154360', '#FF5733')),
              fill.scale = tm_scale(values = c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3')),
              fill.legend = tm_legend("Land Use")) +
  tm_title_in(str_sub(shp_files[8], -9, -5), size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
map8

# 2000s
shp_map<-read_sf(shp_files[9])
shp_map$LU_Type<-as.factor(shp_map$LU_Type)
map9<-tm_shape(shp_map) +
  tm_polygons("LU_Type", 
              #fill.scale = tm_scale(values = c('#FFC300', '#1ABC9C', '#FF5733', '#154360')),
              fill.scale = tm_scale(values = c('#66C2A5', '#FC8D62', '#E78AC3', '#8DA0CB')),
              fill.legend = tm_legend("Land Use")) +
  tm_title_in(str_sub(shp_files[9], -9, -5), size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
map9

# 2020s
shp_map<-read_sf(shp_files[10])
shp_map$LUSE_DESCR<-as.factor(shp_map$LUSE_DESCR)
map10<-tm_shape(shp_map) +
  tm_polygons("LUSE_DESCR", 
              #fill.scale = tm_scale(values = c('#FFC300', '#1ABC9C', '#FF5733', '#154360')),
              fill.scale = tm_scale(values = c('#66C2A5', '#FC8D62', '#E78AC3', '#8DA0CB')),
              fill.legend = tm_legend("Land Use")) +
  tm_title_in(str_sub(shp_files[10], -9, -5), size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
map10

# display map
all_lu<-tmap_arrange(map1, map2, map3, map4, map5, map6, map7, map8, map9, 
                     map10, ncol = 2, nrow = 5)
all_lu
# save map as a png
tmap_save(all_lu, filename = paste0(outDir, "images/lu_over_time-v2.png"), 
          width = 10, height = 12, units = "in", dpi = 300)

###########
### END ###
###########