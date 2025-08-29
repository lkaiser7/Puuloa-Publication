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

# load data
svi_shp<-read_sf(paste0(outDir, "Puuloa_SVI_Contaminants/Puuloa_SVI_Contaminants.shp"))
svi_shp
head(as.data.frame(svi_shp))
names(svi_shp)
## E = estimate
## M = MOE (margins of error)
## EP = estimate percentage

##############
### FORMAT ###
##############

# create output file
toxic_risk<-svi_shp %>%
  select("objectid_1", "geoid20", "name20", "tractname", "Ahupuaʻa", "AREA_SQMI", "Area_sqmi_", "Area_ha", "Join_Count")
toxic_risk

################
### ANALYSIS ###
################

### TOXIC BURDEN RISK ###
# (tract pop X tract contaminants)/total pop = toxic burden risk
### RATIOS ###
# proportion of toxic burden risk/total population toxic burden risk
# 1 = complete equality
# > 1 = greater toxic burden risk
# < 1 = less toxic burden risk

# total population risk
# puuloa_pop<-sum(svi_shp$pop20)
# puuloa_pop<-sum(svi_shp$E_TOTPOP)
toxic_risk$TOT_POP_RISK<-(svi_shp$E_TOTPOP*svi_shp$Join_Count)/sum(svi_shp$E_TOTPOP)
mapview(toxic_risk, zcol = "TOT_POP_RISK")

# poverty
toxic_risk$POV_RISK<-(svi_shp$E_POV150*svi_shp$Join_Count)/sum(svi_shp$E_POV150)
mapview(toxic_risk, zcol = "POV_RISK")
toxic_risk$POV_RATIO<-toxic_risk$POV_RISK/toxic_risk$TOT_POP_RISK
mapview(toxic_risk, zcol = "POV_RATIO")

# minority
toxic_risk$MINRTY_RISK<-(svi_shp$E_MINRTY*svi_shp$Join_Count)/sum(svi_shp$E_MINRTY)
mapview(toxic_risk, zcol = "MINRTY_RISK")
toxic_risk$MINRTY_RATIO<-toxic_risk$MINRTY_RISK/toxic_risk$TOT_POP_RISK
mapview(toxic_risk, zcol = "MINRTY_RATIO")

### RACE & ETHNICITY ###
# african population
toxic_risk$AFAM_POP_RISK<-(svi_shp$E_AFAM*svi_shp$Join_Count)/sum(svi_shp$E_AFAM)
mapview(toxic_risk, zcol = "AFAM_POP_RISK")
toxic_risk$AFAM_RATIO<-toxic_risk$AFAM_POP_RISK/toxic_risk$TOT_POP_RISK
mapview(toxic_risk, zcol = "AFAM_RATIO")

# hispanic population
toxic_risk$HISP_POP_RISK<-(svi_shp$E_HISP*svi_shp$Join_Count)/sum(svi_shp$E_HISP)
mapview(toxic_risk, zcol = "HISP_POP_RISK")
toxic_risk$HISP_RATIO<-toxic_risk$HISP_POP_RISK/toxic_risk$TOT_POP_RISK
mapview(toxic_risk, zcol = "HISP_RATIO")

# asian population
toxic_risk$ASIAN_POP_RISK<-(svi_shp$E_ASIAN*svi_shp$Join_Count)/sum(svi_shp$E_ASIAN)
mapview(toxic_risk, zcol = "ASIAN_POP_RISK")
toxic_risk$ASIAN_RATIO<-toxic_risk$ASIAN_POP_RISK/toxic_risk$TOT_POP_RISK
mapview(toxic_risk, zcol = "ASIAN_RATIO")

# indigenous population
toxic_risk$AIAN_POP_RISK<-(svi_shp$E_AIAN*svi_shp$Join_Count)/sum(svi_shp$E_AIAN)
mapview(toxic_risk, zcol = "AIAN_POP_RISK")
toxic_risk$AIAN_RATIO<-toxic_risk$AIAN_POP_RISK/toxic_risk$TOT_POP_RISK
mapview(toxic_risk, zcol = "AIAN_RATIO")

# hawaiian population
toxic_risk$NHPI_POP_RISK<-(svi_shp$E_NHPI*svi_shp$Join_Count)/sum(svi_shp$E_NHPI)
mapview(toxic_risk, zcol = "NHPI_POP_RISK")
toxic_risk$NHPI_RATIO<-toxic_risk$NHPI_POP_RISK/toxic_risk$TOT_POP_RISK
mapview(toxic_risk, zcol = "NHPI_RATIO")

### AGE ### 
# over 65
toxic_risk$AGE65_POP_RISK<-(svi_shp$E_AGE65*svi_shp$Join_Count)/sum(svi_shp$E_AGE65)
mapview(toxic_risk, zcol = "AGE65_POP_RISK")
toxic_risk$AGE65_RATIO<-toxic_risk$AGE65_POP_RISK/toxic_risk$TOT_POP_RISK
mapview(toxic_risk, zcol = "AGE65_RATIO")

# under 17
toxic_risk$AGE17_POP_RISK<-(svi_shp$E_AGE17*svi_shp$Join_Count)/sum(svi_shp$E_AGE17)
mapview(toxic_risk, zcol = "AGE17_POP_RISK")
toxic_risk$AGE17_RATIO<-toxic_risk$AGE17_POP_RISK/toxic_risk$TOT_POP_RISK
mapview(toxic_risk, zcol = "AGE17_RATIO")

# save
toxic_risk
names(toxic_risk)
# format short names to save
names(toxic_risk)<-c("ObjID", "geoid20", "name20", "tractnm", "Ahupuua", "AREA_sm","A_sqmi", "A_ha" ,
                     "Cont_pt", "geometry", "POP_RISK", "POV_RISK", "POV_RAT", "MIN_RISK", "MIN_RAT",
                     "AF_RISK", "AF_RAT", "H_RISK", "H_RAT", "AS_RISK", "AS_RAT", "IN_RISK", "IN_RAT",
                     "HI_RISK", "HI_RAT", "AG65_RISK", "AG65_RAT", "AG17_RISK", "AG17_RAT")
# save toxic risk burden
st_write(toxic_risk, paste0(outDir, "Toxic_Risk_Burden/toxic_risk_shp/toxic_risk.gpkg"),  delete_layer = T)
write_csv(toxic_risk, paste0(outDir, "Toxic_Risk_Burden/toxic_risk_tract.csv"))

#############
### PLOTS ###
#############

### TOXIC BURDEN RISK ###
# (tract pop X tract contaminants)/total pop = toxic burden risk
### RATIOS ###
# proportion of toxic burden risk/total population toxic burden risk
# 1 = complete equality
# > 1 = greater toxic burden risk
# < 1 = less toxic burden risk

# start tmap plot
tmap_mode("plot")  # "vie"' for interactive

# Total Population
tot_pop<-tm_shape(toxic_risk) +
  tm_polygons("POP_RISK",
              fill.scale = tm_scale_continuous(values = "brewer.yl_or_rd"),
              fill.legend = tm_legend("Risk")) +
              #style = "cont", palette = "YlOrRd", title = "Risk") +
  tm_basemap("Esri.WorldGrayCanvas") +
  tm_title("Puʻuloa Population Toxic Risk Burden")
# display map
tot_pop
# save map as a png
tmap_save(tot_pop, filename = paste0(outDir, "images/total_population_risk.png"), 
          width = 8, height = 6, units = "in", dpi = 300)

# library(ggplot2)
# ggplot(toxic_risk) +
#   geom_sf(aes(fill = POP_RISK)) +
#   ggtitle("Puʻuloa Population Toxic Risk Burden") +
#   scale_fill_distiller(palette = "YlOrRd", direction = 1) +
#   theme_void() + theme(plot.title = element_text(hjust = 0.5))
# ggsave(paste0(outDir, "images/test.png"))

# Poverty Ratio
pov_rat<-tm_shape(toxic_risk) +
  tm_polygons("POV_RAT", 
              fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1),
              fill.legend = tm_legend("Risk")) +
              # palette = "-RdBu", title = "Risk",
              # style = "cont", midpoint = 1) +
  tm_basemap("Esri.WorldGrayCanvas") +
  tm_title("Toxic Burden Risk to Population Below 150% Poverty")
# display map
pov_rat
# save map as a png
tmap_save(pov_rat, filename = paste0(outDir, "images/poverty_ratio_risk.png"), 
          width = 8, height = 6, units = "in", dpi = 300)

# Minority Ratio
pov_min<-tm_shape(toxic_risk) +
  tm_polygons("MIN_RAT", 
              fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1),
              fill.legend = tm_legend("Risk")) +
  tm_basemap("Esri.WorldGrayCanvas") +
  tm_title("Toxic Burden Risk to Minority Population")
# display map
pov_min
# save map as a png
tmap_save(pov_min, filename = paste0(outDir, "images/minority_ratio_risk.png"), 
          width = 8, height = 6, units = "in", dpi = 300)

# Age Ratios
age_65<-tm_shape(toxic_risk) +
  tm_polygons("AG65_RAT", 
              fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1, limits = c(0, 3)),
              fill.legend = tm_legend("Risk")) +
  tm_basemap("Esri.WorldGrayCanvas") +
  tm_title("Persons aged 65 and older")
age_17<-tm_shape(toxic_risk) +
  tm_polygons("AG17_RAT", 
              fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1, limits = c(0, 3)),
              fill.legend = tm_legend("Risk")) +
  tm_basemap("Esri.WorldGrayCanvas") +
  tm_title("Persons aged 17 and younger")
# display map
age_rat<-tmap_arrange(age_65, age_17, ncol = 2, nrow = 1)
age_rat
# save map as a png
tmap_save(age_rat, filename = paste0(outDir, "images/age_ratio_risk.png"), 
          width = 12, height = 6, units = "in", dpi = 300)

# Ethnicity Ratios
af_rat<-tm_shape(toxic_risk) +
  tm_polygons("AF_RAT", 
              #fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1),
              fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1, limits = c(0, 8)),
              fill.legend = tm_legend("Risk")) +
  tm_title_in("African American", size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
his_rat<-tm_shape(toxic_risk) +
  tm_polygons("H_RAT", 
              #fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1),
              fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1, limits = c(0, 8)),
              fill.legend = tm_legend("Risk")) +
  tm_title_in("Hispanic/Latino", size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
as_rat<-tm_shape(toxic_risk) +
  tm_polygons("AS_RAT", 
              #fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1),
              fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1, limits = c(0, 8)),
              fill.legend = tm_legend("Risk")) +
  tm_title_in("Asian", size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
# tm_shape(toxic_risk) +
#   tm_polygons("IN_RAT", 
#               fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1),
#               fill.legend = tm_legend("Risk")) +
#   tm_layout(title = "Indigenous") +
#   tm_basemap("Esri.WorldGrayCanvas")
hi_rat<-tm_shape(toxic_risk) +
  tm_polygons("HI_RAT", 
              #fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1),
              fill.scale = tm_scale_continuous(values = "-brewer.rd_bu", midpoint = 1, limits = c(0, 8)),
              fill.legend = tm_legend("Risk")) +
  tm_title_in("Hawaiian", size = 0.8, fontface = "bold") +
  tm_basemap("Esri.WorldGrayCanvas")
# display map
eth_rat<-tmap_arrange(hi_rat, as_rat, af_rat, his_rat, ncol = 2, nrow = 2)
eth_rat
# save map as a png
tmap_save(eth_rat, filename = paste0(outDir, "images/ethnicity_ratio_risk-scaled.png"), 
          width = 12, height = 6, units = "in", dpi = 300)

###########
### END ###
###########