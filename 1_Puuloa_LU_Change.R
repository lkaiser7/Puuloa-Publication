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
library(readr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(geomtextpath)
library(sf)
library(mapview)

# # map path
# mapDir<-paste0(rootDir, "map_data/")
# list.files(mapDir)
# csv path
csv_path<-paste0(outDir, "LU_Tract_Tables/")
list.files(csv_path, pattern = "\\.csv$")
csv_files<-list.files(csv_path, pattern = "\\.csv$", full.names = T)

################
### ANALYSIS ###
################

# land use categories
lu_cat<-c("Agriculture", "Conservation", "Urban", "Unclassified")

# loop through files
for(f in 1:length(csv_files)){  # set f = 1 for debugging
  # select first file
  csv_nm<-csv_files[f]
  # get csv year
  csv_yr<-str_sub(csv_nm, -9, -5)
  # open file
  f_csv<-read_csv(csv_nm)
  
  # format column names for 1970s & 2020s only
  if(csv_yr == "1970s"){
    names(f_csv)[which(names(f_csv) == "Name")]<-"LU_Type"
    f_csv$LU_Type[which(f_csv$LU_Type == "Agricultural")]<-"Agriculture"
  }
  if(csv_yr == "2020s"){
    names(f_csv)[which(names(f_csv) == "LUSE_DESCR")]<-"LU_Type"
  }
  
  # create output file (hectares)
  ha_out<-f_csv %>%
    select(geoid20, LU_Type, AREA) %>% 
    group_by(geoid20, LU_Type) %>%
    pivot_wider(names_from = LU_Type,
                values_from = AREA,
                values_fill = 0,
                names_prefix = "HA_")
  # find if any land use category is missting 
  ha_cols<-names(ha_out)[names(ha_out) != "geoid20"]
  ha_miss<-setdiff(lu_cat, gsub("HA_", "", ha_cols))
  if(length(ha_miss) != 4){
    for(h in 1:length(ha_miss)){
      ha_out[[paste0("HA_", ha_miss[h])]]<-0
    }
  }
  
  # create output file (percentage)
  pct_out<-f_csv %>%
    select(geoid20, LU_Type, PERCENTAGE) %>% 
    group_by(geoid20, LU_Type) %>%
    pivot_wider(names_from = LU_Type,
                values_from = PERCENTAGE,
                values_fill = 0,
                names_prefix = "PCT_")
  # find if any land use category is missting 
  pct_cols<-names(pct_out)[names(pct_out) != "geoid20"]
  pct_miss<-setdiff(lu_cat, gsub("PCT_", "", pct_cols))
  if(length(pct_miss) != 4){
    for(p in 1:length(pct_miss)){
      pct_out[[paste0("PCT_", pct_miss[p])]]<-0
    }
  }
  
  # format output table
  out_table<-unique(f_csv[,1:2])
  dim(out_table)
  
  # merge with area output
  out_table<-merge(out_table, ha_out)
  # merge with percentage output
  out_table<-merge(out_table, pct_out)

  # save output file
  write_csv(out_table, paste0(outDir, "Tract_by_LU/Tract_LU_Type_", csv_yr, ".csv"))
  
  # table by land use over years
  names(ha_out)[names(ha_out) != "geoid20"]<-paste0(names(ha_out)[names(ha_out) != "geoid20"], "_", csv_yr)
  names(pct_out)[names(pct_out) != "geoid20"]<-paste0(names(pct_out)[names(pct_out) != "geoid20"], "_", csv_yr)
  
  # separate land use columns
  ha_ag_col<-select(ha_out, c("geoid20", starts_with("HA_Agriculture")))
  ha_con_col<-select(ha_out, c("geoid20", starts_with("HA_Conservation")))
  ha_urb_col<-select(ha_out, c("geoid20", starts_with("HA_Urban")))
  ha_un_col<-select(ha_out, c("geoid20", starts_with("HA_Unclassified")))
  
  pct_ag_col<-select(pct_out, c("geoid20", starts_with("PCT_Agriculture")))
  pct_con_col<-select(pct_out, c("geoid20", starts_with("PCT_Conservation")))
  pct_urb_col<-select(pct_out, c("geoid20", starts_with("PCT_Urban")))
  pct_un_col<-select(pct_out, c("geoid20", starts_with("PCT_Unclassified")))
  
  # create data frame per land use type
  if(f == 1){
    ha_Agriculture<-ha_ag_col
    ha_Conservation<-ha_con_col
    ha_Urban<-ha_urb_col
    ha_Unclassified<-ha_un_col
    
    pct_Agriculture<-pct_ag_col
    pct_Conservation<-pct_con_col
    pct_Urban<-pct_urb_col
    pct_Unclassified<-pct_un_col
    
    # change over time
    ag_change<-ha_ag_col[,1]
    con_change<-ha_ag_col[,1]
    urb_change<-ha_ag_col[,1]
    un_change<-ha_ag_col[,1]
    
  }else{
    ha_Agriculture<-merge(ha_Agriculture, ha_ag_col)
    ha_Conservation<-merge(ha_Conservation, ha_con_col)
    ha_Urban<-merge(ha_Urban, ha_urb_col)
    ha_Unclassified<-merge(ha_Unclassified, ha_un_col)

    pct_Agriculture<-merge(pct_Agriculture, pct_ag_col)
    pct_Conservation<-merge(pct_Conservation, pct_con_col)
    pct_Urban<-merge(pct_Urban, pct_urb_col)
    pct_Unclassified<-merge(pct_Unclassified, pct_un_col)
    
    # calculate difference
    ha_ag_delta<-ha_ag_col[,2] - ha_Agriculture[,f]
    pct_ag_delta<-pct_ag_col[,2] - pct_Agriculture[,f]
    ha_con_delta<-ha_con_col[,2] - ha_Conservation[,f]
    pct_con_delta<-pct_con_col[,2] - pct_Conservation[,f]
    ha_urb_delta<-ha_urb_col[,2] - ha_Urban[,f]
    pct_urb_delta<-pct_urb_col[,2] - pct_Urban[,f]
    ha_un_delta<-ha_un_col[,2] - ha_Unclassified[,f]
    pct_un_delta<-pct_un_col[,2] - pct_Unclassified[,f]
    # add to data frame
    ag_change<-cbind(ag_change, ha_ag_delta, pct_ag_delta)
    con_change<-cbind(con_change, ha_con_delta, pct_con_delta)
    urb_change<-cbind(urb_change, ha_urb_delta, pct_urb_delta)
    un_change<-cbind(un_change, ha_un_delta, pct_un_delta)
    
  }

} # END f

# save change calculations
write_csv(ag_change, paste0(outDir, "LU_Tract_Change/Agriculture_deltas.csv"))
write_csv(con_change, paste0(outDir, "LU_Tract_Change/Conservation_deltas.csv"))
write_csv(urb_change, paste0(outDir, "LU_Tract_Change/Urban_deltas.csv"))
write_csv(un_change, paste0(outDir, "LU_Tract_Change/Unclassified_deltas.csv"))

#############
### PLOTS ###
#############

# land use AREA by category
ha_lu_sum<-tibble(Year = c(1820, 1870, 1890, 1907, 1950, 1960, 1970, 1980, 2000, 2020), 
                  Agriculture = colSums(ha_Agriculture)[-1],
                  Conservation = colSums(ha_Conservation)[-1],
                  Urban = colSums(ha_Urban)[-1],
                  Unclassified = colSums(ha_Unclassified)[-1])
rowSums(ha_lu_sum[,-1])
# land use over the years
ha_lu_long<-ha_lu_sum %>%
  pivot_longer(cols = lu_cat,
               names_to = "land_use_type", 
               values_to = "area_sum")

# BAR PLOT: Land Use Category X Time X Area (pct)
ggplot(ha_lu_long, aes(x = as.factor(Year), y = area_sum, fill = land_use_type)) +
  geom_col(position = "fill") +
  ggtitle("Land Use Classification in Puʻuloa Through the Years") + 
  labs(x = "Years from Selected Time Periods", 
       y = "Percentage of Area (ha) in Puʻuloa",
       fill = "Land Use Category") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) 
# save plot
ggsave(paste0(outDir, "images/LandUse_Time_Periods-pct.png"))

# LINE PLOT: Land Use Category X Time X Area (ha)
ggplot(ha_lu_long, aes(x = as.factor(Year), y = area_sum, group = land_use_type, color = land_use_type)) +
  geom_line(lwd = 1) + geom_point(size = 2) + 
  ggtitle("Total Land Use Area in Puʻuloa Through the Years") + 
  labs(x = "Years from Selected Time Periods", 
       y = "Total Area (ha)",
       color = "Land Use Category") + 
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# save plot
ggsave(paste0(outDir, "images/LandUse_Time_Periods-ha.png"))

# PLOT: Land Use Category X Time X Trends
ggplot(ha_lu_long, aes(x = as.factor(Year), y = area_sum, group = land_use_type, color = land_use_type)) +
  geom_point(size = 2) + 
  geom_labelsmooth(aes(label = land_use_type), fill = "white", 
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4, 
                   hjust = 0.05, show.legend = F) +
  ylim(-2600, 12600) + 
  ggtitle("Land Use Area Change in Puʻuloa Through the Years") + 
  labs(x = "Years from Selected Time Periods", 
       y = "Change in Area (ha)",
       color = "Land Use Category") + 
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# save plot
ggsave(paste0(outDir, "images/LandUse_Area_Time-trends.png"))

# sum of CHANGE in land use area per year
ha_change_sum<-tibble(Year = c(1870, 1890, 1907, 1950, 1960, 1970, 1980, 2000, 2020), 
                      Agriculture = colSums(select(ag_change, starts_with("HA_"))[,-1]),
                      Conservation = colSums(select(con_change, starts_with("HA_"))[,-1]),
                      Urban = colSums(select(urb_change, starts_with("HA_"))[,-1]),
                      Unclassified = colSums(select(un_change, starts_with("HA_"))[,-1]))
# land use over the years
ha_change_long<-ha_change_sum %>%
  pivot_longer(cols = lu_cat,
               names_to = "land_use_type", 
               values_to = "area_sum")

# PLOT: Land Use Change X Time X Area (ha)
ggplot(ha_change_long, aes(x = as.factor(Year), y = area_sum, group = land_use_type, color = land_use_type)) +
  geom_line(lwd = 1) + geom_point(size = 2) + 
  ggtitle("Land Use Area Change in Puʻuloa Through the Years") + 
  labs(x = "Years from Selected Time Periods", y = "Change in Area (ha)", 
       color = "Land Use Category") + 
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# save plot
ggsave(paste0(outDir, "images/LandUse_Area_Change.png"))

# PLOT: Land Use Change X Time X Trends
ggplot(ha_change_long, aes(x = as.factor(Year), y = area_sum, group = land_use_type, color = land_use_type)) +
  geom_point(size = 2) + 
  geom_labelsmooth(aes(label = land_use_type), fill = "white", 
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4,
                   hjust = 0.05, show.legend = F)+
  ggtitle("Land Use Area Change in Puʻuloa Through the Years") + 
  labs(x = "Years from Selected Time Periods", 
       y = "Change in Area (ha)",
       color = "Land Use Category") + 
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# save plot
ggsave(paste0(outDir, "images/LandUse_Area_Change-trends.png"))

###########
### END ###
###########