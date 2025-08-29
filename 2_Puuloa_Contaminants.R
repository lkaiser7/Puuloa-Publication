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
library(dplyr)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(forcats)

# load data
cont_csv<-read_csv(paste0(rootDir, "contaminant_data/Historic Contamination Points  - Contaminants.csv"))
cont_csv
names(cont_csv)

##############
### FORMAT ###
##############

# # contaminant
# table(cont_csv$Contaminant)
# unique(cont_csv$Contaminant)

### contamination type
table(cont_csv$`Contaminant Type`, useNA = "ifany")
sort(unique(cont_csv$`Contaminant Type`))
# format contamination type names 
cont_df<-cont_csv %>%
  mutate(`Contaminant Type` = case_when(
    `Contaminant Type` == "Dioxin/Furans" ~ "Dioxins",
    `Contaminant Type` == "Dioxins/furans" ~ "Dioxins",
    `Contaminant Type` == "Dioxins/Furans" ~ "Dioxins",
    
    `Contaminant Type` == "Halogenated hydrocarbon" ~ "Legacy Pesticides",
    `Contaminant Type` == "Organochlorine pesticide" ~ "Legacy Pesticides",

    `Contaminant Type` == "Heavy metal" ~ "Metals",
    `Contaminant Type` == "Metalloid" ~ "Metals",
    
    `Contaminant Type` == "Per-and poly-fluoroalkyl substances (PFAS)" ~ "Per-and Poly-Fluoroalkyl Substances (PFAS)",
    
    `Contaminant Type` == "Synthetic organic pesticide" ~ "Pesticides",
    `Contaminant Type` == "Triazine herbicide" ~ "Pesticides",
    
    `Contaminant Type` == "Hydrocarbon (petroleum)" ~ "Petroleum",
    `Contaminant Type` == "Petroleum hydrocarbon" ~ "Petroleum",
    
    `Contaminant Type` == "Chlorinated hdrocarbon" ~ "Polychlorinated Biphenyls (PCBs)",
    `Contaminant Type` == "Chlorinated hydrocarbon" ~ "Polychlorinated Biphenyls (PCBs)",

    `Contaminant Type` == "Polycyclic aromatic hydrocarbon (PAHs)" ~ "Polycyclic Aromatic Hydrocarbons (PAHs)",
    
    `Contaminant Type` == "Volatile organic compound (VOCs)" ~ "Volatile Organic Compounds (VOCs)",
    
    `Contaminant Type` == "Heavy metal, Chlorinated hydrocarbon" ~ "Multiple Contaminants",
    `Contaminant Type` == "Heavy metal, Hydrocarbon (petroleum), PAHs, PCBs" ~ "Multiple Contaminants",
    `Contaminant Type` == "Heavy metal, Organochlorine pesticide" ~ "Multiple Contaminants",
    `Contaminant Type` == "Heavy metal, Petroleum hydrocarbon, Pesticide" ~ "Multiple Contaminants",
    `Contaminant Type` == "Particulate Matter (PM), Heavy metal, Carbon Dioxide, Petroleum hydrocarbon" ~ "Multiple Contaminants",
    
    TRUE ~ `Contaminant Type`  # Keep other values unchanged
  ))
# table output 
table(cont_df$`Contaminant Type`)
sort(unique(cont_df$`Contaminant Type`))

# # contamination area
# table(cont_csv$`Contamination Area`)
# sort(unique(cont_csv$`Contamination Area`))

# # contamination source
# table(cont_csv$`Contamination Source`)
# sort(unique(cont_csv$`Contamination Source`))

### land use type
table(cont_csv$`Land Use Type`, useNA = "ifany")
sort(unique(cont_csv$`Land Use Type`))
# # format land use type names 
# cont_df<-cont_df %>%
#   mutate(`Land Use Type` = case_when(
#     `Land Use Type` == "Agricultural" ~ "Agriculture",
#     `Land Use Type` == "Agriculture, Industrial" ~ "Agriculture/Industrial",
#     `Land Use Type` == "Agriculture, Military" ~ "Agriculture/Military",
#     `Land Use Type` == "Agriculture, Residential" ~ "Agriculture/Residential",
#     `Land Use Type` == "Agriculture,Urban" ~ "Agriculture/Urban",
#     
#     `Land Use Type` == "Industrial/residential" ~ "Industrial/Residential",
#     
#     `Land Use Type` == "Military, Industrial" ~ "Military/Industrial",
#     
#     `Land Use Type` == "Residential/agricultural" ~ "Residential/Agriculture",
#     `Land Use Type` == "Residential/agricultural" ~ "Residential/Agriculture",
#     `Land Use Type` == "Residential/Commerical" ~ "Residential/Commercial",
#     
#     `Land Use Type` == "Urban and Agricultural" ~ "Urban/Agriculture",
#     `Land Use Type` == "Urban, Residential" ~ "Urban/Residential",
#     
#     `Land Use Type` == "N/A" ~ NA,
#     
#     TRUE ~ `Land Use Type`  # Keep other values unchanged
#   ))
# table output 
table(cont_df$`Land Use Type`)
sort(unique(cont_df$`Land Use Type`))

# date of event
table(cont_csv$`Date of Pollution Event`, useNA = "ifany")
sort(unique(cont_csv$`Date of Pollution Event`))

# remove NAs
cont_csv$`Date of Pollution Event`[which(is.na(cont_csv$`Date of Pollution Event`))]<-"Unknown"
# format years
cont_yrs<-cont_csv$`Date of Pollution Event`

# add start year
cont_df$`Start Year`<-sub("\\-.*", "", cont_yrs)
cont_df$`Start Year`<-sub("s", "", cont_df$`Start Year`)
table(cont_df$`Start Year`, useNA = "ifany")
# add end year
cont_df$`End Year`<-str_split(cont_yrs, "\\-", simplify=T)[,2]
cont_df$`End Year`<-sub("s", "", cont_df$`End Year`)
cont_df$`End Year`[which(cont_df$`End Year` == "")]<-"Unknown"
cont_df$`End Year`[which(cont_df$`End Year` == "Current")]<-"2020"
table(cont_df$`End Year`, useNA = "ifany")

### ongoing or historical 
table(cont_csv$`Ongoing or Historic Contamination`, useNA = "ifany")
sort(unique(cont_csv$`Ongoing or Historic Contamination`))
# # format historical or contamination names 
# cont_df<-cont_df %>%
#   mutate(`Ongoing or Historic Contamination` = case_when(
#     `Ongoing or Historic Contamination` == "Contained" ~ "Ongoing, Contained",
#     `Ongoing or Historic Contamination` == "Historic" ~ "Historical",
#     `Ongoing or Historic Contamination` == "Historic Contamination, Ongoing Issue" ~ "Historical, Ongoing",
#     `Ongoing or Historic Contamination` == "Historic, Contained" ~ "Historical, Contained",
#     `Ongoing or Historic Contamination` == "N/A" ~ NA,
#     
#     TRUE ~ `Ongoing or Historic Contamination`  # Keep other values unchanged
#   ))
# table output 
table(cont_df$`Ongoing or Historic Contamination`, useNA = "ifany")
sort(unique(cont_df$`Ongoing or Historic Contamination`))

### sample media 
table(cont_csv$`Media where contaminant was measured`, useNA = "ifany")
sort(unique(cont_csv$`Media where contaminant was measured`))
# format media names 
cont_df<-cont_df %>%
  mutate(`Media where contaminant was measured` = case_when(
    # Sediment
    `Media where contaminant was measured` == "Avg of sediment samples (S. Channel, SE Loch)" ~ "Sediment",
    `Media where contaminant was measured` == "Pearl Harbor Sediment (max. at any depth)" ~ "Sediment",
    
    # Groundwater
    `Media where contaminant was measured` == "Groundwater (Kunia Well)" ~ "Groundwater",
    `Media where contaminant was measured` == "Groundwater (monitoring wells 27-30 ft bgs)" ~ "Groundwater",
    
    # Soil
    `Media where contaminant was measured` == "Discrete soil sample" ~ "Soil (depth not specified)",
    `Media where contaminant was measured` == "HDOH EAL: 2.1 mg/kg; C/I: 8.7 mg/kg" ~ "Soil (depth not specified)",
    `Media where contaminant was measured` == "Soil" ~ "Soil (depth not specified)",
    `Media where contaminant was measured` == "Soil (0-14 ft bgs)" ~ "Soil (depth not specified)",
    `Media where contaminant was measured` == "Soil (depth not specified)" ~ "Soil (depth not specified)",
    `Media where contaminant was measured` == "Soil (max conc.)" ~ "Soil (depth not specified)",
    `Media where contaminant was measured` == "Soil (surface and subsurface)" ~ "Soil (depth not specified)",
    `Media where contaminant was measured` == "Soil Core/Well sample" ~ "Soil (depth not specified)",
    `Media where contaminant was measured` == "Soil near UST" ~ "Soil (depth not specified)",
    `Media where contaminant was measured` == "Soil Stockpile" ~ "Soil (depth not specified)",
    `Media where contaminant was measured` == "Subsurface Soil" ~ "Soil (depth not specified)",
    # Shallow Soil 0-36" (0-3')
    `Media where contaminant was measured` == "Soil (0-1 ft bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Soil (0-6 in bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "soil (2-3 ft bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Soil (6-12 in bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Surface soil" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Surface Soil" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Surface Soil (0-0.5 ft bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Surface Soil (0-1 ft bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Surface soil (0-6 in)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Surface Soil (0.1-1 ft bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Surface soil (0.5-1 ft bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Surface soil (1-2 ft bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Soil (1.5 ft bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Soil (2-3 ft bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Soil (2 ft bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Surface Soil (0-6 in)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Surface Soil (1 ft bgs)" ~ "Shallow Soil (0-3 ft.)",
    `Media where contaminant was measured` == "Surface Soil (2-10 in bgs)" ~ "Shallow Soil (0-3 ft.)",
     # Deep 36+" (3+')
    `Media where contaminant was measured` == "Soil (13-17 ft bgs)" ~ "Deep Soil (3+ ft.)",
    `Media where contaminant was measured` == "Soil (20-22 ft bgs)" ~ "Deep Soil (3+ ft.)",
    `Media where contaminant was measured` == "soil (4.5-6 ft bgs)" ~ "Deep Soil (3+ ft.)",
    `Media where contaminant was measured` == "Soil (5.5ft deep)" ~ "Deep Soil (3+ ft.)",
    `Media where contaminant was measured` == "Soil (6-7 ft bgs)" ~ "Deep Soil (3+ ft.)",
    `Media where contaminant was measured` == "Soil (9 ft bgs)" ~ "Deep Soil (3+ ft.)",
    `Media where contaminant was measured` == "Soil (~46 ft bgs)" ~ "Deep Soil (3+ ft.)",
    `Media where contaminant was measured` == "Soil (4.5-6 ft bgs)" ~ "Deep Soil (3+ ft.)",
    
    # Water
    `Media where contaminant was measured` == "Water (8 ft bgs)" ~ "Water",
    `Media where contaminant was measured` == "Water sample (max. concentration at any depth)" ~ "Water",
    `Media where contaminant was measured` == "Water sample (Middle/East loch)" ~ "Water",
    `Media where contaminant was measured` == "Stormwater runoff" ~ "Water",
    
    # Other
    `Media where contaminant was measured` == "Concrete" ~ "Other",
    `Media where contaminant was measured` == "Human Breast Milk" ~ "Other",
    `Media where contaminant was measured` == "NA" ~ "Other",
    
    TRUE ~ `Media where contaminant was measured`  # Keep other values unchanged
  ))
cont_df$`Media where contaminant was measured`[which(is.na(cont_df$`Media where contaminant was measured`))]<-"Other"
# table output 
table(cont_df$`Media where contaminant was measured`, useNA = "ifany")
sort(unique(cont_df$`Media where contaminant was measured`))

# save formatted data frame
write_csv(cont_df, "contaminant_data/Contamination_Formatted.csv")

################
### ANALYSIS ###
################

# # save plot
# png(paste0(outDir, "images/contamination_count.png"), 
#     width = 8.71, height = 5.09, units = "in", res = 320)
# # Increase margin size
# par(mar=c(4,16,4,4))
# # PLOT: Count of `Contaminant Type`
# barplot(rev(table(cont_df$`Contaminant Type`)), las = 1, horiz = T,
#         col = rev(brewer.pal(n = 10, name = "Set3")),
#         cex.names = 0.9, main = "Puʻuloa Documented Incidents by Contaminant Type")
# dev.off()

# table contaminants 
cont_table<-as.data.frame(table(cont_df$`Contaminant Type`))
# PLOT: Contaminant (Var) Count (Freq)
ggplot(cont_table, aes(reorder(Var1, Freq), Freq, fill = Var1)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  ggtitle("Puʻuloa Reported Contamination by Type") + 
  labs(x = "", y = "Counts of Reported Contamination") + coord_flip() +
  theme_bw()  + theme(plot.title = element_text(hjust = 0.5),
                      legend.position = "none")
# save plot
ggsave(paste0(outDir, "images/contamination_count.png"), width = 8, height = 6)

# PLOT: `Contaminant Type` X Ahupuaʻa X `Ongoing or Historic Contamination`
ggplot(cont_df, aes(x = `Start Year`, xend = `End Year`, 
                    y = fct_rev(Ahupuaʻa), yend = Ahupuaʻa, 
                    col = `Contaminant Type`)) +
  geom_segment(linewidth = 3) +
  scale_color_brewer(type = "qual", palette = "Set3") +
  ggtitle("Contamination over Time by Ahupuaʻa") + labs(x = "Year", y = "Ahupuaʻa") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),
                     axis.text.x = element_text(size = 6, angle = 45, hjust = 1))
# save plot
ggsave(paste0(outDir, "images/ahupuaa_contamination_time.png"))

# # PLOT: `Contaminant Type` X Ahupuaʻa X `Ongoing or Historic Contamination`
# ggplot(cont_df, aes(x = `Start Year`, y = fct_rev(Ahupuaʻa), 
#                     col = `Contaminant Type`)) +
#   # geom_segment(size = 1, arrow = arrow(length = unit(0.08, "inches"))) + 
#   geom_segment(size = 3.5, aes(xend = `End Year`, yend = Ahupuaʻa),
#                color = "black", alpha = 1, show.legend = F) +
#   geom_segment(size = 3, aes(xend = `End Year`, yend = Ahupuaʻa)) +
#   scale_color_brewer(type = "qual", palette = "Set3") +
#   scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
#   theme_bw() + theme(plot.title = element_text(hjust = 0.5),
#                      axis.text.x = element_text(angle = 45, hjust = 1))

# # PLOT: Ahupuaʻa X `Ongoing or Historic Contamination`
# ggplot(cont_df, aes(x = `Start Year`, xend = `End Year`,
#                     y = fct_rev(Ahupuaʻa), yend = Ahupuaʻa,
#                     col = `Ongoing or Historic Contamination`)) +
#   geom_segment(size = 3) +
#   scale_color_brewer(type = "qual", palette = "Set3") +
#   ggtitle("Contamination over Time by Ahupuaʻa") + labs(x = "Year", y = "Ahupuaʻa") +
#   theme_bw() + theme(plot.title = element_text(hjust = 0.5))

# PLOT: `Contaminant Type` X `Ongoing or Historic Contamination`
ggplot(cont_df, aes(x = fct_rev(`Ongoing or Historic Contamination`), fill = `Contaminant Type`)) + 
  geom_bar(position="fill") +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  ggtitle("Historical or Ongoing Contamination by Type") + labs(x = NULL, y = NULL) + 
  coord_flip() + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# save plot
ggsave(paste0(outDir, "images/historical_ongoing_contamination.png"))

# PLOT: `Contaminant Type` X `Land Use Type`
ggplot(cont_df, aes(x = fct_rev(`Land Use Type`), fill = `Contaminant Type`)) + 
  geom_bar() + 
  #geom_bar(position="fill") + 
  scale_fill_brewer(type = "qual", palette = "Set3") +
  ggtitle("Contaminant Type by Land Use") + labs(x = NULL, y = NULL) + 
  coord_flip() + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# save plot
ggsave(paste0(outDir, "images/landuse_contamination-nofill.png"))
#ggsave(paste0(outDir, "images/landuse_contamination.png"))

# PLOT: `Contaminant Type` X `Media where contaminant was measured`
ggplot(cont_df, aes(x = fct_rev(`Media where contaminant was measured`), fill = `Contaminant Type`)) + 
  geom_bar(position="fill") + 
  #scale_x_discrete(limits = rev(levels(cont_df$`Media where contaminant was measured`))) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  ggtitle("Contaminant Type by Sample Media") + labs(x = NULL, y = NULL) + 
  coord_flip() + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# save plot
ggsave(paste0(outDir, "images/media_contamination.png"))

### Mia's Plots ###

# remove missing ahupuaa (location in water)
cont_na<-cont_df[which(complete.cases(cont_df$Ahupuaʻa)),]

# PLOT: Ahupuaʻa X `Contaminant Type`
ggplot(cont_na, aes(x = fct_rev(fct_infreq(Ahupuaʻa)), fill = `Contaminant Type`)) + 
  geom_bar() + coord_flip() + 
  scale_fill_brewer(type = "qual", palette = "Set3") +
  ggtitle("Contaminant Type by Ahupuaʻa") + 
  labs(x = "Ahupuaʻa", y = "Reported Contamination Incident Count") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# save plot
ggsave(paste0(outDir, "images/ahupuaa_contamination.png"), width = 8, height = 6)

# PLOT: Ahupuaʻa X `Contaminant Type` X `Land Use Type`
ggplot(cont_na, aes(x = fct_rev(Ahupuaʻa), fill = `Contaminant Type`)) + 
  geom_bar() + coord_flip() + 
  scale_fill_brewer(type = "qual", palette = "Set3") +
  facet_wrap(~`Land Use Type`) +
  ggtitle("Contaminant Type by Land Use per Ahupuaʻa") + 
  labs(x = "Ahupuaʻa", y = "Conaminant Count") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# save plot
ggsave(paste0(outDir, "images/ahupuaa_landuse_contamination.png"))

# length of contamination
cont_yr<-cont_df[which(cont_df$`Start Year` != "Unknown"),]
cont_yr<-cont_yr[which(cont_yr$`End Year` != "Unknown"),]
cont_yr<-cont_yr[which(complete.cases(cont_yr$Ahupuaʻa)),]
cont_yr$Duration<-as.numeric(cont_yr$`End Year`) - as.numeric(cont_yr$`Start Year`)

# PLOT: Ahupuaʻa X `Contaminant Type` X `Land Use Type`
ggplot(cont_yr, aes(x = fct_rev(Ahupuaʻa), y = Duration, fill = `Contaminant Type`)) + 
  geom_col() + coord_flip() + 
  scale_fill_brewer(type = "qual", palette = "Set3") +
  facet_wrap(~`Land Use Type`) +
  ggtitle("Contamination Duration by Land Use per Ahupuaʻa") + 
  labs(x = "Ahupuaʻa", y = "Duration (Years)") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# save plot
ggsave(paste0(outDir, "images/ahupuaa_contamination_duration.png"))

###########
### END ###
###########