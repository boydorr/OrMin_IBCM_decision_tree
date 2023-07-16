#################################################################
#####  Results - Summary of HIGH-RISK bites   ######
#################################################################

# TO DO 
# CS to check if deid data is correct and uptodate (Katie wrote code and tested on older data)
# Remove any investigation code if not being used to simplify repo

rm(list=ls()) # Clean workspace

library(dplyr)
library(rgdal)
library(readr)
library(tidyverse)
library(forcats)
library(data.table)
library(ggsci)
library(ggpubr)
library(scales)
library(lubridate)
library(sp)
library(rgeos)
library(maptools)
library(sf)
library(cowplot)
library(patchwork)

# Import data - PHO, IBCM, & RADDL
PHO_patients <- read.csv("data/PHO_annual_bite_data.csv") # All bite patients from PHO records 
deaths <- read.csv("outputs/deaths_deid.csv", stringsAsFactors = FALSE) # Deidentified death data
IBCM <- read.csv("outputs/ibcm_deid.csv") # IBCM patient records - deidentified
investigations <- read.csv("outputs/invest_deid.csv") # IBCM animal investigations - deidentified
tests <- read.csv("outputs/RADDL_deid.csv") # OrMin confirmed animal cases from RADDL 
# Fix IBCM data for mapping
IBCM$MUNICIPALITY[grep("Calapan", IBCM$MUNICIPALITY)] <- "Calapan City"
IBCM$Loc_ID <- paste0(IBCM$PROVINCE, "-", IBCM$MUNICIPALITY)

# OMPH ABTC focus
OMPH_risk <- read.csv("data/OMPH_risk_data.csv") # OMPH risk data for extrapolation - @Catherine - where is the code to produce this?
OMPH <- read.csv("outputs/OMPH_deid.csv") # OMPH data for extrapolation 

# Geographic & population data 
municipalities <- read.csv("data/municipality.csv")
municipalities$name[grep("Calapan", municipalities$name)] <- "Calapan City"
mun = as.factor(municipalities$name)
municipalities$Loc_ID <- paste0("Oriental Mindoro-", municipalities$name)
mun_loc = as.factor(municipalities$Loc_ID)
pop <- sum(municipalities$population) # population of Oriental Mindoro 

# GIS
province <- readOGR("data/GIS/", "PHLsmallTEST_fixed", stringsAsFactors=FALSE)
municipality <- readOGR("data/GIS/", "PHL_municipality", stringsAsFactors=FALSE)
villages <- readOGR("data/GIS/", "PHL_village", stringsAsFactors=FALSE)
municipality_loc <- read.csv(paste0("data/GIS/","PHL_municipality_centroids.csv")); dim(municipality_loc) # 1647
village_loc <- read.csv(paste0("data/GIS/","PHL_village_centroids.csv")); dim(village_loc) # 41933

# transform to dataframe
shp_to_df <- function(shapefile){
  shapefile$id <- rownames(shapefile@data)
  shapefile_df <- fortify(shapefile, region = "id")
  shapefile_df <- left_join(shapefile_df, shapefile@data, by = "id")
  return(shapefile_df)
}

province_df <- shp_to_df(province) 
municipality_df <- shp_to_df(municipality)
municipality_df_ormin <- municipality_df %>% filter(NAME_1 == "Oriental Mindoro")

# Set required provinces  & subset shapefiles 
req_prov <- c("Oriental Mindoro") 
province_sub <- province[which(province$NAME_1 %in% req_prov),] #
municipality_sub <- municipality[which(municipality$NAME_1 %in% req_prov),] # 15 municipalities
villages_sub <- villages[which(villages$NAME_1 %in% req_prov),]; dim(villages_sub@data)
municipality_prov <- municipality_loc[grep("Oriental Mindoro-", municipality_loc$Loc_ID),] # centroids
village_prov <- village_loc[grep("Oriental Mindoro-", village_loc$Loc_ID),] # centroids

# Assign municipality locations
cols_trips_split <- strsplit(municipality_prov$Loc_ID, split = "-")
municipality_prov$PROVINCE <- vapply(cols_trips_split, FUN = function(x) x[1], FUN.VALUE = character(1))
municipality_prov$MUNICIPALITY <- vapply(cols_trips_split, FUN = function(x) x[2], FUN.VALUE = character(1))
ormin = subset(municipality_prov, PROVINCE == "Oriental Mindoro")
locs <- unique(municipality$Loc_ID[which(municipality$NAME_1 %in% "Oriental Mindoro")])

###############################################################################
# PHO data
# PHO bite patient stats for study period (2020-2022)
Total_patients <- sum(subset(PHO_patients, Year %in% 2020:2022)$bite_patients) # 33,947 patients from 2020-2022
Avg_patients_year <- mean(subset(PHO_patients, Year %in% 2020:2022)$bite_patients) # 11,316 average per year
Avg_patients_month <- Avg_patients_year / 12 # 943 average patients per month 
Overall_bite_inc_year <- Avg_patients_year * 100000/ pop # 1,246 per 100k incidence of patients seeking PEP 

# Summary characteristics for study period (2020-2022) fro PHO data
PHO_3y_summary <- PHO_patients %>% 
  filter(Year == "Total_3year_study")  %>% 
  summarize(
    dog = species_dog *100 / Total_patients, # % biting species
    cat = species_cat *100 / Total_patients,
    other = species_other*100 / Total_patients,
    ERIG = ERIG_given,
    CatIII = CAT_III,
    pc_ERIG = ERIG_given*100/ Total_patients, # % total bites receiving ERIG
    pc_ERIG_catIII = ERIG_given*100/ CAT_III) # % Cat III bites receiving ERIG 
PHO_3y_summary
  # Biting animals: 67.8% dogs,  31.5% cats, 0.74% other species 
  # 79.6% of total patients received ERIG 
  # 15.5% of Cat III patients received ERIG                                       

# Total IBCM records for first-visit patients
IBCM_tot <- IBCM %>% dplyr::summarise(n=n()); IBCM_tot # 11,501 records

# Percent of PHO recorded bites in IBCM database
IBCM_tot*100 / Total_patients # 33.88%

###############################################################################
##** Summarize OMPH risk data for extrapolation*
## % high-risk bites per year at OMPH 
years = as.character(2020:2022)
n <- high_risk_pc <- high_unk_risk_pc <- numeric(length = length(years))

for (i in 1:length(years)){
  n[i] <- subset(OMPH_risk, year_visit == years[i] & risk == "all")$n
  high_risk_pc[i] <- subset(OMPH_risk, year_visit == years[i] & risk == "high")$n/n[i]
  high_unk_risk_pc[i] <- subset(OMPH_risk, year_visit == years[i] & risk == "high_max")$n/n[i]
}
p_high_risk <- mean(high_risk_pc); p_high_risk ## Average proportion high risk = 0.01212938
p_high_risk_max <- mean(high_unk_risk_pc); p_high_risk_max ## Average proportion high/unknown risk = 0.04177898

Total_patients/3 * p_high_risk # 137 high-risk bite patients
Total_patients/3 * p_high_risk_max # 473 high + unknown risk bite patients 
subset(PHO_patients, Year %in% 2020:2022)$bite_patients * p_high_risk
subset(PHO_patients, Year %in% 2020:2022)$bite_patients * p_high_risk_max

###############################################################################
### Alternative risk assessment variable
OMPH$risk <- "low"
OMPH$risk[which(OMPH$RISK_ASSESSMENT=="suspicious_for_rabies" | OMPH$ALIVE=="no" | OMPH$SUSPECT=="yes")] <- "high"
OMPH$risk[which(OMPH$RISK_ASSESSMENT=="unknown" | OMPH$ALIVE=="unknown")] <- "unknown" 

OMPH_yr <- OMPH %>%
  group_by(year_visit) %>%
  dplyr::summarise(bites =n(), 
                   dog_bites = length(which(ANIMAL == "dog")),
                   high_risk_dogs = length(which(risk == "high" & ANIMAL == "dog")),
                   high_risk = length(which(risk == "high")),
                   unk_risk = length(which(risk == "unknown"))) 
p_high_risk_omph <- mean(OMPH_yr$high_risk_dogs/OMPH_yr$bites); p_high_risk ## Average proportion high risk = 0.01212938
p_high_risk_max_omph <- mean((OMPH_yr$high_risk + OMPH_yr$unk_risk)/OMPH_yr$bites); p_high_risk_max ## Average proportion high/unknown risk = 0.04177898

###############################################################################
######## RISK ##########
###############################################################################
### Create risk assessment variable
IBCM$risk <- "low"
IBCM$risk[which(IBCM$RISK_ASSESSMENT=="suspicious_for_rabies" | IBCM$ALIVE=="no" | IBCM$SUSPECT=="yes")] <- "high"
IBCM$risk[which(IBCM$RISK_ASSESSMENT=="unknown" | IBCM$ALIVE=="unknown")] <- "unknown" # patients$SUSPECT=="do_not_know"
risk_factor = ordered(levels(factor(IBCM$risk)), levels = c("low", "unknown", "high"))

## Summary of bites categorized by RISK in OrMIN (all records)
OrMIN_RISK <- IBCM %>% 
  group_by(risk) %>%
  dplyr::summarize(
    n=n(), # high-253 : unknown-331 : low-10,917
    male = length(which(SEX == "male")), ## Sex of high-risk bite patients (demographics)  
    female = length(which(SEX == "female")), # HIGH-RISK: 132 female & 121 male 
    dog = length(which(ANIMAL == "dog")), ## Biting animal for high-risk bites 
    cat = length(which(ANIMAL == "cat")), # HIGH RISK: dog-199, cat-54
    I = length(which(CATEGORY == "I")), ## Category of exposure for high-risk 
    II = length(which(CATEGORY == "II")), 
    III = length(which(CATEGORY == "III")), # HIGH-RISK: CatI-2, CatII-151, CatIII-100
    dead = length(which(ALIVE == "no")), # Biting animal DEAD
    dead_unk = length(which(ALIVE == "no"| ALIVE == "unknown")), # HIGH-RISK: dead-227, alive-26
    alive = length(which(ALIVE == "yes")),
    suspect = length(which(SUSPECT == "yes")),
    suspect_unk = length(which(SUSPECT == "yes"| SUSPECT == "do_not_know")), # HIGH-RISK: yes-17, no-177, do_not_know-59
    risk_assess_suspect = length(which(RISK_ASSESSMENT == "suspicious_for_rabies")), # Biting animal RISK_ASSESSMENT for rabies 
    risk_assess_sick = length(which(RISK_ASSESSMENT == "sick_not_rabies")),
    risk_assess_healthy = length(which(RISK_ASSESSMENT == "healthy"))) # suspicious 68,  sick 26,  healthy-159
OrMIN_RISK 

# Proportion high-risk & unknown-risk bites out of total bites 
Prop_high_risk <- subset(OrMIN_RISK, risk == "unknown" | risk == "high")$n/sum(OrMIN_RISK$n) 

# Extrapolate to all patients in Oriental Mindoro from PHO records   
Total_patients * Prop_high_risk[1]  # 747 high-risk in 3 years Extra_high_risk_3year <- 
Avg_patients_year * Prop_high_risk[1]  # 249 high-risk per year - Extra_high_risk_per_year <-  

# IBCM first visit patients by year 
IBCM_yr <- IBCM %>%
  group_by(year_visit) %>%
  dplyr::summarise(bites =n(), 
                   dog_bites = length(which(ANIMAL == "dog")),
                   high_risk_dogs = length(which(risk == "high" & ANIMAL == "dog")),
                   high_risk = length(which(risk == "high")),
                   unk_risk = length(which(risk == "unknown"))) 

# Summarise deaths by year
death_summary <- deaths %>% 
  mutate(date = as.Date(DATE_DIED),
          year = year(date)) %>% 
  group_by(year) %>% 
  dplyr::summarise(n = n()) 
IBCM_yr$deaths <- subset(death_summary, year %in% 2020:2022)$n

### IBCM risk summary for bite patients by municipality (15 in total)
Mun_RISK <- IBCM %>%
  filter(!is.na(MUNICIPALITY)) %>% # remove the 3 NAs
  mutate(MUNICIPALITY = factor(MUNICIPALITY, levels = mun)) %>%
  group_by(MUNICIPALITY) %>%
  dplyr::summarise(bites =n(), 
                   dog_bites = length(which(ANIMAL == "dog")),
                   high_risk_dogs = length(which(risk == "high" & ANIMAL == "dog")),
                   high_risk = length(which(risk == "high")),
                   unk_risk = length(which(risk == "unknown"))) 
Mun_RISK

# Summarise deaths by municipality
death_municipality <- deaths %>% 
  mutate(date = as.Date(DATE_DIED),
         year = year(date),
         MUNICIPALITY = factor(MUNICIPALITY, levels = mun)) %>% 
  filter(year >2019)  %>% 
  group_by(MUNICIPALITY, .drop = F) %>% 
  dplyr::summarise(n = n()) 
Mun_RISK$deaths <- subset(death_municipality)$n

###############################################################################
####  Summarize Animal Investigations ####
###############################################################################
# Investigations for OrMIN only 
table(investigations$GII_NUMBER_OF_PEOPLE_BITTEN, useNA = "always") # 101 animals that bite >1 person 
table(investigations$LOCATION_OF_EVENT_MUNICIPALITY, useNA = "always") # investigations by municipality 
table(investigations$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_DONE, useNA = "always") # 17 LFD completed 
table(investigations$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME, useNA = "always") # 10 positive LFD 
table(investigations$GII_ANIMAL_OWNED, useNA = "always") # 61 owned  :  19 not owned  :  21 unknown 
table(investigations$ANIMAL_OUTCOME, useNA = "always") # 55 alive  :  30 dead   : 16 not found 

# Summarise animal cases by year
case_summary <- investigations %>% 
  mutate(date = as.Date(DATE_OF_INVESTIGATION),
         year = year(date)) %>% 
  group_by(year) %>% 
  dplyr::summarise(n = length(which(SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME == "positive"))) 
IBCM_yr$positive <- subset(case_summary, year %in% 2020:2022)$n

test_summary <- tests %>% 
  mutate(date = as.Date(as.POSIXlt(strptime(as.character(DATE_SUBMITTED), "%m/%d/%Y"))),
         year = year(date)) %>% 
  group_by(year) %>% 
  dplyr::summarise(n = n())

# Summarise cases by municipality
case_municipality <- investigations %>% 
  filter(!is.na(MUNICIPALITY)) %>% # remove the NAs
  mutate(MUNICIPALITY = factor(MUNICIPALITY, levels = mun)) %>% 
  group_by(MUNICIPALITY, .drop = F) %>% 
  dplyr::summarise(n = n(),
                   positive = length(which(SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME == "positive"))) 

test_municipality <- tests %>% 
  filter(!is.na(MUNICIPALITY)) %>% # remove the NAs
  mutate(MUNICIPALITY = factor(MUNICIPALITY, levels = mun)) %>% 
  group_by(MUNICIPALITY, .drop = F) %>% 
  dplyr::summarise(n = n())

# Save all the ibcm summary data
write.csv(IBCM_yr, "outputs/ibcm_summary.csv", row.names = FALSE) # Save to use in decision tree calculations
write.csv(Mun_RISK, "outputs/municipality_summary.csv", row.names = FALSE) # Save to use in decision tree calculations

###############################################################################
######  FIGURE
###############################################################################
## Time series of bite patient by risk categories in Oriental Mindoro (Jan 2020 - present)
risk_ts <- IBCM %>% 
  mutate(risk = factor(risk, levels = c("low", "unknown", "high"))) %>%
  mutate(month_visit = as.Date(month_visit)) %>%
  group_by(month_visit, risk, year_visit) %>% 
  dplyr::summarize(n=n(), 
                   high_risk = length(which(risk == "high")),
                   unknown_risk = length(which(risk == "unknown")), 
                   low_risk = length(which(risk == "low")))

# Stacked barplot
risk_ts_plot <- risk_ts %>%
  ggplot(aes(x = month_visit, y = n)) + 
  geom_col(aes(fill=risk)) +
  scale_fill_manual(values = c("high" = "red", "unknown" = "orange", "low" = "grey")) +
  labs(x = "", y = "IBCM patients", fill = "Risk Category") +
  theme_bw(base_size = 8) + 
  theme(panel.grid.major = element_blank(), 
        text = element_text(size = 12), legend.title = element_text(face = "bold"), 
        plot.title = element_text(size = 15, face = "bold")) 
risk_ts_plot
# ggsave("figs/Risk_TimeSeries.pdf", width = 8, height = 4) # Save as pdf 

###############################################################################
# Set up dataframe of risk_exposures
years = as.factor(2020:2022)
mun_loc = as.factor(municipalities$Loc_ID)

risk_exposures <- IBCM %>%
  filter(risk=="high") %>%
  mutate(Loc_ID = factor(Loc_ID, levels = mun_loc)) %>%
  mutate(year = factor(year_visit, levels = years)) %>%
  group_by(Loc_ID, year, .drop = F) %>%
  summarise(n = n())
risk_exposures <- merge(risk_exposures, municipalities, "Loc_ID")
risk_exposures$high_inc <- risk_exposures$n*100000/risk_exposures$pop

# Deaths
deaths$Loc_ID = paste0(deaths$PROVINCE, "-", deaths$MUNICIPALITY, "-", deaths$BARANGAY)
deaths$Lat <- village_prov$Latitude[match(deaths$Loc_ID, village_prov$Loc_ID)]
deaths$Lon <- village_prov$Longitude[match(deaths$Loc_ID, village_prov$Loc_ID)]

deaths_yr_ormin <- deaths %>%
  mutate(DATE = as.Date(DATE_DIED),
         year = year(DATE_DIED),
         Lat = as.numeric(Lat),
         Lon = as.numeric(Lon)) %>%
  filter(PROVINCE == "Oriental Mindoro" & !is.na(Lat))
deaths_yr_ormin$type <- "human death"
deaths_yr_ormin_short <- deaths_yr_ormin[c("type", "DATE", "year", "Loc_ID", "Lat", "Lon")]

# Confirmed cases
tests$Loc_ID = paste0(tests$PROVINCE, "-", tests$MUNICIPALITY, "-", tests$BARANGAY)
tests$Lat <- village_prov$Latitude[match(tests$Loc_ID, village_prov$Loc_ID)]
tests$Lon <- village_prov$Longitude[match(tests$Loc_ID, village_prov$Loc_ID)]

lab_cases_yr_ormin <- tests %>%
  mutate(DATE = as.Date(DATE_SUBMITTED),
         year = year(DATE_SUBMITTED),
         Lat = as.numeric(Lat),
         Lon = as.numeric(Lon)) %>%
  filter(PROVINCE == "Oriental Mindoro" & !is.na(Lat))
lab_cases_yr_ormin$type <- "confirmed"
# manual corrections
lab_cases_yr_ormin$Lat[which(lab_cases_yr_ormin$Loc_ID == "Oriental Mindoro-Roxas-Bagumbayan")] <- 12.5902022037386 # coords for Labangan Poblacion village
lab_cases_yr_ormin$Lon[which(lab_cases_yr_ormin$Loc_ID == "Oriental Mindoro-Roxas-Bagumbayan")] <- 121.520949114033 
# doesn't match properly WITH CENTROIDS!!!
# Bagumbayan present in df with the identical Loc_ID
lab_cases_yr_ormin_short <- lab_cases_yr_ormin[c("type", "DATE", "year", "Loc_ID", "Lat", "Lon")]
rabies_yr_ormin <- rbind(deaths_yr_ormin_short, lab_cases_yr_ormin_short)

# prepare maps by year
for (i in 1:length(years)){
  risk_exp <- risk_exposures %>% filter(year == years[i]) 
  locs_exp <- locs[-which(locs %in% risk_exp$Loc_ID)] # DF for risk maps - merge in study municipalities
  deaths_y <- deaths_yr_ormin %>% filter(year == years[i])
  lab_y <- lab_cases_yr_ormin %>% filter(year == years[i])
  cases_y <- rabies_yr_ormin %>% filter(year == years[i])
  
  # prep for map
  risk_municipality <- municipality[which(municipality$Loc_ID %in% risk_exp$Loc_ID),] # Subset shp for high-risk / rabies locations
  risk_municipality <- merge(risk_municipality, risk_exp, by = 'Loc_ID', duplicateGeoms = TRUE) # Merge in summary data to df
  risk_municipality_df <- shp_to_df(risk_municipality)
  
  # Produce map of high risk bite incidence by municipality
  if(years[i] == 2020){
    risk_map_inc_2020 <- ggplot() +
      geom_polygon(data=province_df, aes(x=long, y=lat, group=group), fill="grey", color="dimgrey") +
      geom_polygon(data=risk_municipality_df, aes(x=long, y=lat, group=group, fill=n)) +
      geom_polygon(data=province_df, aes(x=long, y=lat, group=group), fill=NA, color="dimgrey") +
      ggtitle(years[i]) +
      scale_fill_gradient(name="High-risk bites \n per 100,000 \n persons",
                        low="white", high="#cc0000",
                        limits=c(0, 40)) +
      geom_polygon(data = municipality_df_ormin, aes(x = long, y = lat, group = group), 
                 fill=NA, color="black", size = 0.2, alpha = 0.3) +
      geom_point(data = deaths_y, aes(x = Lon, y = Lat), shape = 23, fill = "black", colour = "yellow", size = 2) +
      geom_point(data = lab_y, aes(x = Lon, y = Lat), shape = 21, fill = "#CC0000", colour = "white", size = 2) +
      theme_void() + theme(legend.position= "none") +
      coord_fixed(xlim=c(120.3,121.6), ylim=c(12.2, 13.6))   
    risk_map_inc_2020
    # ggsave(paste0("figs/risk_map_inc_",years[i],".pdf"), height=5, width=8)
  }
  
  if(years[i] == 2021){
    risk_map_inc_2021 <- ggplot() +
      geom_polygon(data=province_df, aes(x=long, y=lat, group=group), fill="grey", color="dimgrey") +
      geom_polygon(data=risk_municipality_df, aes(x=long, y=lat, group=group, fill=n)) +
      geom_polygon(data=province_df, aes(x=long, y=lat, group=group), fill=NA, color="dimgrey") +
      ggtitle(years[i]) +
      scale_fill_gradient(name="High-risk bites \n per 100,000 \n persons",
                          low="white", high="#cc0000",
                          limits=c(0, 40)) +
      geom_polygon(data = municipality_df_ormin, aes(x = long, y = lat, group = group), 
                   fill=NA, color="black", size = 0.2, alpha = 0.3) +
      geom_point(data = lab_y, aes(x = Lon, y = Lat), shape = 21, colour = "white", fill = "#CC0000", size = 2, alpha = 1) +
      geom_point(data = deaths_y, aes(x = Lon, y = Lat), shape = 23, fill = "black", colour = "yellow", size = 2) +
      theme_void() + theme(legend.position="none") +
      coord_fixed(xlim=c(120.3,121.6), ylim=c(12.2, 13.6))   
    risk_map_inc_2021
  }
  
  if(years[i] == 2022){
    risk_map_inc_2022 <- ggplot() +
      geom_polygon(data=province_df, aes(x=long, y=lat, group=group), fill="grey", color="dimgrey") +
      geom_polygon(data=risk_municipality_df, aes(x=long, y=lat, group=group, fill=n)) +
      geom_polygon(data=province_df, aes(x=long, y=lat, group=group), fill=NA, color="dimgrey") +
      ggtitle(years[i]) +
      scale_fill_gradient(name="High-risk bites \n per 100,000 \n persons",
                          low="white", high="#cc0000",
                          limits=c(0, 40)) +
      geom_polygon(data = municipality_df_ormin, aes(x = long, y = lat, group = group), 
                   fill=NA, color="black", size = 0.2, alpha = 0.3) +
      geom_point(data = lab_y, aes(x = Lon, y = Lat), shape = 21, colour = "white", fill = "#CC0000", size = 2, alpha = 1) +
      geom_point(data = deaths_y, aes(x = Lon, y = Lat), shape = 23, fill = "black", colour = "yellow", size = 2) + # shape 21 better
      theme_void() + # theme(legend.position=c(1,0),legend.justification=c(1,0)) +
      coord_fixed(xlim=c(120.3,121.6), ylim=c(12.2, 13.6))   
    risk_map_inc_2022
  }
}
map_panel =  (risk_map_inc_2020 + risk_map_inc_2021 + risk_map_inc_2022)

fig3_combined <- ggarrange(risk_ts_plot,
          map_panel, 
          nrow = 2, labels=c("A", "B"), heights = c(1,1.6))
ggsave("figs/fig3_combined.jpeg", height = 6, width = 9)

