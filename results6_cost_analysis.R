#############################################################
#####  Code for Economic Analysis #####
#############################################################
# TO DO:
# CS to update file to include calculations requested from R3
rm(list=ls())

# Import libraries & source functions
library(tidyverse)
source("functions/Cost_analysis.R") # Costs of PEP 

# data
RiskYr <- read.csv("outputs/ibcm_summary.csv") # by year/risk category 
RiskYr$high_risk_min <- RiskYr$high_risk_dogs/ RiskYr$bites
RiskYr$high_risk_max <- (RiskYr$high_risk + RiskYr$unk_risk)/ RiskYr$bites
PHO_biteINFO <- read.csv("data/PHO_annual_bite_data.csv") # Data compiled from PHO annual bite reports 
PHO_bites <- subset(PHO_biteINFO, Year %in% 2020:2022)

# parameters
avg_ID_PEP <- 6 # Average number of ID injections of PEP per patient 
PEP_ID_cost <- 6.25 # Price of 1 ID injection of vaccine in $ USD 
avg_vial_PEP <- 2 # Average vial of PEP per patient 
PEP_vial_cost <- 25 # Price of 1 vial of vaccine in $ USD 
avg_vial_ERIG <- 2 # Average vial of ERIG per patient 
ERIG_cost <- 45 # Price 1 vial of ERIG 
pDeath <- 0.165 # Prob of developing rabies if bitten by rabid dog (ignore age & bite site)
DALY <- 37 # Number of DALYS per death from rabies 

Cost_PEPDataFRAME <- data.frame() # Creating an empty dataframe

# Loop through model
for (i in 1:nrow(RiskYr)) {
  # Running the model
  Est_costs <- Cost_analysis(Total_bites = PHO_bites$bite_patients[i], 
                             CatIII_bites = PHO_bites$CAT_III[i], 
                             ERIG_given = PHO_bites$ERIG_given[i],
                             N = 1000,
                             high_risk_min = round(PHO_bites$bite_patients[i] * RiskYr$high_risk_min[i]), 
                             high_risk_max = round(PHO_bites$bite_patients[i] * RiskYr$high_risk_max[i]),  
                             avg_ID_PEP, PEP_ID_cost, 
                             avg_vial_ERIG, ERIG_cost, 
                             pDeath, DALY)
  
## average model outputs 
Est_costs$year <- PHO_bites$Year[i]

## Add new rows to table in loop
  Cost_PEPDataFRAME <- rbind(Cost_PEPDataFRAME, Est_costs)
}

Est_overall_costs <- Cost_analysis(Total_bites = sum(PHO_bites$bite_patients), 
                           CatIII_bites = sum(PHO_bites$CAT_III), 
                           ERIG_given = sum(PHO_bites$ERIG_given),
                           N = 1000,
                           high_risk_min = min(round(PHO_bites$bite_patients * RiskYr$high_risk_min)), 
                           high_risk_max = max(round(PHO_bites$bite_patients * RiskYr$high_risk_max)),  
                           avg_ID_PEP, PEP_ID_cost, 
                           avg_vial_ERIG, ERIG_cost, 
                           pDeath, DALY)
Est_overall_costs$year <- "overall"

# write to csv
write.csv(t(rbind(Cost_PEPDataFRAME, Est_overall_costs)), 
          "outputs/Results_Cost_Analysis.csv")


# Back of the envelope economic costs
humans <- 113959168 
HDR <- 11
dogs <- humans/HDR
vax_cov <- 0.7
dog_vaccine <- 0.25
dogs * vax_cov * dog_vaccine #  i.e. A fraction of PEP! 
cost_per_dog_vax <- 2
dogs * vax_cov * cost_per_dog_vax # STILL MUCH CHEAPER