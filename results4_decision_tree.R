#################################################################
########  Decision Tree estimates  #########
#################################################################
# TO DO
# CS check code works in public repo as Katie wrote & tested code to local private repo only
# CS to address DALY calculation details 

# Clear environment 
rm(list = ls())

# Import libraries 
library(dplyr)
library(readr)
library(ggplot2)
library(rgdal)
library(tidyverse)
library(truncnorm)
library(data.table)
library(reshape2)
library(forcats)

# Import functions and dataframes 
source("functions/HelperFunctions.R")
source("functions/decision_tree.R")

pSeekDF <- read.csv("outputs/Prob_Seek_PEP.csv") # probability of seeking PEP
municipalities <- read.csv("data/municipality.csv")
RiskMun <- read.csv("outputs/municipality_summary.csv") # by municipality/risk category 
RiskYr <- read.csv("outputs/ibcm_summary.csv") # by year/risk category 

mun <- as.factor(municipalities$name)
pop <- sum(municipalities$population) # population of Oriental Mindoro 
RiskMun <- left_join(municipalities, RiskMun, by = c("name" = "MUNICIPALITY"))

# Probabilities 
pDeath <- 0.165 # developing rabies if bitten by rabid dog (ignore age & bite site)
mu <-  0.3862 # Mu for proportion of rabid dogs that bite a person 
k <- 0.7055 # Size for negative binomial for P(bite|rabid_dog)
pObsDeath <- 0.9 # Probability of human death being recorded 
pSeek <- subset(pSeekDF, Year=="overall")$pPEP # Exposures seeking PEP 
HDR <- c(3, 10) # human:dog ratio - reasonable range from 3-10

############################################
######  Run decision tree #######
############################################
N = 1000

# Run model for entire period
RiskOverall <- cbind(total = "total", data.frame(t(apply(RiskYr[,-1], 2, sum))))
DT <- decision_tree(N = N,
                    pop = pop * 3, HDR_min = HDR[1], HDR_max = HDR[2], # population x 3 because 3 year study!
                    pObsDeath = pObsDeath, pDeath = pDeath, mean_bites = mu, k_bites = k,
                    death_data = RiskOverall$deaths, 
                    high_risk_min = RiskOverall$high_risk_dogs, 
                    high_risk_max = RiskOverall$high_risk + RiskOverall$unk_risk,
                    confirmed_animal = RiskOverall$positive_dogs)
Qs <- apply(DT, 2, FUN=function(x){quantile(x, probs=c(0.025, 0.5, 0.975))})
write.csv(Qs, "outputs/decision_tree_overall_summary.csv", row.names = FALSE)

############################################
# Run model for each year
DT_df <- data.frame() # empty dataframe
# Loop through model
for (i in 1:nrow(RiskYr)) {
  # model
  DT_y <- decision_tree(N = N, 
                  pop = pop, HDR_min = HDR[1], HDR_max = HDR[2], 
                  pObsDeath = pObsDeath, pDeath = pDeath, mean_bites = mu, k_bites = k,
                  death_data = RiskYr$deaths[i], 
                  high_risk_min = RiskYr$high_risk_dogs[i], 
                  high_risk_max = RiskYr$high_risk[i] + RiskYr$unk_risk[i],
                  confirmed_animal = RiskYr$positive_dogs[i])
  
  # quantiles across model outputs 
  Q_y <- as.data.frame(apply(DT_y, 2, FUN=function(x){quantile(x, probs=c(0.025, 0.5, 0.975))}))

  ## fill table
  DatFram <- data.frame(
    Year = RiskYr$year_visit[i], 
    Recorded_deaths = RiskYr$deaths[i], 

    # Model estimates
    Est_deaths = paste0(round(Q_y$estimated_deaths[2], digits = 0), "  [", 
                        round(Q_y$estimated_deaths[1], digits=0), " - ", round(Q_y$estimated_deaths[3], digits=0), "]"),
    Est_exposures = paste0(round(Q_y$total_exposures[2], digits = 0), "  [", 
                           round(Q_y$total_exposures[1], digits = 0), " - ", round(Q_y$total_exposures[3], digits=0), "]"),
    Est_exposures_no_PEP = paste0(round(Q_y$exposures_not_seek[2], digits=0), "  [", 
                                  round(Q_y$exposures_not_seek[1], digits = 0), " - ", round(Q_y$exposures_not_seek[3], digits=0), "]"),
    Est_exposure_incidence = paste0(round(Q_y$exposure_incidence[2], digits=0), "  [", 
                                    round(Q_y$exposure_incidence[1], digits=0), " - ", round(Q_y$exposure_incidence[3], digits=0), "]"),
    Est_dog_pop = paste0(round(Q_y$dog_population[2], digits = 0), "  [", 
                         round(Q_y$dog_population[1], digits=0), " - ", round(Q_y$dog_population[3], digits=0), "]"),
    Recorded_positives = RiskYr$positive[i], 
    Est_rabid_dogs = paste0(round(Q_y$rabid_dogs[2], digits = 0), "  [", 
                            round(Q_y$rabid_dogs[1], digits = 0), " - ", round(Q_y$rabid_dogs[3], digits = 0), "]"),
    Pc_cases_confirmed = paste0(round(Q_y$percent_confirm[2], digits = 2), " %  [", 
                                round(Q_y$percent_confirm[1], digits = 2), " - ", round(Q_y$percent_confirm[3], digits = 2), "]"),
    Est_rabid_dog_inc = paste0(round(Q_y$rabies_incidence[2], digits = 2), # Divide dog rabies incidence by 3 because over study years
                               "  [", round(Q_y$rabies_incidence[1], digits = 2), " - ", round(Q_y$rabies_incidence[3], digits = 2), "]"))
  
  DT_df <- rbind(DT_df, DatFram) ## Add rows
}
DT_df

# Results Table 4 - Decision tree estimates by year with Q50 [Q025-Q975] formatting 
DT_yr_summary <- as_tibble(t(DT_df))
DT_yr_summary$names <- c("Year", "Recorded deaths", "Estimated deaths", "Estimated exposures", "Estimated exposures not given PEP",
              "Estimated exposure incidence per 100,000", "Estimated dog pop", "Laboratory confirmed animal cases", "Estimated rabid dogs",
              "Estimated % confirmed cases", "Estimated rabid dogs per 1000 dogs")
write.csv(DT_yr_summary[,c(4,1,2,3)], "outputs/Table4_decision_tree_by_year.csv", row.names = FALSE)

############################################
# Run model for each municipality over all 3 years 
DT_df_mun <- data.frame() # empty dataframe
# Loop through model
for (i in 1:nrow(RiskMun)) {
  # model
  DT_mun <- decision_tree(N = N, 
                        pop = RiskMun$population[i], HDR_min = HDR[1], HDR_max = HDR[2], 
                        pObsDeath = pObsDeath, pDeath = pDeath, mean_bites = mu, k_bites = k,
                        death_data = RiskMun$deaths[i], 
                        high_risk_min = RiskMun$high_risk_dogs[i], 
                        high_risk_max = RiskMun$high_risk[i] + RiskMun$unk_risk[i],
                        confirmed_animal = RiskMun$positive_dogs [i])
  
  # quantiles across model outputs 
  Q_mun <- as.data.frame(apply(DT_mun, 2, FUN=function(x){quantile(x, probs=c(0.025, 0.5, 0.975))}))
  
  ## fill table
  DatFram <- data.frame(
    Municipality = RiskMun$name[i], 
    Population = RiskMun$population[i], 
    Est_dogs =  paste0(round(Q_mun$dog_population[2], digits = -1), "  [", 
                       round(Q_mun$dog_population[1], digits = -1), " - ", round(Q_mun$dog_population[3], digits = -1), "]"),
    Est_rabid_dogs = paste0(round(Q_mun$rabid_dogs[2], digits = 0), "  [", 
                            round(Q_mun$rabid_dogs[1], digits = 0), " - ", round(Q_mun$rabid_dogs[3], digits = 0), "]"),
    Recorded_positives = RiskMun$positive_dogs[i], 
    Pc_cases_confirmed = paste0(round(Q_mun$percent_confirm[2], digits = 2), " %  [", 
                                round(Q_mun$percent_confirm[1], digits = 2), " - ", round(Q_mun$percent_confirm[3], digits = 2), "]"),
    Est_exposures = paste0(round(Q_mun$total_exposures[2], digits = 0), "  [", 
                           round(Q_mun$total_exposures[1], digits = 0), " - ", round(Q_mun$total_exposures[3], digits=0), "]"),
    Recorded_deaths = RiskMun$deaths[i], 
    Est_deaths = paste0(round(Q_mun$estimated_deaths[2], digits = 0), "  [", 
                        round(Q_mun$estimated_deaths[1], digits=0), " - ", round(Q_mun$estimated_deaths[3], digits=0), "]"),
    # Est_exposure _incidence must be divided by 3 years since inputted data cumulative over study period 
    Est_exposure_incidence = paste0(round((Q_mun$exposure_incidence[2] / 3), digits=0), "  [", 
                                    round((Q_mun$exposure_incidence[1] / 3), digits=0), " - ", round((Q_mun$exposure_incidence[3] / 3), digits=0), "]"))
  DT_df_mun <- rbind(DT_df_mun, DatFram) ## Add rows
}
DT_df_mun

# Results Table 5 - Decision tree estimates by municipality with Q50 [Q025-Q975] formatting 
DT_mun_summary <- as_tibble(DT_df_mun)
names(DT_mun_summary) <- c("Municipality", "Human population (2020 census)", "Estimated dog population",
                         "Estimated rabid dogs", "Confirmed animal cases", "Estimated % confirmed cases",
                         "Estimated exposures", "Recorded deaths", "Estimated deaths", "Estimated exposure incidence per 100,000")
write.csv(DT_mun_summary, "outputs/Table5_decision_tree_by_municipality.csv", row.names = FALSE)

