### Function to estimate PEP vaccine & ERIG costs  

Cost_analysis <- function(
    Total_bites, CatIII_bites, ERIG_given,
    N, high_risk_min, high_risk_max,  
    avg_ID_PEP, PEP_ID_cost,  
    avg_vial_ERIG, ERIG_cost,
    pDeath, DALY) {
# Function take arguments:
  # Total_bites - overall bite patients (PHO records)
  # CatIII_bites - category III bite patients (PHO records) 
  # ERIG_given - number of bite patients received ERIG 
  # avg_ID_PEP - average number of ID injections of PEP per patient 
  # PEP_ID_cost - cost per ID injection PEP vaccine  
  # avg_vial_ERIG - average number of vials of ERIG per patient 
  # ERIG_cost - cost per vial of ERIG 

# Estimating PEP and ERIG costs per year and overall  
  Total_Cost_HRV <- Total_bites * avg_ID_PEP * PEP_ID_cost # PEP costs per year & 3-years
  Total_Cost_ERIG <- ERIG_given * avg_vial_ERIG * ERIG_cost # ERIG costs per year & 3-years
  Total_Cost <- Total_Cost_HRV + Total_Cost_ERIG # Total costs both PEP & ERIG 

# Estimating costs per person 
  Cost_per_Cat2_noERIG <- Total_Cost_HRV / Total_bites # for Cat2 bites no ERIG 
  Cost_per_Cat3_ERIG <- (avg_ID_PEP * PEP_ID_cost) + (avg_vial_ERIG * ERIG_cost) # for Cat3 bites received ERIG
  Avg_cost_per_bite <- (Total_Cost / Total_bites) # average cost per person 
  
# Proportion of Category III bite patients receiving ERIG 
  P_ERIG_Cat3 <- ERIG_given / CatIII_bites 

# Rabies deaths averted by PEP 
  avert_min <- quantile(rbinom(n = N, size = high_risk_min, prob = pDeath), 0.025)
  avert_max <- quantile(rbinom(n = N, size = high_risk_max, prob = pDeath), 0.975)
  death_averted <- (avert_min + avert_max)/2
  
# Cost per rabies death averted 
  cost_death_avert_min <- Total_Cost / avert_max
  cost_death_avert_max <- Total_Cost / avert_min
  cost_death_avert <- Total_Cost / death_averted
  
# DALYs averted per year by PEP  
  DALY_avert_avg <- death_averted * DALY
  
# Cost per DALY averted by PEP 
  cost_DALY_avert <- Total_Cost / DALY_avert_avg
  
  # Estimates for $$$ wasted on unnecessary use of PEP on healthy bites 
  low_risk_bites <- Total_bites - high_risk_max
  waste_PEP_costs <- low_risk_bites * avg_ID_PEP * PEP_ID_cost
  prop_USD_wasted_PEP <- waste_PEP_costs / Total_Cost_HRV 
  
# Estimates for $$$ wasted on necessary use of ERIG on healthy bites 
  wasted_ERIG <- ERIG_given - high_risk_max
  waste_ERIG_costs <- wasted_ERIG * avg_vial_ERIG * ERIG_cost 
  prop_USD_wasted_ERIG <- waste_ERIG_costs / Total_Cost_ERIG 
  
  return( # return function results 
    data.frame(Cost_HRV_USD = Total_Cost_HRV,
               Cost_ERIG_USD = Total_Cost_ERIG,
               Total_PEP_Cost_USD = Total_Cost,
               Cost_per_Cat2_noERIG_bite = Cost_per_Cat2_noERIG,
               Cost_per_Cat3_ERIG_bite = Cost_per_Cat3_ERIG, 
               Average_cost_per_person = Avg_cost_per_bite, 
               Percent_ERIG_all = P_ERIG_Cat3,
               Deaths_averted_min = avert_min,
               Deaths_averted_max = avert_max,
               Deaths_averted = death_averted,
               Cost_per_death_averted_min = cost_death_avert_min, 
               Cost_per_death_averted_max = cost_death_avert_max, 
               Cost_per_death_averted = cost_death_avert,
               DALYs_averted = DALY_avert_avg,  
               Cost_per_DALY_averted = cost_DALY_avert, 
               Unnecessary_PEP_costs = waste_PEP_costs,
               Proportion_Unnecessary_PEP_costs = prop_USD_wasted_PEP, 
               Unnecessary_ERIG_costs = waste_ERIG_costs,
               Proportion_Unnecessary_ERIG_costs = prop_USD_wasted_ERIG))
}
