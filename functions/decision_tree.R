### Decision tree function using IBCM data and PHO/RADDL human & animal case records 
decision_tree <- function(N, 
                          pop, HDR_min, HDR_max, 
                          pObsDeath, pDeath, mean_bites, k_bites,
                          death_data, high_risk_min, high_risk_max, confirmed_animal){
                        
  # Uncertainty in HDR - uniform distribution w/ upper & lower limits
  HDR <- runif(n = N, min = HDR_min, max = HDR_max)
  dog_pop <- round(pop/HDR) # Approx dog population
  
  # Total deaths, based on probability of recording deaths & observed deaths
  deaths <- replicate(N, nDeaths(death_data, pObsDeath))

  # Rabies exposures from:
  ###** 1) IBCM data  **
  exp_seek <- runif(n = N, min = high_risk_min, max = high_risk_max) 
  ###** 2) extrapolation of those not seeking care **
  exp_not_seek <- round(deaths / pDeath)
  # Total exposures
  total_exp <- exp_seek + exp_not_seek

  # Export estimated Probability of seeking PEP
  pSeek <- exp_seek / total_exp #

  # Deaths averted
  deaths_averted <- rbinom(n = N, size = round(exp_seek), prob = pDeath) # Deaths averted by PEP 

  # Annual rabies exposure (high-risk bite) incidence 
  exp_incidence <- (total_exp / pop) * 100000 # multiply by 100,000 for table 

  # Number of rabid dogs per year 
  rabid_dogs <- unlist(
    lapply(FUN = nRabid, mean_bites = mean_bites, k_bites = k_bites, X = total_exp) # replaces replicate (won't across vector) replicate(N, nRabid(total_exp, P_bite_rabid))
    ) 
  
  ## Percent of confirmed animal cases 
  percent_confirm <- confirmed_animal *100 / rabid_dogs # multiply by 100 for percent for table
  
  # Estimate rabies incidence
  rabies_inc <- rabid_dogs *1000 / dog_pop # multiply by 1000 

  return( # return function results
    data.frame(dog_population = dog_pop,
               estimated_deaths = deaths,
               total_exposures = total_exp,
               exposures_seek = exp_seek,
               exposures_not_seek = exp_not_seek,
               P_seek_PEP = pSeek,
               exposure_incidence = exp_incidence,
               deaths_averted = deaths_averted,
               rabid_dogs = rabid_dogs,
               percent_confirm = percent_confirm,
               rabies_incidence = rabies_inc)
    )
}
# baseline_runs <- decision_tree(N = 1000,
# pop = pop, HDR_min = HDR[1], HDR_max = HDR[2], 
# pObsDeath = pObsDeath, pDeath = pDeath, mean_bites = mu, k_bites = k,
# death_data = RiskOverall$deaths/3, 
# high_risk_min = RiskOverall$high_risk_dogs/3, 
# high_risk_max = (RiskOverall$high_risk + RiskOverall$unk_risk)/3,
# confirmed_animal = RiskOverall$positive/3)
# baseline_est <- apply(baseline_runs, 2, quantile, c(0.025, 0.5, 0.975))

# ## Check runs ok:
# test <- decision_tree(N = 1000, pop = 100000, HDR_min = 3, HDR_max = 10,
#               pObsDeath = 0.9, pDeath = 0.165, mean_bites = 0.3862, k_bites = 0.7055,
#               death_data = 18, high_risk_min = 100, high_risk_max = 500, confirmed_animal = 12)
# head(test)
# quantile(test$rabies_incidence, 0.5)
# quantile(test$deaths_averted, 0.5)
# apply(test, 2, quantile, 0.5)

################################################
# Deterministic function to check decision tree code
################################################

DT_det <- function(
    pop, HDR_min, HDR_max,  
    pObsDeath, pDeath, mean_bites, # doesn't use NB distribution
    death_data, high_risk_min, high_risk_max, confirmed_animal){
# Takes deterministic arguments, outputs estimates
  
  HDR <- (HDR_min + HDR_max)/2
  dog_pop <- round(pop / HDR) # dog population
  
# Rabies deaths, exposures & deaths averted
  deaths <- death_data/ pObsDeath # estimated deaths
  exp_seek <- (high_risk_min + high_risk_max)/2 ###
  exp_not_seek <- round(deaths / pDeath) ###
  total_exp <- (exp_seek + exp_not_seek) # Total exposures
  P_seek_PEP <- exp_seek / total_exp # Probability of exposed persons seeking PEP
  exp_inc <- total_exp *100000/ pop # Exposure incidence per 100k
  deaths_averted <- exp_seek * pDeath # Deaths averted by PEP 

# Rabies in dogs & surveillance
  rabid_dogs <- total_exp / mean_bites # Rabid dogs
  rabies_inc <- rabid_dogs *1000 / dog_pop # Rabid dogs per 1000 dogs
  percent_confirm <- confirmed_animal *100 / rabid_dogs
  
  return( # return function results
    data.frame(dog_population = dog_pop,
               estimated_deaths = deaths,
               total_exposures = total_exp,
               exposures_seek = exp_seek,
               exposures_not_seek = exp_not_seek,
               P_seek_PEP = P_seek_PEP,
               exposure_incidence = exp_inc,
               deaths_averted = deaths_averted,
               rabid_dogs = rabid_dogs,
               percent_confirmed = percent_confirm,
               rabies_incidence = rabies_inc)
  )
}

# baseline_det <- DT_det(
#   pop = pop, HDR_min = HDR[1], HDR_max = HDR[2], 
#   pObsDeath = pObsDeath, pDeath = pDeath, mean_bites = mu, 
#   death_data = RiskOverall$deaths/3, 
#   high_risk_min = RiskOverall$high_risk_dogs/3, 
#   high_risk_max = (RiskOverall$high_risk + RiskOverall$unk_risk)/3,
#   confirmed_animal = RiskOverall$positive/3)
# baseline_est; baseline_det

################################################
#####  Functions for Sensitivity Analysis  #####
################################################
# Univariate function for decision tree (allows variation of 1 variable at a time)
DT_univ <- function(vary, low, high,
                    pop, HDR,
                    pObsDeath, pDeath, mean_bites, 
                    death_data, high_risk, confirmed_animal){ #

  # Takes draw of selected variable & keep others constant
  HDR <- ifelse(vary == "HDR", runif(1, low, high), HDR) # runif(1, 3, 10)
  pObsDeath <- ifelse(vary == "pObsDeath", runif(1, low, high), pObsDeath) 
  pDeath <- ifelse(vary == "pDeath", runif(1, low, high), pDeath)   
  mean_bites <- ifelse(vary == "mean_bites", runif(1, low, high), mean_bites) 
  death_data <- ifelse(vary == "death_data", runif(1, low, high), death_data) # runif(1, 9, 18)
  high_risk <- ifelse(vary == "high_risk", runif(1, low, high), high_risk) 
  confirmed_animal <- ifelse(vary == "confirmed_animal", runif(1, low, high), confirmed_animal) 

  # RUN PROBABILISTIC MODEL:  
  dog_pop <- round(pop/HDR) # Approx dog population
  deaths <- nDeaths(death_data, pObsDeath)
  exp_seek <- high_risk 
  exp_not_seek <- round(deaths / pDeath)
  total_exp <- exp_seek + exp_not_seek
  pSeek <- exp_seek / total_exp #
  deaths_averted <- rbinom(n = 1, size = round(exp_seek), prob = pDeath) # Deaths averted by PEP 
  exp_inc <- (total_exp / pop) * 100000 
  rabid_dogs <- nRabid(total_exp, mean_bites= mean_bites, k_bites = 0.7055) 
  percent_confirm <- confirmed_animal *100 / rabid_dogs # multiply by 100 for percent for table
  rabies_inc <- rabid_dogs *1000 / dog_pop # rabies incidence 

  return( # return function results
    data.frame(dog_population = dog_pop,
               estimated_deaths = deaths,
               total_exposures = total_exp,
               exposures_seek = exp_seek,
               exposures_not_seek = exp_not_seek,
               pSeekPEP = pSeek,
               exposure_incidence = exp_inc,
               deaths_averted = deaths_averted,
               rabid_dogs = rabid_dogs,
               percent_confirmed = percent_confirm,
               rabies_incidence = rabies_inc)
  )
}

# Check:
# test3 = DT_univ(vary = "HDR", low = 3, high = 10,
#                 pop = 908339, HDR = 5,
#                 pObsDeath =0.9, pDeath =0.165, mean_bites =0.38,
#                 death_data = 9,  high_risk = 160, confirmed_animal = 6)
# test3


# Function to take replicate uniform draws across the parameter range
DT_ndraw <- function(ndraw, vary, low, high,
                     pop, HDR, 
                     pObsDeath, pDeath, mean_bites,
                     death_data, high_risk, confirmed_animal){
  
  draws <- vector("list", ndraw)
  for(i in 1:ndraw){
    draws[[i]] <- cbind.data.frame(
    DT_univ(vary = vary, low = low, high = high,
            pop = pop, HDR = HDR, 
            pObsDeath = pObsDeath, pDeath = pDeath, mean_bites = mean_bites,
            death_data = death_data, high_risk = high_risk, confirmed_animal = confirmed_animal),
      iter=i)
  }
  draws<-do.call("rbind", draws)
  return(draws)
}

# # # Check
# test_pop <- DT_ndraw(ndraw=1000, vary= "pop", low = 800000, high = 1000000, 
#           pop = 908339, HDR = 5, 
#           pObsDeath =0.9, pDeath =0.165, mean_bites =0.38, 
#           death_data = 9,  high_risk = 160, confirmed_animal = 6)
#           
test_high_risk <- DT_ndraw(ndraw=1000, vary= "high_risk", low = 100, high = 500,
                     pop = 908339, HDR = 5,
                     pObsDeath =0.9, pDeath =0.165, mean_bites =0.38,
                     death_data = 9,  high_risk = 160, confirmed_animal = 6)
# 
# test_pObsDeath <- DT_ndraw(ndraw=1000, vary= "pObsDeath", low = 0.5, high = 1, 
#                            pop = 908339, HDR = 5, 
#                            pObsDeath =0.9, pDeath =0.165, mean_bites =0.38, 
#                            death_data = 9,  high_risk = 160, confirmed_animal = 6)
# 
# test_mean_bites <- DT_ndraw(ndraw=1000, vary= "mean_bites", low = 0.1, high = 0.5, 
#                             pop = 908339, HDR = 5, 
#                             pObsDeath =0.9, pDeath =0.165, mean_bites =0.38, 
#                             death_data = 9,  high_risk = 160, confirmed_animal = 6)
# 
# apply(test_pop, 2, quantile, c(0.05, 0.5, 0.975))
# apply(test_high_risk, 2, quantile, c(0.05, 0.5, 0.975))
# apply(test_pObsDeath, 2, quantile, c(0.05, 0.5, 0.975))
# apply(test_mean_bites, 2, quantile, c(0.05, 0.5, 0.975))
# 
