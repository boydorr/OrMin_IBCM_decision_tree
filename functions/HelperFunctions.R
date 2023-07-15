########################################################
# Function to simulate expected deaths given observation probability and n observations (deaths)
########################################################
nDeaths <- function(pObsDeath, Pobs){
  
  # initialize before starting while loop
  TotDeaths <- 0 
  Obs <- 0
  
  # simulate trials (deaths and observation process) until all observed events seen
  while(Obs < pObsDeath){
    TotDeaths <- TotDeaths +1
    Obs <- Obs + rbinom(1,1,Pobs)
  }
  TotDeaths # print total events given input of observed events
}
## Check results ok:
# replicate(100, nDeaths(25, 0.90))


########################################################
# Function to estimate the probability of seeking PEP if bitten by a RABID dog
########################################################
seekPEP <- function(N, death_data, pObs, high_risk_min, high_risk_max, pDeath)
{
  # Arguments:
  # N - iterations
  # data - recorded death data
  # pObs - probability of observing/ recording (e.g. deaths)
  # high_risk_min, high_risk_max - range of bites due to rabid animals who seek PEP!
  # pDeath - probability of death if exposed (and no PEP)
  
  deaths <- replicate(N, nDeaths(death_data, pObs)) # Generate likely deaths
  exp_no_PEP <- round(deaths / pDeath) # 
  exp_PEP <- runif(n = N, min = high_risk_min, max = high_risk_max) # Exposures who sought PEP incl. unk_risk (from IBCM)
  total_exp <- exp_PEP + exp_no_PEP # Estimate total rabies exposures 
  pSeek <- exp_PEP / total_exp # Estimate prob of seeking PEP after exposure 
  
  return( # return function results 
    data.frame(exposures_no_PEP = exp_no_PEP,
               exposures_PEP = exp_PEP,
               total_exposures = total_exp,
               probability_seek_PEP = pSeek))
}

# ## CHECK:
## Seek_PEP(N = 10, death_data = 9, pObsDeath = 0.9, high_risk_min = 30, high_risk_max = 50, pdeath = 0.165)

########################################################
# Function to simulate expected rabid dogs given exposures
########################################################
nRabid <- function(exposures, mean_bites, k_bites){
  # initialize before starting while loop
  bites <- 0 
  RabidDogs <- 0
  
  # simulate trials (bites by rabid dogs) until all exposures occurred
  while(bites < exposures){
    RabidDogs <- RabidDogs + 1
    bites <- bites + rnbinom(n = 1,  mu = mean_bites, size = k_bites) # mu = 0.3862, size = 0.7055
  }
  RabidDogs 
}
# nRabid(exposures = 100, mean_bites = 0.38, k_bites = 0.7)

## Check results ok:
# hist(replicate(1000, nRabid(1000, 0.38, 0.7)))

## check function can be applied to replicated exposures
# exp_vec = c(3,4,5,6,5,6)
# unlist(lapply(FUN = nRabid, Mu = 0.38, K = 0.7, X = exp_vec))


########################################################
# Function to simulate bites per rabid dog using negative binomial distribution
########################################################
# nBites <- function(dogs, pBite, pBiteK){
#   sum(rnbinom(n = dogs,  mu = pBite, size = pBiteK))
# }


