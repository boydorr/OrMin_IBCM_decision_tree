######################################################################
### Results - Probability of seeking PEP after rabies exposure ###
######################################################################
rm(list=ls())

# Import libraries, source functions and data
library(tidyverse)
source("functions/HelperFunctions.R") 
ibcm_risk <- read.csv("outputs/ibcm_summary.csv") # ibcm risk data summarized 
overall_risk <- apply(ibcm_risk, 2, sum)
pSeek_inputs <- rbind(ibcm_risk, overall_risk)
pSeek_inputs$year_visit[4] <- "overall"

# Values for use in the function
pDeath <- 0.165 # Probability of death if bitten by rabid dog and no PEP
pObsDeath <- 0.9 # Probability of observing or recording a rabies death
N <- 1000 # Iterations

# Create empty dataframe
pSeekDF <- data.frame()

# Estimate probabilities in loop
for (i in 1:nrow(pSeek_inputs)) {
  
  input <- pSeek_inputs %>% slice(i)  # Select each row in dataframe
  
  out_pSeekPEP <- seekPEP(N, # Estimate probability 
                      death_data = input$deaths,
                      pObs = pObsDeath,
                      high_risk_min = input$high_risk_dogs,
                      high_risk_max = input$high_risk + input$unk_risk,
                      pDeath = pDeath)
  pPEP_qs <- quantile(out_pSeekPEP$probability_seek_PEP, c(0.025, 0.5, 0.975)) # Extract quantiles 

  ## Add rows to table in loop
  pSeekDF <- rbind(pSeekDF, 
                   data.frame(Year = input$year_visit, 
                              pPEP = pPEP_qs[2], 
                              pPEP_LCI = pPEP_qs[1], 
                              pPEP_UCI = pPEP_qs[3]))
}

# Create csv table showing estimates for Probability exposure seeks PEP 
write.csv(pSeekDF, "outputs/Prob_Seek_PEP.csv")

paste0("We estimated the probability of rabies exposures seeking PEP over this three year period as ", 
       round(subset(pSeekDF, Year=="overall")$pPEP, 3), " ranging from ", 
       round(subset(pSeekDF, Year=="2020")$pPEP, 3), " (95%PrI: ", 
       round(subset(pSeekDF, Year=="2020")$pPEP_LCI, 3), "-", 
       round(subset(pSeekDF, Year=="2020")$pPEP_UCI, 3), ") in 2020 to ",
       round(subset(pSeekDF, Year=="2022")$pPEP, 3), " (95%PrI: ", 
       round(subset(pSeekDF, Year=="2022")$pPEP_LCI, 3), "-", 
       round(subset(pSeekDF, Year=="2022")$pPEP_UCI, 3), ") in 2022.") 

