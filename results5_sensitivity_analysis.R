# Run IBCM decision tree sensitivity analyses
rm(list = ls())

# Import libraries and source functions
library(Rmisc)
library(ggplot2)
library(dplyr)
library(cowplot)
library(gridExtra)
library(grid)
library(ggpubr)

source("functions/HelperFunctions.R") # inputs function for Decision Tree Model 
source("functions/decision_tree.R") # inputs function for Decision Tree Model 

# Import data
municipalities <- read.csv("data/municipality.csv")
RiskMun <- read.csv("outputs/municipality_summary.csv") # by municipality/risk category 
RiskYr <- read.csv("outputs/ibcm_summary.csv") # by year/risk category 
tests <- read.csv("outputs/RADDL_deid.csv") # OrMin confirmed animal cases from RADDL 
pSeekDF <- read.csv("outputs/Prob_Seek_PEP.csv") # probability of seeking PEP

mun <- as.factor(municipalities$name)
pop <- sum(municipalities$population) # population of Oriental Mindoro 
RiskMun <- left_join(municipalities, RiskMun, by = join_by(name == MUNICIPALITY))
RiskOverall <- cbind(total = "total", data.frame(t(apply(RiskYr[,-1], 2, sum))))

# Probabilities 
pDeath <- 0.165 # developing rabies if bitten by rabid dog (ignore age & bite site)
mu <-  0.3862 # Mu for proportion of rabid dogs that bite a person 
k <- 0.7055 # Size for negative binomial for P(bite|rabid_dog)
pObsDeath <- 0.9 # Probability of human death being recorded 
pSeek <- subset(pSeekDF, Year=="overall")$pPEP # Exposures seeking PEP 
HDR <- c(3, 10) # human:dog ratio - reasonable range from 3-10

# Run probabilistic model to get point estimate and PrI:
baseline_runs <- decision_tree(N = 1000,
                    pop = pop, HDR_min = HDR[1], HDR_max = HDR[2], 
                    pObsDeath = pObsDeath, pDeath = pDeath, mean_bites = mu, k_bites = k,
                    death_data = RiskOverall$deaths/3, 
                    high_risk_min = RiskOverall$high_risk_dogs/3, 
                    high_risk_max = (RiskOverall$high_risk + RiskOverall$unk_risk)/3,
                    confirmed_animal = RiskOverall$positive/3)

qs =  c(0.025, 0.5, 0.975)
baseline_est <- apply(baseline_runs, 2, quantile, qs)
baseline_avg = apply(baseline_runs, 2, mean) # add baseline to sensitivity analysis plot

## One-way sensitivity analysis working through all parameters
SA_range <- data.frame(
  HDR = c(3,10),
  pObsDeath = c(0.5,1),
  pDeath = c(pDeath*.75, pDeath*1.25), # 0.124 - 0.206
  mean_bites = c(0.15, 0.5),
  death_data = c(min(RiskYr$deaths), max(RiskYr$deaths)),
  high_risk = c(min(RiskYr$high_risk_dogs/2), max(RiskYr$high_risk + RiskYr$unk_risk)),
  confirmed_animal = c(min(RiskYr$positive), max(RiskYr$positive)))
SA_base <- data.frame(
  HDR = sum(SA_range$HDR)/2,
  pObsDeath = pObsDeath,
  pDeath = pDeath, # 0.124 - 0.206
  mean_bites = mu,
  death_data = RiskOverall$deaths/3,
  high_risk = RiskOverall$high_risk/3,
  confirmed_animal = RiskOverall$positive/3)
# rbind(SA_range, SA_base)

# Run and save outputs for all parameters in one-way sensitivity analyses
variables <- names(SA_range)
oneway_est = oneway_lower = oneway_upper = data.frame(matrix(ncol = 12, nrow = 0))
oneway_SA <- data.frame(matrix(ncol = 13, nrow = 0))

# Loop through all variables, with 1000 draws across the range of uncertainty for each variable in turn
for(i in 1:length(variables)){
    set.seed(5) # set the seed before running all the draws for this variable - same random numbers will be drawn 

    results <- DT_ndraw(ndraw=1000, 
                        vary= names(SA_range)[i], low = SA_range[1,i], high = SA_range[2,i], 
                        pop = pop, HDR = SA_base$HDR, 
                        pObsDeath = SA_base$pObsDeath, pDeath = SA_base$pDeath, mean_bites = SA_base$mean_bites,
                        death_data = SA_base$death_data,
                        high_risk = SA_base$high_risk,
                        confirmed_animal= SA_base$confirmed_animal)
        results_sum <- apply(results, 2, quantile, qs)
    results$var <- names(SA_range)[i]
    print(i)
    
    # Combine extracted prediction intervals into a single dataframe
    oneway_lower <- rbind.data.frame(oneway_lower, results_sum[1,])
    oneway_est <- rbind.data.frame(oneway_est, results_sum[2,])
    oneway_upper <- rbind.data.frame(oneway_upper, results_sum[3,])
    oneway_SA <- rbind.data.frame(oneway_SA, results)
}
# Give heading names to dataframes
colnames(oneway_lower) <- colnames(oneway_est) <- colnames(oneway_upper) <- names(oneway_SA)[-13]

# Print out the values to check them
point_SA <- cbind(oneway_est, variables) # Median estimates (They do NOT centre on the point estimates because the sensitivity ranges are not symmetric around the baseline)
lower_SA <- cbind(oneway_lower, variables) # The lower bounds for each parameter one way analysis
upper_SA <- cbind(oneway_upper, variables) # The upper bounds for each parameter one way analysis

my_labels = c("confirmed cases", 
           "recorded deaths", 
           "HDR", 
           "high-risk bite patients",
           "Prabid dog bites",
           "Prabies|exposure",
           "Pobs|death",
           "human population")

# BOXPLOTs - 
# names(oneway_est)
# [1] "dog_population"     "estimated_deaths"   "total_exposures"    "exposures_seek"     "exposures_not_seek"
# [6] "pSeekPEP"           "exposure_incidence" "deaths_averted"     "rabid_dogs"         "percent_confirmed" 
# [11] "rabies_incidence"  
# variation in dog population
SA_DogPop <- ggplot(data = oneway_SA, aes(x = var, y = dog_population)) +
  geom_boxplot(fill = "#0099f8") +
  scale_x_discrete(name = "", labels = my_labels) +
  coord_flip() +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  xlab(" ") +
  ylim(0, 350000) +
  ylab('Dogs') +
  theme_classic() +
  geom_hline(yintercept = baseline_avg[1], color = "gray", size = 0.3 )
SA_DogPop

# Variation in estimated deaths 
SA_deaths <- ggplot(data = oneway_SA, aes(x = var, y = estimated_deaths)) +
  geom_boxplot(fill = "#0099f8") +
  scale_x_discrete(name = "", labels = my_labels) +
  coord_flip() +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  xlab(" ") +
  ylim(0, 35) +
  ylab('Human deaths') + 
  theme_classic() + 
  geom_hline(yintercept = baseline_avg[2], color = "gray", size = 0.3 )
SA_deaths

# variation in total exposures
SA_TotExp <- ggplot(data = oneway_SA, aes(x = var, y = total_exposures)) +
  geom_boxplot(fill = "#0099f8") +
  scale_x_discrete(name = "", labels = my_labels) +
  coord_flip() +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  xlab(" ") +
  ylim(0, 350) +
  ylab('Exposures') +
  theme_classic() +
  geom_hline(yintercept = baseline_avg[3], color = "gray", size = 0.3 )
SA_TotExp

# Exposures seeking care
SA_ExpSeek <- ggplot(data = oneway_SA, aes(x = var, y = exposures_seek)) +
  geom_boxplot(fill = "#0099f8") +
  scale_x_discrete(name = "", labels = my_labels) +
  coord_flip() +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  xlab(" ") +
  ylim(0, 300) +
  ylab('Exposures seeking PEP') +
  theme_classic() +
  geom_hline(yintercept = baseline_avg[4], color = "gray", size = 0.3 )
SA_ExpSeek

# exposures not seeking care
SA_NoSeek <- ggplot(data = oneway_SA, aes(x = var, y = exposures_not_seek)) +
  geom_boxplot(fill = "#0099f8") +
  scale_x_discrete(name = "", labels = my_labels) +
  coord_flip() +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  xlab(" ") +
  ylim(0, 250) +
  ylab('Exposures not seeking PEP') +
  theme_classic() +
  geom_hline(yintercept = baseline_avg[5], color = "gray", size = 0.3 )
SA_NoSeek

# BOXPLOT - variation in PseekPEP 
SA_Pseek <- ggplot(data = oneway_SA, aes(x = var, y = pSeekPEP)) +
  geom_boxplot(fill = "#0099f8") +
  scale_x_discrete(name = "", labels = my_labels) +
  coord_flip() +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  xlab(" ") +
  ylim(0, 1) +
  ylab(expression(P[seekPEP])) + 
  theme_classic() + 
  geom_hline(yintercept = baseline_avg[6], color = "gray", size = 0.3 )
SA_Pseek

# BOXPLOT - exposure incidence
SA_ExpInc <- ggplot(data = oneway_SA, aes(x = var, y = exposure_incidence)) +
  geom_boxplot(fill = "#0099f8") +
  scale_x_discrete(name = "", labels = my_labels) +
  coord_flip() +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  xlab(" ") +
  ylim(0, 50) +
  ylab("Exposure incidence") + 
  theme_classic() + 
  geom_hline(yintercept = baseline_avg[7], color = "gray", size = 0.3 )
SA_ExpInc

# Deaths averted
SA_DA <- ggplot(data = oneway_SA, aes(x = var, y = deaths_averted)) +
  geom_boxplot(fill = "#0099f8") +
  scale_x_discrete(name = "", labels = my_labels) +
  coord_flip() +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  xlab(" ") +
  ylim(0, 50) +
  ylab('Deaths averted') +
  theme_classic() +
  geom_hline(yintercept = baseline_avg[8], color = "gray", size = 0.3 )
SA_DA

# BOXPLOT - variation in rabid dogs 
SA_rabiddogs <- ggplot(data = oneway_SA, aes(x = var, y = rabid_dogs)) +
  geom_boxplot(fill = "#0099f8") +
  scale_x_discrete(name = "", labels = my_labels) +
  coord_flip() +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  xlab(" ") +
  ylim(0, 1200) +
  ylab('Rabid dogs') + 
  theme_classic() +
  geom_hline(yintercept = baseline_avg[9], color = "gray", size = 0.3 )
SA_rabiddogs

# % confirmed
SA_pc_confirm <- ggplot(data = oneway_SA, aes(x = var, y = percent_confirmed)) +
  geom_boxplot(fill = "#0099f8") +
  scale_x_discrete(name = "", labels = my_labels) +
  coord_flip() +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  xlab(" ") +
  ylim(0, 10) +
  ylab('Percent cases confirmed') +
  theme_classic() +
  geom_hline(yintercept = baseline_avg[10], color = "gray", size = 0.3 )
SA_pc_confirm

# rabid dog incidence
SA_rabies_inc <- ggplot(data = oneway_SA, aes(x = var, y = rabies_incidence)) +
  geom_boxplot(fill = "#0099f8") +
  scale_x_discrete(name = "", labels = my_labels) +
  coord_flip() +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  xlab(" ") +
  ylim(0, 10) +
  ylab('Percent cases confirmed') +
  theme_classic() +
  geom_hline(yintercept = baseline_avg[11], color = "gray", size = 0.3 )
SA_rabies_inc

# Create 4 panels for Figure 4 
SA_panel <- ggarrange(SA_deaths, 
                      SA_DA,
                      SA_pc_confirm, 
                      SA_Pseek, 
                      nrow = 2, ncol = 2, align = "hv",
                      labels=c("A", "B", "C", "D"))
SA_panel

# adding summary axis labels 
annotate_figure(SA_panel, 
                left = textGrob("Parameters/ data inputs", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Model estimates", gp = gpar(cex = 1.3)))
# Save to pdf 
ggsave("figs/SA_Panel_Figure4.pdf", width = 11, height = 7)

