#################################################################
#####   Code for Results: Characteristics of bite patients  #####
#################################################################
rm(list=ls())

# Libraries & Source functions
library(dplyr)
library(readr)
library(tidyverse)
library(truncnorm)
library(data.table)
source("functions/bite_summary.R") # function for Table 3 outputs 

# Data
PHO_biteINFO <- read.csv("data/PHO_annual_bite_data.csv") # Data compiled from PHO annual bite reports 
municipalities <- read.csv("data/municipality.csv")
pop <- sum(municipalities$population) # population of Oriental Mindoro 

# Create table
study_avg <- subset(PHO_biteINFO, Year == "Avg_year_2020_to_2022")
TableDF <- data.frame() # Create empty dataframe 

for (i in 1:5) { # Loop across rows years

  row_df <- PHO_biteINFO %>% slice(i) # Select row  

  # Run function 
  sum_df <- bite_summary(
    Year = row_df$Year,
    months=12,
    deaths_rec = row_df$deaths_rec,
    OrMin_pop = pop,
    bite_patients = row_df$bite_patients,
    male = row_df$male,
    age_under_15 = row_df$age_under_15, 
    species_dog = row_df$species_dog, 
    species_cat = row_df$species_cat, 
    species_other = row_df$species_other, 
    CAT_I = row_df$CAT_I, 
    CAT_II = row_df$CAT_II, 
    CAT_III = row_df$CAT_III, 
    ERIG_given = row_df$ERIG_given)

# Creating table row 
DF <- data.frame(Year = row_df$Year,
                      Recorded_human_deaths = round(row_df$deaths_rec, digits = 0),
                      Total_bite_patients = round(row_df$bite_patients, digits = 0),
                      Mean_patients_per_month = round(sum_df$Mean_patients_per_month, digits = 1),
                      Bite_incidence_per_100k = round(sum_df$Overall_bite_incidence, digits = 1), 
                      Percent_male = round(sum_df$percent_male, digits=1), 
                      Bites_under_15 = paste0(row_df$age_under_15, "  (", round(sum_df$percent_under_15, digits = 1), ")"),  
                      Category_I_bites = paste0(row_df$CAT_I, "  (", round(sum_df$Percent_Cat1, digits = 1), ")"), 
                      Category_II_bites = paste0(row_df$CAT_II, "  (", round(sum_df$Percent_Cat2, digits = 1), ")"), 
                      Category_III_bites = paste0(row_df$CAT_III, "  (", round(sum_df$Percent_Cat3, digits = 1), ")"),
                      Bites_received_ERIG_Percent_CategoryIII = paste0(row_df$ERIG_given, "  (", round(sum_df$percent_bites_ERIG, digits = 1), ")"),
                      Biting_animal_dog = paste0(row_df$species_dog, "  (", round(sum_df$percent_species_dog, digits = 1), ")"), 
                      Biting_animal_cat = paste0(row_df$species_cat, "  (", round(sum_df$percent_species_cat, digits = 1), ")"),
                      Biting_animal_other = paste0(row_df$species_other, "  (", round(sum_df$percent_species_other, digits = 1), ")"))

TableDF <- rbind(TableDF, DF)
}

# Fix formatting for averages
fix_avgs = which(TableDF$Year == "Avg_year_2020_to_2022")
TableDF_avg <- TableDF[fix_avgs,] 
TableDF_avg$Recorded_human_deaths <- round(study_avg$deaths_rec, digits = 1)
TableDF_avg$Bites_under_15 <- paste0(round(study_avg$age_under_15, digits = 0), "  (", 
                                     round(study_avg$age_under_15 *100/ study_avg$bite_patients, digits = 1), ")")
TableDF_avg$Category_II_bites <- paste0(round(study_avg$CAT_II, digits = 0), "  (", 
                                        round(study_avg$CAT_II *100/ study_avg$bite_patients, digits = 1), ")")
TableDF_avg$Biting_animal_cat <- paste0(round(study_avg$species_cat, digits = 0), "  (", 
                                     round(study_avg$species_cat *100/ study_avg$bite_patients, digits = 1), ")")
TableDF_avg$Biting_animal_other <- paste0(round(study_avg$species_other, digits = 0), "  (", 
                                        round(study_avg$species_other *100/ study_avg$bite_patients, digits = 1), ")")

# Transpose columns into the rows 
Table_summary <- as_tibble(t(TableDF))
Table_summary$V5 <- as.character(t(TableDF_avg[1,]))
Table_summary$V1[1] <- "2019 pre-study"
Table_summary$V5[1] <- "Average per study year (2020-2022)"
Table_summary$names <- c("Year", "Recorded human deaths", "Total bite patients", "Mean monthly patients", "Bite incidence per 100k", 
                         "% male", "Bites U15y (%)", "Category I (%)", "Category II (%)","Category III (%)",
                         "ERIG (% of Category III patients)", "Dog bite (%)", "Cat bite (%)", "Bite by other animal (%)")
 
# Save table showing PHO Overall Bite Patient summary for Results 
write.csv(Table_summary[,c(6,1,2,3,4,5)], "outputs/Table_3_Characteristics_bite_patients.csv")

