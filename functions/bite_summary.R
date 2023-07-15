### Function to summarize Provincial Health Office bite records

bite_summary <- function(Year, months, deaths_rec, OrMin_pop, bite_patients, 
                         male, age_under_15, 
                         species_dog, species_cat, species_other, 
                         CAT_I, CAT_II, CAT_III, ERIG_given){
  
  mean_patient_month <- (bite_patients / months) # Average bite patients per month  
  overall_inc <- (bite_patients / OrMin_pop) * 100000 # Bite patient incidence per 100,000 
  percent_male <- (male / bite_patients) * 100 # % male bite patients 
  percent_child <- (age_under_15 / bite_patients) * 100 # % <15 bite patients
  percent_ERIG <- (ERIG_given / CAT_III) * 100 # % Cat III patients given ERIG 
  
  # % WHO Category of exposure 
  percent_CATI <- (CAT_I / bite_patients) * 100
  percent_CATII <- (CAT_II / bite_patients) * 100
  percent_CATIII <- (CAT_III / bite_patients) * 100 
  
  # % biting animal species 
  percent_dog <- (species_dog / bite_patients) * 100
  percent_cat <- (species_cat / bite_patients) * 100
  percent_other <- (species_other / bite_patients) * 100
  
  return(
    data.frame(Year = Year,
               Human_deaths = deaths_rec,
               Total_bite_patients = bite_patients,
               Mean_patients_per_month = mean_patient_month,
               Overall_bite_incidence = overall_inc,
               percent_male = percent_male,
               bites_under_15 = age_under_15, 
               percent_under_15 = percent_child, 
               bites_received_ERIG = ERIG_given, 
               percent_bites_ERIG = percent_ERIG, 
               CATI_bites = CAT_I, 
               Percent_Cat1 = percent_CATI, 
               CATII_bites = CAT_II, 
               Percent_Cat2 = percent_CATII, 
               CATIII_bites = CAT_III, 
               Percent_Cat3 = percent_CATIII, 
               bites_dog = species_dog, 
               percent_species_dog = percent_dog,
               bites_cat = species_cat, 
               percent_species_cat = percent_cat, 
               bites_other = species_other, 
               percent_species_other = percent_other))
}


