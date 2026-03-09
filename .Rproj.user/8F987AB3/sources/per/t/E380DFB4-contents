library(tidyverse)

#Transfer raw data from CSV into data frames
noins_child_data <- read.csv("C:/Users/Amora/Documents/CU Boulder/MCDB/Classes/MCDB 6440/Project/Data/Children without Health Insurance Disparities/ECCO_disparities_county.csv")
householdinc_data <- read.csv("C:/Users/Amora/Documents/CU Boulder/MCDB/Classes/MCDB 6440/Project/Data/Household Income ($) Economics & Insurance/ECCO_economy_county.csv")
teenhpv_vac_data <- read.csv("C:/Users/Amora/Documents/CU Boulder/MCDB/Classes/MCDB 6440/Project/Data/Up-To-Date Percent Teen HPV Immunization Rate/ECCO_hpv_county.csv")

#Remove NAs from data
noins_child_data_clean <- na.omit(noins_child_data)
householdinc_data_clean <- na.omit(householdinc_data)
teenhpv_vac_data_clean <- na.omit(teenhpv_vac_data)

#Removing "GEOID" and "State" columns
noins_child_data_clean <- noins_child_data_clean[,-1]
noins_child_data_clean <- noins_child_data_clean[,-2]

#Only keeping rows with measurement of child insurance rates
noins_child_data_clean <- noins_child_data_clean %>% 
  filter(measure=="Children without Health Insurance")
