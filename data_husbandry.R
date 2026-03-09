library(tidyverse)

#Transfer raw data from CSV into data frames
noins_child_data <- read.csv("C:/Users/Amora/Documents/CU Boulder/MCDB/Classes/MCDB 6440/Project/null-hypothesis/Data/Children without Health Insurance Disparities/ECCO_disparities_county.csv")
householdinc_data <- read.csv("C:/Users/Amora/Documents/CU Boulder/MCDB/Classes/MCDB 6440/Project/null-hypothesis/Data/Household Income ($) Economics & Insurance/ECCO_economy_county.csv")
teenhpv_vac_data <- read.csv("C:/Users/Amora/Documents/CU Boulder/MCDB/Classes/MCDB 6440/Project/null-hypothesis/Data/Up-To-Date Percent Teen HPV Immunization Rate/ECCO_hpv_county.csv")

#Remove NAs from data
noins_child_data_clean <- na.omit(noins_child_data)
householdinc_data_clean <- na.omit(householdinc_data)
teenhpv_vac_data_clean <- na.omit(teenhpv_vac_data)

#Removing "GEOID" and "State" columns
noins_child_data_clean <- noins_child_data_clean[,-1]
noins_child_data_clean <- noins_child_data_clean[,-2]
householdinc_data_clean <- householdinc_data_clean[,-1]
householdinc_data_clean <- householdinc_data_clean[,-2]
teenhpv_vac_data_clean <- teenhpv_vac_data_clean[,-1]
teenhpv_vac_data_clean <- teenhpv_vac_data_clean[,-2]

#Only keeping rows with measurement of child insurance rates + pivoting data
noins_child_data_clean <- noins_child_data_clean %>% 
  filter(measure=="Children without Health Insurance")
noins_child_longer <- pivot_longer(noins_child_data_clean, -County, names_to=".value")

#Only keeping rows with measurement of household income + pivoting data
householdinc_data_clean <- householdinc_data_clean %>% 
  filter(measure=="Household Income ($)")
houseinc_longer <- pivot_longer(householdinc_data_clean, -County, names_to=".value")

#Only keeping rows with both sexes included + pivoting data
teenhpv_vac_data_clean <- teenhpv_vac_data_clean %>% 
  filter(sex=="All")
teenhpv_vac_data_clean <- teenhpv_vac_data_clean[,-4]
teenhpv_longer <- pivot_longer(teenhpv_vac_data_clean, -County, names_to=".value")

#Only keeping rows with measurement of child insurance rates + pivoting data
noins_child_data_clean <- noins_child_data_clean %>% 
  filter(measure=="Children without Health Insurance")
noins_child_longer <- pivot_longer(noins_child_data_clean, -County, names_to=".value")

#Combine longer data frames into one
alldata_comb <- rbind(noins_child_longer, houseinc_longer, teenhpv_longer)