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

#Removing "County" from county labels so that labels match
noins_child_data_clean$County <- str_remove_all(noins_child_data_clean$County, " County")
householdinc_data_clean$County <- str_remove_all(householdinc_data_clean$County, " County")
teenhpv_vac_data_clean$County <- str_remove_all(teenhpv_vac_data_clean$County, " County")

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

#Convert full data frame into a wider format for linear regression analysis
alldata_wider <- pivot_wider(alldata_comb, names_from = measure, values_from = value)

#General logical regression model generation for evaluation of the factors with most impact
set.seed(6440)
rnum <- runif(nrow(alldata_wider))
traindata <- subset(alldata_wider, rnum < 0.8)
testdata <- subset(alldata_wider, rnum >= 0.8)
tmultimodel <- lm(formula=`Up-To-Date Percent` ~ `Household Income ($)` + `Children without Health Insurance`, data=traindata)
tmm_predicted <- predict.lm(tmultimodel, newdata = testdata)
tmm_error <- tmm_predicted - testdata$`Up-To-Date Percent`
modelcor <- cor(tmm_predicted, testdata$`Up-To-Date Percent`, method = "pearson")
varcor <- cor(traindata[, c("Household Income ($)", "Children without Health Insurance", "Up-To-Date Percent")], method = "pearson")
mean(tmm_error)
summary(tmultimodel)
plot(tmultimodel)
modelcor
varcor

#Plotting the model
testdata$predicted <- tmm_predicted
ggplot(testdata, aes(x = `Up-To-Date Percent`, y = predicted)) +
  labs(x="Actual HPV Immunization Rate", y= "Predicted HPV Immunization Rate") +
  theme_light() +
  geom_point() +                                        # raw data points
  geom_abline(slope = 1, intercept = 0, color = "red")

#Plotting household income vs. health insurance percentage
ggplot(alldata_wider, aes(x=`Household Income ($)`, y=`Children without Health Insurance`)) +
  labs(x = "Median Household Income ($)", 
       y = "Children without Health Insurance Rate", 
       title = "Children without Health Insurance vs. Median Household Income",
       subtitle = "Colorado Counties") +
  theme_minimal() +
  geom_point(color = "steelblue", size = 2.5) +
  geom_vline(aes(xintercept = mean(`Household Income ($)`)), color = "lightblue", size = 1.5) +
  geom_smooth(method = "lm", linetype = "dashed", formula = y~x, color = "darkred")

