library(tidyverse)
library(Hmisc)

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

#Creating subsets with only females or only males
male_vac_data_clean <- teenhpv_vac_data_clean %>% 
  filter(sex=="Male")
male_vac_data_clean <- male_vac_data_clean[,-4]
malevac_longer <- pivot_longer(male_vac_data_clean, -County, names_to=".value")

female_vac_data_clean <- teenhpv_vac_data_clean %>% 
  filter(sex=="Female")
female_vac_data_clean <- female_vac_data_clean[,-4]
femalevac_longer <- pivot_longer(female_vac_data_clean, -County, names_to=".value")

#Only keeping rows with both sexes included + pivoting data
teenhpv_vac_data_clean <- teenhpv_vac_data_clean %>% 
  filter(sex=="All")
teenhpv_vac_data_clean <- teenhpv_vac_data_clean[,-4]
teenhpv_longer <- pivot_longer(teenhpv_vac_data_clean, -County, names_to=".value")

#Combine longer data frames into one
alldata_comb <- rbind(noins_child_longer, houseinc_longer, teenhpv_longer)
maledata_comb <- rbind(noins_child_longer, houseinc_longer, malevac_longer)
femaledata_comb <- rbind(noins_child_longer, houseinc_longer, femalevac_longer)

#Convert full data frame into a wider format for linear regression analysis
alldata_wider <- pivot_wider(alldata_comb, names_from = measure, values_from = value)
maledata_wider <- pivot_wider(maledata_comb, names_from = measure, values_from = value)
femaledata_wider <- pivot_wider(femaledata_comb, names_from = measure, values_from = value)

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
modelcor #Pearson's correlation for the overall model
varcor #Pearson's correlation matrix for each individual pairing of variables

#Pearson's correlation
result <- rcorr(as.matrix(traindata[, c("Household Income ($)", 
                                        "Children without Health Insurance", 
                                        "Up-To-Date Percent")]), 
                type = "pearson")

result$r
result$P

#Plotting the model
testdata$predicted <- tmm_predicted
ggplot(testdata, aes(x = `Up-To-Date Percent`, y = predicted)) +
  labs(x="Actual HPV Immunization Rate", y= "Predicted HPV Immunization Rate") +
  theme_light() +
  geom_point() +                                        # raw data points
  geom_abline(slope = 1, intercept = 0, color = "red")

#Statistical analysis
lm(`Up-To-Date Percent`~`Household Income ($)`, alldata_wider)
lm(`Up-To-Date Percent`~`Children without Health Insurance`, alldata_wider)
summary(lm(`Children without Health Insurance`~`Household Income ($)`, alldata_wider))

# Figure 1: HPV Rate vs. Median Household Income
ggplot(alldata_wider, aes(x = `Household Income ($)`, y = `Up-To-Date Percent`)) +
  ylim(0, 1) +
  geom_point(color = "steelblue", size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
  labs(
    title = "Teen HPV Immunization Rate vs. Median Household Income",
    subtitle = "Colorado Counties",
    x = "Median Household Income ($)",
    y = "Up-to-Date HPV Immunization Rate"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("figure1_income_vs_hpv.png", width = 8, height = 6, dpi = 300)

# Figure 2: HPV Rate vs. Uninsured Rate
ggplot(alldata_wider, aes(x = `Children without Health Insurance`, y = `Up-To-Date Percent`)) +
  ylim(0, 1) +
  geom_point(color = "steelblue", size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
  labs(
    title = "Teen HPV Immunization Rate vs. Children Without Health Insurance",
    subtitle = "Colorado Counties",
    x = "Proportion of Children Without Health Insurance",
    y = "Up-to-Date HPV Immunization Rate"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("figure2_uninsured_vs_hpv.png", width = 8, height = 6, dpi = 300)

#Figure 3: Uninsured Rate vs. Median Household Income, quintile boxplot
alldata_wider_cut <- alldata_wider %>%
  mutate(income_bin = cut(`Household Income ($)`,
                          breaks = quantile(`Household Income ($)`, 
                                            probs = seq(0, 1, 0.2)),
                          include.lowest = TRUE,
                          labels = c("37-55", "55-65", "65-77 (State Average)", "77-93", "93-146")))

ggplot(alldata_wider_cut, aes(x = income_bin, y = `Children without Health Insurance`)) +
  geom_boxplot(aes(fill = income_bin == "65-77 (State Average)"), 
               outlier.shape = NA, 
               alpha = 0.5,
               linewidth = 1) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "lightblue"), guide = "none")+
  geom_jitter(width = 0.15, color = "steelblue", size = 2.5, alpha = 0.3) +
  labs(x = "Median Household Income Range ($k)",
       y = "Proportion of Children Without Health Insurance",
       title = expression(bold("Distribution of Uninsured Rate by Income Quintile")),
       subtitle = "Colorado Counties") +
  theme_minimal()

ggsave("figure3_uninsured_vs_income.png", width = 8, height = 6, dpi = 300)

