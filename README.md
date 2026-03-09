# Impact of health insurance and household income on teenage HPV immunization rates

Project Overview:
	This project examines the relationship between health insurance coverage, household 
	income, and teenage HPV immunization rates across Colorado counties. Data was sourced
	from Exploring Cancer in Colorado (ECCO).

Data Source:
	Provider: Exploring Cancer in Colorado (ECCO) https://coe-ecco.org
	Geographic Level: County-level data for Colorado
	Key Measures:
		Children without Health Insurance
		Household Income
		Up-to-date Percent Teen HPV Immunization

Data Cleaning and Tidying:
	Data preparation was performed in RStudio using R. The raw data comes in separate 
	files, each requiring slightly different cleaning steps. The following steps were
	taken to produce an analysis ready dataset.
	
	Health Insurance and Household Income Files
	
		Column Removal:
			"GeoID" and "State" columbs were dropped from each file as they aren't needed
			for county-level analysis. The state is constant across all observations.
		
		Measure Filtering:
			Each source file contained multiple measures. We filtered to keep only the
			measures relevant to our research question: "Children without Health Insurance"
			and "Household Income ($)".
		
		Reshaping:
			The data was pivoted to a longer format on a per-county basis using
			pivot_longer() to generate a tidy structure where each row represents a single
			county-measure observation.
			
	HPV Immunization File
		This file contains only one measure ("Up-To-Date Percent") but includes a "sex"
		column with values of Male, Female, or All. The following steps were applied.
		
		Column Removal:
			"GeoID" and "State" columbs were dropped.
		
		Sex filtering:
			We filtered to keep only the rows where sex == "All", as our analysis focuses
			on overall immunization rates rather than sex-specific breakdowns.
			
		Reshaping:
			The data was pivoted to a longer format on a per-county basis to match the 
			structure of the other two datasets.
			
	General
	
		Missing Values: We inspecgted all datasets for NA values and confirmed none were
		present. An na.omit() command is included in the pipeline as a safeguard but does
		not modify the data from its raw form.

	Clean Data Visualization:
		<img width="572" height="759" alt="image" src="https://github.com/user-attachments/assets/eac343be-b398-46f8-b440-7752b5f51ad0" />
		
Key Packages:
	tidyverse (dplyr, tidyr)
	
Repository Structure:

Authors:
Morignot, A.-E. and Kipp, O.
