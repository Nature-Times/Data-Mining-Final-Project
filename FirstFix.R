datasetone <- read.csv("energy1.csv", sep=",")
View(datasetone)
colnames(datasetone)

datasettwo <- read.csv("energy2.csv", sep=",")
View(datasetsecond)
colnames(datasetsecond)

library(dplyr)

datasettwo <- datasettwo %>%
  filter(Year==2016)

View(datasettwo)

unique_values_one <- unique(datasetone$name)
print(unique_values_one)

unique_values_second <- unique(datasettwo$Country)
print(unique_values_second)

difference_one <- setdiff(unique_values_one, unique_values_second)

difference_two <- setdiff(unique_values_second, unique_values_one)

if (length(difference_one) > 0) {
  print("Values in datasetone but not in datasetsecond:")
  print(difference_one)
}

if (length(difference_two) > 0) {
  print("Values in datasetsecond but not in datasetone:")
  print(difference_two)
}

datasetone_filtered <- datasetone %>%
  filter(name %in% unique_values_second)

datasettwo_filtered <- datasettwo %>%
  filter(Country %in% unique_values_one)

View(datasetone_filtered)
View(datasettwo_filtered)

unique_values_one <- unique(datasetone_filtered$name)
print(unique_values_one)

unique_values_second <- unique(datasettwo_filtered$Country)
print(unique_values_second)

difference_one <- setdiff(unique_values_one, unique_values_second)

difference_two <- setdiff(unique_values_second, unique_values_one)

if (length(difference_one) > 0) {
  print("Values in datasetone but not in datasetsecond:")
  print(difference_one)
}

if (length(difference_two) > 0) {
  print("Values in datasetsecond but not in datasetone:")
  print(difference_two)
}

View(datasettwo_filtered)
     
library(tidyverse)

print(colnames(datasettwo_filtered))

# Group by Country and Year, then summarize all energy types into individual columns
reshaped_data <- datasettwo_filtered %>%
  group_by(Country, Year) %>%
  summarize(
    GDP = first(GDP),
    Population = first(Population),
    Energy_intensity_per_capita = first(Energy_intensity_per_capita),
    Energy_intensity_by_GDP = first(Energy_intensity_by_GDP),
    
    Energy_consumption_coal = sum(Energy_consumption[Energy_type == "coal"], na.rm = TRUE),
    Energy_consumption_natural_gas = sum(Energy_consumption[Energy_type == "natural_gas"], na.rm = TRUE),
    Energy_consumption_petroleum = sum(Energy_consumption[Energy_type == "petroleum_n_other_liquids"], na.rm = TRUE),
    Energy_consumption_nuclear = sum(Energy_consumption[Energy_type == "nuclear"], na.rm = TRUE),
    Energy_consumption_renewables = sum(Energy_consumption[Energy_type == "renewables_n_others"], na.rm = TRUE),
    
    Energy_production_coal = sum(Energy_production[Energy_type == "coal"], na.rm = TRUE),
    Energy_production_natural_gas = sum(Energy_production[Energy_type == "natural_gas"], na.rm = TRUE),
    Energy_production_petroleum = sum(Energy_production[Energy_type == "petroleum_n_other_liquids"], na.rm = TRUE),
    Energy_production_nuclear = sum(Energy_consumption[Energy_production == "nuclear"], na.rm = TRUE),
    Energy_production_renewables = sum(Energy_consumption[Energy_production == "renewables_n_others"], na.rm = TRUE),
    
    CO2_emission_coal = sum(CO2_emission[Energy_type == "col"], na.rm = TRUE),
    CO2_emission_natural_gas = sum(CO2_emission[Energy_type == "natural_gas"], na.rm = TRUE),
    CO2_emission_petroleum = sum(CO2_emission[Energy_type == "petroleum_n_others_liquids"], na.rm = TRUE),
    CO2_emission_nuclear = sum(CO2_emission[Energy_type == "nuclear"], na.rm = TRUE),
    CO2_emission_renewables = sum(CO2_emission[Energy_type == "renewables_n_others"], na.rm = TRUE),
  ) %>%
  ungroup()

# View the reshaped data
View(reshaped_data)

library(dplyr)

# Merge the datasets based on common columns (Country and Year)
merged_data <- left_join(datasetone_filtered, reshaped_data, by = c("name" = "Country"))

# View the merged dataset
View(merged_data)
