# Inserting two datasets
datasetone <- read.csv("energy1.csv", sep=",")
View(datasetone)
colnames(datasetone)

datasettwo <- read.csv("energy2.csv", sep=",")
View(datasettwo)
colnames(datasettwo)

library(dplyr)

# Filtering to only year 2016
datasettwo <- datasettwo %>%
  filter(Year==2016)

View(datasettwo)

# Integrating by country
# Looking for the same countries in both dataset
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

# Filtering the dataset with the same countries
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

# Preprocessing
dim(merged_data)

# Removing column with a lot of missing values and zeroes
merged_data$coal_net_imports <- NULL
merged_data$coal_net_exports <- NULL
merged_data$gas_net_imports <- NULL
merged_data$gas_net_exports <- NULL
merged_data$oil_net_imports <- NULL
merged_data$oil_net_exports <- NULL
merged_data$Energy_consumption_renewables <- NULL
merged_data$Energy_production_nuclear <- NULL
merged_data$coal_year <- NULL
merged_data$coal_units <- NULL
merged_data$gas_year <- NULL
merged_data$gas_units <- NULL
merged_data$oil_year <- NULL
merged_data$oil_units <- NULL
merged_data$Energy_production_renewables <- NULL
merged_data$CO2_emission_coal <- NULL
merged_data$CO2_emission_petroleum <- NULL
merged_data$CO2_emission_nuclear <- NULL
merged_data$CO2_emission_renewables <- NULL
merged_data$Year <- NULL

merged_data <- na.omit(merged_data)

merged_data <- merged_data %>%
  mutate(across(everything(), ~ifelse(. == 0, mean(., na.rm = TRUE), .)))

# boxplot(merged_data$energy_consumption_btu)

summary(merged_data)

# Splitting country names and other data to be normalized
country_names <- data.frame(merged_data$name)
merged_data <- subset(merged_data, select = -name)

# Change percentage to decimal
merged_data <- as.data.frame(apply(merged_data,2, function(x){
  as.numeric(sub("%", "", x, fixed=TRUE))/100
}))
  
# Normalize
#install.packages('caret')
library(caret)

minMax <- preProcess(merged_data, method=c("range"))

normalised_data <- predict(minMax, merged_data)

# Correlation Matrix
#install.packages("corrplot")
library(corrplot)

correlation_matrix <- cor(normalised_data)
corrplot(correlation_matrix, order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.5)

# After EDA, we can remove columns with weak correlation with others
normalised_data$re_nuclear <- NULL
normalised_data$coal_yearly_deficit.surplus <- NULL
normalised_data$gas_yearly_deficit.surplus <- NULL
normalised_data$oil_daily_deficit.surplus <- NULL
normalised_data$oil_consump <- NULL
normalised_data$gas_consump <- NULL
normalised_data$coal_consump <- NULL
normalised_data$co2_emiss_one_year_change <- NULL
normalised_data$gas_exports <- NULL
normalised_data$coal_exports <- NULL
normalised_data$oil_exports <- NULL

# We realized there are similar meaning columns because of integrating
# two different dataset
normalised_data$population_2016 <- NULL


correlation_matrix <- cor(normalised_data)
corrplot(correlation_matrix, order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.5)

# Merging country names with the data
final_data = cbind(country_names, normalised_data)

View(final_data)

# Training using k-means
kmeans.result <- kmeans(normalised_data, centers=3)
kmeans.result$centers
kmeans.result$cluster

centers <- kmeans.result$centers[kmeans.result$cluster, ] # "centers" is a data frame of 3 centers but the length of iris dataset so we can canlculate distance difference easily.

distances <- sqrt(rowSums((normalised_data - centers)^2))

outliers <- order(distances, decreasing=T)[1:5]

print(outliers)

print(final_data[outliers, "merged_data.name"])
print(country_names[outliers, ])

columns_used = c("GDP", "Energy_intensity_by_GDP")
plot(normalised_data[,columns_used], pch=19, col=kmeans.result$cluster, cex=1)
#points(kmeans.result$centers[,columns_used], col=1:3, pch=15, cex=2)
points(normalised_data[outliers, columns_used], pch="+", col=4, cex=3)

# Plot 3D
#install.packages("scatterplot3d")
library("scatterplot3d")
install.packages("plot3D")

columns_used = c("GDP", "pc_yearly_btu", "energy_consumption_btu")
s3d <- scatterplot3d(normalised_data[, columns_used], pch=19, color=kmeans.result$cluster
              , angle=120)
s3d$points3d(normalised_data[outliers, columns_used], pch="+", col=4, cex=3)
