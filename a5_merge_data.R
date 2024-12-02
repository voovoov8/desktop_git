# Read the CSV file
data <- read.csv("data.csv")
or <- read.csv("ing_2012_2023.csv")
# View the first few rows
head(data)
View(data)
View(or)
colnames(data)

# Set first row as column names
colnames(data) <- as.character(data[1,])

# Rename second column to 'value'
names(data)[2] <- "value"

# Remove the first row
data <- data[-1,]

# View result
View(data)
# Filter rows for ownership types
data <- data[data$value %in% c("Own outright", "Owner with mortgage"), ]

# View result
View(data)


library(dplyr)
# Group by Country and sum ownership values
result <- aggregate(data[,3:15], by=list(Country=data$Country), FUN=function(x) {
  if(all(is.na(x))) {
    return(NA)
  } else {
    return(sum(as.numeric(x), na.rm=TRUE))
  }
})

# View result
View(result)
head(result)

# Convert to long format
library(tidyr)

long_data <- tidyr::pivot_longer(result, 
                                cols = -Country,
                                names_to = "Year", 
                                values_to = "Ownership")

# View result
View(long_data)


# Filter years 2013-2021
long_data_filtered <- long_data[long_data$Year >= 2012 & long_data$Year <= 2021,]

# View result
View(long_data_filtered)



# Replace 0 values with NA in Ownership column
long_data_filtered$Ownership[long_data_filtered$Ownership == 0] <- NA

# View result
View(long_data_filtered)

write.csv(long_data_filtered, "Ownership_clean.csv", row.names = FALSE)




data <- read.csv("Ownership_clean.csv")

# Count total number of NA values
na_count <- sum(is.na(data$Ownership))

# Get rows with NA values showing Country and Year
na_rows <- data[is.na(data$Ownership), c("Country", "Year")]

# Display results
print(paste("Total number of NA values:", na_count))
print("Countries and Years with NA values:")
print(na_rows)
str(data)




# 2012-2021 데이터 불러오기
data_2012_2021 <- read.csv("inging_2012_2021.csv")
View(data_2012_2021)

data <- read.csv("Ownership_clean.csv")
View(data)




# Get unique country names from both datasets
countries_2012_2021 <- unique(data_2012_2021$Country.name)
countries_ownership <- unique(data$Country)

# Print both lists
print("Countries in data_2012_2021:")
print(countries_2012_2021)
print("\nCountries in Ownership data:")
print(countries_ownership)

# Find differences between the two lists
countries_only_in_2012_2021 <- setdiff(countries_2012_2021, countries_ownership)
countries_only_in_ownership <- setdiff(countries_ownership, countries_2012_2021)

print("\nCountries only in data_2012_2021:")
print(countries_only_in_2012_2021)
print("\nCountries only in Ownership data:")
print(countries_only_in_ownership)
# Rename the countries
data$Country <- gsub("Slovak Republic", "Slovakia", data$Country)
data$Country <- gsub("Türkiye", "Turkiye", data$Country)
data$Country <- gsub("Korea", "South Korea", data$Country)


# Verify the changes
unique(data$Country)
unique(data_2012_2021$Country.name)

unique(data$Country)
unique(data_2012_2021$Country.name)

library(dplyr)

# Rename year to Year
colnames(data_2012_2021)[colnames(data_2012_2021) == "year"] <- "Year"

# Verify the change
names(data_2012_2021)
# View(data_2012_2021)
# Check data types
class(data_2012_2021$year)
class(data$Year)

# Convert both Year columns to numeric
data_2012_2021$Year <- as.numeric(data_2012_2021$Year)
data$Year <- as.numeric(data$Year)

# Now try the left join again
merged_data <- data_2012_2021 %>%
  left_join(data[c("Country.name", "Year", "Ownership")], 
            by = c("Country.name", "Year"))

# View result
View(merged_data)



# 데이터 확인 ----
str(merged_data)
# 행복지수 없는 나라 확인
# Create complete sequence of years for each country
complete_data <- expand.grid(
  Country.name = unique(merged_data$Country.name),
  Year = 2012:2021
)

# Find missing combinations by anti-joining with merged_data
missing_rows <- anti_join(
  complete_data,
  merged_data,
  by = c("Country.name", "Year")
)

# Display countries and years with completely missing rows
print(missing_rows)



# 보간 시도 ----
write.csv(merged_data, "final_data.csv", row.names = FALSE)
