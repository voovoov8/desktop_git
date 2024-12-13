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




# 데이터 확인 ----
data   <-  read.csv("final_data.csv")
str(data)


colnames(data)
# 필요한 패키지 로드
library(dplyr)

# 결측값 개수 세기
missing_values <- data %>%
  summarise(
    lifeladder = sum(is.na(Life.Ladder)),
    house_price = sum(is.na(house_price)),
    loggdppercapita = sum(is.na(Log.GDP.per.capita)),
    gini = sum(is.na(gini)),
    employment_rate = sum(is.na(employment_rate)),
    socialsupport = sum(is.na(Social.support)),
    social_expenditure = sum(is.na(social_expenditure)),
    freedomtomakelifechoices = sum(is.na(Freedom.to.make.life.choices)),
    housing_expenditure_ratio = sum(is.na(housing_expenditure_ratio)),
    ownership = sum(is.na(Ownership))
  )

# 결과 출력
View(missing_values)

# 결측치가 있는 행 필터링
na_rows <- data[is.na(data$Social.support) | is.na(data$gini) | is.na(data$Ownership), c("Country.name", "Year", "Social.support", "gini", "Ownership")]

# 결과 출력
print(na_rows)



# 데이터 확인용 ----
data   <-  read.csv("data_final.csv")
str(data)
# Filter data to exclude 2020 and 2021
data <- data %>%
  filter(year < 2020)

# Verify the filtered data
str(data)
unique(data$year)

### -----
library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(tidyverse)
# 1. Average happiness by country with confidence intervals
happiness_stats <- data %>%
  group_by(countryname) %>%
  summarise(
    avg_happiness = mean(lifeladder, na.rm = TRUE),
    se = sd(lifeladder, na.rm = TRUE) / sqrt(n()),
    avg_gdp = mean(loggdppercapita, na.rm = TRUE),
    avg_gini = mean(gini, na.rm = TRUE)
  )

# Plot 1: Average Happiness by Country with Error Bars
p1 <- plot_ly(
  data = happiness_stats,
  x = ~reorder(countryname, avg_happiness),
  y = ~avg_happiness,
  type = 'bar',
  error_y = list(array = ~se * 1.96)
) %>%
  layout(
    title = 'Average Life Satisfaction by Country (2012-2021)',
    xaxis = list(title = '', tickangle = 45),
    yaxis = list(title = 'Life Satisfaction Score'),
    showlegend = FALSE
  )

# Plot 2: Life Satisfaction vs GDP Scatter
p2 <- plot_ly(
  data = data,
  x = ~loggdppercapita,
  y = ~lifeladder,
  color = ~countryname,
  type = 'scatter',
  mode = 'markers+lines',
  text = ~paste('Country:', countryname,
                '<br>Year:', year,
                '<br>GDP:', round(loggdppercapita, 2),
                '<br>Life Satisfaction:', round(lifeladder, 2))
) %>%
  layout(
    title = 'Life Satisfaction vs Log GDP per capita (2012-2021)',
    xaxis = list(title = 'Log GDP per capita'),
    yaxis = list(title = 'Life Satisfaction Score')
  )

# Plot 3: Life Satisfaction vs GINI
p3 <- plot_ly(
  data = data[!is.na(data$gini),],  # Remove NA values for GINI
  x = ~gini,
  y = ~lifeladder,
  color = ~countryname,
  type = 'scatter',
  mode = 'markers+lines',
  text = ~paste('Country:', countryname,
                '<br>Year:', year,
                '<br>GINI:', round(gini, 3),
                '<br>Life Satisfaction:', round(lifeladder, 2))
) %>%
  layout(
    title = 'Life Satisfaction vs GINI Coefficient',
    xaxis = list(title = 'GINI Coefficient'),
    yaxis = list(title = 'Life Satisfaction Score')
  )

# Display individual plots
p1
p2
p3

# Create combined subplot
subplot_all <- subplot(p1, p2, p3, nrows = 3, heights = c(0.4, 0.3, 0.3)) %>%
  layout(height = 1500, title = 'Life Satisfaction Analysis Dashboard')

subplot_all

# Plot 4: Life Satisfaction vs GINI (Clean Scatter Plot)
p4 <- plot_ly(
  data = data_filtered[!is.na(data_filtered$gini),],
  x = ~gini,
  y = ~lifeladder,
  color = ~countryname,
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 10),
  text = ~paste('Country:', countryname,
                '<br>Year:', year,
                '<br>GINI:', round(gini, 3),
                '<br>Life Satisfaction:', round(lifeladder, 2))
) %>%
  layout(
    title = 'Life Satisfaction vs GINI Coefficient (2012-2019)',
    xaxis = list(title = 'GINI Coefficient'),
    yaxis = list(title = 'Life Satisfaction Score'),
    showlegend = TRUE
  )

# Display the plot
p4


# Plot 5: Life Satisfaction vs House Prices
p5 <- plot_ly(
  data = data_filtered[!is.na(data_filtered$r_house_price),],
  x = ~r_house_price,
  y = ~lifeladder,
  color = ~countryname,
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 10),
  text = ~paste('Country:', countryname,
                '<br>Year:', year,
                '<br>House Price Index:', round(r_house_price, 1),
                '<br>Life Satisfaction:', round(lifeladder, 2))
) %>%
  layout(
    title = 'Life Satisfaction vs Real House Price Index (2012-2019)',
    xaxis = list(title = 'Real House Price Index'),
    yaxis = list(title = 'Life Satisfaction Score'),
    showlegend = TRUE
  )

# Display the plot
p5


# Calculate house price growth rates by country and year
house_price_growth <- data_filtered %>%
  group_by(countryname) %>%
  arrange(year) %>%
  mutate(price_growth = (r_house_price - lag(r_house_price))/lag(r_house_price) * 100)

# Create scatter plot of life satisfaction vs house price growth
p6 <- plot_ly(
  data = house_price_growth[!is.na(house_price_growth$price_growth),],
  x = ~price_growth,
  y = ~lifeladder,
  color = ~countryname,
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 10),
  text = ~paste('Country:', countryname,
                '<br>Year:', year,
                '<br>House Price Growth (%):', round(price_growth, 2),
                '<br>Life Satisfaction:', round(lifeladder, 2))
) %>%
  layout(
    title = 'Life Satisfaction vs House Price Growth Rate (2012-2019)',
    xaxis = list(title = 'House Price Growth Rate (%)'),
    yaxis = list(title = 'Life Satisfaction Score'),
    showlegend = TRUE
  )

# Display the plot
p6

# Create line plot of house price growth rates
p6 <- plot_ly(
  data = house_price_growth[!is.na(house_price_growth$price_growth),],
  x = ~year,
  y = ~price_growth,
  color = ~countryname,
  type = 'scatter',
  mode = 'lines+markers',
  marker = list(size = 8),
  text = ~paste('Country:', countryname,
                '<br>Year:', year,
                '<br>House Price Growth (%):', round(price_growth, 2),
                '<br>Life Satisfaction:', round(lifeladder, 2))
) %>%
  layout(
    title = 'House Price Growth Rate Trends by Country (2012-2019)',
    xaxis = list(title = 'Year', dtick = 1),
    yaxis = list(title = 'House Price Growth Rate (%)'),
    showlegend = TRUE
  )

# Display the plot
p6



# 공공지출과 행복지수
# Create scatter plot of life satisfaction vs social expenditure
p7 <- plot_ly(
  data = data_filtered[!is.na(data_filtered$social_expenditure),],
  x = ~social_expenditure,
  y = ~lifeladder,
  color = ~countryname,
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 10),
  text = ~paste('Country:', countryname,
                '<br>Year:', year,
                '<br>Social Expenditure (%):', round(social_expenditure, 2),
                '<br>Life Satisfaction:', round(lifeladder, 2))
) %>%
  layout(
    title = 'Life Satisfaction vs Social Expenditure (2012-2019)',
    xaxis = list(title = 'Social Expenditure (% of GDP)'),
    yaxis = list(title = 'Life Satisfaction Score'),
    showlegend = TRUE
  )

# Display the plot
p7


# Create scatter plot of life satisfaction vs ownership rates
p8 <- plot_ly(
  data = data_filtered[!is.na(data_filtered$ownership),],
  x = ~ownership,
  y = ~lifeladder,
  color = ~countryname,
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 10),
  text = ~paste('Country:', countryname,
                '<br>Year:', year,
                '<br>Ownership Rate (%):', round(ownership, 2),
                '<br>Life Satisfaction:', round(lifeladder, 2))
) %>%
  layout(
    title = 'Life Satisfaction vs Home Ownership Rate (2012-2019)',
    xaxis = list(title = 'Home Ownership Rate (%)'),
    yaxis = list(title = 'Life Satisfaction Score'),
    showlegend = TRUE
  )

# Display the plot
p8

# Create scatter plot of life satisfaction vs GDP and employment rate
p9 <- plot_ly(
  data = data_filtered[!is.na(data_filtered$employment_rate),],
  x = ~loggdppercapita,
  y = ~employment_rate,
  color = ~lifeladder,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    size = 10,
    colorscale = 'Viridis',
    showscale = TRUE,
    colorbar = list(title = 'Life Satisfaction')
  ),
  text = ~paste('Country:', countryname,
                '<br>Year:', year,
                '<br>Log GDP per capita:', round(loggdppercapita, 2),
                '<br>Employment Rate (%):', round(employment_rate, 2),
                '<br>Life Satisfaction:', round(lifeladder, 2))
) %>%
  layout(
    title = 'GDP vs Employment Rate with Life Satisfaction (2012-2019)',
    xaxis = list(title = 'Log GDP per capita'),
    yaxis = list(title = 'Employment Rate (%)'),
    showlegend = FALSE
  )

# Display the plot
p9



library(ggplot2)

ggplot(data = data_filtered[!is.na(data_filtered$r_house_price),], 
       aes(x = r_house_price, y = lifeladder)) +
  geom_line(aes(color = countryname), linewidth = 1) +
  geom_point(aes(color = countryname), size = 3) +
  facet_wrap(~countryname, scales = "free") +
  labs(title = "Life Satisfaction vs Real House Price Index (2012-2019)",
       x = "Real House Price Index",
       y = "Life Satisfaction Score") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

ggplot(data = data_filtered[!is.na(data_filtered$r_house_price),], 
       aes(x = r_house_price, y = lifeladder)) +
  geom_point(aes(color = countryname), size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~countryname, scales = "free") +
  labs(title = "Life Satisfaction vs Real House Price Index (2012-2019)",
       x = "Real House Price Index",
       y = "Life Satisfaction Score") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

str(data)
data2 <- read.csv("soci_exp_3.csv", header = TRUE, sep = ",")
str(data2)
colnames(data2)[colnames(data2) == "Reference.area"] <- "countryname"

library(tidyr)

data2_long <- data2 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(year = as.numeric(gsub("X", "", year)))

# Check the result
head(data2_long)
data2_long

library(dplyr)

data_merged <- data %>%
  left_join(
    data2_long %>% rename(soci_ex_2 = value),
    by = c("countryname", "year")
  )

# Check the merged result
str(data_merged)
write.csv(data_merged, "data_merged.csv", row.names = FALSE)
