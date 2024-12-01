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
long_data_filtered <- long_data[long_data$Year >= 2013 & long_data$Year <= 2021,]

# View result
View(long_data_filtered)



# Replace 0 values with NA in Ownership column
long_data_filtered$Ownership[long_data_filtered$Ownership == 0] <- NA

# View result
View(long_data_filtered)

write.csv(long_data_filtered, "Ownership_clean.csv", row.names = FALSE)
