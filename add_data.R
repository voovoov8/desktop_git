library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)
library()



# 데이터 불러오기
employ_data <- read.csv("oecd_employ_or.csv")
gini_data <- read.csv("oecd_gini_or.csv") 
lifeex_data <- read.csv("oecd_lifeex_or.csv")
social_data <- read.csv("oecd_social_expenditure_pnm_or.csv")
main_data <- read.csv("data_2012_2023.csv")

# 데이터 확인
str(employ_data)
str(main_data)


# join 
# Rename columns and select necessary data from each dataset
employ_value <- employ_data %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  rename(code = REF_AREA,
         year = TIME_PERIOD,
         employ = OBS_VALUE)

gini_value <- gini_data %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  rename(code = REF_AREA,
         year = TIME_PERIOD,
         gini = OBS_VALUE)

lifeex_value <- lifeex_data %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  rename(code = REF_AREA,
         year = TIME_PERIOD,
         lifeex = OBS_VALUE)

social_value <- social_data %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  rename(code = REF_AREA,
         year = TIME_PERIOD,
         social = OBS_VALUE)

# Join all datasets with main_data
final_data <- main_data %>%
  left_join(employ_value, by = c("code", "year")) %>%
  left_join(gini_value, by = c("code", "year")) %>%
  left_join(lifeex_value, by = c("code", "year")) %>%
  left_join(social_value, by = c("code", "year"))
View(final_data)
str(final_data)
colnames(final_data)

# 컬럼명 정리 
final_data <- final_data %>%
  rename(
    employment_rate = employ,
    life_expectancy = lifeex,
    social_expenditure = social
  )
str(final_data)
