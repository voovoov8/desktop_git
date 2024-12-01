# 데이터 중 국가명 달라서 누락된 데이터 발견 

## 1. 데이터 불러오기 ----
happiness_oecd_with_code <- read_csv("happiness_oecd_with_code.csv")
nominal_hp <- read_csv("/Users/yunchaeho/Desktop/exer/DB_PJ_stata/Nominal_house_price_indices.csv")
pti_ratio <- read_csv("/Users/yunchaeho/Desktop/exer/DB_PJ_stata/Price_to_income_ratio.csv")
ptr_ratio <- read_csv("/Users/yunchaeho/Desktop/exer/DB_PJ_stata/Price_to_rent_ratio.csv")
r_house_price <- read_csv("/Users/yunchaeho/Desktop/exer/DB_PJ_stata/Real_house_price_indices.csv")
Rent_price <- read_csv("/Users/yunchaeho/Desktop/exer/DB_PJ_stata/Rent_price.csv")

## 2. 국가명 전환 ----
# nominal_hp data frame column renaming
names(nominal_hp) <- gsub("Slovak Republic", "Slovakia", names(nominal_hp))
names(nominal_hp) <- gsub("Korea", "South Korea", names(nominal_hp))

# pti_ratio data frame column renaming
names(pti_ratio) <- gsub("Slovak Republic", "Slovakia", names(pti_ratio))
names(pti_ratio) <- gsub("Korea", "South Korea", names(pti_ratio))

# ptr_ratio data frame column renaming
names(ptr_ratio) <- gsub("Slovak Republic", "Slovakia", names(ptr_ratio))
names(ptr_ratio) <- gsub("Korea", "South Korea", names(ptr_ratio))

# r_house_price data frame column renaming
names(r_house_price) <- gsub("Slovak Republic", "Slovakia", names(r_house_price))
names(r_house_price) <- gsub("Korea", "South Korea", names(r_house_price))

# Rent_price data frame column renaming
names(Rent_price) <- gsub("Slovak Republic", "Slovakia", names(Rent_price))
names(Rent_price) <- gsub("Korea", "South Korea", names(Rent_price))

## 3. 확인 ----
# Check column names for each dataset
print("Nominal HP column names:")
names(nominal_hp)

print("PTI Ratio column names:")
names(pti_ratio)

print("PTR Ratio column names:")
names(ptr_ratio)

print("Real House Price column names:")
names(r_house_price)

print("Rent Price column names:")
names(Rent_price)


## 4. 데이터 조인 ----
### hp 데이터 시계열 형식 풀기
nominal_hp_long <- nominal_hp %>%
  pivot_longer(
    cols = -`Time period`,  # Time period 컬럼을 제외한 모든 열을 변환
    names_to = "country",   # 국가명들을 country 컬럼으로
    values_to = "house_price"  # 값들을 house_price 컬럼으로
  ) %>%
  rename(year = `Time period`)  # Time period를 year로 변경

happiness_housing <- happiness_oecd_with_code %>%
  left_join(
    nominal_hp_long,
    by = c("Country name" = "country", "year" = "year")
  )
View(happiness_housing)

### pti 데이터 시계열 형식 풀기----
#### 1. 시계열 형식 풀기
pti_ratio_or <- pti_ratio %>%
  pivot_longer(
    cols = -`Time period`,  # Time period 컬럼을 제외한 모든 열을 변환
    names_to = "country",   # 국가명들을 country 컬럼으로
    values_to = "pri_ratio"  # 값들을 house_price 컬럼으로
  ) %>%
  rename(year = `Time period`)  # Time period를 year로 변경

#### 2. 바탕으로 조인하기
happiness_h_p <- happiness_housing %>%
  left_join(
    pti_ratio_or,
    by = c("Country name" = "country", "year" = "year")
  )
View(happiness_h_p)

### ptr 추가 
# 1. 시계열 형식 풀기
ptr_ratio_or <- ptr_ratio %>%
  pivot_longer(
    cols = -`Time period`,  # Time period 컬럼을 제외한 모든 열을 변환
    names_to = "country",   # 국가명들을 country 컬럼으로
    values_to = "ptr_ratio"  # 값들을 house_price 컬럼으로
  ) %>%
  rename(year = `Time period`)  # Time period를 year로 변경


# 2. 바탕으로 조인하기
happiness_h_p_ptr <- happiness_h_p %>%
  left_join(
    ptr_ratio_or,
    by = c("Country name" = "country", "year" = "year")
  )
View(happiness_h_p_ptr)


### rhpa 추가 
# 1. 시계열 형식 풀기
r_house_price_added <- r_house_price %>%
  pivot_longer(
    cols = -`Time period`,  # Time period 컬럼을 제외한 모든 열을 변환
    names_to = "country",   # 국가명들을 country 컬럼으로
    values_to = "r_house_price"  # 값들을 house_price 컬럼으로
  ) %>%
  rename(year = `Time period`)  # Time period를 year로 변경

str(r_house_price_added)

# 2. 바탕으로 조인하기
happiness_h_p_ptr_rhpa <- happiness_h_p_ptr %>%
  left_join(
    r_house_price_added,
    by = c("Country name" = "country", "year" = "year")
  )
View(happiness_h_p_ptr_rhpa)


### Rent_price 추가 
# 1. 시계열 형식 풀기
Rent_price_added <- Rent_price %>%
  pivot_longer(
    cols = -`Time period`,  # Time period 컬럼을 제외한 모든 열을 변환
    names_to = "country",   # 국가명들을 country 컬럼으로
    values_to = "Rent_price"  # 값들을 house_price 컬럼으로
  ) %>%
  rename(year = `Time period`)  # Time period를 year로 변경

str(Rent_price_added)

# 2. 바탕으로 조인하기
happiness_h_p_ptr_rhpa_rp <- happiness_h_p_ptr_rhpa %>%
  left_join(
    Rent_price_added,
    by = c("Country name" = "country", "year" = "year")
  )
View(happiness_h_p_ptr_rhpa_rp)

# 컬럼명 정리 
happiness_housing_add <- happiness_h_p_ptr_rhpa_rp %>%
  rename(code = area)
str(happiness_housing_add)

write.csv(happiness_housing_add, "happiness_housing_add.csv", row.names = FALSE)




# 5. 데이터 시각화----
data <- read.csv("ing_2012_2023.csv")
str(data)
View(data)

library(ggplot2)

# 결측치 시각화
ggplot(data, aes(x = year, y = `Country.name`)) +
  geom_tile(aes(fill = is.na(Life.Ladder))) +
  scale_fill_manual(values = c("lightblue", "red"), 
                   labels = c("데이터 있음", "결측치"),
                   name = "데이터 상태") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(x = "연도", 
       y = "국가명",
       title = "Life.Ladder 결측치 현황") +
  theme(plot.title = element_text(hjust = 0.5))

library(plotly)
library(dplyr)
library(tidyr)

# 데이터 처리
plot_data <- data %>%
  mutate(missing = as.integer(is.na(Life.Ladder))) %>%
  pivot_wider(names_from = year, values_from = missing, id_cols = Country.name)

# Plotly를 사용한 히트맵 생성
plot_ly(z = as.matrix(plot_data[,-1]), 
        x = colnames(plot_data)[-1], 
        y = plot_data$Country.name, 
        type = "heatmap",
        colors = colorRamp(c("yellow", "blue")),
        text = as.matrix(plot_data[,-1]),
        texttemplate = "%{text}",
        hoverinfo = "x+y+z") %>%
  layout(title = "Missing Values in Life.Ladder by Year and Country",

         xaxis = list(
           title = "Year",
           tickangle = 45,
           tickfont = list(size = 8)
         ),
         yaxis = list(title = "Country"))





# 6. 함수화 ----

### 6-1 ggplot 함수 만들기 ----
create_missing_plot <- function(data, x1) {
  ggplot(data, aes(x = year, y = `Country.name`)) +
    geom_tile(aes(fill = is.na(!!sym(x1)))) +
    scale_fill_manual(values = c("lightblue", "red"),
                     labels = c("데이터 있음", "결측치"),
                     name = "데이터 상태") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8)) +
    labs(x = "연도",
         y = "국가명",
         title = paste(x1, "결측치 현황")) +
    theme(plot.title = element_text(hjust = 0.5))
}

# Usage example:
create_missing_plot(data, "Life.Ladder")


### 6-2. plotly 함수 만들기 ----
create_missing_heatmap <- function(data, x1) {
  plot_data <- data %>%
    mutate(missing = as.integer(is.na(!!sym(x1)))) %>%
    pivot_wider(names_from = year, values_from = missing, id_cols = Country.name)
  
  plot_ly(z = as.matrix(plot_data[,-1]), 
          x = colnames(plot_data)[-1], 
          y = plot_data$Country.name, 
          type = "heatmap",
          colors = colorRamp(c("yellow", "blue")),
          text = as.matrix(plot_data[,-1]),
          texttemplate = "%{text}",
          hoverinfo = "x+y+z") %>%
    layout(title = paste("Missing Values in", x1, "by Year and Country"),
           xaxis = list(
             title = "Year",
             tickangle = 45,
             tickfont = list(size = 8)
           ),
           yaxis = list(title = "Country"))
}



# 7. 함수 적용하기 ----
colnames(data)
### 7-1. plotly 함수 적용 ----
create_missing_heatmap(data, "gini")

### 7-2. ggplot 함수 적용 ----
create_missing_plot(data, "house_price")
