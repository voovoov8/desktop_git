# ==============================================================================
# 패키지 로드
# ==============================================================================
install.packages("readr")
library(readr)
library(dplyr)

# ==============================================================================
# 데이터 로드
# ==============================================================================
happiness <- read_csv("happiness ladder.csv")
View(happiness)


# ==============================================================================
# 1. OECD 회원국 목록으로 정리하기

# ==============================================================================
# OECD 회원국 목록
oecd_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica",
  "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "South Korea", "Latvia",
  "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway",
  "Poland", "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden",
  "Switzerland", "Türkiye", "United Kingdom", "United States"
)

# OECD 국가들만 필터링
happiness_oecd <- happiness[happiness$`Country name` %in% oecd_countries, ]

# 결과 확인
View(happiness_oecd)
length(unique(happiness_oecd$`Country name`)) #34개국이 들어가게 되었다
length(unique(oecd_countries)) #38개국 둥 2개국이 누락

# 누락된 OECD 국가 찾기
setdiff(oecd_countries, happiness_oecd$`Country name`)
# "Czech Republic", "Korea", "Slovak Republic", "Turkey" # 4개의 국가 누락 
# 따라서 oecd_countries를 수정함 




# ==============================================================================
# 2. oecd 국가들의 시계열 데이터 상태 확인
# ==============================================================================
# 수정 결과 총 38 개국을 모두 알게 됨
str(happiness_oecd)
setdiff(oecd_countries, happiness_oecd$`Country name`)

# 각 국가별 연도 확인 및 공통 연도 찾기
str(happiness_oecd)
# 데이터 시각화를 위한 패키지 로드
library(ggplot2)
library(tidyr)
# 한글 새팅
theme_set(theme_grey(base_family='NanumGothic')) # 나눔고딕 ; 가장 깔끔함

# 데이터 존재 여부를 확인하는 히트맵 만들기
happiness_oecd %>%
  mutate(value = 1) %>%
  ggplot(aes(x = `Country name`, y = year)) +
  geom_tile(aes(fill = value)) +
  geom_hline(yintercept = 2012, color = "red", linewidth = 0.5, alpha = 0.7) +  # 2012년 참조선 추가
  scale_fill_gradient(low = "white", high = "brown") +
  theme_minimal(base_family='NanumGothic') +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    figure.width = 12,
    aspect.ratio = 1
  ) +
  labs(title = "OECD 국가별 데이터 가용성",
       x = "국가",
       y = "연도",
       fill = "데이터 존재") +
  coord_flip() +
  scale_y_continuous(breaks = unique(happiness_oecd$year))

# 해복 사다리만 기준으로 평가한다면? (2013년 이후 기준)
# 2013년 이후 데이터 필터링 및 연도별 국가 수 계산
happiness_oecd %>%
  filter(year >= 2013) %>%
  group_by(year) %>%
  summarise(
    country_count = n_distinct(`Country name`),
    .groups = 'drop'
  ) %>%
  arrange(year)

# 가로 막대 그래프 생성
happiness_oecd %>%
  filter(year >= 2013) %>%
  group_by(year) %>%
  summarise(
    country_count = n_distinct(`Country name`),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = year, y = country_count)) +
  geom_col(fill = "#233bd5d6") +
  coord_flip() +
  theme_minimal(base_family='NanumGothic') +
  labs(title = "연도별 OECD 국가 데이터 수",
       x = "연도",
       y = "국가 수") +
  geom_text(aes(label = country_count), hjust = -0.2) +
  scale_y_continuous(limits = c(0, 40))  # y축 범위 설정

# 연도별 누락된 국가 확인
missing_countries <- happiness_oecd %>%
  filter(year >= 2013) %>%
  group_by(year) %>%
  summarise(
    missing_countries = paste(setdiff(oecd_countries, `Country name`), collapse = ", "),
    missing_count = length(setdiff(oecd_countries, `Country name`)),
    .groups = 'drop'
  ) %>%
  arrange(year)

# 결과 출력
print("연도별 누락된 국가 목록:")
print(missing_countries) # 38 * 11중 7개 누락, 13년 2개국 누락

# happiness_oecd 데이터를 CSV 파일로 저장
write.csv(happiness_oecd, "happiness_oecd_2013_2023.csv", row.names = FALSE)


# ==============================================================================
# 3. 데이터 추가
# ==============================================================================
# oecd 국가들을 아우를 수 있는 데이터들을 불러와 보는 것으로 해야겠다.


#### oecd cpi_w
# OECD CPI 데이터 불러오기
oecd_cpi <- read_csv("/Users/yunchaeho/Desktop/exer/DB_PJ_stata/oecd_cpi_weghted.csv")

# 데이터 확인
str(oecd_cpi)
View(oecd_cpi)
# 국가 이름과 ISO 코드 매핑 생성
country_codes <- c(
    "Australia" = "AUS", "Austria" = "AUT", "Belgium" = "BEL", 
    "Canada" = "CAN", "Chile" = "CHL", "Colombia" = "COL", 
    "Costa Rica" = "CRI", "Czechia" = "CZE", "Denmark" = "DNK",
    "Estonia" = "EST", "Finland" = "FIN", "France" = "FRA",
    "Germany" = "DEU", "Greece" = "GRC", "Hungary" = "HUN",
    "Iceland" = "ISL", "Ireland" = "IRL", "Israel" = "ISR",
    "Italy" = "ITA", "Japan" = "JPN", "South Korea" = "KOR",
    "Latvia" = "LVA", "Lithuania" = "LTU", "Luxembourg" = "LUX",
    "Mexico" = "MEX", "Netherlands" = "NLD", "New Zealand" = "NZL",
    "Norway" = "NOR", "Poland" = "POL", "Portugal" = "PRT",
    "Slovakia" = "SVK", "Slovenia" = "SVN", "Spain" = "ESP",
    "Sweden" = "SWE", "Switzerland" = "CHE", "Türkiye" = "TUR",
    "United Kingdom" = "GBR", "United States" = "USA"
)

# oecd_countries에 ISO 코드 추가
oecd_countries_with_code <- data.frame(
    country_name = oecd_countries,
    area = unname(country_codes[oecd_countries])
)
View(oecd_countries_with_code)
str(happiness_oecd)

# Add ISO codes to happiness_oecd
happiness_oecd_with_code <- happiness_oecd %>%
  mutate(area = country_codes[`Country name`])

# Check the result
View(happiness_oecd_with_code)
# write.csv(happiness_oecd_with_code, "happiness_oecd_with_code.csv", row.names = FALSE)
happiness_oecd_with_code <- read_csv("happiness_oecd_with_code.csv")




### test ###
## 코드 바탕으로 cpi_w 붙여보기 
str(oecd_cpi)
str(happiness_oecd_with_code)

# Merge the datasets
happiness_oecd_with_cpi <- happiness_oecd_with_code %>%
  left_join(
    select(oecd_cpi, area, year, cpi_w),
    by = c("area", "year")
  )

# Check the result
View(happiness_oecd_with_cpi)

### test 이상없음 ###

library(readr)
library(tidyr)
library(dplyr)
##############################################################
# housing data 추가
nominal_hp <- read_csv("/Users/yunchaeho/Desktop/exer/DB_PJ_stata/Nominal_house_price_indices.csv")
str(nominal_hp)
View(nominal_hp)

nominal_hp_long <- nominal_hp %>%
  pivot_longer(
    cols = -`Time period`,  # Time period 컬럼을 제외한 모든 열을 변환
    names_to = "country",   # 국가명들을 country 컬럼으로
    values_to = "house_price"  # 값들을 house_price 컬럼으로
  ) %>%
  rename(year = `Time period`)  # Time period를 year로 변경

str(nominal_hp_long)
str(happiness_oecd_with_code)


happiness_housing <- happiness_oecd_with_code %>%
  left_join(
    nominal_hp_long,
    by = c("Country name" = "country", "year" = "year")
  )
View(happiness_housing)
data_oecd <- read.csv("happiness_oecd_with_code.csv")

##############################################################
pti_ratio <- read_csv("/Users/yunchaeho/Desktop/exer/DB_PJ_stata/Price_to_income_ratio.csv")
str(pti_ratio)

# 1. 시계열 형식 풀기
pti_ratio_or <- pti_ratio %>%
  pivot_longer(
    cols = -`Time period`,  # Time period 컬럼을 제외한 모든 열을 변환
    names_to = "country",   # 국가명들을 country 컬럼으로
    values_to = "pri_ratio"  # 값들을 house_price 컬럼으로
  ) %>%
  rename(year = `Time period`)  # Time period를 year로 변경

str(pti_ratio_or)
View(pti_ratio_or)

# 2. 바탕으로 조인하기
happiness_h_p <- happiness_housing %>%
  left_join(
    pti_ratio_or,
    by = c("Country name" = "country", "year" = "year")
  )
View(happiness_h_p)


##############################################################
ptr_ratio <- read_csv("/Users/yunchaeho/Desktop/exer/DB_PJ_stata/Price_to_rent_ratio.csv")
r_house_price <- read_csv("/Users/yunchaeho/Desktop/exer/DB_PJ_stata/Real_house_price_indices.csv")
Rent_price <- read_csv("/Users/yunchaeho/Desktop/exer/DB_PJ_stata/Rent_price.csv")
str(Rent_price)
# Rename countries
# nominal_hp 데이터프레임 국가명 변경
nominal_hp$Country.name <- ifelse(nominal_hp$Country.name == "Slovak Republic", "Slovakia", nominal_hp$Country.name)
nominal_hp$Country.name <- ifelse(nominal_hp$Country.name == "Korea", "South Korea", nominal_hp$Country.name)

# pti_ratio 데이터프레임 국가명 변경
pti_ratio$Country.name <- ifelse(pti_ratio$Country.name == "Slovak Republic", "Slovakia", pti_ratio$Country.name)
pti_ratio$Country.name <- ifelse(pti_ratio$Country.name == "Korea", "South Korea", pti_ratio$Country.name)

# ptr_ratio 데이터프레임 국가명 변경
ptr_ratio$Country.name <- ifelse(ptr_ratio$Country.name == "Slovak Republic", "Slovakia", ptr_ratio$Country.name)
ptr_ratio$Country.name <- ifelse(ptr_ratio$Country.name == "Korea", "South Korea", ptr_ratio$Country.name)

# r_house_price 데이터프레임 국가명 변경
r_house_price$Country.name <- ifelse(r_house_price$Country.name == "Slovak Republic", "Slovakia", r_house_price$Country.name)
r_house_price$Country.name <- ifelse(r_house_price$Country.name == "Korea", "South Korea", r_house_price$Country.name)

# Rent_price 데이터프레임 국가명 변경
Rent_price$Country.name <- ifelse(Rent_price$Country.name == "Slovak Republic", "Slovakia", Rent_price$Country.name)
Rent_price$Country.name <- ifelse(Rent_price$Country.name == "Korea", "South Korea", Rent_price$Country.name)

################################################################
# ptr 추가 
# 1. 시계열 형식 풀기
ptr_ratio_or <- ptr_ratio %>%
  pivot_longer(
    cols = -`Time period`,  # Time period 컬럼을 제외한 모든 열을 변환
    names_to = "country",   # 국가명들을 country 컬럼으로
    values_to = "ptr_ratio"  # 값들을 house_price 컬럼으로
  ) %>%
  rename(year = `Time period`)  # Time period를 year로 변경

str(ptr_ratio_or)

# 2. 바탕으로 조인하기
happiness_h_p_ptr <- happiness_h_p %>%
  left_join(
    ptr_ratio_or,
    by = c("Country name" = "country", "year" = "year")
  )
View(happiness_h_p_ptr)


################################################################
# rhpa 추가 
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


####################################################################
# Rent_price 추가 
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


happiness_housing_add <- happiness_h_p_ptr_rhpa_rp %>%
  rename(code = area)
str(happiness_housing_add)

# 저장 
write.csv(happiness_housing_add, "happiness_housing_add.csv", row.names = FALSE)


