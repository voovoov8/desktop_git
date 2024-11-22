# ==============================================================================
# 패키지 로드
# ==============================================================================
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
write.csv(happiness_oecd_with_code, "happiness_oecd_with_code.csv", row.names = FALSE)




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