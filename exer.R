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
# 
# ==============================================================================
## OECD 회원국 목록으로 정리하기
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
#  oecd 국가들의 시계열 데이터 상태 확인
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
