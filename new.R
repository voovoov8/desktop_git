data <- read.csv("저체중아출산.csv")
head(data)

# race 더미변수 생성
data$race_other <- ifelse(data$race == "Other", 1, 0)
data$race_black <- ifelse(data$race == "Black", 1, 0)
data$race_white <- ifelse(data$race == "White", 1, 0)

# smoke 더미변수 생성
data$smoke_dummy <- ifelse(data$smoke == "Smoker", 1, 0)

# 생성된 더미변수 확인
View(data)
write.csv(data, "저체중아출산_더미변수.csv", row.names = FALSE)
