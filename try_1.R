#  데이터 로드
data <- read.csv("happiness_housing_add.csv")
str(data)
View(data)

# 필요한 패키지 설치 및 로드
install.packages(c("plm", "lmtest", "sandwich"))
library(plm)
library(lmtest)
library(sandwich)

# 패널 데이터 설정
pdata <- pdata.frame(data, index = c("Country.name", "year"))
View(pdata)

# 패널 OLS 모델 추정
panel_model <- plm(Life.Ladder ~ Log.GDP.per.capita + Social.support + 
                   Healthy.life.expectancy.at.birth + Freedom.to.make.life.choices + 
                   Generosity + Perceptions.of.corruption + Positive.affect + 
                   Negative.affect + house_price + pri_ratio + ptr_ratio + 
                   r_house_price + Rent_price,
                   data = pdata, model = "within")

# 결과 출력
summary(panel_model)

# 강건한 표준오차를 사용한 결과
coeftest(panel_model, vcov = vcovHC(panel_model, type = "HC1"))



library(plotly)

plot <- plot_ly(data, 
                x = ~house_price, 
                y = ~Life.Ladder, 
                type = 'scatter', 
                mode = 'markers',
                marker = list(size = 8),
                text = ~paste("Country:", Country.name)) %>%
        layout(title = "House Price vs Life Ladder Score",
               xaxis = list(title = "House Price"),
               yaxis = list(title = "Life Ladder Score"))

plot
