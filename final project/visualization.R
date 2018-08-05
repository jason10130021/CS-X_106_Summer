library(ggplot2)
library(dplyr)
#哪種類型最受歡迎 (依年次)
#各年度bar chart
ggplot(animate, aes(x= Category, fill=Category))+
  geom_bar()+
  facet_wrap(.~Year)
#總和pie chart
ggplot(animate, aes(x= factor(1), fill = Category)) +
  geom_bar(width=1)+
  coord_polar(theta = "y")
#特定年次動畫類型佔比
year_input<- 2017
particular_year<- animate%>%
  filter(Year==year_input)        #以2017為例，後應改為input內容，須注意input後為數字或字串
#pie chart
ggplot(particular_year, aes(x=factor(1), fill= Category))+
  geom_bar(width=1)+
  coord_polar(theta="y")+
  labs(title=paste(as.character(year_input), "年動畫類型佔比"))
#bar chart
ggplot(particular_year, aes(x=Category, fill= Category))+
  geom_bar()+
  labs(title=paste(as.character(year_input), "年動畫類型佔比"))
#特定年次動畫人氣比較
ggplot(particular_year, aes(x=Category, y=Popularity, fill=Category))+
  geom_col()+
  labs(title=paste(as.character(year_input), "年動畫類型人氣比較"))
#指定時段內，動畫...
year_start<- 2008
year_end<- 2015     #這邊一樣先自己設值，最後應改為input的
time_period<- animated%>%
  filter(Year>=year_start, Year<=year_end)
#後面就不畫圖，套用前面任何格式圖

#特定類型各年度人氣變化
category_input<- animate[2605, 2]
particular_category<- animate%>%
  filter(Category==category_input)
ggplot(particular_category, aes(x=Year, y=Popularity))+
  geom_col(position="identity")+
  labs(title=paste(category_input, "類型各年度人氣變化"))
#各類型年度人氣變化
ggplot(animate, aes(x=Year, y=Popularity))+
  geom_col()+
  facet_wrap(.~Category)
#各年度類型人氣變化
ggplot(animate, aes(x=Category, y=log(Popularity), fill=Category))+
  geom_col()+
  facet_wrap(.~Year)


#2. 每年種類與特色變化 (依人氣)
#3. 人氣帶動產量 or 產量帶動人氣