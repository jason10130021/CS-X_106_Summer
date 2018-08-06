library(ggplot2)
library(dplyr)
library(gridExtra)
# 種類 VS 特色
# 1. 種類、特色占比
# 1.1 十年間種類占比 
#  bar chart
ggplot(animate, aes(x= Category, fill=Category))+
  geom_bar()+
  facet_wrap(.~Year)
#  pie chart
ggplot(animate, aes(x= factor(1), fill = Category)) +
  geom_bar(width=1)+
  coord_polar(theta = "y")
# 1.2 2008-2018 每年人氣前三
top_3<- data.frame()
for (i in c(2008:2018)){
  temp<- animate%>%
    filter(Year==i)%>%
      arrange(desc(Popularity))
  top_3<- rbind(top_3, temp[c(1:3),])
}
#將結果建立成清單並存檔
saveRDS(top_3, "top_3.rds")
# 1.3 使用top_3清單製作十一年間種類占比
#  bar chart
ggplot(top_3, aes(x= Category, fill=Category))+
  geom_bar()+
  labs(title=paste("2008-2018年間動畫類型數量"))
#  pie chart
ggplot(top_3, aes(x= factor(1), fill = Category)) +
  geom_bar(width=1)+
  coord_polar(theta = "y")+
  labs(title=paste("2008-2018年間動畫類型佔比"))
# 1.4 使用top_3清單製作十一年間特色占比
# 先統計出top_3中的特色
feature_count<- data.frame()
for (j in c(1:33)){
  temp<- strsplit(top_3[j,3], split='[、]')[[1]]
  for (k in c(1:3)){
    feature_count[3*(j-1)+k,1]<- temp[k]
  }
}
names(feature_count)<- "Feature"
#  bar chart
ggplot(feature_count, aes(x= Feature, fill=Feature))+
  geom_bar()+
  labs(title=paste("2008-2018年間動畫特色數量"))
#  pie chart
ggplot(feature_count, aes(x= factor(1), fill = Feature)) +
  geom_bar(width=1)+
  coord_polar(theta = "y")+
  labs(title=paste("2008-2018年間動畫特色佔比"))

# 2. 人氣指標: 種類 VS 特色
# 2.1 建立over_Q3清單 (人氣高於該年Q3者)
over_Q3<- data.frame()
for (i in c(2008:2018)){
  anime_20xx<- animate%>%
    filter(Year==i)
  summ<- summary(anime_20xx$Popularity)
  anime_20xx<- anime_20xx%>%
    filter(Popularity>=summ[5])
  over_Q3<- rbind(over_Q3, anime_20xx)
  plot_20xx<- ggplot(anime_20xx, aes(x=(as.Date(MD, format="%m-%d")), y=Popularity, col=Category))+
    geom_point()+
    scale_y_continuous(limits = c(0,3000))+
    geom_hline(aes(yintercept=summ[5]))+
    labs(title=paste0(i), x="Time")
  name<- paste0("plot_", i)
  assign(name, plot_20xx)
}
saveRDS(over_Q3, "over_Q3.rds")



# 2.2 每年人氣超過Q3的動畫
grid.arrange(plot_2008, plot_2009, plot_2010, plot_2011, plot_2012, plot_2013, plot_2014, plot_2015, plot_2016, plot_2017, plot_2018, as.table=FALSE, newpage = FALSE)
# 單看某一年分
year<- 2017        #先亂設一個
anime_20xx<- animate%>%
  filter(Year==year)
summ<- summary(anime_20xx$Popularity)
ggplot(anime_20xx, aes(x=(as.Date(MD, format="%m-%d")), y=Popularity, col=Category))+
  geom_point()+
  scale_y_continuous(limits= c(0,3000))+
  geom_hline(aes(yintercept=summ[5]))+
  labs(title=year, x="Time")    #X軸有個他媽的2018
# 2.3 人氣高於該年Q3動畫之類型數量比較
ggplot(over_Q3, aes(x=Category, fill=Category))+
  geom_bar()+
  labs(title=paste("人氣高於該年Q3動畫之類型數量比較"))
# 2.4 建立 over_Q3_feature_count
over_Q3_feature_count<- data.frame()
for (k in c(1:738)){
  temp<- strsplit(over_Q3[k,3], split='[、]')[[1]]
  for (l in c(1:3)){
    over_Q3_feature_count[3*(k-1)+l,1]<- temp[l]
  }
}
names(over_Q3_feature_count)<- "Feature"
saveRDS(over_Q3_feature_count, "over_Q3_feature_count.rds")
# 2.5 人氣高於該年Q3動畫之特色數量比較 
ggplot(over_Q3_feature_count, aes(x=Feature, fill=Feature))+
  geom_bar()+
  labs(title=paste("人氣高於該年Q3動畫之特色數量比較"))+
  scale_y_continuous(limits=c(0,500))

# 3. 特定年次類型與特色顯著性比較
# 3.1 先建立over_Q3_20xx 和 feature_count_20xx
year<- 2009    #亂設一個
over_Q3_20xx<- over_Q3%>%
  filter(Year==year)
feature_count_20xx<- data.frame()
for (i in c(1:nrow(over_Q3_20xx))){
  temp<- strsplit(over_Q3_20xx[i,3], split='[、]')[[1]]
  for (j in c(1:3)){
    feature_count_20xx[3*(i-1)+j,1]<- temp[j]
  }
}
names(feature_count_20xx)<- "Feature"
# 3.2.1 建立要呈現的data.frame
show<- data.frame()
# 3.2.2 將 feature_count_20xx 中的特色計次
feature_count_20xx<- mutate(feature_count_20xx, count=1)
feature_count_20xx<- feature_count_20xx%>%
  group_by(Feature)%>%
    summarize(sum(count))%>%
      as.data.frame()
feature_count_20xx<- arrange(feature_count_20xx, desc(`sum(count)`))
# 3.2.3 將 over_Q3_20xx 中的類型計次
over_Q3_20xx<- mutate(over_Q3_20xx, count=1)
over_Q3_20xx<- over_Q3_20xx%>%
  group_by(Category)%>%
    summarize(sum(count))%>%
      as.data.frame()
over_Q3_20xx<- arrange(over_Q3_20xx, desc(`sum(count)`))
# 3.2.4 將 over_Q3_20xx 和 feature_count_20xx 中數值匯入 show
show[1,1]<- over_Q3_20xx[1,1]
show[1,2]<- over_Q3_20xx[1,2]
show[1,3]<- "類型"
show[2,1]<- feature_count_20xx[1,1]
show[2,2]<- feature_count_20xx[1,2]
show[2,3]<- "特色"
names(show)<- c("Name", "Counts", "Type")
# 3.2.5 將show 畫成圖
ggplot(show, aes(x=Type, y=Counts, fill=Name))+
  geom_col()+
  labs(title=paste0(year, "年最多動畫數類型與特色"), x="類型VS特色", y="數量")




# 4. 過去11年類型與特色顯著性比較
# 4.1 先建立feature_count_20xx
feature_count<- data.frame()
for (i in c(1:nrow(over_Q3))){
  temp<- strsplit(over_Q3[i,3], split='[、]')[[1]]
  for (j in c(1:3)){
    feature_count[3*(i-1)+j,1]<- temp[j]
  }
}
names(feature_count)<- "Feature"
# 4.2.1 建立要呈現的data.frame
show<- data.frame()
# 4.2.2 將 feature_count 中的特色計次
feature_count<- mutate(feature_count, count=1)
feature_count<- feature_count%>%
  group_by(Feature)%>%
  summarize(sum(count))%>%
  as.data.frame()
feature_count<- arrange(feature_count, desc(`sum(count)`))
# 4.2.3 將 over_Q3_20xx 中的類型計次
over_Q3<- mutate(over_Q3, count=1)
over_Q3<- over_Q3%>%
  group_by(Category)%>%
  summarize(sum(count))%>%
  as.data.frame()
over_Q3<- arrange(over_Q3, desc(`sum(count)`))
# 4.2.4 將 over_Q3_20xx 和 feature_count_20xx 中數值匯入 show
show[1,1]<- over_Q3[1,1]
show[1,2]<- over_Q3[1,2]
show[1,3]<- "類型"
show[2,1]<- feature_count[1,1]
show[2,2]<- feature_count[1,2]
show[2,3]<- "特色"
names(show)<- c("Name", "Counts", "Type")
# 4.2.5 將show 畫成圖
ggplot(show, aes(x=Type, y=Counts, fill=Name))+
  geom_col()+
  labs(title=paste0(year, "2008-2018年間最多動畫數類型與特色"), x="類型VS特色", y="數量")




