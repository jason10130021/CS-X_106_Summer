#完整資料
beaver1
#雙變數:離散VS連續
ggplot(data=beaver1, aes(x=time, y=temp))+
  geom_point()
#雙變數:離散VS連續
ggplot(data=beaver1, aes(x=time, y=activ))+
  geom_point()
#雙變數:連續VS連續
ggplot(data=beaver1, aes(group=activ, y=temp))+
  geom_boxplot()