library(ggplot2)
#完整資料
beaver1
#雙變數:離散VS連續
ggplot(data=beaver1, aes(x=time, y=temp))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_grid(.~day)
#雙變數:離散VS離散
ggplot(data=beaver1, aes(x=factor(time), y=activ))+
  geom_point()+
  facet_grid(.~day)
#雙變數:離散VS連續
ggplot(data=beaver1, aes(x=factor(activ), y=temp))+
  geom_boxplot()