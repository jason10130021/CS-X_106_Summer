library(xlsx)
library(dplyr)
library(readxl)
library(reshape)
#turn Date back to character
animate$Date<- sapply(animate$Date, as.character)
#??��?�年??�日
animate<- animate%>%
  mutate(Year= strsplit(Date, split= "[-]")[[1]][1], Month=strsplit(Date, split= "[-]")[[1]][2], date=strsplit(Date, split= "[-]")[[1]][3])
for (i in c(1:2934)){
  temp<- strsplit(animate[i,6], split= "[-]")[[1]]
  animate[i,7]<- temp[1]
  animate[i,8]<- temp[2]
  animate[i,9]<- temp[3]
}
animate<- rename(animate, c(Date="Time", date="Date"))
#turn Year, Month, Date as numeric
animate$Year<- sapply(animate$Year, as.numeric)
animate$Month<- sapply(animate$Month, as.numeric)
animate$Date<- sapply(animate$Date, as.numeric)

#turn "統�?�中" into NA
for (j in c(1:2934)){
  if (animate[j,3]=="統�?�中"){
    animate[j,3]<- NA
  }
}
#turn Title, Category and Feature into characters
animate$Title<- sapply(animate$Title, as.character)
animate$Category<- sapply(animate$Category, as.character)
animate$Feature<- sapply(animate$Feature, as.character)
#??��?�修�?
#1074 DOG DAYS
animate[1074,]<- c("DOG DAYS", "奇幻??�險", "角色設�?�、聲?��??�音?���?�畫??�質", 269, 8.5, "2015-01-10", 2015, 1, 10)
animate[1074,2]<- animate[1075,2]
animate[1074,3]<- animate[1055,3]
#1040
animate[1040,1]<- "??��?��?��?�女 All Stars ?��天�?��?�年?��"

#turn Rating and Popularity into numeric
animate$Rating<- sapply(animate$Rating, as.numeric)
animate$Popularity<- sapply(animate$Popularity, as.numeric)
#�s�W��B�鬰MD
animate<- animate%>%
  mutate(MD= sub('^......','',Time))
#�s��
saveRDS(animate,"animate.rds")
