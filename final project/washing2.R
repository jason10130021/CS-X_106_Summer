library(xlsx)
library(dplyr)
library(readxl)
data<- read.xlsx(file="C://Users/Loard Jason/Desktop/Data2.xlsx", sheetIndex=1, startRow = 1, header = TRUE, encoding = "UTF-8")
#turn Popularity into numeric
data$Popularity<- sapply(data$Popularity, as.numeric)
#delete --
for (i in c(1:2955)){
  if (data[i, 5]=="--"){
    data[i,5]<- NA
  }
}
#turn Rating into numeric
data$Rating<- sapply(data$Rating, as.numeric)
#turn date into time format
for (j in c(1:2955)){
  data[j,6]<- format(data[j,6], format="%Y-%m-%d")
}
#存檔
write.xlsx(data, "animate.xlsx")
saveRDS(data,"animate.rds")
