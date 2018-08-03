#title
title <- readLines("G://Programming/homework/CS-X_106_Summer/final project/Data/title.csv", encoding="big5")
title <- iconv(title, "big5", "utf8")
#type and time
typetime <- readLines("G://Programming/homework/CS-X_106_Summer/final project/Data/typetime.csv", encoding="big5")
typetime <- iconv(typetime, "big5", "utf8")
#feature
feature <- readLines("G://Programming/homework/CS-X_106_Summer/final project/Data/feature.csv", encoding="big5")
feature <- iconv(feature, "big5", "utf8")
#popularity
popularity <- readLines("G://Programming/homework/CS-X_106_Summer/final project/Data/popul.csv", encoding="big5")
popularity <- iconv(popularity, "big5", "utf8")
#rating
rating <- readLines("G://Programming/homework/CS-X_106_Summer/final project/Data/rating.csv", encoding="big5")
rating <- iconv(rating, "big5", "utf8")
#combining
time<- readLines("G://Programming/homework/CS-X_106_Summer/final project/Data/rating.csv", encoding="big5")
rating <- iconv(rating, "big5", "utf8")
Data<- data.frame(title=title, type=typetime, feature=feature, popularity=popularity, rating=rating, time= time, stringsAsFactors=FALSE)
#補齊
Data[1980,]<- c('"電影 K-ON！輕音部"', '"幽默搞笑 / 2011-12-03"', '"特色：音樂音效、動畫品質、聲優配音"', '"想看2147"', '"評分9.4"', '"e04"')
Data[1981,]<- c('"魯邦三世 血之刻印～永遠的 Mermaid～"', '"其他 / 2011-12-02"', '"特色：動畫品質、作品劇情、題材創意"','"想看44"', '"評分--"','"e04"')
#拆掉type 與 time
for (i in c(2: 2956)) {
  temp<- strsplit(Data[i,2], split='[/]')[[1]]
  Data[i,2]<- temp[1]
  Data[i,6]<- (temp[2])
}
#拿掉"
for (k in c(2:2956)){
  Data[k,2]<- sub('^.','',Data[k,2])
}
#存檔
write.table(Data, file="G://Programming/homework/CS-X_106_Summer/final project/Data/Data2.csv", sep=",", row.names=F, na = "NA")
Data<- Data[-1,]

