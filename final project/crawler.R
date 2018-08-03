library(xml2)
library(tmcn)
library(rvest)
#URL   = https://acg.gamer.com.tw/index.php?page=1&p=ANIME&t=1&tnum=4816
featureData<- data.frame()

for (j in c(1:196)){
  URL<- paste0("https://acg.gamer.com.tw/index.php?page=", j,"&p=ANIME&t=1&tnum=4816")
  html  = read_html(URL)
  for (i in c(1:15)) {
    featurepath = paste0('')
    featuretarget = xml_find_all(html, featurepath)
    feature = xml_text(featuretarget)  
    rownum<- 15*(j-1)+i
    featureData[rownum,1] <- feature 
  }
}
write.table(featureData, file="G://Programming/homework/CS-X_106_Summer/final project/featureData.csv", sep=",", row.names=F, na = "NA")




