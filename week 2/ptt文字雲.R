#網路爬蟲
library(xml2)
library(tmcn)
library(rvest)
pttTestFunction <- function(URL, filename)
{
  #URL   = "https://www.ptt.cc/bbs/NTUcourse/index.html"
  html  = read_html(URL)
  title = html_nodes(html, "a")
  href  = html_attr(title, "href")
  data = data.frame(title = toUTF8(html_text(title)),
                    href = href)
  data = data[-c(1:10),]
  getContent <- function(x) {
    url  = paste0("https://www.ptt.cc", x)
    tag  = html_node(read_html(url), 'div#main-content.bbs-screen.bbs-content')
    text = toUTF8(html_text(tag))
  }
  #getContent(data$href[1])
  allText = sapply(data$href, getContent)
  allText
  #out <- file(filename, "w", encoding="BIG-5") 
  write.table(allText, filename) 
  #close(out) 
}

#PTT爐石版
id = c(1:1)
URL = paste0("https://www.ptt.cc/bbs/Hearthstone/index", id, ".html")
filename = paste0(id, ".txt")
pttTestFunction(URL[1], filename[1])
mapply(pttTestFunction, 
       URL = URL, filename = filename)


#文本清理

rm(list=ls(all.names = TRUE))
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines)
docs <- Corpus(VectorSource(files))
#移除可能有問題的符號
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
docs <- tm_map(docs, toSpace, "※")
docs <- tm_map(docs, toSpace, "◆")
docs <- tm_map(docs, toSpace, "‧")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "我")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "看板")
docs <- tm_map(docs, toSpace, "作者")
docs <- tm_map(docs, toSpace, "發信站")
docs <- tm_map(docs, toSpace, "批踢踢實業坊")
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
docs <- tm_map(docs, toSpace, "推")
docs <- tm_map(docs, toSpace, "標題")
docs <- tm_map(docs, toSpace, "時間")
docs <- tm_map(docs, toSpace, "開版")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "在")
docs <- tm_map(docs, toSpace, "防守")
docs <- tm_map(docs, toSpace, "就")
docs <- tm_map(docs, toSpace, "不")
docs <- tm_map(docs, toSpace, "有")
docs <- tm_map(docs, toSpace, "不")
docs <- tm_map(docs, toSpace, "他")
docs <- tm_map(docs, toSpace, "恭喜")
docs <- tm_map(docs, toSpace, "玩")
docs <- tm_map(docs, toSpace, "閒聊")
docs <- tm_map(docs, toSpace, "文")
docs <- tm_map(docs, toSpace, "遊戲")
docs <- tm_map(docs, toSpace, "文章")
docs <- tm_map(docs, toSpace, "等")
docs <- tm_map(docs, toSpace, "討論")
docs <- tm_map(docs, toSpace, "開板")
docs <- tm_map(docs, toSpace, "戰記")
docs <- tm_map(docs, toSpace, "大家")
docs <- tm_map(docs, toSpace, "分享")
docs <- tm_map(docs, toSpace, "噓")
docs <- tm_map(docs, toSpace, "都")
docs <- tm_map(docs, toSpace, "警告")
docs <- tm_map(docs, toSpace, "水桶")
docs <- tm_map(docs, toSpace, "目前")
docs <- tm_map(docs, toSpace, "連署")
docs <- tm_map(docs, toSpace, "到此一遊")
docs <- tm_map(docs, toSpace, "人")
docs <- tm_map(docs, toSpace, "請")
docs <- tm_map(docs, toSpace, "阿")
docs <- tm_map(docs, toSpace, "要")
docs <- tm_map(docs, toSpace, "章")
docs <- tm_map(docs, toSpace, "會")
docs <- tm_map(docs, toSpace, "之")
docs <- tm_map(docs, toSpace, "中")
docs <- tm_map(docs, toSpace, "喔")
docs <- tm_map(docs, toSpace, "以為")
docs <- tm_map(docs, toSpace, "如果")
docs <- tm_map(docs, toSpace, "板規")
docs <- tm_map(docs, toSpace, "最近")
docs <- tm_map(docs, toSpace, "這個")
docs <- tm_map(docs, toSpace, "一些")
docs <- tm_map(docs, toSpace, "啊")
docs <- tm_map(docs, toSpace, "能")
docs <- tm_map(docs, toSpace, "可")
docs <- tm_map(docs, toSpace, "還")

#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)

#詞頻矩陣

mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[order(freqFrame$Freq,decreasing=TRUE), ]
library(knitr)
kable(head(freqFrame), format = "markdown")

#製作文字雲

wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.5),min.freq=50,max.words=150,
          random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
