#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# set up
library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Analysis on ANIME!"),
   # Menu
   navbarPage("MENU", 
              tabPanel("About Us",
                       sidebarLayout(headerPanel("About Us"),
                                     navlistPanel("Meet The Makers",
                                                  tabPanel("Liang-Hsuan Wang",
                                                           mainPanel(headerPanel("Liang-Hsuan Wang"),
                                                                     h3("National Taiwan Normal University"),
                                                                     h3("English Major"),
                                                                     a("Github: https://github.com/jason10130021/CS-X_106_Summer")
                                                           )
                                                  ),
                                                  tabPanel("Pei-Shan Tsai", 
                                                           mainPanel(headerPanel("Pei-Shan Tsai"),
                                                                     h3("National Cheng Chi University"),
                                                                     h3("Accounting Major"),
                                                                     a("Github: https://github.com/Pei4/cs-x-programming")
                                                           )
                                                  ),
                                                  tabPanel("Ying-Ru Lee", 
                                                           mainPanel(headerPanel("Ying-Ju Lee"),
                                                                     h3("National Taiwan Normal University"),
                                                                     h3("Applied Chinese Major"),
                                                                     a("Github: https://github.com/Lulu-Lee/106-Summer-Class")
                                                           ))
                                     )
                       )
                       ),
              navbarMenu("Proportion of Category and Feature",
                         tabPanel("Proportion of Different Categories",
                                    sidebarLayout(sidebarPanel(
                                                    selectInput("year1",
                                                                "Select A Year",  
                                                                choices=c("2008-2018", "2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008"),
                                                                multiple = FALSE
                                                                ),
                                                      hr(),
                                                      helpText("The plot shows the proportion of different anime categories in a particular year or over the past 11 years.")
                                                    ),
                                                  mainPanel(plotOutput("cate_pro_bar"))
                                    )
                                  ),
                         tabPanel("Proportion of Feature", 
                                    sidebarLayout(sidebarPanel(
                                                      selectInput("year2",
                                                                "Select A Year",  
                                                                choices=c("2008-2018", "2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008"),
                                                                multiple = FALSE
                                                                ),
                                                      hr(),
                                                      helpText("The plot shows the proportion of different anime features in a particular year or over the past 11 years.")
                                                    ),
                                                  mainPanel(plotOutput("feat_pro_bar"))
                                    )
                                  ),
                         tabPanel("Annual Top 3 Anime", 
                                  fluidRow(
                                    column(4,
                                           selectInput("year3",
                                                       "Select A Year",
                                                       choices=c("2008-2018", "2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008"),
                                                       multiple = FALSE
                                                       ),
                                            hr(),
                                            helpText("The table shows three most popular animes of a particular year or from 2008 to 2018.")
                                          )
                                  ),
                                  fluidRow(
                                    dataTableOutput("top_3_annual")
                                  )
                         )
              ),
              navbarMenu("Influence of Category and Feature", 
                         tabPanel("Popular Animes",
                                fluidRow(
                                  column(4,
                                         selectInput("year4",
                                                     "Select A Year",
                                                     choices=c("2008-2018", "2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008"),
                                                     multiple = FALSE
                                                     ),
                                         hr(),
                                         helpText("The table shows every anime whose popularity is greater than the third quartile of its belonging year.")
                                        )
                                  ),
                                  fluidRow(
                                    DT::dataTableOutput("over_Q3")
                                  )
                          ),
                         tabPanel("Counts of Categories over Belonging Year's Q3",
                                  sidebarLayout(sidebarPanel(
                                    selectInput("year5",
                                                "Select A Year",  
                                                choices=c("2008-2018", "2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008"),
                                                multiple = FALSE
                                                ),
                                        hr(),
                                        helpText("The plot shows the proportion of different anime features in a particular year or over the past 11 years.")
                                    ),
                                    mainPanel(plotOutput("cate_overQ3"))
                                  )
                          ), 
                         tabPanel("Counts of Features over Belonging Year's Q3",
                                  sidebarLayout(sidebarPanel(
                                    selectInput("year6",
                                                "Select A Year",  
                                                choices=c("2008-2018", "2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008"),
                                                multiple = FALSE
                                                ),
                                        hr(),
                                        helpText("The plot shows the proportion of different anime features in a particular year or over the past 11 years.")
                                    ),
                                    mainPanel(plotOutput("feat_overQ3"))
                                  )
                          ),
                         tabPanel("Category vs Feature", 
                                  sidebarLayout(sidebarPanel(
                                    selectInput("year7",
                                                "Select A Year",  
                                                choices=c("2008-2018", "2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008"),
                                                multiple = FALSE
                                                ),
                                        hr(),
                                        helpText("The plot shows the proportion of different anime features in a particular year or over the past 11 years.")
                                    ),
                                    mainPanel(plotOutput("compare_cate_feat"))
                                  )
                          )
              ), 
              navbarMenu("Popularity and Quantity", 
                         tabPanel("Popularity Changes among Categories",
                                  sidebarLayout(sidebarPanel(
                                    selectInput("cate",
                                                "Select A Category",
                                                choices=c("全部","料理美食 ","溫馨 ","社會寫實 ","推理懸疑 ","歷史傳記 ","運動競技 ","戀愛 ","幽默搞笑 ","科幻未來 ","靈異神怪 ","青春校園 ","奇幻冒險 ","其他 ")
                                                ),
                                        hr(),
                                        helpText("The plot shows changes of popularity Q3 among different categories every year.")
                                  ),
                                  mainPanel(plotOutput("popularityQ3_change"))
                                  )
                         ), 
                         tabPanel("Popularity Changes among Features",
                                  sidebarLayout(sidebarPanel(
                                    selectInput("feat",
                                                "Select A Feature",
                                                choices=c("全部","勵志","感動","熱血","女性取向","重視思考","發人深省","音樂音效","大眾取向","輕鬆歡樂","男性取向","心靈治癒","角色設定","世界觀","作品劇情","題材創意","聲優配音","動畫品質")
                                                ),
                                        hr(),
                                        helpText("The plot shows changes of popularity Q3 among different features every year.")
                                  ),
                                  mainPanel(plotOutput("popularityQ3_change_feature"))
                                  )
                         )
              ),
              h1("Source"),
              a("https://acg.gamer.com.tw/index.php?page=1&p=ANIME&t=1&tnum=4816")
    ) #navbarPage end
) #fluitPage end


   
              
  
# Define server logic required to draw a histogram
server <- function(input, output) {
   #bar plot: cate_pro_bar
   output$cate_pro_bar<- renderPlot({
     if (input$year1=="2008-2018"){
       anime_20xx<- animate
     }else {
       anime_20xx<- animate%>%
         filter(Year==input$year1)
     }
     ggplot(anime_20xx, aes(x= Category, fill=Category))+
      geom_bar()+
       labs(title=input$year1)
    })
  #bar plot: feat_pro_bar
   output$feat_pro_bar<- renderPlot({
     # input$year 
    if (input$year2=="2008-2018"){
         anime_20xx<- animate
    }else {
        anime_20xx<- animate%>%
          filter(Year==input$year2)
    }
     # feature_count
     feature_count_vector<- vector()
     for (i in c(1:nrow(anime_20xx))){
       temp <- strsplit(anime_20xx[i,3], split="[、]")[[1]]
       for (j in c(1:length(temp))) {
         feature_count_vector<- append(feature_count_vector, temp[j])
       }
     }
     feature_count<- data.frame(Feature= feature_count_vector)
     #  bar chart
     ggplot(feature_count, aes(x= Feature, fill=Feature))+
       geom_bar()+
       labs(title=input$year2)
   })
   #table: top_3_annual
   output$top_3_annual <- renderDataTable(datatable({
     if (input$year3=="2008-2018"){
       anime_20xx<- animate
     }else {
       anime_20xx<- animate%>%
         filter(Year==input$year3)
     }
     anime_20xx<- arrange(anime_20xx, desc(Popularity))
     top_3_annual<- anime_20xx[c(1:3),c(1:6)]
     top_3_annual
   }))
   #table: over_Q3 
   output$over_Q3<- DT::renderDataTable(DT::datatable({
     if (input$year4=="2008-2018"){
       anime_20xx<- animate
     }else {
       anime_20xx<- animate%>%
         filter(Year==input$year4)
     }
     summ<- summary(anime_20xx$Popularity)
     over_Q3<- anime_20xx%>%
      filter(Popularity>=summ[5])
     over_Q3
   }))
   #bar plot: cate_overQ3
   output$cate_overQ3<- renderPlot({
     #over_Q3
     if (input$year5=="2008-2018"){
       anime_20xx<- animate
     }else {
       anime_20xx<- animate%>%
         filter(Year==input$year5)
     }
     summ<- summary(anime_20xx$Popularity)
     over_Q3<- anime_20xx%>%
       filter(Popularity>=summ[5])
     #ggplot
     ggplot(over_Q3, aes(x=Category, fill=Category))+
       geom_bar()+
       labs(title=input$year5)
   })
   #bar plot: feat_overQ3
   output$feat_overQ3<- renderPlot({
     #over_Q3
     if (input$year6=="2008-2018"){
       anime_20xx<- animate
     }else {
       anime_20xx<- animate%>%
         filter(Year==input$year6)
     }
     summ<- summary(anime_20xx$Popularity)
     over_Q3<- anime_20xx%>%
       filter(Popularity>=summ[5])
     #over_Q3_feature_count
     over_Q3_feature_count<- data.frame()
     for (k in c(1:length(over_Q3))){
       temp<- strsplit(over_Q3[k,3], split='[、]')[[1]]
       for (l in c(1:3)){
         over_Q3_feature_count[3*(k-1)+l,1]<- temp[l]
       }
     }
     names(over_Q3_feature_count)<- "Feature"
     #ggplot
     ggplot(over_Q3_feature_count, aes(x=Feature, fill=Feature))+
       geom_bar()+
       labs(title=input$year6)
   })
   #bar plot: compare_cate_feat
   output$compare_cate_feat<- renderPlot({
     #over_Q3
     if (input$year7=="2008-2018"){
       anime_20xx<- animate
     }else {
       anime_20xx<- animate%>%
         filter(Year==input$year7)
     }
     summ<- summary(anime_20xx$Popularity)
     over_Q3<- anime_20xx%>%
       filter(Popularity>=summ[5])
     # build compare_feat
     compare_feat<- data.frame()
     for (m in c(1:nrow(over_Q3))){
       temp<- strsplit(over_Q3[m,3], split='[、]')[[1]]
       for (n in c(1:3)){
         compare_feat[3*(m-1)+n,1]<- temp[n]
       }
     }
     names(compare_feat)<- "Feature"
     # build compare_cate_feat
     compare_cate_feat<- data.frame()
     # count feature in compare_feat
     compare_feat<- mutate(compare_feat, count=1)
     compare_feat<- compare_feat%>%
       group_by(Feature)%>%
        summarize(sum(count))%>%
          as.data.frame()
     compare_feat<- arrange(compare_feat, desc(`sum(count)`))
     # count category in over_Q3
     over_Q3<- mutate(over_Q3, count=1)
     over_Q3<- over_Q3%>%
       group_by(Category)%>%
        summarize(sum(count))%>%
          as.data.frame()
     over_Q3<- arrange(over_Q3, desc(`sum(count)`))
     # import values in over_Q3 and compare_feat into compare_cate_feat
     compare_cate_feat[1,1]<- over_Q3[1,1]
     compare_cate_feat[1,2]<- over_Q3[1,2]
     compare_cate_feat[1,3]<- "Category"
     compare_cate_feat[2,1]<- compare_feat[1,1]
     compare_cate_feat[2,2]<- (compare_feat[1,2])/3
     compare_cate_feat[2,3]<- "Feature"
     names(compare_cate_feat)<- c("Name", "Counts", "Type")
     # plot compare_cate_feat
     ggplot(compare_cate_feat, aes(x=Type, y=Counts, fill=Name))+
       geom_col()+
       labs(title=input$year7, x="Category vs Feature", y="Counts")
   })
   output$popularityQ3_change<- renderPlot({
     popularityQ3_change <- data.frame()
     for(o in c(2008:2018)){
       anime_20xx <- animate %>% filter(Year == o)
       Cate2 <- unique(anime_20xx$Category)
       for (p in Cate2){
         anime_xx <- anime_20xx %>% filter(Category == p)
         summ =summary(anime_xx$Popularity)
         anime_xx <- as.vector(summ[5])
         Temp <- data.frame(Year=o, Category=p, Q3=anime_xx)
         popularityQ3_change <- rbind(Temp, popularityQ3_change)}
     }
     popularityQ3_change$Year<- sapply(popularityQ3_change$Year, as.numeric)
     if (input$cate=="全部"){
       popularityQ3_change<- popularityQ3_change
     }else{
       popularityQ3_change<- popularityQ3_change%>%
                              filter(Category==enc2utf8(input$cate))
     }
     ggplot(popularityQ3_change, aes(Year, Q3, color= Category))+
       geom_jitter()+
       geom_line()+
       scale_y_log10()
   })
   #popularityQ3_change_feature
   output$popularityQ3_change_feature<- renderPlot({
     popularityQ3_change_feature <- data.frame()
     completeFun <- function(data, desiredCols) {
       completeVec <- complete.cases(data[, desiredCols])
       return(data[completeVec, ])
     }
     splitTitle <- function(x){
       strsplit(x, split='[、]')[[1]]
     }
     FeatureList <- completeFun(animate, "Feature")
     FeatureList$Feature = sapply(as.character(FeatureList$Feature), splitTitle)
     Feat2 <- unique(unlist(FeatureList$Feature))
     for(q in c(2008:2018)){
       anime_20xx <- animate %>% filter(Year == q)
       anime_20xx <- completeFun(anime_20xx, "Feature")
       feature_vector <- vector()
       popularity_vector <- vector()
       Year_vector <- vector()
       for (r in c(1:nrow(anime_20xx))){
         temp <- strsplit(anime_20xx[r,3],split = "[、]")[[1]]
         for(s in c(1:length(temp))){
           temp2 <- anime_20xx[r,4]
           temp3 <- anime_20xx[r,7]
           feature_vector <- append(feature_vector,temp[s])
           popularity_vector <- append(popularity_vector, temp2)
           Year_vector <- append(Year_vector, temp3)
         }
       }
       featureCount <- data.frame(Feature=feature_vector, Popularity=popularity_vector, Year=Year_vector)
       for(t in Feat2){
         anime_xx <- featureCount %>% filter(Feature == t)
         summ =summary(anime_xx$Popularity)
         anime_xx <- as.vector(summ[5])
         Temp2 <- data.frame(Year=q, Feature=t, Q3=anime_xx)
         popularityQ3_change_feature <- rbind(Temp2, popularityQ3_change_feature)
       }
     }
     popularityQ3_change_feature$Year<- sapply(popularityQ3_change_feature$Year, as.numeric)
     if (input$feat=="全部"){
       popularityQ3_change_feature<- popularityQ3_change_feature
     }else{
       popularityQ3_change_feature<- popularityQ3_change_feature%>%
         filter(Feature==enc2utf8(input$feat))
     }
     ggplot(popularityQ3_change_feature, aes(Year, Q3, color= Feature))+
       geom_jitter()+
       geom_line()+
       scale_y_log10()
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

