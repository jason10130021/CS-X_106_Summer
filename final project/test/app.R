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
                                                                     h3()
                                                           )),
                                                  tabPanel("Ying-Ju Lee", 
                                                           mainPanel(headerPanel("Ying-Ju Lee"),
                                                                     h3()
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
                                    DT::dataTableOutput("top_3_annual")
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
              navbarMenu("Part 2", 
                         tabPanel("first"), 
                         tabPanel("second"), 
                         tabPanel("third"))
   )
)#fluitPage end


   
              
  
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
     feature_count_vector<- as.vector()
     for (i in c(1:nrow(anime_20xx))){
       temp<- strsplit(anime_20xx[i,3], split="[¡B]")[[1]]
       for (j in c(1:length(temp))) {
         feature_count_vector<- append(feature_count, temp[j])
       }
     }
     feature_count<- data.frame(Feature= feature_count_vector)
     #  bar chart
     ggplot(feature_count, aes(x= Feature, fill=Feature))+
       geom_bar()+
       labs(title=input$year2)
   })
   #table: top_3_annual
   output$top_3_annual <- DT::renderDataTable(DT::datatable({
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
     
     #ggplot
     ggplot(over_Q3_feature_count, aes(x=Feature, fill=Feature))+
       geom_bar()+
       labs(title=input$year6)
   })
   #bar plot: compare_cate_feat
   output$compare_cate_feat<- renderPlot({
     #over_W3
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
       temp<- strsplit(over_Q3[i,3], split='[¡B]')[[1]]
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
     compare_cate_feat[2,2]<- compare_feat[1,2]
     compare_cate_feat[2,3]<- "Feature"
     names(compare_cate_feat)<- c("Name", "Counts", "Type")
     # plot compare_cate_feat
     ggplot(compare_cate_feat, aes(x=Type, y=Counts, fill=Name))+
       geom_col()+
       labs(title=input$year7, x="Category vs Feature", y="Counts")
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

