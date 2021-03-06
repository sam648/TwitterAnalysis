
require(shinydashboard)
require(shiny)
require(googleVis)
require(DT)
require(twitteR)
require(RCurl)
require(wordcloud) # text visual package
require(tm) # text mining package
require(RColorBrewer)
require(stringi)
require(stringr)
require(syuzhet)
require(ggplot2)
require(dplyr)
require(Hmisc)
require(shinythemes)
require(shinyBS)
require(wordcloud2)

shinyServer(function(input, output){
  connectTwitter()
  
  
  #################################### searching Tweets function #######################################
  search_tweet_df= reactive({
      input$SearchButton2
      SearchTweets(isolate(input$Candidates2), NoOfTweets2(),Tweets_DateRange2()[1],
                   Tweets_DateRange2()[2])
                  })  

  
  #################################### slider for Tweets  ############################################
  NoOfTweets2= reactive({
    return (input$NoOfTweets2)
  })  
 
 
  ################################### Daterange  #################################################
  Tweets_DateRange2 = reactive({
      #paste(as.character(input$dateRange), collapse = " to ")
        fromDate = as.character(input$dateRange2[1])
        toDate = as.character(input$dateRange2[2])
        return( c(fromDate, toDate) )
  }) 
 
 
  ################################# sentiments plot of peoples tweets  #############################################
  
  tot_sentiment2 = reactive({
    getSentiments(search_tweet_df())
  })
  
  # show columnchart using googleVis
  output$Sentiments2 = renderGvis({
    gvisColumnChart(tot_sentiment2(), xvar = "sentiment", yvar = "count",
                    options=list(
                      chartArea="{left:40,top:50,width:'90%',height:'75%'}",
                      bar="{groupWidth:'40%'}",
                      seriesType="bars",
                      hAxis= "{slantedText: true}",
                      legend= "{position : 'none'}", 
                      #title="Sentimental Analysis of tweets",
                      height = 450,
                      width = 1050
                    )
                )
  })
  

    ##############################333 wordCloud for people tweets ###########################################
  
   output$wordcloud2 = renderPlot({
               wordcloud(words = getTextData(search_tweet_df()),
                         scale=c(6,.2),min.freq=3,
                         max.words=200, random.order=FALSE, rot.per=.35, 
                         colors=brewer.pal(8, "Dark2")
               )
  })
 
  
  ################################ DataTable for tweets ###################################################
   
  output$table2 = DT::renderDataTable({
    datatable(search_tweet_df()[, c("text","screenName", "created"),drop = FALSE],
              extensions = 'AutoFill', 
              options = list(
                #  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  ColReorder = TRUE,
                  buttons = I('colvis'),
                  scrollX = TRUE,
                  fixedColumns = TRUE
                  )
              ) 
  })
  

  ################################################### Top 10 Users #####################################
  top10UserTweets = reactive({
    top10UserTweets = Top10UserTweets(search_tweet_df())
  })
   output$plot2 = renderGvis({
                 gvisBarChart(top10UserTweets(), xvar="screenName", yvar="TotalTweets",
                          options=list(
                           chartArea="{left:150,top:50,width:'100%',height:'70%'}",
                           legend="bottom", 
                           title="Users who tweeted most",
                           height = 400,
                           width = 700
                          )
                  )
     })

 
   ################################################## Candidates Tweets (Trump vs Clinton) ###############33###
    
    NoOfTweets1= reactive({
      return (input$NoOfTweets1)
    }) 
 
   # date range for candidates Tweets
    Tweets_DateRange1 = reactive({
     fromDate = as.character(input$dateRange1[1])
     toDate = as.character(input$dateRange1[2])
     return( c(fromDate, toDate) )
   }) 
   
   tweetsByUserDate_df= reactive({
     input$SearchButton1
     tweetsByUserDate(isolate(input$candidate1), NoOfTweets1(),Tweets_DateRange1()[1],
                  Tweets_DateRange1()[2])
   })  
   
  
   
  ###############################  Datatable for Candidates  tweets ##########################################
   output$table1 = DT::renderDataTable({
     datatable(tweetsByUserDate_df()[, c("text","retweetCount", "created"),drop = FALSE],
               extensions = 'AutoFill', 
               options = list(
                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                 ColReorder = TRUE,
                 buttons = I('colvis'),
                 scrollX = TRUE,
                 fixedColumns = TRUE
               )
     ) 
   })
 
  ############################# Calendar of Candidates Tweets  ##################################################
      Usertweets_calendar_df = reactive({
      Usertweets_calendar(input$candidate1, NoOfTweets1())
   })
   
  # show Calendar using googleVis
     output$Calendar1 = renderGvis({
                 gvisCalendar(Usertweets_calendar_df(), datevar = "Tweetdate", 
                          numvar = "totalTweets",
                          options=list(
                            title="Tweets in 2016",
                            chartArea="{left:350,top:50,width:'100%',height:'75%'}",
                            calendar="{yearLabel: { fontName: 'Times-Roman',
                            fontSize: 32, color: '#1A8763', bold: true},
                            cellSize: 20,
                            cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                            focusedCellColor: {stroke:'red'}}",
                            height = 300,
                            width = 1200))
    })
   

   ####################################### word cloud  of Candidates tweets #####################################
     
     output$wordcloud1 = renderPlot({
       wordcloud(words = getTextData(tweetsByUserDate_df()),
                 scale=c(6,.2),min.freq=3,
                 max.words=200, random.order=FALSE, rot.per=.35, 
                 colors=brewer.pal(8, "Dark2")
       )
     })
     
   
   ######################################## sentiment Plot of candidates ##########################################
     tot_sentiment1 = reactive({
       getSentiments(tweetsByUserDate_df())
     })
 
     output$Sentiments1 = renderGvis({
       gvisColumnChart(tot_sentiment1(), xvar = "sentiment", yvar = "count",
                       options=list(
                         chartArea="{left:40,top:50,width:'90%',height:'75%'}",
                         bar="{groupWidth:'40%'}",
                         seriesType="bars",
                         hAxis= "{slantedText: true}",
                         legend= "{position : 'none'}", 
                         #title="Sentimental Analysis of tweets",
                         height = 450,
                         width = 1050
                         )
                      )
     })
     

})