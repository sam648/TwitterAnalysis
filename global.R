
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

#############################twitter connection#####################################################
connectTwitter = function(){
  consumer_key = 'bE7OZyqGwtVaJmowVEsUNdFmf'
  consumer_secret = 'UWjxpnscW3LooEUSKbgTaI1u3TAv13AZbZmsfuka6QQfzfi5pr'
  access_token = '701978984888868865-OZRSuWjJFJw5z53Z5HHU9p0RyPqVEY2'
  access_secret = '68XxLu15BkjQjBfAKYitpu21RQfgF1LIwKp86n2ALeE7I'
  setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)
}


######################################3 search tweet function #########################################

SearchTweets = function(Searchvalue = 'Twitter', NoOfTweets, FromDate, Todate){
  tweets = searchTwitter(searchString = Searchvalue , n=NoOfTweets, lang='en',
                         since = FromDate
                         , until= Todate ,geocode='42.08185,-78.43214,100mi',
                         retryOnRateLimit=1000)
  tweets_df = twListToDF(tweets)
  return(tweets_df)
}

#########################################3 getTextdata function #######################################
getTextData = function(df) {
  textdata = Corpus(VectorSource(df$text))
  #textdata = tm_map(textdata,content_transformer(stri_trans_tolower))
  textdata =tm_map(textdata,content_transformer(function(x)
                            str_replace_all(x, "@\\w+", "")) 
                          )  
  textdata = tm_map(textdata, content_transformer( function(x) 
    if(Encoding(x)=='unknown'){
      iconv(x, from='ASCII',to='UTF-8', sub='byte')
    }))
  textdata = tm_map(textdata,content_transformer(tolower), lazy=TRUE)
  textdata = tm_map(textdata, removePunctuation) 
  textdata = tm_map(textdata, removeWords, stopwords("english"))
  textdata = tm_map(textdata, removeWords,  c("thy", "thou", "thee", "the", "and", 
                                              "you","donald","trump"
                           ,"this",  "but","will","say","like","and","for","just",
                           "hillary","clinton",  "hillari")) 
    textdata = tm_map(textdata, removeNumbers)
    textdata = tm_map(textdata ,stemDocument) 
    textdata = tm_map(textdata, stripWhitespace)
  return (textdata)
}

############################33 get sentiments function ############################################

getSentiments = function(df){
  textdata = getTextData(df)
  sentiments = sapply(textdata, function(x) get_nrc_sentiment(as.character(x)))
  sentiments = as.data.frame(aperm(sentiments)) # transpose and save as dataframe
  sentiments = as.data.frame(lapply(sentiments, as.numeric)) 
  sentiments =
    sentiments %>%
    mutate(positivity = positive - negative)
  tweet_sentiments = lapply(sentiments,as.numeric)
  tweet_sentiments = as.data.frame(tweet_sentiments)
  tweet_sentiments = tweet_sentiments[,c(1,2,3,4,6,5,7,8,9,10)]
  feelings = data.frame("count"=colSums(tweet_sentiments))
  tot_sentiment = cbind("sentiment" = rownames(feelings), feelings)
  return(tot_sentiment)
}

##################################### top 10 users #################################################3 

Top10UserTweets = function(df) {
  User_10_tweets = df %>% 
    group_by(screenName) %>% 
    summarise(TotalTweets= n()) %>% 
    arrange(desc(TotalTweets)) %>%
    head(10)
  return (User_10_tweets)
}

################################ Tweets by candidates username ################################################3

tweetsByUser = function(user,numberTweets){
  Tweets = userTimeline(user,n=numberTweets,includeRts = FALSE,
                        excludeReplies=TRUE) # tweets from a user
  Tweets_df = twListToDF(Tweets)
  Tweets_df$month = sapply(Tweets_df$created,
                           function(x) {
                             p = as.POSIXlt(x);
                             p$mon})
  Tweets_df$hour= sapply(Tweets_df$created, 
                         function(x) {
                           p=as.POSIXlt(x);
                           p$hour})
  #label a tweet with a number corresponding to the day of the week
  Tweets_df$wday = sapply(Tweets_df$created, 
                          function(x) {
                            p=as.POSIXlt(x);
                            p$wday + 1})
  return (Tweets_df)
}

################################################################################################3

tweetsByUserDate = function(username,numberTweets,FromDate, ToDate){
  Tweets = userTimeline(username,n=numberTweets,includeRts = FALSE,
                        excludeReplies=TRUE) # tweets from a user
  Tweets_df = twListToDF(Tweets)
  UserTweetsDate_df = Tweets_df %>%
    mutate (Tweetdate = format(as.Date(created,format="%Y-%m-%d")))
  
  UserTweetsDate_df =  UserTweetsDate_df[UserTweetsDate_df$Tweetdate >=  FromDate & 
                                           UserTweetsDate_df$Tweetdate <=  ToDate , ]
  UserTweetsDate_df$Tweetdate = as.Date(UserTweetsDate_df$Tweetdate)
  return (UserTweetsDate_df)
}



################################### number of user tweets in 2016 calendar#######################
  
 Usertweets_calendar = function(user, numberTweets){
     UserTweets_df = tweetsByUser(user,numberTweets) # ('realDonaldTrump',3000)
     UserTweetsDate_df = UserTweets_df %>%
       mutate (Tweetdate = format(as.Date(created,format="%Y-%m-%d"))) %>%
       group_by(Tweetdate) %>% 
       summarise(totalTweets = n())
     UserTweetsDate_df =  UserTweetsDate_df[UserTweetsDate_df$Tweetdate > "2015-12-31", ]
     UserTweetsDate_df$Tweetdate = as.Date(UserTweetsDate_df$Tweetdate)
     return (UserTweetsDate_df)
 }


