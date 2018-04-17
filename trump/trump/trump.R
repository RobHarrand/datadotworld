library(RSentiment)
library(lubridate)

#tweets = read.csv('all_trump_tweets.csv', stringsAsFactors = F)
#news = read.csv('uci-news-aggregator.csv', stringsAsFactors = F)
djia = read.csv('DJIA.csv', skip = 37, stringsAsFactors = F)


#Sentiments

# options(java.parameters = "-Xmx8000m")
# 
# removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)
# tweets$text = removeURL(tweets$text)
# # 
# removeStuff <- function(x) gsub("[[:punct:]]", " ", x)
# tweets$text = removeStuff(tweets$text)

#sentiments_trump = calculate_sentiment(tweets$text[1:10000])
#sentiments_trump2 = calculate_sentiment(tweets$text[10001:20000])
#sentiments_trump3 = calculate_sentiment(tweets$text[20001:30829])

#sentiments_all = rbind(sentiments_trump,sentiments_trump2,sentiments_trump3)
#tweets$Sentiment = sentiments_all$sentiment

#write.csv(tweets, 'all_trump_tweets_sentiments.csv')
tweets = read.csv('all_trump_tweets_sentiments.csv', stringsAsFactors = F)

tweets$created_at = sapply(strsplit(tweets$created_at," "), `[`, 1)


tweets$Score = 0
tweets$Score[tweets$Sentiment == 'Positive'] = 1
tweets$Score[tweets$Sentiment == 'Very Positive'] = 2
tweets$Score[tweets$Sentiment == 'Negative'] = -1
tweets$Score[tweets$Sentiment == 'Very Negative'] = -2

tweets$created_at = as.POSIXct(tweets$created_at, tz = 'EST')
tweets = tweets[order(tweets$created_at),]
tweets$Total_sentiment = cumsum(tweets$Score)

djia$DATE = as.POSIXct(djia$DATE, tz = 'EST')

#library(anytime)
#options(digits=22)


#news$TIMESTAMP = substr(news$TIMESTAMP,1,nchar(news$TIMESTAMP)-3)
#news$TIMESTAMP = as.numeric(news$TIMESTAMP)
#news$TIMESTAMP = anytime(news$TIMESTAMP)


#all = merge(tweets, djia, by.x = 'created_at', by.y = 'DATE', all.x = TRUE)
all = merge(tweets, djia, by.x = 'created_at', by.y = 'DATE')
all$VALUE = as.numeric(all$VALUE)

all$Scaled_sentiment = scale(all$Total_sentiment)[,1]
all$Scaled_djia = scale(all$VALUE)[,1]

removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)
all$text = removeURL(all$text)

removeStuff <- function(x) gsub("[[:punct:]]", " ", x)
all$text = removeStuff(all$text)


#Plots

plot(tweets$created_at, tweets$Total_sentiment)

plot(djia$DATE, djia$VALUE)

plot(all$created_at, all$Scaled_sentiment, ylim = c(-4, 3), type = 'line', lwd = 2)
lines(all$created_at, all$Scaled_djia, col = 'red', lwd = 2)



library(plotly)

ex = (all$is_retweet == 'TRUE')
all = all[!ex,]

all$X.1 = NULL
all$X = NULL
all$id_str = NULL
all$in_reply_to_user_id_str = NULL
all$is_retweet = NULL
all$source = NULL
all$Score = NULL
#all$Scaled_sentiment = NULL
#all$Scaled_djia = NULL

Nth.delete<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
all_small = Nth.delete(all, 2)

all_small = Nth.delete(all_small, 2)
all_small = Nth.delete(all_small, 2)
all_small = Nth.delete(all_small, 2)

all_small$retweet_count[all_small$retweet_count == 0] = 1

ex = is.na(all_small$VALUE)
all_small = all_small[!ex,]

all_small = Nth.delete(all_small, 2)


#Sys.setenv("plotly_username"="robh")
#Sys.setenv("plotly_api_key"="ILTpvNQjVCzCgH99TgYW")

Sys.setenv("plotly_username"="tentotheminus9")
Sys.setenv("plotly_api_key"="VU1BYmduL9acUMjVYt2W")

values = all_small$favorite_count[all_small$Sentiment == 'Positive']
values = sqrt(values)*2
values = as.data.frame(values)
values = cbind(values, all_small$favorite_count[all_small$Sentiment == 'Positive'])
values = cbind(values, all_small$Sentiment[all_small$Sentiment == 'Positive'])
values = cbind(values, all_small$text[all_small$Sentiment == 'Positive'])
#values = sqrt(all_small$favorite_count)*2
write.csv(values, "values.csv")



p = plot_ly(all_small,
            x = ~created_at, 
            y = ~Scaled_sentiment,
            mode = "markers", 
            type = "scatter",
            showlegend = T, 
            color = ~Sentiment,
            #size = ~values,
            #marker = list(size = sqrt(all_small$favorite_count)*2/10),
            hoverinfo = "text", 
            text = paste("Tweet: ", 
                         all_small$text,
                         "</br>Estimated Sentiment: ",
                         all_small$Sentiment,
                         "</br>No. Retweets: ",
                         all_small$retweet_count,
                         "</br>No. Favourites: ",
                         all_small$favorite_count,
                         "</br>Dow Jones Industrial Average: ", 
                         all_small$VALUE)) %>%
    add_trace(x = all_small$created_at,
              y = all_small$Scaled_djia,
              opacity = 0.2,
              name = 'Dow Jones Industrial Average',
              #type = 'scatter',
              mode = 'lines',
              color = I('black'),
              size = I(1),
              hoverinfo = "none") %>%
              #hoverinfo = "text",
              #text = paste("</br>DJIA: ",
               #             all$VALUE)) %>%
layout(title ="Trump Cumulative Sentiment Over Time", 
       # annotations = list(text = paste("Point size proportional to </br> the number of favourites"),
       #                    x = 2015,
       #                    y = -2,
       #                    showarrow = F,
       #                    font = list(size = 12,
       #                                color = 'black')),
       # titlefont = t,
       # plot_bgcolor='white',
       xaxis = list(title = "Date"), 
       yaxis = list(title = "Scaled Values (arb)"))


api_create(p)

#plotly_POST(p)

p













