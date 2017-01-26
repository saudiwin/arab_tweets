
#require(RPostgreSQL)
require(stringr)
require(dplyr)
require(rtweet)
require(parallel)
require(RSQLite)
# conn <- dbConnect(dbDriver('PostgreSQL'),dbname='alltweets',
#                   host='arabtweets.cytudbr9ljth.us-west-2.rds.amazonaws.com',
#                   user='uva_tweet',
#                   password='A3abT7eets')
# 
# dbListFields(conn,'users')
# dbListFields(conn,'tweets')
# 
# 
# user_ids<- dbGetQuery(conn,'SELECt username FROM users LIMIT 100;')
source('helper_functions.R')
twitter_tokens <- list(list(create_token(app = "For Analysis II", # whatever you named app
                              consumer_key = "6ae7rNvww5q13Kc86d8YvvLZG",
                              consumer_secret = "gfZOqDIGpzkr53UXHrsqgzWRwIiqlerm982NWPcxP1hIf2uwuV"),
                       create_token(app = "For Analysis III", # whatever you named app
                                    consumer_key = "E7RDTfQjwZAKGeRl7PVxbxGzO",
                                    consumer_secret = "TefV2t5zOWD3IhCepl8UzX5jxna11YeUcwHdMQS2gzGKXe5KXy")),
                       list(create_token(app = "For Analysis IV", # whatever you named app
                                    consumer_key = "Q0zOsWNjncK677fkErC9u9FGn",
                                    consumer_secret = "gCA883t0Vbk8MfQw5by0odHtE7BXrlikA4G0o1oHcSoDK2F2R4"),
                       create_token(app = "For Analysis IV", # whatever you named app
                                    consumer_key = "xg9aQq4VXRaRynmThU86pjyv5",
                                    consumer_secret = "GI96VKrSqGdX92TB1SwwZiobo6z55gD1hjCbUDcPDvpEpWxqHw")))
                       


# Get unique Cairo usernames
tweets_text <- readRDS('tweets/cairo_tweets.rds')
cairo_user <- unique(tweets_text$username)[1:1000]

# City to process 
city <- 'Cairo'

# divide users into 4 parts for parallel

breaks <- rep(1:length(twitter_tokens),each=floor(length(cairo_user)/length(twitter_tokens)))
breaks <- c(breaks,rep(length(twitter_tokens),length(cairo_user)-length(breaks)))

# Need to make an RSQLite to store the data locally

time1 <- Sys.time()

finished <- lapply(1:length(twitter_tokens),wrapper_func,tokens=twitter_tokens,all_users=cairo_user,
                       breaks=breaks,city=city,sql_db=paste0(city,'.sqlite'))

time2 <- Sys.time()
#,mc.cores=length(twitter_tokens)
dbListTables(mydb)
dbListFields(mydb,'Cairo_tweets')
check_table <- DBI::dbGetQuery(mydb,'SELECT * from Cairo_tweets')






