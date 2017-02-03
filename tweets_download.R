
#require(RPostgreSQL)
require(stringr)
require(dplyr)
require(rtweet)
require(parallel)
require(RSQLite)
require(lubridate)

source('helper_functions.R')
twitter_tokens <- list(
  list(
    create_token(app = "For Analysis II", # whatever you named app
                 consumer_key = "6ae7rNvww5q13Kc86d8YvvLZG",
                 consumer_secret = "gfZOqDIGpzkr53UXHrsqgzWRwIiqlerm982NWPcxP1hIf2uwuV"),
    create_token(app = "For Analysis III", # whatever you named app
                 consumer_key = "E7RDTfQjwZAKGeRl7PVxbxGzO",
                 consumer_secret = "TefV2t5zOWD3IhCepl8UzX5jxna11YeUcwHdMQS2gzGKXe5KXy")),
  list(
    create_token(app = "For Analysis VI", # whatever you named app
                 consumer_key = "Tv5Leg6m8azjOU9CppevH4wQX",
                 consumer_secret = "aCbKlWEzc2BDp1314utCfupxieYRW3Rv5qTTvlpuZoylti7uwE"),
    create_token(app = "For Analysis VII", # whatever you named app
                 consumer_key = "IsQFWZHk1fxKzWJvSL8wZOx13",
                 consumer_secret = "VLJJCgSgXg00DahOHlQinAgP9WdgUS62DfHDb6zxFtBFEHTHVX")),
  list(
    create_token(app = "For Analysis IV", # whatever you named app
                 consumer_key = "Q0zOsWNjncK677fkErC9u9FGn",
                 consumer_secret = "gCA883t0Vbk8MfQw5by0odHtE7BXrlikA4G0o1oHcSoDK2F2R4"),
    create_token(app = "For Analysis IV", # whatever you named app
                 consumer_key = "xg9aQq4VXRaRynmThU86pjyv5",
                 consumer_secret = "GI96VKrSqGdX92TB1SwwZiobo6z55gD1hjCbUDcPDvpEpWxqHw")),
  list(
    create_token(app = "For Analysis VIII", # whatever you named app
                 consumer_key = "NKcT4JwM45bkXdPb09jyWui2B",
                 consumer_secret = "NijPrXjKyrzpAWZbrSB6gPXoZLK7Ot0evrSSxFehvrNnuWMuqO")
  ))
                       


# Get unique Cairo usernames
tweets_text <- readRDS('tweets/cairo_users.rds')
cairo_user <- tweets_text$username[1:1000]

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
mydb <- dbConnect(RSQLite::SQLite(),paste0(city,'.sqlite'))
dbListTables(mydb)
dbListFields(mydb,'Cairo_tweets')
check_table <- DBI::dbGetQuery(mydb,'SELECT * from Cairo_tweets')






