
#require(RPostgreSQL)
require(stringr)
require(dplyr)
require(rtweet)
require(parallel)
require(RSQLite)
require(lubridate)
require(readr)

source('helper_functions.R')


twitter_tokens <- list(
    create_token(app = "For Analysis II", # whatever you named app
                 consumer_key = "6ae7rNvww5q13Kc86d8YvvLZG",
                 consumer_secret = "gfZOqDIGpzkr53UXHrsqgzWRwIiqlerm982NWPcxP1hIf2uwuV"),
    create_token(app = "For Analysis III", # whatever you named app
                 consumer_key = "E7RDTfQjwZAKGeRl7PVxbxGzO",
                 consumer_secret = "TefV2t5zOWD3IhCepl8UzX5jxna11YeUcwHdMQS2gzGKXe5KXy"),
    create_token(app = "For Analysis VI", # whatever you named app
                 consumer_key = "Tv5Leg6m8azjOU9CppevH4wQX",
                 consumer_secret = "aCbKlWEzc2BDp1314utCfupxieYRW3Rv5qTTvlpuZoylti7uwE"),
    create_token(app = "For Analysis VII", # whatever you named app
                 consumer_key = "IsQFWZHk1fxKzWJvSL8wZOx13",
                 consumer_secret = "VLJJCgSgXg00DahOHlQinAgP9WdgUS62DfHDb6zxFtBFEHTHVX"),
    create_token(app = "For Analysis IV", # whatever you named app
                 consumer_key = "Q0zOsWNjncK677fkErC9u9FGn",
                 consumer_secret = "gCA883t0Vbk8MfQw5by0odHtE7BXrlikA4G0o1oHcSoDK2F2R4"),
    create_token(app = "For Analysis IV", # whatever you named app
                 consumer_key = "xg9aQq4VXRaRynmThU86pjyv5",
                 consumer_secret = "GI96VKrSqGdX92TB1SwwZiobo6z55gD1hjCbUDcPDvpEpWxqHw"),
    create_token(app = "For Analysis VIII", # whatever you named app
                 consumer_key = "NKcT4JwM45bkXdPb09jyWui2B",
                 consumer_secret = "NijPrXjKyrzpAWZbrSB6gPXoZLK7Ot0evrSSxFehvrNnuWMuqO")
  )
                       
city <- 'test_city'

test_tweets <- bind_rows(get_timeline('rlmcelreath',n = 1000),
                         get_timeline('hadleywickham',n = 1000))

# Need to make an RSQLite to store the data locally

# Need to divide up date range 
date_range <- seq(from=min(test_tweets$created_at),
                  to=max(test_tweets$created_at),
                  length.out=100)
sink('tweets_download_log.txt',append=TRUE)
time1 <- Sys.time()
out_list <- lapply(date_range[-1],all_time_rts,token=twitter_tokens,city=city,
                   sql_db=paste0(city,'_elite_RTs.sqlite'),
                   dataset=test_tweets)
time2 <- Sys.time()
difftime(time2,time1)
print(out_list)
sink()


#Check and make sure that the tweets are loaded correctly
# table_name <- paste0(city,'.sqlite')
# mydb <- dbConnect(RSQLite::SQLite(),table_name)
# dbListTables(mydb)
# dbListFields(mydb,paste0(city,'_tweets'))
# check_table <- DBI::dbGetQuery(mydb,paste0('SELECT DISTINCT screen_name FROM ',paste0(city,'_tweets'),' ;'))
# sum(check_table$screen_name %in% all_user)
# 
# unique_tweets <- DBI::dbGetQuery(mydb,paste0('SELECT screen_name,count(*) from ', paste0(city,'_tweets'), ' GROUP BY screen_name;'))
# summary(unique_tweets$`count(*)`)
# hist(unique_tweets$`count(*)`)

