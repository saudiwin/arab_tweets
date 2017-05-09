setwd("F:/UVA_Research/Bob_Owen_projec/arab_tweets")


require(RPostgreSQL)
require(stringr)
require(dplyr)
require(rtweet)
require(parallel)
require(RSQLite)
require(lubridate)

drv  <-  dbDriver("PostgreSQL")

conn <- dbConnect(dbDriver('PostgreSQL'),dbname='alltweets',
                  host='arabtweets.cytudbr9ljth.us-west-2.rds.amazonaws.com',
                  user='uva_tweet',
                  password='A3abT7eets')
# This pulls all users and tweets
 # tweets_text  <- dbGetQuery(conn, "SELECT tweets.tweet_id,tweets.posted_time,tweets.body,tweets.unique_id,users.user_id,users.username,
 #                            users.followers_count,users.location FROM tweets LEFT JOIN users ON tweets.unique_id=users.unique_id WHERE users.location SIMILAR to '%(S|s)anaa%';")
 # tweets_text <- as_data_frame(tweets_text)
#  saveRDS(tweets_text,"sanaa_tweets.rds")
# 
# # This will pull just user IDs
# 
# users_only <- dbGetQuery(conn, "SELECT DISTINCT username FROM users WHERE location SIMILAR to '%(S|s)anaa%';")
# 
# saveRDS(object = users_only,'sanaa_users.rds')


source('helper_functions.R')
# twitter_tokens <- list(
#   list(
#     create_token(app = "For Analysis II", # whatever you named app
#                  consumer_key = "6ae7rNvww5q13Kc86d8YvvLZG",
#                  consumer_secret = "gfZOqDIGpzkr53UXHrsqgzWRwIiqlerm982NWPcxP1hIf2uwuV"),
#     create_token(app = "For Analysis III", # whatever you named app
#                  consumer_key = "E7RDTfQjwZAKGeRl7PVxbxGzO",
#                  consumer_secret = "TefV2t5zOWD3IhCepl8UzX5jxna11YeUcwHdMQS2gzGKXe5KXy")),
#   list(
#     create_token(app = "For Analysis VI", # whatever you named app
#                  consumer_key = "Tv5Leg6m8azjOU9CppevH4wQX",
#                  consumer_secret = "aCbKlWEzc2BDp1314utCfupxieYRW3Rv5qTTvlpuZoylti7uwE"),
#     create_token(app = "For Analysis VII", # whatever you named app
#                  consumer_key = "IsQFWZHk1fxKzWJvSL8wZOx13",
#                  consumer_secret = "VLJJCgSgXg00DahOHlQinAgP9WdgUS62DfHDb6zxFtBFEHTHVX")),
#   list(
#     create_token(app = "For Analysis IV", # whatever you named app
#                  consumer_key = "Q0zOsWNjncK677fkErC9u9FGn",
#                  consumer_secret = "gCA883t0Vbk8MfQw5by0odHtE7BXrlikA4G0o1oHcSoDK2F2R4"),
#     create_token(app = "For Analysis IV", # whatever you named app
#                  consumer_key = "xg9aQq4VXRaRynmThU86pjyv5",
#                  consumer_secret = "GI96VKrSqGdX92TB1SwwZiobo6z55gD1hjCbUDcPDvpEpWxqHw")),
#   list(
#     create_token(app = "For Analysis VIII", # whatever you named app
#                  consumer_key = "NKcT4JwM45bkXdPb09jyWui2B",
#                  consumer_secret = "NijPrXjKyrzpAWZbrSB6gPXoZLK7Ot0evrSSxFehvrNnuWMuqO")
#   ))

twitter_tokens <-
               list(create_token(app = "ximigo1", # whatever you named app
                            consumer_key = "8VwSl5Q2E8Pvlz5WMbRtpNXQE",
                            consumer_secret = "PgrvNyfCyWsVc2mpaQVhWWkg0ngTMMxHcqaofqn43281Vm9BRc"),
               
               create_token(app = "ximigo2", # whatever you named app
                            consumer_key = "syUdpyHIcpLcp56BAW7D3JUN8",
                            consumer_secret = "XOSWbw4ZB3KHhKkOh3eQCgrYdi652iNG4lLmcidkM1UuTKAcLJ"),
              create_token(app = "ximigo3", # whatever you named app
                            consumer_key = "lskAIunwR6IJbYrnBrBFhoPEp",
                            consumer_secret = "CaWgYfnWS2ikHkMOF7RaPgnlj5UEOt5NJyNHjqFyObZk3jEbj5"),
               
               create_token(app = "ximigo4", # whatever you named app
                            consumer_key = "uKkZmJnhTlLBqHYoGun8hN2XW",
                            consumer_secret = "PjvBQs8yvC2UuOc6NlTUldgTZgqMjM2vWejcnyO67o4Q3rRUmg"),
               
               create_token(app = "ximigo3", # whatever you named app
                                 consumer_key = "lskAIunwR6IJbYrnBrBFhoPEp",
                                 consumer_secret = "CaWgYfnWS2ikHkMOF7RaPgnlj5UEOt5NJyNHjqFyObZk3jEbj5"),
                    
                    create_token(app = "ximigo4", # whatever you named app
                                 consumer_key = "uKkZmJnhTlLBqHYoGun8hN2XW",
                                 consumer_secret = "PjvBQs8yvC2UuOc6NlTUldgTZgqMjM2vWejcnyO67o4Q3rRUmg"))



# Get unique Cairo usernames

sanaa_user <- unique(readRDS(file = 'sanaa_users.rds'))

# City to process 
city <- 'Sanaa'

# Need to make an RSQLite to store the data locally

sink('tweets_download_log.txt',append=TRUE)
time1 <- Sys.time()
out_list <- lapply(sanaa_user$username,all_time,token=twitter_tokens,these_users=sanaa_user,city=city,
                   sql_db=paste0(city,'.sqlite'),end_date='2011-04-01')
time2 <- Sys.time()
sink()


#Check and make sure that the tweets are loaded correctly

mydb <- dbConnect(RSQLite::SQLite(),paste0(city,'.sqlite'))
dbListTables(mydb)
dbListFields(mydb,'Sanaa_tweets')
check_table <- DBI::dbGetQuery(mydb,'SELECT * from Sanaa_tweets')






