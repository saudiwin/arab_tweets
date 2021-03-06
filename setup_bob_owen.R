
#require(RPostgreSQL)
require(stringr)
require(dplyr)
require(rtweet)
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

#setup_twitter_oauth("6ae7rNvww5q13Kc86d8YvvLZG", "gfZOqDIGpzkr53UXHrsqgzWRwIiqlerm982NWPcxP1hIf2uwuV")

twitter_tokens <- list(create_token(app = "For Analysis II", # whatever you named app
                              consumer_key = "6ae7rNvww5q13Kc86d8YvvLZG",
                              consumer_secret = "gfZOqDIGpzkr53UXHrsqgzWRwIiqlerm982NWPcxP1hIf2uwuV"),
                       create_token(app = "For Analysis III", # whatever you named app
                                    consumer_key = "E7RDTfQjwZAKGeRl7PVxbxGzO",
                                    consumer_secret = "TefV2t5zOWD3IhCepl8UzX5jxna11YeUcwHdMQS2gzGKXe5KXy"),
                       create_token(app = "For Analysis IV", # whatever you named app
                                    consumer_key = "Q0zOsWNjncK677fkErC9u9FGn",
                                    consumer_secret = "gCA883t0Vbk8MfQw5by0odHtE7BXrlikA4G0o1oHcSoDK2F2R4"),
                       create_token(app = "For Analysis IV", # whatever you named app
                                    consumer_key = "xg9aQq4VXRaRynmThU86pjyv5",
                                    consumer_secret = "GI96VKrSqGdX92TB1SwwZiobo6z55gD1hjCbUDcPDvpEpWxqHw"))
                       


# Get unique Cairo usernames
tweets_text <- readRDS('tweets/cairo_tweets.rds')
cairo_user <- unique(tweets_text$username)

# divide users into 4 parts for parallel

breaks <- rep(1:length(twitter_tokens),each=floor(length(cairo_user)/4))
breaks <- c(breaks,rep(length(twitter_tokens),length(cairo_user)-length(breaks)))

get_tweets <- lapply(cairo_user[1:10],all_time,token=twitter_token,all_users=cairo_user)





