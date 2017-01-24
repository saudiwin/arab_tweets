
require(RPostgreSQL)
require(stringr)
require(dplyr)
require(twitteR)
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

twitter_token <- create_token(app = "For Analysis II", # whatever you named app
                              consumer_key = "6ae7rNvww5q13Kc86d8YvvLZG",
                              consumer_secret = "gfZOqDIGpzkr53UXHrsqgzWRwIiqlerm982NWPcxP1hIf2uwuV")


# Get unique Cairo usernames

cairo_user <- unique(tweets_text$username)

get_

# Loop over people and get tweets
all_tweets <- lapply(cairo_user[1:5],function(x) {
    y <- get_timeline(x)
    return(y)
})



