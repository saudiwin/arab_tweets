require(RPostgreSQL)
require(dplyr)
drv  <-  dbDriver("PostgreSQL")

conn <- dbConnect(dbDriver('PostgreSQL'),dbname='alltweets',
                  host='arabtweets.cytudbr9ljth.us-west-2.rds.amazonaws.com',
                  user='uva_tweet',
                  password='A3abT7eets')
# This pulls all users and tweets
# tweets_text  <- dbGetQuery(conn, "SELECT tweets.tweet_id,tweets.posted_time,tweets.body,tweets.unique_id,users.user_id,users.username,
#                            users.followers_count,users.location FROM tweets LEFT JOIN users ON tweets.unique_id=users.unique_id WHERE users.location SIMILAR to '%(C|c)airo%';")
# tweets_text <- as_data_frame(tweets_text)
# saveRDS(tweets_text,"tweets/cairo_tweets.rds")

# This will pull just user IDs

users_only <- dbGetQuery(conn, "SELECT DISTINCT username FROM users WHERE location SIMILAR to '%(C|c)airo%';")

saveRDS(object = users_only,'tweets/cairo_users.rds')

