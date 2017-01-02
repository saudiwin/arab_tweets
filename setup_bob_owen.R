install.packages("RPostgreSQL")
require(RPostgreSQL)
install.packages("stringr")
require(stringr)


conn <- dbConnect(dbDriver('PostgreSQL'),dbname='alltweets',
                  host='arabtweets.cytudbr9ljth.us-west-2.rds.amazonaws.com',
                  user='uva_tweet',
                  password='A3abT7eets')

dbListFields(conn,'users')
dbListFields(conn,'tweets')


user_id<- dbGetQuery(conn,'SELECt user_id FROM users;')

#tweet_ids<- dbGetQuery()

# What is the purpose of this file?
#practice & change

install.packages("devtools")
devtools::install_github("mkearney/rtweet")
require("rtweet")

twitter_token <- create_token(app = "uva_tweet_project", # whatever you named app
                              consumer_key = "YlMpKrXa30fQgoPfOQhoXrmaj",
                              consumer_secret = "HLguQaq9eowgSCP2M5jOHPZDn89hXT3u5bXGqAw9fOjHEjf5q6")

rate_limit(twitter_token, query = NULL, rest = TRUE)

#get_timeline(user, n = 200, max_id = NULL, parse = TRUE,
#            clean_tweets = FALSE, as_double = FALSE, token = NULL, ...)

