install.packages("RPostgreSQL")
require(RPostgreSQL)

conn <- dbConnect(dbDriver('PostgreSQL'),dbname='alltweets',
                  host='arabtweets.cytudbr9ljth.us-west-2.rds.amazonaws.com',
                  user='uva_tweet',
                  password='A3abT7eets')

user_ids<- dbGetQuery(conn,'SELECt unique_id FROM users;')

#tweet_ids<- dbGetQuery()

# What is the purpose of this file?

