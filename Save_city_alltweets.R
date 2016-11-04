require(RPostgreSQL)

drv  <-  dbDriver("PostgreSQL")

con  <- dbConnect(drv, dbname="new_test1",
                  host="localhost",
                  user="postgres",password="6235$$wa",port=5432)

tweets_text  <- dbGetQuery(con, "SELECT tweets.tweet_id,tweets.posted_time,tweets.body,tweets.unique_id,users.user_id,users.username,
                           users.followers_count,users.location FROM tweets LEFT JOIN users ON tweets.unique_id=users.unique_id WHERE users.location SIMILAR to '%(R|r)iyadh%';")
tweets_text <- data.table::data.table(tweets_text,key="username")
save_tweets <- c('salman_alodah','balooot','Saudiwoman','msalsaif','mohmd_alhodaif','ibrahemsu','khaled','SaudiLawyer','Khalid_Aljubair','jamalkhashoggi')
tweets_text <- tweets_text[username %in% save_tweets,]
saveRDS(tweets_text,"R Objects/riyadh_select_tweets.rds")
