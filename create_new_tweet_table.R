# Need to make a new tweet table in order to store new data retrieved from Twitter

require(RPostgreSQL)

conn <- dbConnect(dbDriver('PostgreSQL'),dbname='alltweets',
                  host='arabtweets.cytudbr9ljth.us-west-2.rds.amazonaws.com',
                  user='uva_tweet',
                  password='A3abT7eets')

dbSendQuery(conn,'CREATE TABLE tweets_new (
                    tweet_id bigint PRIMARY KEY,
            posted_time timestamp without time zone,
            tweet_retweet_reply integer,
            rr_tweet_id bigint,
            platform varchar(100),
            retweet_count integer,
            body varchar(1500),
            link varchar(500),
            hashtags_count integer,
            urls_count integer,
            mentions_count integer,
            geo_cord varchar(500),
            user_id bigint,
            tweet_day date);')