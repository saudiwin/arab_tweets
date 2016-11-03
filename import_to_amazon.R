# Import data into Amazon Postgresql Database

require(RPostgreSQL)

# open connection to Amazon postgresql server
# Note that before this will work, you need to add your computer's IP address to the DB instance security group



conn <- dbConnect(dbDriver('PostgreSQL'),dbname='alltweets',
                  host='arabtweets.cytudbr9ljth.us-west-2.rds.amazonaws.com',
                  user='uva_tweet',
                  password='A3abT7eets')


# Load data in raw text file form

users <- data.table::fread('C:\\Users\\bobku\\Box Sync\\Big Data\\Data\\SQL_DATA\\users.dat',
                           data.table = FALSE,integer64='double',encoding='UTF-8')
tweets <- data.table::fread('C:\\Users\\bobku\\Box Sync\\Big Data\\Data\\SQL_DATA\\tweets.dat',
                            data.table=FALSE,integer64='double',encoding='UTF-8')

names(users) <- c('user_id','username','display_name','signup_time','image_url','summary',
                  'personal_url','friends_count','followers_count','listed_count','favorites_count',
                  'time_zone','utc_offset','location','language','verified','unique_id')

names(tweets) <- c('tweet_id','posted_time','tweet_retweet_reply','rr_tweet_id','platform','retweet_count',
                   'body','link','hashtags_count','urls_count','mentions_count','geo_cord','unique_id',
                   'tweet_day')

# Need to create tables in the postgres database before we can upload them

dbSendQuery(conn,'CREATE TABLE users (
                    user_id bigint,
                    username varchar(100),
                    display_name varchar(150),
                    signup_time timestamp without time zone,
                    image_url varchar(500),
                    summary varchar(1000),
                    personal_url varchar(500),
                    friends_count integer,
                    followers_count integer,
                    listed_count integer,
                    favorites_count integer,
                    time_zone varchar(100),
                    utc_offset integer,
                    location varchar(500),
                    language varchar(10),
                    verified boolean,
                    unique_id integer PRIMARY KEY);')

dbSendQuery(conn,'CREATE TABLE tweets (
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
                    unique_id bigint,
                    tweet_day date);')

# Check to make sure tables exist

dbListTables(conn)

# Use separate program (psql) to load data into Amazon server

# Check whether uploaded data matches the original files
# Check based on IDs

user_ids <- dbGetQuery(conn,'SELECT unique_id FROM users;')
tweet_ids <- dbGetQuery(conn,'SELECT tweet_id FROM tweets;')
