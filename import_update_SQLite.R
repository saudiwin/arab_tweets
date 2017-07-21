# Import and update SQL tables
# Robert Kubinec
# May 24, 2017

require(RSQLite)
require(dplyr)
require(stringr)
require(readr)
# load tables

current_users <- dbConnect(SQLite(),'data/user.sqlite')
current_tweets <- dbConnect(SQLite(),'data/tweets.sqlite')
new_tweets <- dbConnect(SQLite(),'data/Tunis.sqlite')
store_tweets <- dbConnect(SQLite(),'data/all_tunis.sqlite')
# First load the new data into memory so we can modify it and put it in the original SQL file

all_new <- dbReadTable(new_tweets,'Tunis_tweets') %>% as_data_frame %>% 
  mutate(status_id=as.numeric(status_id),
         user_id=as.numeric(user_id))
all_new_dist <- distinct(all_new,user_id,.keep_all=TRUE)

# Add in unique_id which is used by the SQL table (for whatever reason)

ids <- dbGetQuery(current_users,"SELECT user_id,unique_id from users WHERE users.location LIKE '%tunis%'") %>% as_data_frame %>% 
  distinct(unique_id,user_id)

all_new_ids <- left_join(all_new_dist,ids,by=c('user_id'='user_id'))

#Modify the existing table to accept new columns from the new data
# dbGetQuery(current_tweets,'ALTER TABLE tweets ADD COLUMN (favorite_count INT,
#             lang TEXT);')
# old_names <- dbListFields(current_tweets,'tweets')

# The new data needs to be set up with the same names and in the same order as the existing data
# First we'll do this with the new tweet text
just_tweets <- select(all_new,created_at,status_id,text,source,in_reply_to_status_id,
                      retweet_count,favorite_count,lang,
                      user_id,retweet_status_id,coordinates) %>% 
  rename(tweet_id=status_id,
         posted_time=created_at,
         rr_tweet_id=retweet_status_id,
         platform=source,
         body=text,
         tweet_retweet_reply=in_reply_to_status_id,
         geo_cord=coordinates) %>% 
  mutate(tweet_day=NA,hashtags_count=NA,mentions_count=NA,link=NA,urls_count=NA,tweet_day=NA)
#dbWriteTable(current_tweets,'tweets',just_tweets,append=TRUE)
dbWriteTable(store_tweets,'tweets',just_tweets,overwrite=TRUE)
dbWriteTable(store_tweets,'users',select(all_new_ids,location,friends,unique_id,user_id,screen_name,coordinates,country,
                                         place_name,account_data,followers),overwrite=TRUE)
