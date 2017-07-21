# Calculate new users to load 
# Robert Kubinec
# May 24, 2017

require(RSQLite)
require(dplyr)
require(stringr)
require(readr)
# load tables

all_new <- dbConnect(SQLite(),'data/all_tunis.sqlite')
current_users <- dbConnect(SQLite(),'data/user.sqlite')
current_tweets <- dbConnect(SQLite(),'data/tweets.sqlite')


cairo_ids <- dbGetQuery(current_users,"SELECT DISTINCT unique_id,user_id from users WHERE location LIKE '%tunis%';")
dbWriteTable(current_tweets,'tunis_ids',cairo_ids,overwrite=T)
cairo_tweets_all <- dbGetQuery(current_tweets,
                               'SELECT DISTINCT * from tunis_ids LEFT JOIN tweets ON tunis_ids.unique_id=tweets.unique_id')
cairo_tweets_all <- cairo_tweets_all[,-1]

dbWriteTable(all_new,name = 'tweets',value =   dplyr::select(cairo_tweets_all,-hashtags_count,-link,-urls_count,-mentions_count,
                                                        -geo_cord,-rr_tweet_id,-tweet_retweet_reply,-unique_id),
             append=TRUE)


ntweets <- dbGetQuery(all_new,'SELECT COUNT(DISTINCT tweet_id) from tweets;')
nrows <- dbGetQuery(all_new,'SELECT count(*) FROM tweets;')

# I'm running a SQL query that pulls a million records at a time. It calculates the total unique users for
# each mentioned username and the total number of mentions of that username for all users

all_text <- dbSendQuery(all_new,'SELECT body,user_id from tweets;')
all_mentions <- lapply(1:ceiling((nrows$`count(*)`/1000000)), function(i) {

  print(paste0('Now on row ',i*1000000))
  this_text <- dbFetch(all_text,n=1000000)
  these_mentions <- str_extract_all(this_text$body,pattern = '@[A-Za-z0-9_]+\\b') 
  num_mentions <- vapply(these_mentions,length,numeric(1))
  these_mentions <- unlist(these_mentions)
  keep_ids <- this_text$unique_id[num_mentions>0]
  keep_ids <- rep(keep_ids,num_mentions[num_mentions>0])
  print('Now doing tables')
  table_ment <- table(these_mentions)
  print("Now counting unique values per screen name")
  just_unique <- unique(these_mentions)
  num_user_ment <- sapply(just_unique, function(m) {
    total_users <- length(unique(keep_ids[these_mentions==m]))
  })
  return(data_frame(just_unique=just_unique,total_users=num_user_ment,table_ment=table_ment[just_unique]))
})
dbClearResult(all_text)
all_mentions <- bind_rows(all_mentions)
# Now summarize again over all the individually summarized slices of the SQL dataset
all_ment_sum <- mutate(all_mentions,just_unique=tolower(just_unique)) %>% 
  group_by(just_unique) %>% summarize(t_users=sum(total_users),
                                      t_ment=sum(table_ment))
saveRDS(all_ment_sum,'tunis_all_ment.rds')

#Need to see how many of these we have and how many we don't have

all_users <- dbGetQuery(current_users,'SELECT DISTINCT username from users;') %>% 
  mutate(username=tolower(username))
need_users <- mutate(all_ment_sum,just_unique=str_replace(just_unique,'@','')) %>% 
  filter(!(just_unique %in% all_users$username))
arrange(need_users,desc(t_users)) %>% slice(1:50000) %>%  write_csv('need_users_tunis.csv')
saveRDS(need_users,'tunis_need_users.rds')
