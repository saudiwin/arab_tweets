# Per reviewer's request, calculate overlap in RTs across countries

require(dplyr)
require(ggplot2)
require(tidyr)
require(readr)
require(RSQLite)

all_tunis <- dbConnect(SQLite(),'data/tunis_tweets.sqlite')
all_egypt <- dbConnect(SQLite(),'data/egypt_tweets_small.sqlite')

tunis_rts <- dbReadTable(all_tunis,'unique_rts')
egypt_rts <- dbReadTable(all_egypt,'unique_rts')

# overlap of networks 


tunis_egypt <- length(unique(tunis_rts$rt_ids[tunis_rts$rt_ids %in% egypt_rts$rt_ids]))/length(unique(tunis_rts$rt_ids))
egypt_tunis <- length(unique(egypt_rts$rt_ids[egypt_rts$rt_ids %in% tunis_rts$rt_ids]))/length(unique(egypt_rts$rt_ids))

# number of overlapping users

tunis_egypt_num <- length(unique(tunis_rts$rt_ids[tunis_rts$rt_ids %in% egypt_rts$rt_ids]))
egypt_tunis_num <- length(unique(egypt_rts$rt_ids[egypt_rts$rt_ids %in% tunis_rts$rt_ids]))

# identical, obviously

# of how many total users?

tunis_egypt_num/sum(length(unique(tunis_rts$rt_ids)) + length(unique(egypt_rts$rt_ids)))


# top users in egypt/tunisia with international followers

top_egypt <- group_by(egypt_rts,username) %>% 
  summarize(int_users=sum(rt_ids %in% tunis_rts$rt_ids),
            int_users_prop=sum(rt_ids %in% tunis_rts$rt_ids)/n()) %>% 
  arrange(desc(int_users),int_users_prop)

top_tunis <- group_by(tunis_rts,username) %>% 
  summarize(int_users=sum(rt_ids %in% egypt_rts$rt_ids),
            int_users_prop=sum(rt_ids %in% egypt_rts$rt_ids)/n()) %>% 
  arrange(desc(int_users),int_users_prop)
