#need to process JSON files


require(dplyr)
require(tidyr)
require(readr)
require(forcats)
require(RSQLite)

tunis_sql <- dbConnect(SQLite(),'data/tunis_tweets.sqlite')
egypt_sql <- dbConnect(SQLite(),'data/egypt_tweets.sqlite')
all_files <- list.files('PTDataDownload/downloads/',full.names = T)

country_list <- read_csv('data/Final Coding -- Dana - Sheet1.csv')

tunisia <- filter(country_list,Country=='Tunisia') %>% pull(Username)
egypt <- filter(country_list,Country=='Egypt') %>% pull(Username)

data1 <- jsonlite::stream_in(file('PTDataDownload/20130331-20131231_hzedzpn45d_2013_10_03_22_40_activities.json'))
data2 <- ndjson::stream_in('PTDataDownload/20130331-20131231_hzedzpn45d_2013_10_03_22_40_activities.json')

tunisia <- lapply(all_files,function(f) {
  data1 <- ndjson::stream_in(f)
  all_names <- c('actor.displayName','actor.followersCount',
                 'actor.location.displayName',
                 'actor.postedTime',
                 'actor.statusesCount',
                 'actor.summary',
                 'actor.twitterTimeZone',
                 'actor.utcOffset',
                 'actor.preferredUsername',
                 'actor.id',
                 'body',
                 'favoritesCount',
                 'id',
                 'object.postedTime',
                 'retweetCount',
                 'gnip.urls.0.expanded_url')
  exist_names <- all_names[all_names %in% names(data1)]
  data1 <- select(data1,one_of(exist_names))
  return(data1)
}) %>% bind_rows
<<<<<<< HEAD
=======

saveRDS(tunisia,'data/all_elites_tweets.rds')

dbDisconnect(tunis_sql)
dbDisconnect(egypt_sql)
>>>>>>> 8817f37f653d7b071719b7da5585e6907ebee3e5
