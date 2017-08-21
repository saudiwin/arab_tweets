# create RT SQL and run RT download code

require(dplyr)
require(tidyr)
require(readr)
require(forcats)
require(RSQLite)

tunis_sql <- dbConnect(SQLite(),'data/tunis_tweets.sqlite')
egypt_sql <- dbConnect(SQLite(),'data/egypt_tweets.sqlite')

country_list <- read_csv('data/Final Coding -- Dana - Sheet1.csv')

tunisia <- filter(country_list,Country=='Tunisia') %>% pull(Username)
egypt <- filter(country_list,Country=='Egypt') %>% pull(Username)


all_data <- readRDS('data/all_elites_tweets.rds')
just_tunis <- filter(all_data,tolower(as.character(actor.preferredUsername)) %in% tolower(tunisia))
just_egypt <- filter(all_data,tolower(as.character(actor.preferredUsername)) %in% tolower(egypt))
missing_tweets <- filter(all_data,!(tolower(actor.preferredUsername) %in% tolower(c(egypt,tunisia))))

# all the missing tweets are just NA rows that returned in the query for some reason

dbWriteTable(tunis_sql,'all_retweets',just_tunis,append=F,overwrite=T)
dbWriteTable(egypt_sql,'all_retweets',just_egypt,append=F,overwrite=T)

dbDisconnect(tunis_sql)
dbDisconnect(egypt_sql)