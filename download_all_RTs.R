# create RT SQL and run RT download code

require(dplyr)
require(tidyr)
require(readr)
require(forcats)
require(purrr)
require(RSQLite)

tunis_sql <- dbConnect(SQLite(),'data/tunis_tweets.sqlite')
egypt_sql <- dbConnect(SQLite(),'data/egypt_tweets.sqlite')

country_list <- read_csv('data/Final Coding -- Dana - Sheet1.csv')

tunisia <- filter(country_list,Country=='Tunisia') %>% pull(Username)
egypt <- filter(country_list,Country=='Egypt') %>% pull(Username)

walk(tunisia,get_tweets)

dbDisconnect(tunis_sql)
dbDisconnect(egypt_sql)