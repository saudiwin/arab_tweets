
#require(RPostgreSQL)
require(stringr)
require(dplyr)
require(rtweet)
require(parallel)
require(RSQLite)
require(lubridate)
require(stringr)
require(readr)
require(purrr)

source('helper_functions_v2.R')


twitter_tokens <- list(   create_token(app = "For Analysis II", # whatever you named app
                 consumer_key = "6ae7rNvww5q13Kc86d8YvvLZG",
                 consumer_secret = "gfZOqDIGpzkr53UXHrsqgzWRwIiqlerm982NWPcxP1hIf2uwuV"),
    create_token(app = "For Analysis III", # whatever you named app
                 consumer_key = "E7RDTfQjwZAKGeRl7PVxbxGzO",
                 consumer_secret = "TefV2t5zOWD3IhCepl8UzX5jxna11YeUcwHdMQS2gzGKXe5KXy"),
    create_token(app = "For Analysis VI", # whatever you named app
                 consumer_key = "Tv5Leg6m8azjOU9CppevH4wQX",
                 consumer_secret = "aCbKlWEzc2BDp1314utCfupxieYRW3Rv5qTTvlpuZoylti7uwE"),
    create_token(app = "For Analysis VII", # whatever you named app
                 consumer_key = "IsQFWZHk1fxKzWJvSL8wZOx13",
                 consumer_secret = "VLJJCgSgXg00DahOHlQinAgP9WdgUS62DfHDb6zxFtBFEHTHVX"),
    create_token(app = "For Analysis IV", # whatever you named app
                 consumer_key = "Q0zOsWNjncK677fkErC9u9FGn",
                 consumer_secret = "gCA883t0Vbk8MfQw5by0odHtE7BXrlikA4G0o1oHcSoDK2F2R4"),
    create_token(app = "For Analysis V", # whatever you named app
                 consumer_key = "xg9aQq4VXRaRynmThU86pjyv5",
                 consumer_secret = "GI96VKrSqGdX92TB1SwwZiobo6z55gD1hjCbUDcPDvpEpWxqHw"),
    create_token(app = "For Analysis VIII", # whatever you named app
                 consumer_key = "NKcT4JwM45bkXdPb09jyWui2B",
                 consumer_secret = "NijPrXjKyrzpAWZbrSB6gPXoZLK7Ot0evrSSxFehvrNnuWMuqO"),
    create_token(app='Writing Research',
                 consumer_key='EpmJwxlPB0SZx6PdfbLQE7dE3',
                 consumer_secret='aVCHV6475w8P5UMEnD9wQgucIzmYglaIZGWyuNhVhneOJ5SFub'),
    create_token(app='Writing Research II',
                 consumer_key='YNUlurUJ9PdUZXSwUk952G7qm',
                 consumer_secret='G0qmgjHhbPNFoFUehooEnJfjeWfoBWQmesfceMBBS5NJB893XW'),
    create_token(app='Writing Research III',
                 consumer_key='L7ueMAaP4jmjM6m7LymxqZVug',
                 consumer_secret='TjhWmKO08clOvfEgDQHbBjObXZi3FmOIOWkBUunorx634zOyZx'),
    create_token(app='Writing Research IV',
                 consumer_key='kWWxzl5v7Itt7FZoqqlOhyMPY',
                 consumer_secret='YNKiGDMB72xhl8mFhbnDe4dEDIoIdHiB4BCnGYYEFAUcOy8CCM'),
    create_token(app='Writing Research V',
                 consumer_key='OIOvuI5XgFyojoxcWeIheoDWG',
                 consumer_secret='kJ0mxFHXbXd7QThkos9Mp8ggLg9VeT5xrdm02gaMvmEVFfADbF'),
    create_token(app='Writing Research VI',
                 consumer_key='oLCG8qcoWWOqh1qoWD2R9X5C2',
                 consumer_secret='UlZ8YyXx2WnhDpyL66Ad4ds3JrKo3vvsYI4dVmiEi8Bsp79oIR'),
    create_token(app = "Churchill Fights I", # whatever you named app
                 consumer_key = "EX6pQLSWCtoiQe3dK9GLBU0aT",
                 consumer_secret = "xxXLF1h0hIw495na9kHZFDbuiaLYuFEbovUehwtYSyCRPidKKo"),
    create_token(app = "Churchill Fights II", # whatever you named app
                 consumer_key = "wBQFLZzL6sKq2uNEn3eEzVamQ",
                 consumer_secret = "wkUeqweQEDhYW5p94b9SMaQ23CWeG8w6jl2rckkGA6M256HIZM"),
    create_token(app = "Churchill Fights III", # whatever you named app
                 consumer_key = "ty7xzq0VSYJVrW63xFPTVODdv",
                 consumer_secret = "N7rvNgCnJebhHqdVVi8EJtFtsIXeHgqe7UBpqFtwCmFWUNGcQd"))
                       

tunis_sql <- dbConnect(SQLite(),'data/tunis_tweets.sqlite')
egypt_sql <- dbConnect(SQLite(),'data/egypt_tweets.sqlite')

country_list <- read_csv('data/Final Coding -- Dana - Sheet1.csv')

tunisia <- filter(country_list,Country=='Tunisia') %>% pull(Username)
egypt <- filter(country_list,Country=='Egypt') %>% pull(Username)

all_tunis_rts <- dbReadTable(tunis_sql,'all_retweets')
all_egypt_rts <- dbReadTable(egypt_sql,'all_retweets')


all_tunis_rts <- mutate(all_tunis_rts,
                        time=ymd_hms(object.postedTime),
                        days=yday(time),
                        t_id=str_extract(id,'[0-9]{18}')) %>% 
  filter(days>88,
         !grepl(pattern='RT @',x = body))
all_egypt_rts <- mutate(all_egypt_rts,
                        time=ymd_hms(object.postedTime),
                        days=yday(time),
                        t_id=str_extract(id,'[0-9]{18}')) %>% 
  filter(days>88,
         !grepl('RT @',x = body))

dbDisconnect(tunis_sql)
dbDisconnect(egypt_sql)

time1 <- Sys.time()

walk(min(all_tunis_rts$days):max(all_tunis_rts$days),all_time_rts,token=twitter_tokens,city=city,
                   sql_db='data/tunis_tweets.sqlite',
                   dataset=all_tunis_rts)
time2 <- Sys.time()

time3 <- Sys.time()

walk(364:max(all_egypt_rts$days),all_time_rts,token=twitter_tokens,city=city,
      sql_db='data/egypt_tweets.sqlite',
      dataset=all_egypt_rts)
time4 <- Sys.time()


#Check and make sure that the tweets are loaded correctly
# table_name <- paste0(city,'.sqlite')
# mydb <- dbConnect(RSQLite::SQLite(),table_name)
# dbListTables(mydb)
# dbListFields(mydb,paste0(city,'_tweets'))
# check_table <- DBI::dbGetQuery(mydb,paste0('SELECT DISTINCT screen_name FROM ',paste0(city,'_tweets'),' ;'))
# sum(check_table$screen_name %in% all_user)
# 
# unique_tweets <- DBI::dbGetQuery(mydb,paste0('SELECT screen_name,count(*) from ', paste0(city,'_tweets'), ' GROUP BY screen_name;'))
# summary(unique_tweets$`count(*)`)
# hist(unique_tweets$`count(*)`)

