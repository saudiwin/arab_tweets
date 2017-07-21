
#require(RPostgreSQL)
require(stringr)
require(dplyr)
require(rtweet)
require(parallel)
require(RSQLite)
require(lubridate)

source('helper_functions.R')


twitter_tokens <- list(
    create_token(app = "For Analysis II", # whatever you named app
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
    create_token(app = "For Analysis IV", # whatever you named app
                 consumer_key = "xg9aQq4VXRaRynmThU86pjyv5",
                 consumer_secret = "GI96VKrSqGdX92TB1SwwZiobo6z55gD1hjCbUDcPDvpEpWxqHw"),
    create_token(app = "For Analysis VIII", # whatever you named app
                 consumer_key = "NKcT4JwM45bkXdPb09jyWui2B",
                 consumer_secret = "NijPrXjKyrzpAWZbrSB6gPXoZLK7Ot0evrSSxFehvrNnuWMuqO")
  )
                       
# set dates
begin_date <- mdy_hms('07-10-2017 00:00:00')
end_date <- mdy_hms('07-20-2013 00:00:00')
all_days <- as.numeric(end_date - begin_date)
current_day <- 1
while(current_day<all_days) {
  first_day <- begin_date + ddays(current_day-1)
  second_day <- begin_date + ddays(current_day+1)
  # pull all tweets from tweet file matching those days 
  
  these_tweets <- filter(combined,created_at>first_day,
                         created_at<second_day)
  
  # for each tweet, get all retweets 
  
  all_rts <- lapply(these_tweets$status_id,
                    all_time_rts,
                    token=twitter_tokens)
  
  
  current_day <- current_day + 2
}
# Get all elites
elites <- read_csv('data/egypt_and_tunis_to_gnip.csv.csv') %>% 
  filter(`Keep?`=='Yes' | is.na(`Keep?`))
#create data set of dates, users and tweets
#elites_data <- 

# Need to make an RSQLite to store the data locally
 
sink('tweets_download_log.txt',append=TRUE)
time1 <- Sys.time()
out_list <- lapply(all_user,all_time,token=twitter_tokens,these_users=all_user,city=city,
                   sql_db=paste0(city,'.sqlite'),end_date='2011-04-01')
time2 <- Sys.time()
difftime(time2,time1)
print(out_list)
sink()


#Check and make sure that the tweets are loaded correctly
table_name <- paste0(city,'.sqlite')
mydb <- dbConnect(RSQLite::SQLite(),table_name)
dbListTables(mydb)
dbListFields(mydb,paste0(city,'_tweets'))
check_table <- DBI::dbGetQuery(mydb,paste0('SELECT DISTINCT screen_name FROM ',paste0(city,'_tweets'),' ;'))
sum(check_table$screen_name %in% all_user)

unique_tweets <- DBI::dbGetQuery(mydb,paste0('SELECT screen_name,count(*) from ', paste0(city,'_tweets'), ' GROUP BY screen_name;'))
summary(unique_tweets$`count(*)`)
hist(unique_tweets$`count(*)`)

