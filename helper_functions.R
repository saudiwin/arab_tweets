# tweet functions

all_time <- function(token,user) {
  
  current_rate <- rate_limit(twitter_token,'statuses/user_timeline')$remaining
  first_round <- get_timeline(user,n=current_rate)
  maxid <- first_round$status_id[nrow(first_round)] %>% as.numeric
  maxid <- maxid + 1
  maxid <- as.character(maxid)
  while(sum(first_round$created_at<'2014-01-01')<1) {
    current_rate <- rate_limit(twitter_token,'statuses/user_timeline')$remaining
    second_round <- get_timeline(user,n=current_rate,maxID=maxid)
    first_round <- bind_rows(first_round,second_round[3:35])
    first_round <- filter(first_round,!is.na(text))
    maxid <- first_round$status_id[nrow(first_round)] %>% as.numeric
    maxid <- maxid + 1
    maxid <- as.character(maxid)
  }
  
  return(first_round)
}