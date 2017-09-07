# tweet functions

# Only for mclapply, which currently doesn't work
wrapper_func <- function(x,tokens=NULL,all_users=NULL,breaks=NULL,city=NULL,
                         sql_db=NULL) {
  
  if(!file.exists('tweet_download_output.txt')) {
    file.create('tweet_download_output.txt')
  }
  sink('tweet_download_output.txt',append = TRUE)
  these_users <- all_users[breaks==x]
  out_list <- lapply(these_users,all_time,token=tokens,these_users=these_users,x=x,city=city,
                     sql_db=sql_db)
  sink()
  return(out_list)
}

all_time_rts <- function(today=NULL,
                         token=NULL,
                         dataset=NULL,
                         city=NULL,
                         sql_db=NULL) {

  
  
  
  dataset <- filter(dataset,days==today)
  if(nrow(dataset)==0) {
    return(NULL)
  }
  cat(paste0('\nNow processing day ',today),file='output.txt',append=T)
  reset <- 15
  current_rate <- 100
  current_token <- new.env()
  current_token$id <- 1
  re_run <- new.env()
  re_run$state <- 0
  out_tweets <- lapply(dataset$t_id,get_tweets,
                       token=token,
                       current_token=current_token,
                       re_run=re_run)
  names(out_tweets) <- dataset$actor.preferredUsername
  out_tweets <- out_tweets[sapply(out_tweets,function(l) length(l)>0)]
  # combine tweets into unique lists
  tweet_list <- lapply(unique(names(out_tweets)),function(n) {
    these_tweets <- out_tweets[names(out_tweets)==n]
    unique_tweets <- unique(unlist(these_tweets))
    data_frame(username=n,
               rt_ids=unique_tweets)
  }) %>% bind_rows
  sql_db <- dbConnect(SQLite(),sql_db)

  if(nrow(tweet_list)>0) {
    uniq_twts <- group_by(tweet_list,username,rt_ids) %>% 
      count() %>% 
      mutate(time=today)
    
    dbWriteTable(sql_db,name = 'unique_rts',value=uniq_twts,append=T)
    
  }
  dbDisconnect(sql_db)
}

get_tweets <- function(t=NULL,
                       token=NULL,
                       current_token=NULL,
                       re_run=NULL) {

  num_tokens <- length(token)

  # wait five seconds between each run
  test_d <- try(statuses_retweeters(id=t,token=token[[current_token$id]]))
  
  if(class(test_d)=='try-error' || is.null(test_d)) {
    
    current_token$id <- current_token$id + 1
    if(current_token$id>num_tokens) {
      if(re_run$state==0) {

        current_token$id <- 1
        re_run$state <- 1
        get_tweets(t=t,
                   token=token,
                   current_token=current_token,
                   re_run=re_run)
        
      } else {
        
        test_d <- try(statuses_retweeters(id=t,token=token[[1]]))
        
        if(class(test_d)=='try-error' || is.null(test_d)) {
          cat(paste0('\nSleeping for ',10,' minutes.'),file='output.txt',append=T)
          Sys.sleep(10*60)
          re_run$state <- 0
          current_token$id <- 1
          get_tweets(t=t,
                     token=token,
                     current_token=current_token,
                     re_run=re_run)
        }
        re_run$state <- 0
        current_token$id <- 1
      }
      
    } else {
      browser()
      get_tweets(t=t,
                 token=token,
                 current_token=current_token,
                 re_run=re_run)
    }
  }

  if(length(test_d)==0 | length(test_d$ids)==0) {
    return(list())
  } else {
    
    retweeters <- try(as.character(test_d$ids))
    if(class(retweeters)=='try-error') {
      get_tweets(t=t,
                 token=token,
                 current_token=current_token,
                 re_run=re_run)
    }
    return(retweeters)
  }
}
