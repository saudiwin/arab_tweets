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

save_sqlite <- function(dataset,sql_db,city,user_attributes)  {

  # modify the data frame to convert dates to strings
  # also check to see if coordinates is a matrix (for whatever reason)
  check_col <- function(x) {
    if((lubridate::is.POSIXct(x))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  if(class(dataset$coordinates)=='matrix') {
    dataset$coordinates <- apply(dataset$coordinates,1,paste,collapse=' ')
  }
  #convert user attributes to data.frame if it is a list
  if(!is.data.frame(user_attributes)) {
    user_attributes <- lapply(user_attributes,function(x) x[1])
    all_names <- names(user_attributes)
    user_attributes <- user_attributes[!duplicated(all_names)]
    user_attributes <- as_data_frame(user_attributes)
  } else {
    all_names <- names(user_attributes)
    user_attributes <- user_attributes[!duplicated(all_names)]
  }
  #Sometimes multiple user attributes are returned, but we only need the first row
  if(nrow(user_attributes)>1) {
    user_attributes <- slice(user_attributes,1L)
  }
  
  dataset <- mutate(dataset,followers=user_attributes$followers_count,
                    friends=user_attributes$friends_count,
                    account_data=as.character(user_attributes$created_at),
                    location=as.character(user_attributes$location)) %>% 
    mutate_if(check_col,as.character)
    
  
  # check to see if table exists, if it doesn't, create one
  
  con <- dbConnect(RSQLite::SQLite(), sql_db)
  
  all_tables <- RSQLite::dbListTables(con)
  if(!any(paste0(city,"_tweets") %in% all_tables)) {
    RSQLite::dbWriteTable(con,paste0(city,"_tweets"),dataset)
  } else {
    # Put in a check to make sure that the columns are in the right order
    dataset <- select(dataset,one_of(RSQLite::dbListFields(con,paste0(city,"_tweets"))))
    RSQLite::dbWriteTable(con,paste0(city,"_tweets"),dataset,append=TRUE)
  }
  dbDisconnect(con)
  }

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

all_time_rts <- function(this_time=NULL,
                         last_time=NULL,
                         token=NULL,
                         dataset=NULL,
                         city=NULL,
                         sql_db=NULL) {
  

    out_table <- RSQLite::dbConnect(SQLite(),
                       dbname=sql_db)

  
  dataset <- filter(dataset,
                    created_at<this_time,
                    created_at>last_time)

  if(nrow(dataset)==0) {
    return(NULL)
  }
  cat(paste0('\nNow processing last time ',last_time),file='output.txt',append=T)
  reset <- 15
  current_rate <- 100
  current_token <- new.env()
  current_token$id <- 1
  out_tweets <- lapply(dataset$status_id,get_tweets,
                       token=token,
                       current_token=current_token)
    
  names(out_tweets) <- dataset$screen_name
  out_tweets <- out_tweets[sapply(out_tweets,function(l) length(l)>0)]
  # combine tweets into unique lists
  tweet_list <- lapply(unique(names(out_tweets)),function(n) {
    these_tweets <- out_tweets[names(out_tweets)==n]
    unique_tweets <- unique(unlist(these_tweets))
    data_frame(username=n,
               rt_ids=unique_tweets)
  }) %>% bind_rows
  
  if(nrow(tweet_list)>0) {
    uniq_twts <- group_by(tweet_list,username,rt_ids) %>% 
      count() %>% 
      mutate(this_time=this_time,
             last_time=last_time)
    dbWriteTable(out_table,name = 'unique_rts',value=uniq_twts,append=T)
    dbDisconnect(out_table)
  }

}

get_tweets <- function(t=NULL,
                       token=NULL,
                       current_token=NULL) {

  num_tokens <- length(token)

  # wait five seconds between each run
  #Sys.sleep(0.1)
  test_d <- try(statuses_retweeters(id=t,token=token[[current_token$id]]))
  
  if(class(test_d)=='try-error' || is.null(test_d)) {

    current_token$id <- current_token$id + 1
    if(current_token$id>num_tokens) {
      cat(paste0('\nSleeping for ',10,' minutes.'),file='output.txt',append=T)
      Sys.sleep(10*60)
      get_tweets(t=t,
                 token=token,
                 current_token=current_token)
    } else {
      get_tweets(t=t,
                 token=token,
                 current_token=current_token)
    }
  }
  
  if(length(test_d)==0 | length(test_d$ids==0)) {
    return(test_d)
  } else {
    retweeters <- test_d$ids
    return(retweeters)
  }
}
