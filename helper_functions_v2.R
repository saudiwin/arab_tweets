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
                         token=NULL,
                         dataset=NULL,
                         city=NULL,
                         sql_db=NULL) {
  
  dataset <- filter(dataset,created_at<this_time)

  
  
  reset <- 15
  current_rate <- 100
  out_tweets <- lapply(dataset$status_id,get_tweets,
                       token=token)
    
  names(out_tweets) <- dataset$screen_name
  out_tweets <- out_tweets[sapply(out_tweets,function(l) length(l)>0)]
  # combine tweets into unique lists
  tweet_list <- lapply(unique(names(out_tweets)),function(n) {
    these_tweets <- out_tweets[names(out_tweets)==n]
    unique_tweets <- unique(unlist(these_tweets))
    data_frame(username=n,
               rt_ids=unique_tweets)
  }) %>% bind_rows

  return(out_tweets)
  
}

get_tweets <- function(t=NULL,
                       token=NULL,
                       current_token=NULL) {

  num_tokens <- length(token)
  
  # Start with first token, then move on until all tokens have been exhausted
  if(is.null(current_token)) {
    current_token <- 1
  }
  
  test_d <- try(statuses_retweeters(id=t,token=token[[current_token]]))
  
  if(class(test_d)=='try-error') {
    browser()
    current_token <- current_token + 1
    if(current_token>num_tokens) {
      print(paste0('Sleeping for ',reset,' minutes.'))
      Sys.sleep(reset*60)
      get_tweets(t=t,
                 token=token)
    } else {
      get_tweets(t=t,
                 token=token,
                 current_token=current_token)
    }
  }
  
  if(length(test_d)==0) {
    return(NULL)
  } else {
    retweeters <- test_d$ids
    return(retweeters)
  }
}
