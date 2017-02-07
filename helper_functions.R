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

all_time <- function(user,token,these_users=NULL,city=NULL,sql_db=NULL,end_date=NULL) {

  num_tokens <- length(token)
  # Start with first token, then move on until all tokens have been exhausted
  current_token <- 1
  print(paste0('Token ',current_token,' now on user ',user,' who is ',which(these_users==user),'th of ',length(these_users),
               ' users.'))
  
  reset <- 15
  current_rate <- 1000

  test_d <- try(get_timeline(user=user,n=current_rate,token=token[[current_token]]))
  
  if(class(test_d)=='try-error') {
    if(grepl(pattern = 'rate limit exceeded',x=test_d[1])) {
      if(num_tokens>1) {
        if(current_token<num_tokens) {
          current_token <- current_token+1
        } else {
          current_token <- 1
        }
        test_d <- try(get_timeline(user,n=current_rate,token=token[[current_token]]))
        if(class(test_d)=='try-error') {
          if(grepl(pattern = 'rate limit exceeded',x=test_d[1])) {
            print(paste0('Sleeping for ',reset,' minutes.'))
            Sys.sleep(reset*60)
          } else if(grepl(pattern = 'subscript out of bounds',x=test_d[1])) {
            return(paste0(user,' finished without any tweets.'))
          } else {
            stop(paste0('Unknown error: ',test_d[1]))
          }
        } else {
          first_round <- test_d
        }
      } else {
        print(paste0('Sleeping for ',reset,' minutes.'))
        Sys.sleep(reset*60)
      }
    } else if(grepl(pattern = 'subscript out of bounds',x=test_d[1])) {
      return(paste0(user,' finished without any tweets and a subscript out of bound error on first try. Is this account de-activated?'))
    } else {
        return(paste0(user,' finished without any tweets and an unknown error: ',test_d[1]))
    }
  } else {
    #If no error, proceed as normal and see if there are any more tweets to download
    first_round <- test_d
    if(nrow(first_round)==0) {
      return(paste0(user,' finished without any tweets.'))
    }
  }

  maxid <- first_round$status_id[nrow(first_round)]
  iter <- as.numeric(substr(maxid,start = nchar(maxid)-8,stop=nchar(maxid))) 
  iter <- iter - 1
  maxid <- paste0(substr(maxid,start=1,stop=nchar(maxid)-9),as.character(iter))
  user_data <- attr(first_round,'users')
  
  
  #Loop over tweets until we reach the date that our GNIP data begins
  while(sum(first_round$created_at<end_date)<1) {

    test_d <- try(get_timeline(user,n=current_rate,max_id=maxid,token=token[[current_token]]))
    if(class(test_d)=='try-error') {
      if(grepl(pattern = 'rate limit exceeded',x=test_d[1])) {
        if(num_tokens>1) {
          if(current_token<num_tokens) {
            current_token <- current_token+1
          } else {
            current_token <- 1
          }
          test_d <- try(get_timeline(user,n=current_rate,token=token[[current_token]]))
          if(class(test_d)=='try-error') {
            if(grepl(pattern = 'rate limit exceeded',x=test_d[1])) {
              print(paste0('Sleeping for ',reset,' minutes.'))
              Sys.sleep(reset*60)
              second_round <- get_timeline(user,n=current_rate,token=token[[current_token]])
            } else if(grepl(pattern = 'subscript out of bounds',x=test_d[1])) {
              save_sqlite(dataset=first_round,city=city,sql_db = sql_db,user_attributes = user_data)
              return(paste0(user,' finished successfully.'))
            } else {
              
              stop(paste0('Unknown error on user ',user,': ',test_d[1]))
            }
          }
        } else {
        print(paste0('Sleeping for ',reset,' minutes.'))
        # System sleep in seconds using reset minutes
        Sys.sleep(reset*60)
        second_round <- get_timeline(user,n=current_rate,token=token[[current_token]])
        }
      } else if(grepl(pattern = 'subscript out of bounds',x=test_d[1])) {
        save_sqlite(dataset=first_round,city=city,sql_db = sql_db,user_attributes = user_data)
        return(paste0(user,' finished successfully.'))
      } else {
        stop(paste0('Unknown error on user ',user,': ',test_d[1]))
      }
    } else {
      second_round <- test_d
    }
    
    if(is.data.frame(second_round)) {
    first_round <- bind_rows(first_round,second_round)
    maxid <- first_round$status_id[nrow(first_round)]
    iter <- as.numeric(substr(maxid,start = nchar(maxid)-8,stop=nchar(maxid))) 
    iter <- iter - 1
    maxid <- paste0(substr(maxid,start=1,stop=nchar(maxid)-9),as.character(iter))
    print(paste0("     Now on tweet created at ",first_round$created_at[nrow(first_round)],
                 ' and tweet ID of ',first_round$status_id[nrow(first_round)]))
    } else {
      # If somehow it didn't work, just write out the tweets already downloaded
      break
    } 
  }
  
  save_sqlite(dataset=first_round,city=city,sql_db = sql_db,user_attributes = user_data)
  return(paste0(user,' finished successfully.'))
}
