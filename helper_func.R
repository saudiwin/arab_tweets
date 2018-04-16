# Helper functions

# generate time series data using environments as counters

gen_ts_data <- function(t,adj_in,adj_out,this_beta,alpha_int,sigma,init_sides,country) {
  current_val <- new.env()
  current_val$t1 <- 0
  current_val$t2 <- 0
  
  out_vec2 <- lapply(1:t,function(t_1) {
    
    if(t_1<(t/2)) {
      this_beta <- rep(0,sides)
    } else {
      this_beta <- this_beta
    }
    if(t_1==1) {
      t_11 <- init_sides[1]
      t_12 <- init_sides[2]
      current_val$t1 <- t_11
      current_val$t2 <- t_12
      return(data_frame(t_11,t_12))
    } else {
      t_11 <- alpha_int[1] + adj_in[1]*current_val$t1 +  adj_out[1]*current_val$t2 + this_beta[1] +
        rnorm(n=1,sd=sigma[1])
      t_12 <- alpha_int[2] + adj_in[2]*current_val$t2 + adj_out[2]*current_val$t1 + this_beta[2] +
        country + rnorm(n=1,sd=sigma[2])
    }
    current_val$t1 <- t_11
    current_val$t2 <- t_12
    return(data_frame(t_11,t_12))
  })  %>% bind_rows
  return(out_vec2)
}

# generate IRFs

irf <- function(time=1,shock=1,
                intercepts=matrix(c(0,0),ncol=2),
                adj_in=NULL,
                adj_out=NULL,
                beta_x = matrix(c(0,0),ncol=2),
                country=matrix(c(0,0),ncol=2),
                y_1=0,
                x_1=0,
                y_2=0,
                x_2=0,
                total_t=10,
                old_output=NULL) {
  
  # set up the exogenous shock
  # unless the shock comes from an exogenous covariate beta_x
  if(time==1) {
    x_1 <- shock 
  }
  
  if(time==1) {
    y_1 <- rep(y_1,times=nrow(adj_in))
    if(length(x_1)==1) {
      x_1 <- rep(x_1,times=nrow(adj_in))
    }
    y_2 <- rep(y_2,times=nrow(adj_in))
    x_2 <- rep(x_2,times=nrow(adj_in))
  }
  print(paste0('Now processing time point ',time))
  
  # Calculate current values of y and x given posterior uncertainty

    output <- data_frame(y_shock=intercepts[,1] + adj_in[,1]*y_1 + adj_out[,1]*x_1 + beta_x[,1] + country[,1],
                         x_shock=intercepts[,2] + adj_in[,2]*x_1 + adj_out[,2]*y_1 + beta_x[,2] + country[,2],
                         y_noshock=intercepts[,1] + adj_in[,1]*y_2 + adj_out[,1]*x_2 + beta_x[,1] + country[,1],
                         x_noshock=intercepts[,2] + adj_in[,2]*y_2 + adj_out[,2]*x_2 + beta_x[,2] + country[,2],
                         time=time,
                         iter=1:nrow(adj_in))
  
  
  if(!is.null(old_output)) {
    new_output <- bind_rows(old_output,output)
  } else {
    new_output <- output
  }
  
  # run function recursively until time limit is reached
  
  if(time<total_t) {
    irf(time=time+1,
        shock=shock,
        intercepts=intercepts,
        adj_in=adj_in,
        adj_out=adj_out,
        beta_x=beta_x,
        y_1=output$y_shock,
        x_1=output$x_shock,
        y_2=output$y_noshock,
        x_2=output$x_noshock,
        total_t=total_t,
        old_output=new_output)
  } else {
    return(mutate(new_output,y_irf=y_shock-y_noshock))  
  }
  
}

hdr <- function(datap) {
  data_frame(y=mean(datap),
             ymin=quantile(datap,0.1),
             ymax=quantile(datap,0.9))
}

#' Helper function for sampling from ordinal cutpoints
.sample_cut <- function(pr_vote=NULL,cutpoints=NULL,n_outcomes=NULL) {
  
  # Given a raw ideal position of a bill-legislator combination, select the most likely outcome from all ordinal categories
  
  cuts <- sapply(cutpoints,function(y) {
    pr_vote - y
  })
  
  
  # Now we pick votes as a function of the number of categories
  # This code should work for any number of categories
  
  pr_bottom <- 1 - plogis(cuts[1])
  
  mid_prs <- sapply(1:(length(cuts)-1), function(c) {
    plogis(cuts[c]) - plogis(cuts[c+1])
  })
  
  pr_top <- plogis(cuts[length(cuts)])
  
  return(as.integer(sample(1:(length(cuts)+1),size=1,prob=c(pr_bottom,mid_prs,pr_top))))
  
}