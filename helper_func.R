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