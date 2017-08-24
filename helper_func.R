# Helper functions

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