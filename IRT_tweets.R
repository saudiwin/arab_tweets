# Run and simulate tweet model
require(tidyr)
require(ggplot2)
require(dplyr)
require(forcats)
# Control panel

# Number of sides

sides <- 4

# Number of elites per side 

elites <- 10

# Number of time points

t <- 100

# Number of citizens

cit <- 100

# Citizen points
cit_pt <- rnorm(n=100)
# Discrim points
cit_dis <- rnorm(n=100)

# Generate side/elite ideal points

init_sides1 <- rnorm(n=2)
init_sides2 <- rnorm(n=2)
adj1 <- c(-0.4,1.2)
adj2 <- c(.5,0.8)

# out_sides1 <- sapply(1:(sides/2), function(s) {
#   current_val <- new.env()
#   current_val$t <- 0
#   out_vec <- sapply(2:t,function(t_1) {
#     if(t_1==1) {
#       t_1 <- init_sides[s] + rnorm(1)
#     } else {
#       t_1 <- current_val$t + rnorm(1)
#     }
#     current_val$t <- t_1
#     return(t_1)
#   })
#   out_vec <- c(init_sides[s],out_vec)
#   return(out_vec)
# })

init_sides2 <- rnorm(n=2)
adj1 <- c(.5,.8)
gamma1 <- 0.01
gamma2 <- 0.9
alpha <- 2.1
  current_val <- new.env()
  current_val$t1 <- 0
  current_val$t2 <- 0
out_vec2 <- sapply(2:t,function(t_1) {
  if(t_1<(t/2)) {
    gamma <- gamma1
  } else {
    gamma <- gamma2
  }
    if(t_1==1) {
      t_11 <- init_sides2[1] - gamma*(init_sides2[1] - (adj2[2]/adj2[1])*init_sides2[2]) + rnorm(1,sd=0.25)
      t_12 <- init_sides2[2] - gamma*(init_sides2[2]- (adj2[1]/adj2[2])*init_sides2[1]) + rnorm(1,sd=0.25)
    } else {
      t_11 <- current_val$t1 -  gamma*(current_val$t1- (adj2[2]/adj2[1])*current_val$t2) + rnorm(1,sd=0.25)
      t_12 <- current_val$t2 - gamma*(current_val$t2- (adj2[1]/adj2[2])*current_val$t1) + rnorm(1,sd=0.25)
    }
    current_val$t1 <- t_11
    current_val$t2 <- t_12
    return(c(t_11,t_12))
  }) %>% t
out_vec2 <- rbind(matrix(init_sides2,ncol=2),out_vec2)

out_vec2 %>% as_data_frame %>% 
  mutate(time=1:t) %>% 
  gather(series,estimates,-time) %>% 
  ggplot(aes(y=estimates,x=time,linetype=series)) +geom_path() +theme_minimal() +
  geom_vline(xintercept=(t/2),linetype=4)


init_sides1 <- rnorm(n=2)

alpha <- 2.1
current_val <- new.env()
current_val$t1 <- 0
current_val$t2 <- 0
out_vec1 <- sapply(2:t,function(t_1) {
  if(t_1<(t/2)) {
    gamma <- gamma1
  } else {
    gamma <- gamma2
  }
  if(t_1==1) {
    t_11 <- init_sides2[1] - gamma*(init_sides1[1] - (adj2[2]/adj2[1])*init_sides1[2]) + rnorm(1,sd=0.25)
    t_12 <- init_sides2[2] - gamma*(init_sides1[2]- (adj2[1]/adj2[2])*init_sides1[1]) + rnorm(1,sd=0.25)
  } else {
    t_11 <- current_val$t1 -  gamma*(current_val$t1- (adj2[2]/adj2[1])*current_val$t2) + rnorm(1,sd=0.25)
    t_12 <- current_val$t2 - gamma*(current_val$t2- (adj2[1]/adj2[2])*current_val$t1) + rnorm(1,sd=0.25)
  }
  current_val$t1 <- t_11
  current_val$t2 <- t_12
  return(c(t_11,t_12))
}) %>% t
out_vec1 <- rbind(matrix(init_sides1,ncol=2),out_vec1)

out_vec1 %>% as_data_frame %>% 
  mutate(time=1:t) %>% 
  gather(series,estimates,-time) %>% 
  ggplot(aes(y=estimates,x=time,linetype=series)) +geom_path() +theme_minimal() +
  geom_vline(xintercept=(t/2),linetype=4)

combine_vec <- cbind(out_vec1,out_vec2)

elite_ids <- rep(1:sides,times=cit*t)
time_ids <- rep(1:t,times=cit*sides)
cit_ids <- rep(1:cit,each=sides*t)
combine_ids <- unique(cbind(elite_ids,cit_ids,time_ids))

gen_out <- sapply(1:nrow(combine_ids), function(n) {
  outcome <- rpois(n=1,lambda=exp(cit_dis[cit_ids[n]] * combine_vec[time_ids[n],elite_ids[n]] - cit_pt[cit_ids[n]]))
})

combine_vec %>% as_data_frame %>% 
  mutate(time=1:t) %>% 
  gather(series,estimates,-time) %>% 
  mutate(series=factor(series),
         series=fct_recode(series,`Tunisia Islamists`='V1',
                           `Egypt Islamists`='V2',
                           `Tunisia Secularists`='V3',
                           `Egypt Secularists`='V4')) %>% 
  ggplot(aes(y=estimates,x=time,linetype=series,colour=series)) +geom_path(size=1) +theme_minimal() +
  geom_vline(xintercept=(t/2),linetype=4) +
  scale_colour_brewer(palette='Paired') +
  theme(panel.grid = element_blank())


