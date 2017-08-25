# Run and simulate tweet model
require(tidyr)
require(ggplot2)
require(dplyr)
require(forcats)
require(rstan)
require(shinystan)
require(bayesplot)

source('helper_func.R')
# Control panel

# Number of sides

sides <- 4

# Number of elites per side 

elites <- 10

# Number of time points

t <- 100

# Number of citizens

cit <- 50

# Citizen points
cit_pt <- rnorm(n=cit)
# cit_pt <- c(-sum(cit_pt),cit_pt)
# Discrim points
cit_dis <- rnorm(n=cit)
#cit_dis <- c(-1,1,cit_dis)

# Generate side/elite ideal points

init_sides1 <- c(1,1)
init_sides2 <- c(-1,-1)
cuts <- c(-1,1)
adj1 <- .9
adj2 <- 1.1

gamma3 <- 0.1
gamma4 <- 0.9
alpha <- 2.1
  current_val <- new.env()
  current_val$t1 <- 0
  current_val$t2 <- 0
  
  out_vec2 <- lapply(1:t,function(t_1) {

    if(t_1<(t/2)) {
      gamma <- gamma3
    } else {
      gamma <- gamma4
    }
    if(t_1==1) {
      t_11 <- init_sides2[1]
      t_12 <- init_sides2[2]
      current_val$t1 <- t_11
      current_val$t2 <- t_12
      return(data_frame(t_11,t_12))
    } else {
      t_11 <- current_val$t1 -  gamma*(current_val$t1- (adj2)*current_val$t2) + rnorm(1,sd=0.25)
      t_12 <- current_val$t2 - gamma*(current_val$t2- (1/adj2)*current_val$t1) + rnorm(1,sd=0.25)
    }
    current_val$t1 <- t_11
    current_val$t2 <- t_12
    return(data_frame(t_11,t_12))
  })  %>% bind_rows

  out_vec2 %>% 
    mutate(time=1:t) %>% 
    gather(series,estimates,-time) %>% 
    ggplot(aes(y=estimates,x=time,linetype=series)) +geom_path() +theme_minimal() +
    geom_vline(xintercept=(t/2),linetype=4)

alpha <- 2.1
current_val <- new.env()
current_val$t1 <- 0
current_val$t2 <- 0

gamma1 <- 0.9
gamma2 <- 0.1

out_vec1 <- lapply(1:t,function(t_1) {
  if(t_1<(t/2)) {
    gamma <- gamma1
  } else {
    gamma <- gamma2
  }
  if(t_1==1) {
    t_11 <- init_sides1[1]
    t_12 <- init_sides1[2]
    current_val$t1 <- t_11
    current_val$t2 <- t_12
    return(data_frame(t_11,t_12))
  } else {
    t_11 <- current_val$t1 -  gamma*(current_val$t1- (adj1)*current_val$t2) + rnorm(1,sd=0.25)
    t_12 <- current_val$t2 - gamma*(current_val$t2- (1/adj1)*current_val$t1) + rnorm(1,sd=0.25)
  }
  current_val$t1 <- t_11
  current_val$t2 <- t_12
  return(data_frame(t_11,t_12))
})  %>% bind_rows

out_vec1 %>% 
  mutate(time=1:t) %>% 
  gather(series,estimates,-time) %>% 
  ggplot(aes(y=estimates,x=time,linetype=series)) +geom_path() +theme_minimal() +
  geom_vline(xintercept=(t/2),linetype=4)

combine_vec <- bind_cols(out_vec1,out_vec2) %>% as.matrix


elite_ids <- rep(1:sides,times=cit)
cit_ids <- rep(1:cit,each=sides)

all_ids <- lapply(1:t,function(i) {
  data_frame(elite_ids,cit_ids)
})
names(all_ids) <- as.character(1:t)
all_ids <- bind_rows(all_ids,
                     .id='time_ids') %>% 
  mutate(time_ids=as.integer(time_ids))
combine_ids <- as.matrix(all_ids)

elite_ids <- all_ids$elite_ids
cit_ids <- all_ids$cit_ids
time_ids <- all_ids$time_ids

# gen_out <- sapply(1:nrow(combine_ids), function(n) {
#   outcome <- rpois(n=1,lambda=exp(cit_dis[cit_ids[n]] * combine_vec[time_ids[n],elite_ids[n]] - cit_pt[cit_ids[n]]))
#   if(is.na(outcome)) {
#     browser()
#   }
#   return(outcome)
# })

gen_out <- sapply(1:nrow(combine_ids), function(n) {
  outcome <- .sample_cut(cit_dis[cit_ids[n]] * combine_vec[time_ids[n],elite_ids[n]] - cit_pt[cit_ids[n]],
                          cutpoints=cuts)
  return(outcome)
})


combine_plot <- combine_vec %>% as_data_frame %>% 
  mutate(time=1:t) %>% 
  gather(series,estimates,-time) %>% 
  mutate(series=factor(series),
         series=fct_recode(series,`Tunisia Islamists`='t_11',
                           `Egypt Islamists`='t_12',
                           `Tunisia Secularists`='t_111',
                           `Egypt Secularists`='t_121'))
  combine_plot %>% ggplot(aes(y=estimates,x=time,linetype=series,colour=series)) +geom_path(size=1) +theme_minimal() +
  geom_vline(xintercept=(t/2),linetype=4) +
  scale_colour_brewer(palette='Paired') +
  theme(panel.grid = element_blank())

code_compile <- stan_model(file='ord_irt_v1.stan')

# need to create time variable for gamma

time_gamma <- c(rep(1L,(t/2)-1),rep(2L,t/2))

#function to create starting values

start_func <- function() {
  list(alpha=rbind(matrix(c(init_sides1,init_sides2),ncol=sides),
                   matrix(rep(0, (t-1)*sides),ncol=sides)),
       gamma1=c(0.5,0.5),
       gamma2=c(0.5,0.5),
       ts_sigma=rep(0.25,sides),
       adj=c(1,1),
       mean_delta=0,
       mean_beta=0,
       sigma_beta=1,
       sigma_delta=1,
       beta=rnorm(n=cit),
       delta=rnorm(cit),
       gamma_par1=0,
       gamma_par2=0)
}

out_fit <- sampling(code_compile,
                    data=list(J=sides,
                              K=cit,
                              `T`=t,
                              C=3,
                              N=length(gen_out),
                              jj=combine_ids[,2],
                              kk=combine_ids[,3],
                              tt=combine_ids[,1],
                      y=as.integer(gen_out),
                    coup=as.integer(t/2),
                    start_vals=c(init_sides1,init_sides2),
                    time_gamma=time_gamma),
                      cores=4,
                    control=list(max_treedepth=10),
                    init=start_func,
                    chains=8)
to_plot <- as.array(out_fit)
mcmc_trace(to_plot,pars='adj[1]')
mcmc_trace(to_plot,pars='adj[2]')
adj_est <- as.array(out_fit,'adj')
mcmc_recover_intervals(x=adj_est,true = c(adj1,adj2))
mcmc_recover_intervals(x=as.array(out_fit,c('gamma1','gamma2')),true = c(gamma1,gamma2,gamma3,gamma4))
mcmc_recover_intervals(x=as.array(out_fit,'alpha'),true = c(combine_vec)*-1+2)

get_time <- rstan::extract(out_fit,pars='alpha',permute=T)$alpha
get_time <- get_time[sample(1:nrow(get_time),101),,]
get_time <- lapply(1:dim(get_time)[3],function(x) get_time[,,x]) %>% 
  lapply(as_data_frame) %>% 
  bind_rows(.id='Series') %>% 
  mutate(Series=factor(Series),
         Series=fct_recode(Series,`Tunisia Islamists`='1',
                           `Egyptian Islamists`='2',
                           `Tunisian Secularists`='3',
                           `Egyptian Secularists`='4')) %>% 
  gather(time_pts,out_vals,-Series) %>% 
  mutate(time_pts=as.numeric(factor(time_pts)))

get_time %>% 
  ggplot(aes(y=out_vals,x=time_pts,colour=Series,linetype=Series)) +
  stat_smooth() + theme_minimal() +
  theme(panel.grid=element_blank()) + xlab('Time') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  scale_linetype(name='')

orig_vals <- combine_vec %>% as_data_frame %>% 
  mutate(time_pts=1:n()) %>% 
  gather(Series,out_vals,-time_pts) %>% 
  mutate(Series=fct_recode(Series,`Tunisia Islamists`='t_11',
                           `Egyptian Islamists`='t_12',
                           `Tunisian Secularists`='t_111',
                           `Egyptian Secularists`='t_121'))

get_time %>% 
  ggplot(aes(y=out_vals,x=time_pts,linetype=Series)) +
  stat_smooth() + theme_minimal() +
  theme(panel.grid=element_blank()) + xlab('Time') + ylab('Ideological Positions') + 
  guides(linetype=F) +
  geom_path(data=orig_vals,aes(y=out_vals,x=time_pts,linetype=Series),size=1) +
  facet_wrap(~Series)

est_alpha <- apply(rstan::extract(out_fit,pars='alpha',permute=T)$alpha,
                   c(2,3),
                   mean)
cor(est_alpha,combine_vec)
