# Run and simulate tweet model
require(tidyr)
require(ggplot2)
require(dplyr)
require(forcats)
require(rstan)
require(shinystan)
require(bayesplot)
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
cit_pt <- rnorm(n=cit-1)
cit_pt <- c(0,cit_pt)
# Discrim points
cit_dis <- rnorm(n=cit)

# Generate side/elite ideal points

init_sides1 <- c(1,-1)
init_sides2 <- c(-1,1)
adj1 <- c(.3,0.7)
adj2 <- c(.5,0.8)

gamma1 <- 0.1
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
    t_11 <- init_sides2[1] - gamma*(init_sides1[1] - (adj1[2]/adj1[1])*init_sides1[2]) + rnorm(1,sd=0.25)
    t_12 <- init_sides2[2] - gamma*(init_sides1[2]- (adj1[1]/adj1[2])*init_sides1[1]) + rnorm(1,sd=0.25)
  } else {
    t_11 <- current_val$t1 -  gamma*(current_val$t1- (adj1[2]/adj1[1])*current_val$t2) + rnorm(1,sd=0.25)
    t_12 <- current_val$t2 - gamma*(current_val$t2- (adj1[1]/adj1[2])*current_val$t1) + rnorm(1,sd=0.25)
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

combine_plot <- combine_vec %>% as_data_frame %>% 
  mutate(time=1:t) %>% 
  gather(series,estimates,-time) %>% 
  mutate(series=factor(series),
         series=fct_recode(series,`Tunisia Islamists`='V1',
                           `Egypt Islamists`='V2',
                           `Tunisia Secularists`='V3',
                           `Egypt Secularists`='V4'))
  combine_plot %>% ggplot(aes(y=estimates,x=time,linetype=series,colour=series)) +geom_path(size=1) +theme_minimal() +
  geom_vline(xintercept=(t/2),linetype=4) +
  scale_colour_brewer(palette='Paired') +
  theme(panel.grid = element_blank())

code_compile <- stan_model(file='poisson_irt.stan')

out_fit <- sampling(code_compile,
                    data=list(J=sides,
                              K=cit,
                              `T`=t,
                              N=length(gen_out),
                              jj=combine_ids[,1],
                              kk=combine_ids[,2],
                              tt=combine_ids[,3],
                      y=as.integer(gen_out),
                    coup=as.integer(t/2),
                    start_vals=c(init_sides1,init_sides2)),
                      cores=4,
                    control=list(adapt_delta=0.95))
to_plot <- as.array(out_fit)
mcmc_trace(to_plot,pars='adj[1]')
mcmc_trace(to_plot,pars='adj[2]')
mcmc_trace(to_plot,pars='adj[3]')
adj_est <- as.array(out_fit,'adj')
mcmc_recover_intervals(x=adj_est,true = c(adj1,adj2))
mcmc_recover_intervals(x=as.array(out_fit,c('gamma1','gamma2')),true = c(c(gamma1,gamma2),
                                                                        c(gamma1,gamma2)))
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
  mutate(Series=fct_recode(Series,`Tunisia Islamists`='V1',
                           `Egyptian Islamists`='V2',
                           `Tunisian Secularists`='V3',
                           `Egyptian Secularists`='V4'))

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
