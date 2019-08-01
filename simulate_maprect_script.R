# Run simulation from R script

require(tidyr)
require(ggplot2)
require(dplyr)
require(forcats)
require(rstan)
require(stringr)
require(shinystan)
require(bayesplot)

source('helper_func.R')

Sys.setenv(STAN_NUM_THREADS = 280)

# Number of sides

sides <- 4

# Number of elites per side 

elites <- 12

# Number of time points

t <- 50

# Number of citizens

cit <- 280


## Simulation

# Intercepts points
cit_pt <- rnorm(n=cit)
# Discrim points
cit_dis1 <- rnorm(n=cit)
cit_dis2 <- rnorm(n=cit)
# Diff points
cit_diff <- rnorm(n=cit)
# separate points for hurdle/missing data model
abs_cit_pt <- rnorm(n=cit)
abs_cit_dis1 <- rnorm(n=cit)
abs_cit_dis2 <- rnorm(n=cit)

init_sides1 <- rnorm(sides/2)
init_sides2 <- rnorm(sides/2)
cuts <- c(-1,1)
betax1 <- rnorm(sides,0,.25)
betax2 <- rnorm(sides,0,.25)
#intercepts
alpha_int1 <- c(-1,1,rnorm(2,0,0.25))
alpha_int2 <- c(-1,1,rnorm(2,0,0.25))
# to generate adjustment parameters, we draw from the unit circle
unit_pts <- pracma::randp(n=sides)
adj_in1 <- runif(sides,min = -0.5,max=0.5)
adj_out1 <- runif(sides,min = -0.5,max=0.5)
adj_in2 <- runif(sides,min = -0.5,max=0.5)
adj_out2 <- runif(sides,min = -0.5,max=0.5)
alpha1 <- rnorm(sides)
alpha2 <- rnorm(sides)
sigma1 <- rnorm(sides,0.1,0.02)
sigma2 <- rnorm(sides,0.1,0.02)
country <- .5


out_vec1 <- gen_ts_data(t=t,
                        adj_in=adj_in1[1:2],
                        adj_out=adj_out1[1:2],
                        alpha_int=alpha_int1[1:2],
                        this_beta=betax1[1:2],
                        sigma=sigma1[1:2],
                        init_sides=init_sides1,
                        country=country)

out_vec1 %>% 
  mutate(time=1:t) %>% 
  gather(series,estimates,-time) %>% 
  ggplot(aes(y=estimates,x=time,linetype=series)) +geom_path() +theme_minimal() +
  geom_vline(xintercept=(t/2),linetype=4)

# simulate second set of elites

out_vec2 <- gen_ts_data(t=t,
                        adj_in=adj_in1[3:4],
                        adj_out=adj_out1[3:4],
                        alpha_int=alpha_int1[2:3],
                        this_beta=betax1[3:4],
                        sigma=sigma1[3:4],
                        init_sides=init_sides2,
                        country=country)

out_vec2 %>% 
  mutate(time=1:t) %>% 
  gather(series,estimates,-time) %>% 
  ggplot(aes(y=estimates,x=time,linetype=series)) +geom_path() +theme_minimal() +
  geom_vline(xintercept=(t/2),linetype=4)

# now generate second dimension

out_vec3 <- gen_ts_data(t=t,
                        adj_in=adj_in2[1:2],
                        adj_out=adj_out2[1:2],
                        alpha_int=alpha_int2[1:2],
                        this_beta=betax2[1:2],
                        sigma=sigma2[1:2],
                        init_sides=rnorm(sides/2),
                        country=country)

out_vec4 <- gen_ts_data(t=t,
                        adj_in=adj_in2[3:4],
                        adj_out=adj_out2[3:4],
                        alpha_int=alpha_int2[3:4],
                        this_beta=betax2[3:4],
                        sigma=sigma2[3:4],
                        init_sides=rnorm(sides/2),
                        country=country)

combine_vec1 <- bind_cols(out_vec1,out_vec2) %>% as.matrix
combine_vec2 <- bind_cols(out_vec3,out_vec4) %>% as.matrix

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
country_id <- as.integer(combine_ids[,2] %in% c(2,4))
# first generate probability of actually seeing the tweet

present <- sapply(1:nrow(combine_ids), function(n) {
  runif(1)<plogis(abs_cit_dis1[cit_ids[n]] * (combine_vec1[time_ids[n],elite_ids[n]]) +
                    abs_cit_dis2[cit_ids[n]] * combine_vec2[time_ids[n],elite_ids[n]] -
                    abs_cit_pt[cit_ids[n]])
})

# loop over and produce tweet count

gen_out <- sapply(1:nrow(combine_ids), function(n) {
  if(present[n]) {
    outcome <- rpois(n=1,lambda = exp(cit_dis1[cit_ids[n]]*(combine_vec1[time_ids[n],elite_ids[n]]) +
                                        cit_dis2[cit_ids[n]]*combine_vec2[time_ids[n],elite_ids[n]] -
                                        cit_diff[cit_ids[n]])) 
    return(outcome)
  } else {
    return(99999)
  }
})

# replace NAs with double max value

#gen_out[is.na(gen_out)] <- max(gen_out[gen_out!=99999],na.rm=T)*2

# create version of the data suitable for map-reduce with Stan (maprect function)

all_data <- cbind(gen_out,country_id,combine_ids) %>% 
  as_tibble %>% 
  gather(key = "variable",value="index",-cit_ids) %>% 
  split(f=combine_ids[,'cit_ids']) %>% 
  lapply(function(d) d$index)



all_data_array <- abind::abind(all_data,along=2)

# create matrix of IDs to pass along with data

all_data_array <- rbind(matrix(c(rep(sides,ncol(all_data_array)),
                                 rep(t,ncol(all_data_array)),
                                 rep(t/2,ncol(all_data_array))),ncol=ncol(all_data_array),byrow = T),
                        all_data_array)

# need to create time variable for gamma

time_gamma <- c(rep(0L,(t/2)-1),rep(1L,t/2))

#function to create starting values

start_func <- function() {
  list(alpha=runif(t*sides,min = -0.5,max=0.5),
       gamma1=c(0.5,0.5),
       gamma2=c(0.5,0.5),
       sigma_time1=rep(0.25,sides),
       sigma_time2=rep(0.25,sides),
       adj_in1=c(-0.25,0.25,-0.25,0.25),
       adj_out1=c(0.25,-0.25,0.25,-0.25),
       adj_in2=c(-0.25,0.25,-0.25,0.25),
       adj_out2=c(0.25,-0.25,-0.25,0.25),
       mean_delta=1,
       mean_beta=0,
       sigma_beta=1,
       sigma_delta=1,
       beta_0=rnorm(n=cit),
       delta_10=rnorm(n=cit),
       delta_11=rnorm(n=cit),
       delta_20=rnorm(n=cit),
       delta_21=rnorm(n=cit))
}

time_counter <- as.matrix(1:t,ncol=1)

# dump to text file

stan_data <- list(J=sides,
                  nshards=t,
                  K=cit,
                  `T`=t,
                  C=5,
                  N=dim(all_data_array)[1],
                  S=dim(all_data_array)[2],
                  alldata=t(all_data_array),
                  id_num_high=1,
                  id_num_low=1,
                  time_points=as.matrix(1:cit),
                  coup=as.integer(t/2),
                  start_vals=c(init_sides1,init_sides2),
                  time_gamma=time_gamma)

stan_rdump(ls(stan_data),"data_2d_stan.R",envir = list2env(stan_data))
