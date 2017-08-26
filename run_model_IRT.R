# Loading data

require(dplyr)
require(tidyr)
require(RSQLite)
require(rstan)
require(bayesplot)
require(ggplot2)
require(readr)
require(forcats)

#Load in codings
elite_coding <- read_csv('data/Coding Comparison - Sheet1.csv') %>% 
  mutate(final_code=coalesce(`Dana Coding`,
                             `Hana Coding`)) %>% 
  separate(final_code,into=c('Religion','Regime'),sep='-') %>% 
  mutate(country=c(rep('Tunisia',64),
                   rep('Egypt',n()-64)),
         coding=paste0(Religion,'_',country)) %>% 
  filter(!is.na(Religion)) %>% 
  mutate(coding_num=as.numeric(factor(coding)),
         Username=tolower(Username))

#SQLite databases
all_tunis <- dbConnect(SQLite(),'data/tunis_tweets.sqlite')
all_egypt <- dbConnect(SQLite(),'data/egypt_tweets_small.sqlite')

tunis_rts <- dbReadTable(all_tunis,'unique_rts')
egypt_rts <- dbReadTable(all_egypt,'unique_rts')

# get rid of all SNs who RT less than 3 different people

filter_tunis <- group_by(tunis_rts,rt_ids,username) %>% count %>% group_by(rt_ids) %>% count() %>% filter(n>2)

filter_tunis %>% ggplot(aes(x=n)) +
  geom_histogram() +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  xlab('Number of Unique Retweeted Elites') +
  ylab('Count of Citizen Users') +
  geom_vline(aes(xintercept=mean(n)),
                 size=1,colour='red',
             linetype=3)
ggsave('tunis_users_RTS.png')

# same for egypt

filter_egypt <- group_by(egypt_rts,rt_ids,username) %>% count %>% group_by(rt_ids) %>% count() %>% filter(n>2)

filter_egypt %>% ggplot(aes(x=n)) +
  geom_histogram() +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  xlab('Number of Unique Retweeted Elites') +
  ylab('Count of Citizen Users') +
  geom_vline(aes(xintercept=mean(n)),
             size=1,colour='red',
             linetype=3)
ggsave('egypt_users_RTS.png')

combined_data <- bind_rows(filter(egypt_rts,rt_ids %in% filter_egypt$rt_ids),
                           filter(tunis_rts,rt_ids %in% filter_tunis$rt_ids)) %>% 
  mutate(username=tolower(username))

# need to make the coup indicator & change time to three day intervals
coup_day <- lubridate::yday('2013-07-03')
old_days <- min(combined_data$time):max(combined_data$time)
seq_val <- floor(length(old_days)/3)
new_days <- rep(1:seq_val,each=3)
if(length(new_days)<length(old_days)) {
  new_days <- c(new_days,rep(seq_val+1,times=length(old_days)-length(new_days)))
}
coup_day_new <- new_days[which(old_days>coup_day & (old_days %% 3))[1]]
times <- data_frame(time=old_days,time_three=new_days,
                    coup=if_else(time_three>coup_day_new,2L,1L),
                    coup_day=coup_day)
combined_data <- left_join(combined_data,
                           times)

times <- distinct(times,time_three,coup)

combined_data_small <- group_by(combined_data,
                                coup,
                                time_three,
                                username,
                                rt_ids) %>% count



# add in codings

combined_data_small <- ungroup(combined_data_small) %>% left_join(elite_coding,
                                 by=c('username'='Username')) %>% 
  mutate(user_ids=as.numeric(factor(username)),
         cit_ids=as.numeric(factor(rt_ids)))

# drop missing

combined_data_small_nomis <- filter(combined_data_small,!is.na(coding_num))

# start_func <- function() {
#   list(alpha=rbind(matrix(c(-1,-1,1,1),ncol=4),
#                    matrix(rep(0, (max(combined_data_small_nomis$time_three)-1)*4),ncol=4)),
#        gamma1=c(0.5,0.5),
#        gamma2=c(0.5,0.5),
#        ts_sigma=rep(0.25,4),
#        adj=c(1,1),
#        mean_delta=0,
#        mean_beta=0,
#        sigma_beta=1,
#        sigma_delta=1,
#        beta=rnorm(max(combined_data_small_nomis$cit_ids)),
#        delta=rnorm(max(combined_data_small_nomis$cit_ids)),
#        gamma_par1=0,
#        gamma_par2=0)
# }

# run it again, and this time constrain deltas

# get_time <- rstan::extract(out_fit,pars='delta',permute=T)$delta
# 
# mean_vals <- apply(get_time,2,mean)
# sd_vals <- apply(get_time,2,sd)
# filtered <- data_frame(mean_vals,sd_vals,discrim_id=1:ncol(get_time)) %>% 
#   filter(sd_vals<2)
# 
# # number to identify
# id_num_high <- 40
# id_num_low <- 4
# top_two <- dplyr::arrange(filtered,desc(mean_vals)) %>% slice(1:id_num_high) %>% pull(discrim_id)
# bottom_two <- dplyr::arrange(filtered,mean_vals) %>% slice(1:id_num_low) %>% pull(discrim_id)
# 
# new_vals <- factor(combined_data_small_nomis$cit_ids) %>% fct_relevel(as.character(c(top_two,bottom_two))) %>% 
#   as.numeric
# 
# new_vals[which(combined_data_small_nomis$cit_ids %in% top_two)]
# 
# combined_data_small_nomis$cit_ids <- new_vals

# code_compile <- stan_model(file='ord_irt_v1.stan')
# 
# out_fit <- vb(code_compile,
#                     data=list(J=max(combined_data_small_nomis$coding_num),
#                               K=max(combined_data_small_nomis$cit_ids),
#                               `T`=max(combined_data_small_nomis$time_three),
#                               N=nrow(combined_data_small_nomis),
#                               C=3,
#                               jj=combined_data_small_nomis$coding_num,
#                               kk=combined_data_small_nomis$cit_ids,
#                               tt=combined_data_small_nomis$time_three,
#                               y=as.integer(combined_data_small_nomis$nn),
#                               coup=as.integer(floor(max(combined_data_small_nomis$time_three)/2)),
#                               start_vals=c(-1,-1,1,1),
#                               time_gamma=times$coup[-nrow(times)]),
#                     init=start_func)
# 
# # run it again, and this time constrain deltas
# 
# get_time <- rstan::extract(out_fit,pars='delta',permute=T)$delta
# 
# mean_vals <- apply(get_time,2,mean)
# sd_vals <- apply(get_time,2,sd)
# filtered <- data_frame(mean_vals,sd_vals,discrim_id=1:ncol(get_time)) %>% 
#   filter(sd_vals<2)
# 
# # number to identify
# id_num_high <- 20
# id_num_low <- 4
# top_two <- dplyr::arrange(filtered,desc(mean_vals)) %>% slice(1:id_num_high) %>% pull(discrim_id)
# bottom_two <- dplyr::arrange(filtered,mean_vals) %>% slice(1:id_num_low) %>% pull(discrim_id)
# 
# # new_vals <- factor(combined_data_small_nomis$cit_ids) %>% fct_relevel(as.character(c(top_two,bottom_two))) %>% 
# #   as.numeric
# # 
# # new_vals[which(combined_data_small_nomis$cit_ids %in% top_two)]
# # 
# # combined_data_small_nomis$cit_ids <- new_vals


start_func <- function() {
  list(alpha=rbind(matrix(c(-1,-1,1,1),ncol=4),
                   matrix(rep(0, (max(combined_data_small_nomis$time_three)-1)*4),ncol=4)),
       gamma1=c(0.5,0.5),
       gamma2=c(0.5,0.5),
       ts_sigma=rep(0.25,4),
       adj=c(1,1),
       mean_delta=1,
       mean_beta=0,
       sigma_beta=1,
       sigma_delta=1,
       beta=rnorm(max(combined_data_small_nomis$cit_ids)),
       delta=rnorm(max(combined_data_small_nomis$cit_ids)),
       gamma_par1=0,
       gamma_par2=0)
}

code_compile <- stan_model(file='ord_irt_id_v2.stan')

out_fit_vb <- vb(code_compile,
              data=list(J=max(combined_data_small_nomis$coding_num),
                        K=max(combined_data_small_nomis$cit_ids),
                        `T`=max(combined_data_small_nomis$time_three),
                        N=nrow(combined_data_small_nomis),
                        C=max(combined_data_small_nomis$nn),
                        id_num_high=1,
                        id_num_low=1,
                        jj=combined_data_small_nomis$coding_num,
                        kk=combined_data_small_nomis$cit_ids,
                        tt=combined_data_small_nomis$time_three,
                        y=as.integer(combined_data_small_nomis$nn),
                        coup=as.integer(floor(max(combined_data_small_nomis$time_three)/2)),
                        start_vals=c(-.5,-.5,.5,.5),
                        time_gamma=times$coup[-nrow(times)]),
              init=start_func)

saveRDS(object = out_fit_vb,paste0('/Volumes/rmk7xy/out_fit_id',lubridate::day(Sys.time()),'-',lubridate::hour(Sys.time()),'.rds'))

out_fit_id <- sampling(code_compile,cores=5,
                    data=list(J=max(combined_data_small_nomis$coding_num),
                              K=max(combined_data_small_nomis$cit_ids),
                              `T`=max(combined_data_small_nomis$time_three),
                              N=nrow(combined_data_small_nomis),
                              C=max(combined_data_small_nomis$nn),
                              id_num_high=1,
                              id_num_low=1,
                              jj=combined_data_small_nomis$coding_num,
                              kk=combined_data_small_nomis$cit_ids,
                              tt=combined_data_small_nomis$time_three,
                              y=as.integer(combined_data_small_nomis$nn),
                              coup=as.integer(floor(max(combined_data_small_nomis$time_three)/2)),
                              start_vals=c(-.5,-.5,.5,.5),
                              time_gamma=times$coup[-nrow(times)]),
                    init=start_func)
saveRDS(out_fit_id,paste0('/Volumes/rmk7xy/out_fit_id',lubridate::day(Sys.time()),'-',lubridate::hour(Sys.time()),'.rds'))


to_plot <- as.array(out_fit_id)

mcmc_intervals(to_plot,regex_pars = 'adj')
mcmc_trace(to_plot,pars='gamma1[1]')
mcmc_trace(to_plot,pars='gamma1[2]')
mcmc_trace(to_plot,pars='gamma2[1]')
mcmc_trace(to_plot,pars='gamma2[2]')

mcmc_intervals(to_plot,regex_pars = c('gamma1|gamma2'))
mcmc_intervals(to_plot,regex_pars = c('alpha'))

gamma1 <- rstan::extract(out_fit_id,pars='gamma1')$gamma1

gamma2 <- rstan::extract(out_fit_id,pars='gamma2')$gamma2

all_gammas <- data_frame(Islamists=gamma1[,2]-gamma1[,1],
                         Secularists=gamma2[,2]-gamma2[,1]) %>% 
  gather(`Ideological\nPairing`,Difference) %>% 
  group_by(`Ideological\nPairing`) %>% 
  mutate(mean_val=median(Difference))

ggplot(all_gammas,aes(x=Difference)) +
  geom_density(aes(fill=`Ideological\nPairing`),colour=NA,alpha=0.5,adjust=0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  xlab('Gamma Difference') +
  ylab('Posterior Density') +
  geom_vline(aes(xintercept=mean_val,linetype=`Ideological\nPairing`))

summarize(all_gammas,mean_val=mean(Difference),
          median_val=median(Difference),
          upper=quantile(Difference,0.9),
          lower=quantile(Difference,0.1))

get_time <- rstan::extract(out_fit_vb,pars='alpha',permute=T)$alpha
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

return_cl <- function(var_out) {
  data_frame()
}

get_time %>% 
  ggplot(aes(y=out_vals,x=time_pts)) +
  stat_smooth() + theme_minimal() +
  theme(panel.grid=element_blank()) + xlab('Time') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series) +
  scale_linetype(name='')

get_time %>% 
  ggplot(aes(y=out_vals,x=time_pts)) +
  stat_summary(geom='ribbon',fun.data = 'median_hilow',fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('Time') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series) +
  scale_linetype(name='') + 
  geom_vline(aes(xintercept=32),linetype=3)

ggsave('arab_ideology.png')


deltas <- rstan::extract(out_fit,pars='delta',permuted=T)$delta
betas <- rstan::extract(out_fit,pars='beta',permuted=T)$beta
apply(deltas,2,mean) %>% hist
apply(betas,2,mean) %>% hist
# lookat <- summary(out_fit_id)
# hist(lookat$summary[,'Rhat'])
# 
# non_identified_parameters <- lookat$summary[which(lookat$summary[,'Rhat']>1.1),]
 mcmc_trace(to_plot,pars='mean_delta')
 mcmc_trace(to_plot,pars='sigma_beta')
# mcmc_trace(to_plot,pars='lp__')
