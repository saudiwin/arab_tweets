# Loading data

require(dplyr)
require(tidyr)
require(RSQLite)
require(rstan)
require(bayesplot)
require(ggplot2)
require(readr)
require(forcats)
require(googledrive)
require(lubridate)

day_count <- 1

#Load in codings
# elite_coding <- read_csv('data/Coding Comparison - Sheet1.csv') %>%
#   mutate(final_code=coalesce(`Dana Coding`,
#                              `Hana Coding`)) %>%
#   separate(final_code,into=c('Religion','Regime'),sep='-') %>%
#   mutate(country=c(rep('Tunisia',64),
#                    rep('Egypt',n()-64)),
#          coding=paste0(Religion,'_',country)) %>%
#   filter(!is.na(Religion)) %>%
#   mutate(coding_num=as.numeric(factor(coding)),
#          Username=tolower(Username))

# Load in revised codings

elite_codings2 <- read_csv('data/check_complete.csv') %>% 
  mutate(coding=paste0(coding,'_',Country),
         coding_num=as.numeric(factor(coding)),
         Username=tolower(Username)) %>% 
  filter(person=='dana') %>% 
  mutate(coding_num=case_when((coding_num==2 & Username %in% c('r_ghannouchi',
                                                              'nahdhatunisie',
                                                              'ali_larayedh',
                                                              'yusraghkh',
                                                              'ziedladhari',
                                                              'yassayari',
                                                              'khamousss')) ~ 2,
                              (coding_num==2 & !(Username %in% c('r_ghannouchi',
                                                               'nahdhatunisie',
                                                               'ali_larayedh',
                                                               'yusraghkh',
                                                               'ziedladhari',
                                                               'yassayari',
                                                               'khamousss')))~4,
         coding_num==1~1,
         coding_num==3~3,
         coding_num==4~4)) %>% 
  filter(Username!="jasminef_tn")

#SQLite databases
all_tunis <- dbConnect(SQLite(),'data/tunis_tweets.sqlite')
all_egypt <- dbConnect(SQLite(),'data/egypt_tweets.sqlite')

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
seq_val <- floor(length(old_days)/day_count)
new_days <- rep(1:seq_val,each=day_count)
if(length(new_days)<length(old_days)) {
  new_days <- c(new_days,rep(seq_val+1,times=length(old_days)-length(new_days)))
}
if(day_count>1) {
  coup_day_new <- new_days[which(old_days>coup_day & (old_days %% day_count))[1]]
} else {
  coup_day_new <- new_days[which(old_days>coup_day)][[1]]
}


times <- data_frame(time=old_days,time_three=new_days,
                    coup=if_else(time_three>coup_day_new,2L,1L),
                    coup_day=coup_day)
combined_data <- left_join(combined_data,
                           times)

times <- distinct(times,time_three,time, coup)

combined_data_small <- left_join(combined_data,
                                 elite_codings2,
                                 by=c('username'='Username')) %>% 
                                   group_by(time_three,
                                coding_num,
                                rt_ids,coup) %>% tally

# drop missing

combined_data_small_nomis <- filter(combined_data_small,!is.na(coding_num))

# drop the random six in the dataset


# combined_data_small_nomis <- mutate(combined_data_small_nomis,
#                                     nn=if_else(nn==6,4L,nn))


# let's look at histograms of tweets

lookat <- group_by(combined_data_small_nomis,time_three,coding_num) %>% summarize(sum_count=sum(nn)) %>% 
  mutate(Series=recode(as.character(coding_num),`1`='Islamist Egypt',
                           `2`='Islamist Tunisia',
                           `3`='Secularist Egypt',
                           `4`='Secularist Tunisia'))

ggplot(lookat,aes(y=sum_count,x=time_three)) + geom_path() + theme_minimal() + facet_wrap(~Series,scales='free_y') +
  ylab('') + xlab('') +
  scale_x_continuous(breaks=c(times$time_three[times$time==yday('2013-03-31')],
                              coup_day_new,
                              times$time_three[times$time==yday('2013-11-08')]),
                     labels=c('2013-03-31','2013-07-02','2013-11-08')) +
  geom_vline(aes(xintercept=32),linetype=3)

ggsave('retweets_counts.png')

# types of retweets over time 

lookat_c_ret <- group_by(combined_data_small_nomis,time_three,coding_num) %>% summarize(onet=sum(nn==1),
                                                                                        twot=sum(nn==2),
                                                                                        threet=sum(nn==3),
                                                                                        fourt=sum(nn==4))

lookat_cit_ratio <- group_by(combined_data_small_nomis,rt_ids,coding_num) %>% tally %>% 
  group_by(rt_ids) %>% 
  mutate(prop_group=n/sum(n))

lookat_cit_top <- lookat_cit_ratio %>% 
  filter(prop_group>.8) %>% 
  group_by(coding_num) %>% 
  top_n(2,n)

lookat_cit_patriot <- lookat_cit_ratio %>% 
  filter(prop_group==1)

#combined_data_small_nomis <- anti_join(combined_data_small_nomis,lookat_cit_patriot,by='rt_ids') %>% 
combined_data_small_nomis  <- ungroup(combined_data_small_nomis) %>% 
  mutate(cit_ids=as.numeric(factor(rt_ids))) 
combined_zero <- select(combined_data_small_nomis,time_three,coding_num,coup,nn,cit_ids) %>% 
                        complete(cit_ids,coding_num,time_three,fill=list(nn=0)) %>% 
  mutate(coup=if_else(time_three>coup_day_new,2L,1L))
#test complete

test_d <- data_frame(rt_ids=c(1,1,1,2,2,3,4),
                     coding_num=c(1,2,3,3,4,2,1),
                     nn=c(3,2,5,6,3,1,5))

combined_zero <- group_by(combined_zero,cit_ids) %>% 
  mutate(nn=if_else(nn>0,scale(nn),nn)) %>% 
  ungroup() %>% 
  distinct(cit_ids,coding_num,time_three,.keep_all = T)

#Now need to add zeros



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
  list(alpha=rbind(matrix(c(-.5,-.5,.5,.5),ncol=4),
                   matrix(rep(0, (max(combined_zero$time_three)-1)*4),ncol=4)),
       gamma1=c(0.5,0.5),
       gamma2=c(0.5,0.5),
       ts_sigma=rep(0.25,4),
       adj1=c(1,1),
       adj2=c(1,1),
       mean_delta=1,
       mean_beta=1,
       sigma_beta_1=1,
       sigma_beta_0=1,
       sigma_delta=.8,
       shape=1,
       beta_1=rnorm(max(combined_zero$cit_ids)),
       beta_0=rnorm(max(combined_zero$cit_ids)),
       delta_1=rnorm(max(combined_zero$cit_ids)),
       delta_0=rnorm(max(combined_zero$cit_ids)),
       gamma_par1=0,
       gamma_par2=0)
}


code_compile <- stan_model(file='std_irt_zip_betax.stan')


# out_fit_vb <- vb(code_compile,
#               data=list(J=max(combined_data_small_nomis$coding_num),
#                         K=max(combined_data_small_nomis$cit_ids),
#                         `T`=max(combined_data_small_nomis$time_three),
#                         N=nrow(combined_data_small_nomis),
#                         C=max(combined_data_small_nomis$nn),
#                         id_num_high=1,
#                         id_num_low=1,
#                         jj=combined_data_small_nomis$coding_num,
#                         kk=combined_data_small_nomis$cit_ids,
#                         tt=combined_data_small_nomis$time_three,
#                         y=as.integer(combined_data_small_nomis$nn),
#                         coup=as.integer(floor(max(combined_data_small_nomis$time_three)/2)),
#                         start_vals=c(-.5,-.5,.5,.5),
#                         time_gamma=times$coup[-nrow(times)]),
#               init=start_func)
this_time <- Sys.time()
# saveRDS(object = out_fit_vb,paste0('out_fit_vb_',this_time,'.rds'))
# drive_upload(paste0('out_fit_vb_',this_time,'.rds'))
# cores=4,thin=5,



out_fit_id <- vb(code_compile,
                    data=list(J=max(combined_zero$coding_num),
                              K=max(combined_zero$cit_ids),
                              `T`=max(combined_zero$time_three),
                              N=nrow(combined_zero),
                              id_num_high=1,
                              id_num_low=1,
                              jj=combined_zero$coding_num,
                              kk=combined_zero$cit_ids,
                              tt=combined_zero$time_three,
                              y=as.integer(combined_zero$nn),
                              coup=as.integer(floor(max(combined_zero$time_three)/2)),
                              start_vals=c(-.5,-.5,.5,.5),
                              time_gamma=distinct(times,time_three,coup) %>% slice(-n()) %>% pull(coup) ),
                    init=start_func)
saveRDS(out_fit_id,paste0('out_fit_id_std_gc',this_time,'.rds'))
drive_upload(paste0('out_fit_id_std_gc',this_time,'.rds'))

to_plot <- as.array(out_fit_id)

mcmc_intervals(to_plot,regex_pars = 'sigma_time')
mcmc_intervals(to_plot,regex_pars = c('alpha'))

get_time <- rstan::extract(out_fit_id,pars='alpha',permute=T)$alpha
get_time <- get_time[sample(1:nrow(get_time),101),,]
get_time <- lapply(1:dim(get_time)[3],function(x) get_time[,,x]) %>% 
  lapply(as_data_frame) %>% 
  bind_rows(.id='Series') %>% 
  mutate(Series=factor(Series),
         Series=fct_recode(Series,`Islamist Egypt`='1',
                           `Islamist Tunisia`='2',
                           `Secularist Egypt`='3',
                           `Secularist Tunisia`='4')) %>% 
  gather(time_pts,out_vals,-Series) %>% 
  mutate(time_pts=as.numeric(factor(time_pts)))

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
  geom_vline(aes(xintercept=coup_day_new),linetype=3)

ggsave('arab_ideology.png')


deltas <- rstan::extract(out_fit_id,pars='delta_1',permuted=T)$delta
betas <- rstan::extract(out_fit_id,pars='beta_0',permuted=T)$beta
apply(deltas,2,mean) %>% hist
apply(betas,2,mean) %>% hist
lookat <- summary(out_fit_id)
#hist(lookat$summary[,'Rhat'])
# 
#non_identified_parameters <- lookat$summary[which(lookat$summary[,'Rhat']>1.1),]
 mcmc_trace(to_plot,regex_pars='steps')
 mcmc_trace(to_plot,pars='delta[7000]')
# mcmc_trace(to_plot,pars='lp__')
