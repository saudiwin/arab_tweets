# This script loads anonymized Twitter data & codings of elite Twitter users
# And then fits the IRT-VAR model described in Kubinec and Owen (2018)
# Note that fitting the full IRT-VAR model will take approximately 1 day & require 16 GB memory


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

# time to aggregate twitter dates to

day_count <- 1

# whether to run a sample for testing purposes
sample_users <- F

# Load in revised codings
# remove jasmine foundation because it is a-political

elite_codings_sect <- read_csv('data/check_complete.csv') %>% 
  mutate(codingd1=paste0(coding,'_',Country),
         coding_numd1=as.numeric(factor(codingd1)),
         Username=tolower(Username)) %>% 
  filter(person=='dana') %>% 
  filter(Username!="jasminef_tn")

elite_codings_dem <- read_csv("data/2d_coding.csv") %>% 
  left_join(select(elite_codings_sect,Username,Country),
            by=c(param_name="Username")) %>% 
  group_by(Country) %>% 
  mutate(codingd2=if_else(med_est>median(med_est),paste0("Democratic_",Country),
                        paste0("Anti-Democratic_",Country))) %>% 
  ungroup %>% 
         mutate(coding_numd2=as.numeric(factor(codingd2))) %>% 
  filter(!is.na(Country))

# write out codings for paper

select(elite_codings_sect,Username,`Secularist/Islamist`=coding) %>% 
  xtable::xtable(align='rll') %>% 
  print(type='latex',file='list_elites.tex',
        include.rownames=F,
        floating=F,
        booktabs=T,
        tabular.environment='longtable')

#SQLite databases
all_tunis <- dbConnect(SQLite(),'data/tunis_tweets.sqlite')
all_egypt <- dbConnect(SQLite(),'data/egypt_tweets_small.sqlite')

tunis_rts <- dbReadTable(all_tunis,'unique_rts')
egypt_rts <- dbReadTable(all_egypt,'unique_rts')

# get rid of all twitter users who RT less than 3 different people

filter_tunis <- group_by(tunis_rts,rt_ids,username) %>% count %>% group_by(rt_ids) %>% count() %>% filter(n>5)

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

filter_egypt <- group_by(egypt_rts,rt_ids,username) %>% count %>% group_by(rt_ids) %>% count() %>% filter(n>5)

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

# need to make the coup indicator & change time to one day intervals
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
  coup_day_new <- new_days[old_days==coup_day]
}

# create a datset of times and save it
times <- data_frame(time=old_days,time_three=new_days,
                    coup=if_else(time_three>coup_day_new,2L,1L),
                    coup_day=coup_day) %>% 
  mutate(time_date=as.Date(time,origin='2012-12-31'))
saveRDS(times,'times.rds')

combined_data <- left_join(combined_data,
                           times)

times <- distinct(times,time_three,time, coup)

combined_data_small <- left_join(combined_data,
                                 elite_codings_sect,
                                 by=c('username'='Username')) %>% 
                      left_join(select(elite_codings_dem,codingd2,coding_numd2,
                                       param_name),
                                by=c("username"="param_name")) %>% 
                                   group_by(time_three,
                                coding_numd1,
                                coding_numd2,
                                rt_ids,coup) %>% tally

# drop missing

combined_data_small_nomis <- filter(combined_data_small,!is.na(coding_numd1),
                                    !is.na(coding_numd2)) %>% 
  distinct(coding_numd1,
           coding_numd2,
           time_three,
           rt_ids,
           coup,
           n)

# let's look at histograms of tweets

lookat <- group_by(combined_data_small_nomis,time_three,coding_numd1) %>% summarize(sum_count=sum(n)) %>% 
  mutate(Series=recode(as.character(coding_numd1),`1`='Islamist Egypt',
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

lookat_c_ret <- group_by(combined_data_small_nomis,time_three,coding_numd1) %>% summarize(onet=sum(n==1),
                                                                                        twot=sum(n==2),
                                                                                        threet=sum(n==3),
                                                                                        fourt=sum(n==4))

lookat_cit_ratio <- group_by(combined_data_small_nomis,rt_ids,coding_numd1) %>% tally %>% 
  group_by(rt_ids) %>% 
  mutate(prop_group=n/sum(n))

lookat_cit_top <- lookat_cit_ratio %>% 
  filter(prop_group>.8) %>% 
  group_by(coding_numd1) %>% 
  top_n(2,n)

lookat_cit_patriot <- lookat_cit_ratio %>% 
  filter(prop_group==1)

# create IDs for Stan
combined_data_small_nomis  <- ungroup(combined_data_small_nomis) %>% 
  mutate(cit_ids=as.numeric(factor(rt_ids))) 

#use 99999 as the placeholder for values that are unobserved

combined_zero <- select(combined_data_small_nomis,time_three,coding_numd1,
                        coding_numd2,coup,n,cit_ids) %>% 
                        complete(cit_ids,nesting(coding_numd1,
                                 coding_numd2),time_three,fill=list(n=99999)) %>% 
  mutate(coup=if_else(time_three>coup_day_new,2L,1L))

# now collapse missing and non-missing

# combined_zero <- group_by(combined_zero,coding_numd1,
#                           coding_numd2,time_three) %>% 
#   mutate(num_miss=sum(n==99999),
#          num_obs=n() - num_miss)

if(sample_users==T) {
  # filter list of retweet users for users with lots of retweets across sectarian groups
  keep_users <- ungroup(lookat_cit_ratio) %>% 
    mutate(cit_ids=as.numeric(factor(rt_ids))) %>% 
    filter(prop_group<.9) %>% 
    arrange(desc(n)) %>% slice(1:10000) %>% 
    select(cit_ids) %>% 
    distinct
  combined_zero <- inner_join(keep_users,combined_zero,by=c('cit_ids')) %>% 
    distinct(cit_ids,coding_num,time_three,.keep_all=T) %>% 
    mutate(cit_ids=as.numeric(factor(cit_ids)))
}

# create version of the data suitable for map-reduce with Stan (maprect function)

all_data <- combined_zero %>% 
  select(-coup) %>% 
  mutate(country=1L) %>% 
  select(cit_ids,n,country,time_three,coding_numd1,coding_numd2) %>% 
  gather(key = "variable",value="index",-cit_ids) %>% 
  split(f=combined_zero$cit_ids) %>% 
  lapply(function(d) d$index)



all_data_array <- abind::abind(all_data,along=2)

# create matrix of IDs to pass along with data

all_data_array <- rbind(matrix(c(rep(max(combined_zero$coding_numd1),ncol(all_data_array)),
                                 rep(max(combined_zero$time_three),ncol(all_data_array)),
                                 rep(coup_day_new,ncol(all_data_array))),ncol=ncol(all_data_array),byrow = T),
                        all_data_array)

all_data_array <- apply(all_data_array,2,as.integer)

out_data <- list(J=max(combined_zero$coding_numd1),
K=max(combined_zero$cit_ids),
`T`=max(combined_zero$time_three),
C=6,
N=dim(all_data_array)[1],
S=dim(all_data_array)[2],
alldata=t(all_data_array),
coup=50L,
id_num_high=1,
id_num_low=1,
time_points=as.matrix(1:max(combined_zero$cit_ids)),
start_vals=rep(c(-.5,-.5,.5,.5),2),
time_gamma=distinct(times,time_three,coup) %>% slice(-n()) %>% pull(coup))

stan_rdump(ls(out_data),file="data/to_maprect_cluster.R",
           envir = list2env(out_data))

