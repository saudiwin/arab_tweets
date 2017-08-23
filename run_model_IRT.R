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

old_days <- min(combined_data$time):max(combined_data$time)
seq_val <- floor(length(old_days)/3)
new_days <- rep(1:seq_val,each=3)
if(length(new_days)<length(old_days)) {
  new_days <- c(new_days,rep(seq_val+1,times=length(old_days)-length(new_days)))
}

combined_data <- left_join(combined_data,
                           data_frame(time=old_days,time_three=new_days))

combined_data_small <- group_by(combined_data,
                                username,
                                rt_ids) %>% count

# add in codings

combined_data_small <- left_join(combined_data,elite_coding,
                                 by=c('username'='Username'))

start_func <- function() {
  list(alpha=rbind(matrix(c(init_sides1,init_sides2),ncol=4),
                   matrix(rep(0, 99*4),ncol=4)),
       gamma1=c(0.5,0.5),
       gamma2=c(0.5,0.5),
       ts_sigma=rep(0.25,4),
       adj=c(1,1),
       mean_delta=0,
       mean_beta=0,
       sigma_beta=1,
       sigma_delta=1,
       beta=rnorm(n=100),
       delta=rnorm(100),
       gamma_par1=0,
       gamma_par2=0)
}

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
                              start_vals=c(init_sides1,init_sides2),
                              time_gamma=time_gamma),
                    cores=4,
                    control=list(max_treedepth=11,
                                 adapt_delta=0.9),
                    init=start_func)
to_plot <- as.array(out_fit)