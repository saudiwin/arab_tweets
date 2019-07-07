# Loading data

require(dplyr)
require(tidyr)
require(RSQLite)
require(rstan)
require(tidybayes)
require(bayesplot)
require(ggplot2)
require(readr)
require(forcats)
require(googledrive)
require(lubridate)

day_count <- 1
sample_users <- F

# Load in revised codings

elite_codings2 <- read_csv('data/check_complete_democracy.csv') %>% 
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

#use -9999 as the placeholder for values that are unobserved
combined_zero <- combined_data %>% 
                      group_by(username,rt_ids) %>% 
                      summarize(rt_count=sum(n)) %>% 
                      ungroup %>% 
                        complete(rt_ids,username,fill=list(rt_count=99999))

# add in coding info

combined_zero <- left_join(combined_zero,select(elite_codings2,coding_num,
                                                Username,Country,democracy),by=c(username="Username"))

# set up pro/anti coding
require(forcats)

pro_coding <- unique(elite_codings2$Username[elite_codings2$democracy=="pro"],na.rm=T)
anti_coding <- unique(elite_codings2$Username[elite_codings2$democracy=="anti"],na.rm=T)
pro_coding <- pro_coding[!is.na(pro_coding)]
anti_coding <- anti_coding[!is.na(anti_coding)]
anti_coding <- anti_coding[!(anti_coding=="bassemloukil")]

combined_zero <- mutate(ungroup(combined_zero),username=fct_relevel(username,pro_coding,
                                                           anti_coding),
                        username_num=as.numeric(username),
                        Country=as.numeric(Country=="Egypt"))

# only take the 10K most prolific users

combined_zero <- group_by(combined_zero,rt_ids) %>% 
  mutate(rt_id_count=sum(rt_count[rt_count!=99999])) 
mean_rt <- mean(combined_zero$rt_id_count)
log_filter <- combined_zero$rt_id_count>mean_rt
combined_zero <- dplyr::filter(ungroup(combined_zero),log_filter)


code_compile <- stan_model(file='static_2d.stan')

# remove NAs in user id

combined_zero <- filter(combined_zero,!is.na(coding_num)) %>% 
  mutate(rt_ids_num=as.numeric(factor(rt_ids)))

this_time <- Sys.time()

# if(sample_users==T) {
#   # filter list of retweet users for users with lots of retweets across sectarian groups
#   keep_users <- ungroup(lookat_cit_ratio) %>% 
#     mutate(cit_ids=as.numeric(factor(rt_ids))) %>% 
#     filter(prop_group<.9) %>% 
#     arrange(desc(n)) %>% slice(1:10000) %>% 
#     select(cit_ids) %>% 
#     distinct
#   combined_zero <- inner_join(keep_users,combined_zero,by=c('cit_ids')) %>% 
#     distinct(cit_ids,coding_num,time_three,.keep_all=T) %>% 
#     mutate(cit_ids=as.numeric(factor(cit_ids)))
# }

#combined_zero <- filter(combined_zero,rt_count!=max(rt_count))

out_fit_id <- sampling(code_compile,
                    data=list(J1=max(combined_zero$coding_num),
                              J2=max(combined_zero$username_num),
                              K=max(combined_zero$rt_ids_num),
                              N=nrow(combined_zero),
                              id_num_high=length(pro_coding),
                              id_num_low=length(anti_coding),
                              jj1=combined_zero$coding_num,
                              jj2=combined_zero$username_num,
                              kk=combined_zero$rt_ids_num,
                              y=combined_zero$rt_count,
                              country_code=combined_zero$Country),cores=3,chains=3,iter=1000)
saveRDS(out_fit_id,"static_2d_full.rds")

require(tidybayes)
require(stringr)

#all_out <- gather_draws(out_fit_id,alpha1,alpha2_full)

all_out <- as.data.frame(out_fit_id,pars=c("alpha1","alpha2_full")) %>% 
  mutate(iter=1:n()) %>% 
  gather(key="var",value="est",-iter) %>% 
  mutate(id=as.numeric(str_extract(var,"(?<=\\[)[0-9]+")),
         paramtype=if_else(grepl(x=var,pattern="alpha1"),"secularism","democracy"))

# join with what the IDs are 

ids <- data_frame(id=c(unique(combined_zero$coding_num),
                              unique(combined_zero$username_num)),
                   param_name=c("Secularist Egypt","Islamist Egypt",
                                "Secularist Tunisia","Islamist Tunisia",
                                unique(as.character(combined_zero$username))),
                  paramtype=c(rep("secularism",4),
                              rep("democracy",length(unique((combined_zero$username_num))))))

all_out <- left_join(all_out,ids)

all_out_sum <- group_by(all_out,paramtype,param_name) %>% 
  summarize(med_est=median(est),
            high_est=quantile(est,.95),
            low_est=quantile(est,.05)) %>% 
  filter(!is.na(param_name))

# write out codings

write_csv(all_out_sum,"data/2d_coding.csv")

# plot the buggers

require(ggrepel)

all_out_sum %>% 
  ggplot(aes(y=med_est,x=reorder(param_name,med_est))) +
  geom_linerange(aes(ymin=high_est,ymax=low_est)) +
  geom_text_repel(aes(label=param_name)) +
  facet_wrap(~paramtype,scales="free") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())


# mcmc_intervals(to_plot,regex_pars = 'sigma_time')
# mcmc_intervals(to_plot,regex_pars = c('adj'))
