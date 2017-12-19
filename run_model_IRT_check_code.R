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
  filter(person=='dana')

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

combined_data_small <- left_join(combined_data,
                                 elite_codings2,
                                 by=c('username'='Username')) %>% 
                                   group_by(username,
                                coding_num,
                                rt_ids) %>% tally

# drop missing

#combined_data_small_nomis <- filter(combined_data_small,!is.na(coding_num)) %>% 


# drop the random six in the dataset

combined_data_small_nomis <-   mutate(combined_data_small,
                                      country=recode(coding_num,`1`=1L,
                                                     `3`=1L,
                                                     `2`=2L,
                                                     `4`=2L)) %>% 
  filter(!is.na(username),!is.na(coding_num))

# types of retweets over time 

lookat_c_ret <- group_by(combined_data_small_nomis,coding_num) %>% summarize(onet=sum(nn==1),
                                                                                        twot=sum(nn==2),
                                                                                        threet=sum(nn==3),
                                                                                        fourt=sum(nn==4))

lookat_cit_ratio <- group_by(combined_data_small_nomis,rt_ids,coding_num) %>% tally %>% 
  group_by(rt_ids) %>% 
  mutate(prop_group=n/sum(n))

lookat_cty_ratio <- combined_data_small_nomis %>% 
                             group_by(rt_ids,country) %>% tally %>% 
  group_by(rt_ids) %>% 
  mutate(prop_group=n/sum(n))

lookat_cit_top <- lookat_cit_ratio %>% 
  filter(prop_group>.8) %>% 
  group_by(coding_num) %>% 
  top_n(2,n)

lookat_cty_top <- lookat_cty_ratio %>% 
  filter(prop_group>.8) %>% 
  group_by(country) %>% 
  top_n(2,n)

lookat_cit_patriot <- lookat_cit_ratio %>% 
  filter(prop_group==1)

combined_data_small_nomis <- ungroup(combined_data_small_nomis) %>% 
  mutate(cit_ids=factor(rt_ids),
         cit_ids=fct_relevel(cit_ids,246904991,52517368),
         cit_ids=as.numeric(cit_ids),
         user_ids=as.numeric(factor(username)))


start_func <- function() {
  list(alpha=rnorm(max(U)),
       gamma1=c(0.5,0.5),
       gamma2=c(0.5,0.5),
       ts_sigma=rep(0.25,4),
       adj1=c(1,1),
       adj2=c(1,1),
       adj=c(1,1),
       mean_delta=1,
       mean_beta=1,
       islamist=1,
       country=1,
       free_params=rnorm(2),
       sigma_beta=1,
       sigma_delta=.8,
       sigma_time=.25,
       beta=rnorm(max(C)),
       delta=rnorm(max(C)),
       gamma_par1=0,
       gamma_par2=0)
}


# code_compile <- stan_model(file='poisson_irt_check_code_2d.stan')


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
#lets do egypt and tunisia separately
combined_data_egypt <- filter(combined_data_small_nomis,country==1) %>% 
  mutate(user_ids=as.numeric(factor(user_ids)),
         cit_ids=as.numeric(factor(cit_ids)))
C <- max(combined_data_egypt$cit_ids)
U <- max(combined_data_egypt$user_ids)
# out_fit_id_egypt <- vb(code_compile,
#                     data=list(J=max(combined_data_egypt$user_ids),
#                               K=max(combined_data_egypt$cit_ids),
# 
#                               N=nrow(combined_data_egypt),
#  #                             C=max(combined_data_egypt$nn),
#                               id_num_high=1,
#                               id_num_low=1,
#                               jj=combined_data_egypt$user_ids,
#                               kk=combined_data_egypt$cit_ids,
#                               cc=combined_data_egypt$country,
#                               y=as.integer(combined_data_egypt$nn),
#                               start_vals=c(-.5,-.5,.5,.5)),
#                     init=start_func)
combined_data_tunis <- filter(combined_data_small_nomis,country==2) %>% 
  mutate(user_ids=as.numeric(factor(user_ids)),
         cit_ids=as.numeric(factor(cit_ids)))
C <- max(combined_data_tunis$cit_ids)
U <- max(combined_data_tunis$user_ids)
# out_fit_id_tunis <- vb(code_compile,
#                        data=list(J=max(combined_data_tunis$user_ids),
#                                  K=max(combined_data_tunis$cit_ids),
#                                  
#                                  N=nrow(combined_data_tunis),
#                                  #                             C=max(combined_data_tunis$nn),
#                                  id_num_high=1,
#                                  id_num_low=1,
#                                  jj=combined_data_tunis$user_ids,
#                                  kk=combined_data_tunis$cit_ids,
#                                  cc=combined_data_tunis$country,
#                                  y=as.integer(combined_data_tunis$nn),
#                                  start_vals=c(-.5,-.5,.5,.5)),
#                        init=start_func)

#distinct usernames and codings 

distinct_code <- distinct(combined_data_small_nomis,username,coding_num,country)


# Let's try some factor analysis for Tunisia

spread_tunis <- ungroup(combined_data_tunis) %>% 
  select(username,nn,rt_ids) %>% spread(key = username,value=nn,
                       fill=0)
tunis_matrix <- select(spread_tunis,-matches('id')) %>% as.matrix %>% 
  apply(2,function(c) c <- if_else(c>0,1,c))
row.names(tunis_matrix) <- spread_tunis$rt_ids 

require(FactoMineR)

# tunis_PCA <- PCA(t(tunis_matrix),ncp=10)
# tunis_PCA_cluster <- HCPC(tunis_PCA)
# tunis_CA <- CA(t(tunis_matrix))
# tunis_kmeans <- kmeans(t(tunis_matrix),10)
#MDS

d_tun <- dist(scale(t(tunis_matrix)))
ord_scale_tun <- MASS::isoMDS(d_tun,k = 3)
met_scale_tun <- cmdscale(d_tun,eig=T, k=2)

data_frame(username=colnames(tunis_matrix),
           dim1=ord_scale_tun$points[,1],
           dim2=ord_scale_tun$points[,2],
           dim3=ord_scale_tun$points[,3]) %>% 
  left_join(distinct_code,'username') %>% 
  mutate(coding=fct_recode(factor(coding_num),`Islamist Egypt`='1',
                           `Islamist Tunisia`='2',
                           `Secularist Egypt`='3',
                           `Secularist Tunisia`='4'),
         coding=fct_relevel(coding,'Secularist Tunisia')) %>% 
  ggplot(aes(y=dim2,x=reorder(username,dim2),size=coding,colour=coding)) + 
  geom_text(aes(label=username)) +
  theme_minimal()
check_tunis <- data_frame(username=colnames(tunis_matrix),
                          dim1=ord_scale_tun$points[,1],
                          dim2=ord_scale_tun$points[,2],
                          dim3=ord_scale_tun$points[,3]) %>% 
  left_join(distinct_code,'username') %>% 
  mutate(coding=fct_recode(factor(coding_num),`Islamist Egypt`='1',
                           `Islamist Tunisia`='2',
                           `Secularist Egypt`='3',
                           `Secularist Tunisia`='4'),
         coding=fct_relevel(coding,'Secularist Tunisia'))
data_frame(username=colnames(tunis_matrix),
           dim1=ord_scale_tun$points[,1],
           dim2=ord_scale_tun$points[,2],
           dim3=ord_scale_tun$points[,3]) %>% 
  left_join(distinct_code,'username') %>% 
  mutate(coding=fct_recode(factor(coding_num),`Islamist Egypt`='1',
                           `Islamist Tunisia`='2',
                           `Secularist Egypt`='3',
                           `Secularist Tunisia`='4'),
         coding=fct_relevel(coding,'Secularist Egypt')) %>% 
  plot_ly(x=~dim1,y=~dim2,z=~dim3,text=~username,color=~coding) %>% 
  add_markers()

#to_plot <- as.array(out_fit_id_egypt)

data_frame(username=colnames(tunis_matrix),
           dim1=tunis_PCA$ind$coord[,1],
           dim2=tunis_PCA$ind$coord[,2],
           dim3=tunis_PCA$ind$coord[,3],
           dim4=tunis_PCA$ind$coord[,4],
           dim5=tunis_PCA$ind$coord[,5],
           dim6=tunis_PCA$ind$coord[,6],
           dim7=tunis_PCA$ind$coord[,7],
           dim8=tunis_PCA$ind$coord[,8],
           dim9=tunis_PCA$ind$coord[,9],
           dim10=tunis_PCA$ind$coord[,10]) %>% 
  ggplot(aes(y=dim8,x=dim9)) + 
  geom_text(aes(label=username)) +
  theme_minimal()

# Now we'll try a similar thing for Egypt

# combined_data_egypt_all <- complete(ungroup(combined_data_egypt),
#                                     rt_ids,cit_ids,
#                                     fill=list(nn=0))
spread_cairo <- ungroup(combined_data_egypt) %>% 
  select(username,nn,rt_ids) %>% spread(key = username,value=nn,
                       fill=0)
cairo_matrix <- select(spread_cairo,-matches('id')) %>% as.matrix
row.names(cairo_matrix) <- spread_cairo$rt_ids 
#remove singletons
check_rows <- apply(cairo_matrix,1,function(r) {
  sum(r>0)
})
# cairo_matrix <- cairo_matrix[check_rows>2,]
# cairo_matrix_std <- apply(cairo_matrix,1,function(r) {
#   (r / sum(r))*100
# })
d_egy <- dist(t(cairo_matrix))
ord_scale_egy <- MASS::isoMDS(d_egy,k = 3)
met_scale_egy <- cmdscale(d_egy,eig=T, k=3)

data_frame(username=colnames(cairo_matrix),
           dim1=ord_scale_egy$points[,1],
           dim2=ord_scale_egy$points[,2],
           dim3=ord_scale_egy$points[,3]) %>% 
  left_join(distinct_code,'username') %>% 
  mutate(coding=fct_recode(factor(coding_num),`Islamist Egypt`='1',
                                      `Islamist Tunisia`='2',
                                      `Secularist Egypt`='3',
                                      `Secularist Tunisia`='4'),
         coding=fct_relevel(coding,'Secularist Egypt')) %>% 
  #filter(dim1<100 & dim1>-100,dim2<100 & dim2>-100,dim3<50,dim3>-50) %>% 
  ggplot(aes(y=dim1,x=dim2,colour=coding)) + 
  geom_point(aes(size=coding),alpha=0.5) +
  theme_minimal() +
  theme(panel.grid=element_blank())
  #geom_text(aes(label=username)) +


data_frame(username=colnames(cairo_matrix),
           dim1=ord_scale_egy$points[,1],
           dim2=ord_scale_egy$points[,2],
           dim3=ord_scale_egy$points[,3]) %>% 
  left_join(distinct_code,'username') %>% 
  mutate(coding=fct_recode(factor(coding_num),`Islamist Egypt`='1',
                           `Islamist Tunisia`='2',
                           `Secularist Egypt`='3',
                           `Secularist Tunisia`='4'),
         coding=fct_relevel(coding,'Secularist Egypt')) %>% 
  #filter(coding=='Islamist Egypt') %>% 
  #filter(dim1<2 & dim1>-2,dim2<2 & dim2>-2,dim3<2 & dim3>-2) %>% 
  plot_ly(x=~dim1,y=~dim2,z=~dim3,text=~username,color=~coding) %>% 
  add_markers()

data_frame(username=colnames(cairo_matrix),
           dim1=ord_scale_egy$points[,1],
           dim2=ord_scale_egy$points[,2],
           dim3=ord_scale_egy$points[,3]) %>% 
  left_join(distinct_code,'username') %>% 
  mutate(coding=fct_recode(factor(coding_num),`Islamist Egypt`='1',
                           `Islamist Tunisia`='2',
                           `Secularist Egypt`='3',
                           `Secularist Tunisia`='4'),
         coding=fct_relevel(coding,'Secularist Egypt')) %>% 
  #filter(coding=='Islamist Egypt') %>% 
  #filter(dim1<2 & dim1>-2,dim2<2 & dim2>-2,dim3<2 & dim3>-2) %>% 
  plot_ly(x=~dim1,y=~reorder(username,dim1),text=~username,color=~coding) %>% 
  add_markers()

  


all_alphas <- rstan::extract(out_fit_id_egypt,'alpha_2d')[[1]] %>% apply(2,function(c) {
  data_frame(mean_est=mean(c),
             high=quantile(c,0.9),
             low=quantile(c,0.1)) %>% 
    return()
}) %>% bind_rows() %>% 
  mutate(user_name=distinct_code$username,
         coding=fct_recode(factor(distinct_code$coding_num),`Islamist Egypt`='1',
                                           `Islamist Tunisia`='2',
                                           `Secularist Egypt`='3',
                                           `Secularist Tunisia`='4'),
         country=fct_collapse(coding,Egypt=c('Secularist Egypt',
                                      'Islamist Egypt'),
                              Tunisia=c('Secularist Tunisia',
                                        'Islamist Tunisia')),
         ideology=fct_collapse(coding,Secularist=c('Secularist Egypt',
                                            'Secularist Tunisia'),
                               Islamist=c('Islamist Egypt',
                                          'Islamist Tunisia')))
# individual panels
all_alphas %>% 
  filter(!is.na(coding)) %>% 
  ggplot(aes(x=reorder(user_name,mean_est),y=mean_est)) +
  geom_pointrange(aes(ymin=low,ymax=high,colour=coding)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank()) +
  coord_flip() +
  facet_wrap(facets = ~coding,scales='free_y') + xlab('') + 
  ylab('Clustering Coefficient from IRT Model')
#country panels
all_alphas %>% 
  filter(!is.na(coding)) %>% 
  ggplot(aes(x=reorder(user_name,mean_est),y=mean_est)) +
  geom_pointrange(aes(ymin=low,ymax=high,colour=coding)) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  coord_flip() +
  facet_wrap(facets = ~country,scales='free_y') + xlab('') + 
  ylab('Clustering Coefficient from IRT Model')

ggsave('clustering_1d.png',width=15,height=10,units='in')

#ideology panels

#1 and 2-d panels

all_alphas_1d <- rstan::extract(out_fit_id_tunis,'alpha_1d')[[1]] %>% apply(2,function(c) {
  data_frame(mean_est_1d=mean(c),
             high_1d=quantile(c,0.9),
             low_1d=quantile(c,0.1)) %>% 
    return()
}) %>% bind_rows() %>% 
  mutate(user_name=distinct_code$username,
         coding=fct_recode(factor(distinct_code$coding_num),`Islamist Egypt`='1',
                           `Islamist Tunisia`='2',
                           `Secularist Egypt`='3',
                           `Secularist Tunisia`='4'),
         country=fct_collapse(coding,Egypt=c('Secularist Egypt',
                                             'Islamist Egypt'),
                              Tunisia=c('Secularist Tunisia',
                                        'Islamist Tunisia')),
         ideology=fct_collapse(coding,Secularist=c('Secularist Egypt',
                                                   'Secularist Tunisia'),
                               Islamist=c('Islamist Egypt',
                                          'Islamist Tunisia')))

all_alphas_2d <- rstan::extract(out_fit_id_tunis,'alpha_2d')[[1]] %>% apply(2,function(c) {
  data_frame(mean_est_2d=mean(c),
             high_2d=quantile(c,0.9),
             low_2d=quantile(c,0.1)) %>% 
    return()
}) %>% bind_rows() %>% 
  mutate(user_name=distinct_code$username,
         coding=fct_recode(factor(distinct_code$coding_num),`Islamist Egypt`='1',
                           `Islamist Tunisia`='2',
                           `Secularist Egypt`='3',
                           `Secularist Tunisia`='4'),
         country=fct_collapse(coding,Egypt=c('Secularist Egypt',
                                             'Islamist Egypt'),
                              Tunisia=c('Secularist Tunisia',
                                        'Islamist Tunisia')),
         ideology=fct_collapse(coding,Secularist=c('Secularist Egypt',
                                                   'Secularist Tunisia'),
                               Islamist=c('Islamist Egypt',
                                          'Islamist Tunisia')))

combined_alpha <- bind_cols(all_alphas_1d,all_alphas_2d)

out_plot <- combined_alpha %>% 
  filter(!is.na(coding)) %>% 
  ggplot(aes(y=mean_est_1d,x=mean_est_2d)) +
  geom_point(aes(size=abs(((high_2d+high_1d)/2)-((low_1d+low_2d)/2)),colour=coding),alpha=0.5) +
  geom_text(aes(label=user_name),check_overlap = T) +
  theme_minimal() +
  theme(panel.grid = element_blank()) 
  # coord_flip() +
  # facet_wrap(facets = ~country,scales='free_y') + xlab('') + 
  ylab('Clustering Coefficient from IRT Model')
ggplotly(out_plot)
ggsave('clustering_1d.png',width=15,height=10,units='in')

lookat <- summary(out_fit_id)
hist(lookat$summary[,'Rhat'])
# 
non_identified_parameters <- lookat$summary[which(lookat$summary[,'Rhat']>1.1),]
# mcmc_trace(to_plot,pars='lp__')
