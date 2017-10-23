# Run Summary Stats on integration model

require(dplyr)
require(ggplot2)
require(rstan)
require(shinystan)
require(ggridges)
require(tidyr)
require(bayesplot)
require(purrr)
require(forcats)
#out_fit_id <- readRDS('data/out_fit_id_2017-08-28 16-53-46.rds')

# to_plot <- as.array(out_fit_id)
# 
# mcmc_intervals(to_plot,regex_pars='gamma')

gamma1 <- rstan::extract(out_fit_id,pars='gamma1')$gamma1

gamma2 <- rstan::extract(out_fit_id,pars='gamma2')$gamma2


all_gammas <- data_frame(Islamists=gamma1[,2]-gamma1[,1],
                         Secularists=gamma2[,2]-gamma2[,1],
                         `Islamists\nPre-Coup`=gamma1[,1],
                         `Islamists\nPost-Coup`=gamma1[,2],
                         `Secularists\nPre-Coup`=gamma2[,1],
                         `Secularists\nPost-Coup`=gamma2[,2]) %>% 
  gather(`Ideological\nPairing`,Difference) %>% 
  group_by(`Ideological\nPairing`) %>% 
  mutate(mean_val=median(Difference))

filter(all_gammas, `Ideological\nPairing` %in% c('Islamists','Secularists')) %>% 
  ggplot(aes(x=Difference)) +
  geom_density(aes(fill=`Ideological\nPairing`),colour=NA,alpha=0.5,adjust=0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  xlab('Gamma Difference') +
  ylab('Posterior Density') +
  geom_vline(aes(xintercept=mean_val,linetype=`Ideological\nPairing`))

ggsave('gamma_diff.png')

filter(all_gammas,!( `Ideological\nPairing` %in% c('Islamists','Secularists'))) %>% 
  ggplot(aes(x=Difference,y=`Ideological\nPairing`,fill=`Ideological\nPairing`)) +
    geom_density_ridges(colour=NA) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges() +
  scale_fill_brewer(palette='Paired') +
  xlab('') +
  ylab('') +
  theme(panel.grid = element_blank()) + 
  guides(fill='none')

ggsave('gamma_joy.png')

summarize(all_gammas,mean_val=mean(Difference),
          median_val=median(Difference),
          upper=quantile(Difference,0.9),
          lower=quantile(Difference,0.1))

get_time_raw <- rstan::extract(out_fit_id,pars='alpha',permute=T)$alpha
get_time <- get_time_raw[sample(1:nrow(get_time_raw),101),,]
get_time <- lapply(1:dim(get_time)[3],function(x) get_time[,,x]) %>% 
  lapply(as_data_frame) %>% 
  bind_rows(.id='Series') %>% 
  mutate(Series=factor(Series),
         Series=fct_recode(Series,`Tunisia Islamists`='2',
                           `Egypt Islamists`='1',
                           `Tunisia Secularists`='4',
                           `Egypt Secularists`='3')) %>% 
  gather(time_pts,out_vals,-Series) %>% 
  mutate(time_pts=as.numeric(factor(time_pts))) %>% 
  separate(Series,into=c('Country','Religion'))


get_time %>% 
  filter(time_pts>1) %>% 
  ggplot(aes(y=out_vals,x=time_pts)) +
  stat_summary(geom='ribbon',fun.data = 'median_hilow',fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Country + Religion) +
  scale_linetype(name='') + 
  geom_vline(aes(xintercept=32),linetype=3) +
  scale_x_continuous(breaks=c(6,32,75),
                     labels=c('2013-03-31','2013-07-02','2013-11-08'))
ggsave('arab_ideology.png')

get_time %>% 
  filter(time_pts>1) %>% 
  ggplot(aes(y=out_vals,x=time_pts)) +
  stat_summary(geom='path',fun.y= 'median',aes(linetype=Country)) + theme_minimal() +
  facet_wrap(~Religion,ncol=1,scales='free_y') +
  theme(panel.grid=element_blank()) + xlab('')  + ylab('Ideological Positions') + 
  geom_vline(aes(xintercept=32),linetype=4) +
  scale_x_continuous(breaks=c(6,32,75),
                     labels=c('2013-03-31','2013-07-02','2013-11-08'))
ggsave('religion_coint.png')

get_time %>% 
  filter(time_pts>1) %>% 
  ggplot(aes(y=out_vals,x=time_pts)) +
  stat_summary(geom='path',fun.y= 'median',aes(linetype=Religion)) + theme_minimal() +
  facet_wrap(~Country,ncol=1,scales='free_y') +
  theme(panel.grid=element_blank()) + xlab('')  + ylab('Ideological Positions') + 
  geom_vline(aes(xintercept=32),linetype=4) +
  scale_x_continuous(breaks=c(6,32,75),
                     labels=c('2013-03-31','2013-07-02','2013-11-08'))
ggsave('country_coint.png')

# Let's do some impulse response functions
adj <- rstan::extract(out_fit_id,pars='adj')$adj

irf <- function(time=1,shock=0.5,gamma=NULL,adj=NULL,num=1,y_1=0,x_1=0,total_t=10,
                old_output=NULL) {
  
  # set up the exogenous shock
  if(time==1) {
    if(num==1) {
      y_1 <- shock
    } else {
      x_1 <- shock
    }
  }
  
  if(time==1) {
    y_1 <- rep(y_1,times=length(gamma))
    x_1 <- rep(x_1,times=length(gamma))
  }
  print(paste0('Now processing time point ',time))

  # Calculate current values of y and x given posterior uncertainty
  output <- data_frame(y=y_1 - gamma*(y_1 - adj * x_1),
                      x=x_1 - gamma*(x_1 - (1/adj)*y_1),
                      time=time,
                      iter=1:length(gamma))
  
  if(!is.null(old_output)) {
    new_output <- bind_rows(old_output,output)
  } else {
    new_output <- output
  }
  
  # run function recursively until time limit is reached
  
  if(time<total_t) {
    irf(time=time+1,
        shock=shock,
        gamma=gamma,
        adj=adj,
        num=num,
        y_1=output$y,
        x_1=output$x,
        total_t=total_t,
        old_output=new_output)
  } else {
    return(new_output)  
  }
  
}

is_prec1 <- irf(gamma=gamma1[,1],
                     adj=adj[,1])
is_post1 <- irf(gamma=gamma1[,2],
                      adj=adj[,1])
se_prec1 <- irf(gamma=gamma2[,1],
                       adj=adj[,2])
se_post1 <- irf(gamma=gamma2[,2],
                        adj=adj[,2])

all_irfs1 <- bind_rows(list(`Islamists\nPre-Coup`=is_prec1,
                      `Islamists\nPost-Coup`=is_post1,
                      `Secularists\nPre-Coup`=se_prec1,
                      `Secularists\nPost-Coup`=se_post1),
                      .id='Series') %>% 
  group_by(Series,iter) %>% 
  mutate(x_irf=x - lag(x,order_by=time),
         y_irf=y-lag(y,order_by=time))

hdr <- function(datap) {
  data_frame(y=mean(datap),
             ymin=quantile(datap,0.1),
             ymax=quantile(datap,0.9))
}
  

all_irfs1 %>% 
  ggplot(aes(y=x_irf,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('Time') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='')

ggsave('irf_egypt.png')

is_prec2 <- irf(gamma=gamma1[,1],
                adj=adj[,1],num=2)
is_post2 <- irf(gamma=gamma1[,2],
                adj=adj[,1],num=2)
se_prec2 <- irf(gamma=gamma2[,1],
                adj=adj[,2],num=2)
se_post2 <- irf(gamma=gamma2[,2],
                adj=adj[,2],num=2)

all_irfs2 <- bind_rows(list(`Islamists\nPre-Coup`=is_prec2,
                            `Islamists\nPost-Coup`=is_post2,
                            `Secularists\nPre-Coup`=se_prec2,
                            `Secularists\nPost-Coup`=se_post2),
                       .id='Series') %>% 
  group_by(Series,iter) %>% 
  mutate(x_irf=x - lag(x,order_by=time),
         y_irf=y-lag(y,order_by=time))

all_irfs2 %>% 
  ggplot(aes(y=y_irf,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('Time') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='')

ggsave('irf_tunisia.png')

# try the same thing while varying the adjustment parameter

out_fit_id <- readRDS('data/out_fit_id_2017-08-28 17-20-18.rds')

adj1 <- rstan::extract(out_fit_id,pars='adj1')$adj1
adj2 <- rstan::extract(out_fit_id,pars='adj2')$adj2

all_adjs <- data_frame(Islamists=adj1[,2]-adj1[,1],
                         Secularists=adj2[,2]-adj2[,1],
                         `Islamists\nPre-Coup`=adj1[,1],
                         `Islamists\nPost-Coup`=adj1[,2],
                         `Secularists\nPre-Coup`=adj2[,1],
                         `Secularists\nPost-Coup`=adj2[,2]) %>% 
  gather(`Ideological\nPairing`,Difference) %>% 
  group_by(`Ideological\nPairing`) %>% 
  mutate(mean_val=median(Difference))

filter(all_adjs, `Ideological\nPairing` %in% c('Islamists','Secularists')) %>% 
  ggplot(aes(x=Difference)) +
  geom_density(aes(fill=`Ideological\nPairing`),colour=NA,alpha=0.5,adjust=0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  xlab('Co-integration Vector Difference') +
  ylab('Posterior Density') +
  geom_vline(aes(xintercept=mean_val,linetype=`Ideological\nPairing`))

ggsave('adj_diff.png')

filter(all_adjs,!( `Ideological\nPairing` %in% c('Islamists','Secularists'))) %>% 
  ggplot(aes(x=Difference,y=`Ideological\nPairing`,fill=`Ideological\nPairing`)) +
  geom_joy(colour=NA,rel_min_height = 0.005) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() +
  scale_fill_brewer(palette='Paired') +
  xlab('') +
  ylab('') +
  theme(panel.grid = element_blank()) + 
  guides(fill='none')

ggsave('adj_joy.png')



gamma1 <-  rstan::extract(out_fit_id,pars='gamma1')$gamma1
gamma2 <-  rstan::extract(out_fit_id,pars='gamma2')$gamma2

is_prec1 <- irf(gamma=gamma1,
                adj=adj1[,1])
is_post1 <- irf(gamma=gamma1,
                adj=adj1[,2])
se_prec1 <- irf(gamma=gamma2,
                adj=adj2[,1])
se_post1 <- irf(gamma=gamma2,
                adj=adj2[,2])

all_irfs1 <- bind_rows(list(`Islamists\nPre-Coup`=is_prec1,
                            `Islamists\nPost-Coup`=is_post1,
                            `Secularists\nPre-Coup`=se_prec1,
                            `Secularists\nPost-Coup`=se_post1),
                       .id='Series') %>% 
  group_by(Series,iter) %>% 
  mutate(x_irf=x - lag(x,order_by=time),
         y_irf=y-lag(y,order_by=time))

all_irfs1 %>% 
  ggplot(aes(y=x_irf,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('Time') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='')

ggsave('irf_egypt_adj.png')

is_prec2 <- irf(gamma=gamma1,
                adj=adj1[,1],num=2)
is_post2 <- irf(gamma=gamma1,
                adj=adj1[,2],num=2)
se_prec2 <- irf(gamma=gamma2,
                adj=adj2[,1],num=2)
se_post2 <- irf(gamma=gamma2,
                adj=adj2[,2],num=2)


all_irfs2 <- bind_rows(list(`Islamists\nPre-Coup`=is_prec2,
                            `Islamists\nPost-Coup`=is_post2,
                            `Secularists\nPre-Coup`=se_prec2,
                            `Secularists\nPost-Coup`=se_post2),
                       .id='Series') %>% 
  group_by(Series,iter) %>% 
  mutate(x_irf=x - lag(x,order_by=time),
         y_irf=y-lag(y,order_by=time))

all_irfs2 %>% 
  ggplot(aes(y=y_irf,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('Time') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='')

ggsave('irf_tunisia_adj.png')
