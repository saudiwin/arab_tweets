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
  filter(time_pts>0) %>% 
  ggplot(aes(y=out_vals,x=time_pts)) +
  stat_summary(geom='ribbon',fun.data = 'median_hilow',fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Country + Religion) +
  scale_linetype(name='') + 
  geom_vline(aes(xintercept=coup_day_new),linetype=3) +
  scale_x_continuous(breaks=c(6,coup_day_new,75),
                     labels=c('2013-03-31','2013-07-02','2013-11-08'))
ggsave('arab_ideology.png')

get_time %>% 
  filter(time_pts>0) %>% 
  ggplot(aes(y=out_vals,x=time_pts)) +
  stat_summary(geom='path',fun.y= 'median',aes(linetype=Country)) + theme_minimal() +
  facet_wrap(~Religion,ncol=1,scales='free_y') +
  theme(panel.grid=element_blank()) + xlab('')  + ylab('Ideological Positions') + 
  geom_vline(aes(xintercept=coup_day_new),linetype=4) +
  scale_x_continuous(breaks=c(6,coup_day_new,75),
                     labels=c('2013-03-31','2013-07-02','2013-11-08'))
ggsave('religion_coint.png')

get_time %>% 
  filter(time_pts>1) %>% 
  ggplot(aes(y=out_vals,x=time_pts)) +
  stat_summary(geom='path',fun.y= 'median',aes(linetype=Religion)) + theme_minimal() +
  facet_wrap(~Country,ncol=1,scales='free_y') +
  theme(panel.grid=element_blank()) + xlab('')  + ylab('Ideological Positions') + 
  geom_vline(aes(xintercept=coup_day_new),linetype=4) +
  scale_x_continuous(breaks=c(6,coup_day_new,75),
                     labels=c('2013-03-31','2013-07-02','2013-11-08'))
ggsave('country_coint.png')

# Let's do some impulse response functions
adj <- rstan::extract(out_fit_id,pars='adj')$adj

irf <- function(time=1,shock=1,gamma1=NULL,
                gamma2=NULL,
                adj=NULL,y_1=0,x_1=0,total_t=10,
                old_output=NULL) {
  
  # set up the exogenous shock
  if(time==1) {
    x_1 <- shock 
  }
  
  if(time==1) {
    y_1 <- rep(y_1,times=length(gamma1))
    x_1 <- rep(x_1,times=length(gamma1))
  }
  print(paste0('Now processing time point ',time))

  # Calculate current values of y and x given posterior uncertainty
  output <- data_frame(y=y_1 - gamma1*(y_1 - adj * x_1),
                      x=x_1 - gamma2*(x_1 - (1/adj)*y_1),
                      time=time,
                      iter=1:length(gamma1))
  
  if(!is.null(old_output)) {
    new_output <- bind_rows(old_output,output)
  } else {
    new_output <- output
  }
  
  # run function recursively until time limit is reached
  
  if(time<total_t) {
    irf(time=time+1,
        shock=shock,
        gamma1=gamma1,
        gamma2=gamma2,
        adj=adj,
        y_1=output$y,
        x_1=output$x,
        total_t=total_t,
        old_output=new_output)
  } else {
    return(new_output)  
  }
  
}

is_prec1 <- irf(gamma1=gamma11[,1],
                gamma2=gamma12[,1],
                     adj=adj[,1])
is_post1 <- irf(gamma1=gamma11[,2],
                gamma2=gamma12[,2],
                      adj=adj[,1])
se_prec1 <- irf(gamma1=gamma21[,1],
                gamma2=gamma22[,1],
                       adj=adj[,2])
se_post1 <- irf(gamma1=gamma21[,2],
                gamma2=gamma22[,2],
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
  ggplot(aes(y=y_irf,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80') + theme_minimal() +
  stat_summary(fun.y='median',geom='path',linetype=2) +
  theme(panel.grid=element_blank()) + xlab('Time') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='')

ggsave('irf_egypt_panels.png')

all_irfs1 %>% 
  separate(Series,into=c('Religion','Time Period'),sep = '\n') %>% 
  ggplot(aes(y=y_irf,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80',aes(group=`Time Period`),alpha=0.5) + theme_minimal() +
  stat_summary(fun.y='median',geom='path',aes(linetype=`Time Period`)) +
  theme(panel.grid=element_blank()) + xlab('Time') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Religion,scales='free_y',ncol = 1) +
  scale_linetype(name='')

ggsave('irf_egypt_overlap.png')

#Now redo where Tunisia is outcome of the IRF

is_prec2 <- irf(gamma2=gamma11[,1],
                gamma1=gamma12[,1],
                adj=1/adj[,1])
is_post2 <- irf(gamma2=gamma11[,2],
                gamma1=gamma12[,2],
                adj=1/adj[,1])
se_prec2 <- irf(gamma2=gamma21[,1],
                gamma1=gamma22[,1],
                adj=1/adj[,2])
se_post2 <- irf(gamma2=gamma21[,2],
                gamma1=gamma22[,2],
                adj=1/adj[,2])

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

ggsave('irf_tunisia_panels.png')

all_irfs2 %>% 
  separate(Series,into=c('Religion','Time Period'),sep = '\n') %>% 
  ggplot(aes(y=y_irf,x=time))   +
  stat_summary(geom='ribbon',fun.data = hdr,fill='grey80',aes(group=`Time Period`),alpha=0.5) + theme_minimal() +
  stat_summary(fun.y='median',geom='path',aes(linetype=`Time Period`)) +
  theme(panel.grid=element_blank()) + xlab('Time') + ylab('Ideological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Religion,scales='free_y',ncol = 1) +
  scale_linetype(name='')

ggsave('irf_tunisia_overlap.png')
