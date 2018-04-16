# Run Summary Stats on fitted Bayesian VAR IRT model

require(dplyr)
require(ggplot2)
require(rstan)
require(shinystan)
require(ggridges)
require(tidyr)
require(bayesplot)
require(purrr)
require(forcats)

# load fitted rstan model

out_fit_id <- readRDS('out_fit_id_std_VAR_betax2018-04-16 08_55_23.rds')

# load times data and check for the coup day
times <- readRDS('times.rds')
coup_day_new <- unique(times$coup_day)-min(times$time)

# currently using elite ideal points with country fixed effects included
get_time <- rstan::extract(out_fit_id,pars='alpha_country',permute=T)$alpha_country

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
  separate(Series,into=c('Country','Religion'),remove=F)


get_time %>% 
  filter(time_pts>0) %>% 
  ggplot(aes(y=out_vals,x=time_pts)) +
  stat_summary(geom='ribbon',fun.data = 'median_hilow',aes(fill=Series),
               alpha=0.5) + theme_minimal() +
  stat_summary(fun.y='median',geom='path',aes(linetype=Series),alpha=0.5) +
  theme(panel.grid=element_blank()) + xlab('') + ylab('Ideological Positions') + 
  scale_fill_brewer(palette='RdBu',name='') + 
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
adj_in <- rstan::extract(out_fit_id,pars='adj_in')$adj_in
adj_out <- rstan::extract(out_fit_id,pars='adj_out')$adj_out
high_int <- rstan::extract(out_fit_id,pars='alpha_int_high')$alpha_int_high
intercepts <- rstan::extract(out_fit_id,pars='alpha_int')$alpha_int
sigma_time <- rstan::extract(out_fit_id,pars='sigma_time')$sigma_time
beta_adj <- rstan::extract(out_fit_id,pars='betax')$sigma_time

irf <- function(time=1,shock=1,
                intercepts=NULL,
                adj_in=NULL,
                adj_out=NULL,
                beta_x = NULL,
                y_1=0,
                x_1=0,
                total_t=10,
                old_output=NULL) {
  
  # set up the exogenous shock
  # unless the shock comes from an exogenous covariate beta_x
  if(time==1 & is.null(beta_x)) {
    x_1 <- shock 
  }
  
  if(time==1) {
    y_1 <- rep(y_1,times=nrow(adj_in))
    if(length(x_1)==1) {
      x_1 <- rep(x_1,times=nrow(adj_in))
    }
    
  }
  print(paste0('Now processing time point ',time))

  # Calculate current values of y and x given posterior uncertainty
  # use different version if external covariates are included
  if(is.null(beta_x)) {
    output <- data_frame(y=intercepts[,1] + adj_in[,1]*y_1 + adj_out[,1]*x_1,
                         x=intercepts[,2] + adj_in[,2]*x_1 + adj_out[,2]*y_1,
                         time=time,
                         iter=1:nrow(adj_in))
  } else {
    output <- data_frame(y=intercepts[,1] + adj_in[,1]*y_1 + adj_out[,1]*x_1 + beta_x[,1],
                         x=intercepts[,2] + adj_in[,2]*x_1 + adj_out[,2]*y_1 + beta_x[,2],
                         time=time,
                         iter=1:nrow(adj_in))
  }

  
  if(!is.null(old_output)) {
    new_output <- bind_rows(old_output,output)
  } else {
    new_output <- output
  }
  
  # run function recursively until time limit is reached
  
  if(time<total_t) {
    irf(time=time+1,
        shock=shock,
        intercepts=intercepts,
        adj_in=adj_in,
        adj_out=adj_out,
        beta_x=beta_x,
        y_1=output$y,
        x_1=output$x,
        total_t=total_t,
        old_output=new_output)
  } else {
    return(new_output)  
  }
  
}

# set shock to 2 SD of the time series
# shock is negative or positive depending on whether it would push them towards or 
# away from the religionists in the same country
islamists_eg <- irf(shock=1,
                    intercepts = cbind(high_int,intercepts[,1]),
                    adj_in=adj_in[,1:2],
                    adj_out=adj_out[,1:2])
islamists_tun <- irf(shock=1,
                    intercepts = cbind(intercepts[,1],high_int),
                    adj_in=adj_in[,2:1],
                    adj_out=adj_out[,2:1])
secularist_eg  <- irf(shock=1,
                      intercepts = intercepts[,2:3],
                      adj_in=adj_in[,3:4],
                      adj_out=adj_out[,3:4])
secularist_tun  <- irf(shock=1,
                      intercepts = intercepts[,3:2],
                      adj_in=adj_in[,4:3],
                      adj_out=adj_out[,4:3])


all_irfs1 <- bind_rows(list(`Islamists\nEgypt`=islamists_eg,
                      `Islamists\nTunisia`=islamists_tun,
                      `Secularists\nEgypt`=secularist_eg,
                      `Secularists\nTunisia`=secularist_tun),
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
  theme(panel.grid=element_blank()) + xlab('Days Since Shock') + ylab('ChangeIdeological Positions') + 
  scale_colour_brewer(palette='paired',name='') + 
  facet_wrap(~Series,scales='free_y') +
  scale_linetype(name='') +
  geom_hline(yintercept = 0,linetype=2)

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
