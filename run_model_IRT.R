# Loading data

require(dplyr)
require(tidyr)
require(RSQLite)
require(rstan)
require(bayesplot)
require(ggplot2)
require(readr)

all_tunis <- dbConnect(SQLite(),'data/tunis_tweets.sqlite')


tunis_rts <- dbReadTable(all_tunis,'unique_rts')

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

elite_coding <- read_csv('data/Coding Comparison - Sheet1.csv') %>% 
  mutate(final_code=coalesce(`Dana Coding`,
                             `Hana Coding`))
