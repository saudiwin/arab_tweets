# Create GNIP JSONs

require(jsonlite)
require(dplyr)
require(tidyr)
require(readr)


elites <- read_csv('data/cairo_elites_all.csv')

to_gnip <- select(elites,just_unique) %>% 
  rename(from='just_unique') %>% 
  mutate(retweets_of=from)
over_rows <- rep(1:4,each=ceiling(nrow(to_gnip)/4))[1:nrow(to_gnip)]

for(i in 1:5) {
  this_gnip <- filter(elites,over_rows==i)
  cat(paste0('from: ',
             paste0(this_gnip$just_unique,collapse = ' OR from: ')),
      file=paste0('data/to_gnip_from_',i,'.txt'),
      append=F)
}

for(i in 1:5) {
  this_gnip <- filter(elites,over_rows==i)
  cat(paste0('retweets_of: ',
             paste0(this_gnip$just_unique,collapse = ' OR from: ')),
      file=paste0('data/to_gnip_retweets_',i,'.txt'),
      append=F)
}


