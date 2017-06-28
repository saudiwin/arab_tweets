# Given new user list, come up with elite users to send to GNIP

require(RSQLite)
require(dplyr)
require(stringr)
require(readr)
require(ggplot2)
# load tables

all_cairo <- dbConnect(SQLite(),'data/all_cairo.sqlite')
current_users <- dbConnect(SQLite(),'data/user.sqlite')
all_ment <- readRDS('cairo_all_ment.rds') %>% 
  mutate(just_unique=str_replace(just_unique,'@','')) 

all_users <- dbGetQuery(current_users,'SELECT DISTINCT username from users;') %>% 
  mutate(username=tolower(username))

new_ids <- read_csv('data/need_users_cairo.csv - need_users_cairo.csv (1).csv')

existing_ids <- filter(all_ment,just_unique %in% all_users$username) 
keep_new_ids <- filter(new_ids,Include=='Yes')

all_gnip <- bind_rows(keep_new_ids,existing_ids) %>% arrange(desc(t_ment))
elites <- filter(all_gnip,t_users>1000)
filter(all_gnip,t_users>1000) %>% 
  ggplot() +
  geom_histogram(aes(x=t_ment)) +
  theme_minimal()


select(elites,just_unique) %>% write_csv('cairo_elites.csv')
write_csv(elites,'cairo_elites_all.csv')
