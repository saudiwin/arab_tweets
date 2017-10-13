# combine codings and validate 

require(dplyr)
require(ggplot2)
require(readr)
require(tidyr)

dana <- read_csv('data/Final Coding -- Dana - Sheet1.csv') %>% 
  select(-X7,-X8)
hana <- read_csv('data/Final Coding -- Hana - Sheet1.csv') %>% 
  filter(Username %in% dana$Username)

combine <- bind_rows(list(dana=dana,hana=hana),.id='person') %>% 
  separate('Ideology',into=c('Religion','Democracy')) %>% 
  gather(variable,coding,Religion, Democracy) %>% 
  filter(coding!='<NA>')

# look at aggregate values of uncertainty

group_by(combine,person,coding,Country) %>% 
  summarize(mean_con=mean(`Confidence (0-100)`,na.rm=T)) %>% 
  arrange(coding, desc(mean_con))

# dana tends to be more certain across the board. 

# certain categories tend to be less certain. Harder to tell who secularists 
# are in Tunisia compared to Egypt. Also very hard to tell who is anti-dem. 
# in Tunisia. 

# check the coding -- if they match, check=1, if they don't, check=2

check_coding <- group_by(combine,
                         Username,variable) %>% 
  summarize(check=length(unique(coding)))

hist(check_coding$check)

# A good number of codings differ -- about 100 out of three hundred or 67%

prop.table(table(check_coding$check))

# but how does this agreement vary by the type of variable (Democracy or Religion)?

prop.table(xtabs(formula = ~check + variable,data=check_coding),margin=1)

# We can see from this 4x4 table that 72% (77) of the agreement differences are for 
# The democracy variable, while only 28% (30) for the Religion variable
# Thus religion has much higher validity as a coding, while Democracy 
# has much less comparatively
# we only need to validate the religion codings further

# save file for reconciliation

check_coding %>% 
  filter(check>1 & variable=='Religion') %>% 
  select(-variable) %>% 
  left_join(combine, 'Username') %>% 
  filter(variable=='Religion') %>% 
  write_csv('check_coding.csv')

# reconcile file 

check_revise <- read_csv('data/check_coding_v2.csv') %>% 
  distinct(Username,coding) %>% 
  mutate(coding2=coding) %>% 
  select(-coding)


# remove data from combined file 

combine <- filter(combine, coding %in% c('Secularist','Islamist')) %>% 
  mutate(coding=ifelse(Username %in% check_revise$Username,
                                         NA,
                                        coding)) %>% 
  left_join(check_revise) %>% 
  mutate(coding=coalesce(coding,coding2))
  
write_csv(combine,'data/check_complete.csv')