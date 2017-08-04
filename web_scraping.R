# test web scraping

library(rvest)

url <- read_html('https://twitter.com/realDonaldTrump/status/890820505330212864')
sns <- url %>% html_nodes('span.username.u-dir b') %>% 
  html_text()
