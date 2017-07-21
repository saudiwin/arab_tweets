# iterate over elite users' tweets
# object is to create a TxE matrix of retweet values for elites in 3-day segments

require(stringr)
require(dplyr)
require(rtweet)
require(parallel)
require(RSQLite)
require(lubridate)


source('helper_functions.R')

twitter_tokens <- list(
  create_token(app = "For Analysis II", # whatever you named app
               consumer_key = "6ae7rNvww5q13Kc86d8YvvLZG",
               consumer_secret = "gfZOqDIGpzkr53UXHrsqgzWRwIiqlerm982NWPcxP1hIf2uwuV"),
  create_token(app = "For Analysis III", # whatever you named app
               consumer_key = "E7RDTfQjwZAKGeRl7PVxbxGzO",
               consumer_secret = "TefV2t5zOWD3IhCepl8UzX5jxna11YeUcwHdMQS2gzGKXe5KXy"),
  create_token(app = "For Analysis VI", # whatever you named app
               consumer_key = "Tv5Leg6m8azjOU9CppevH4wQX",
               consumer_secret = "aCbKlWEzc2BDp1314utCfupxieYRW3Rv5qTTvlpuZoylti7uwE"),
  create_token(app = "For Analysis VII", # whatever you named app
               consumer_key = "IsQFWZHk1fxKzWJvSL8wZOx13",
               consumer_secret = "VLJJCgSgXg00DahOHlQinAgP9WdgUS62DfHDb6zxFtBFEHTHVX"),
  create_token(app = "For Analysis IV", # whatever you named app
               consumer_key = "Q0zOsWNjncK677fkErC9u9FGn",
               consumer_secret = "gCA883t0Vbk8MfQw5by0odHtE7BXrlikA4G0o1oHcSoDK2F2R4"),
  create_token(app = "For Analysis IV", # whatever you named app
               consumer_key = "xg9aQq4VXRaRynmThU86pjyv5",
               consumer_secret = "GI96VKrSqGdX92TB1SwwZiobo6z55gD1hjCbUDcPDvpEpWxqHw"),
  create_token(app = "For Analysis VIII", # whatever you named app
               consumer_key = "NKcT4JwM45bkXdPb09jyWui2B",
               consumer_secret = "NijPrXjKyrzpAWZbrSB6gPXoZLK7Ot0evrSSxFehvrNnuWMuqO")
)

