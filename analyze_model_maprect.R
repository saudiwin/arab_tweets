# analyze model fitted by cmdstan on NYU-ABU DHABI cluster

require(dplyr)
require(tidyr)
require(rstan)
require(bayesplot)
require(ggplot2)
require(readr)
require(forcats)
require(lubridate)


# load and combine fitted posteriors

fit_mod <- rstan::read_stan_csv("data/arab_full_1.csv")
