# test coding

require(readr)
require(dplyr)

hana <- read_csv('data/Hana Cairo Elites - cairo_elites_all.csv.csv')
dana <- read_csv('data/Dana Cairo Elites - cairo_elites_all.csv.csv')


cor(as.numeric(as.factor(hana$Ideology)),
    as.numeric(as.factor(dana$Ideology)),
    use='complete.obs')

check_ideology <- data_frame(hana$Ideology,
                             dana$Ideology)
