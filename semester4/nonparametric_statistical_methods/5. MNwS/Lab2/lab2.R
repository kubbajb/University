
library(tidyr)
library(dplyr)
library(ggplot2)

#ilosc sym
N <- 1000
#wart ocz
mus <- seq(.1, 1, by = .1)
#poziomy ist
alphas <- c(.001, .01, .05, .1)
#dlugosc proby
lengths <- seq(10, 20, by = 2)

#ramka danych z kazda mozliwa 3 z tych wartosci
params <- expand.grid(mu = mus, alpha = alphas, length = lengths)
View(params)

#names(params) <- c("mu", "alpha", "length")
str(params)

set.seed(999)
now <- Sys.time()
#dla kazdej 3 liczymy moc testu
powers <- sapply(1:nrow(params),
                 function(i){
                   mu <- params[i, 1]
                   alpha <- params[i, 2]
                   n <- params[i, 3]
                   p_vector <- sapply(rep(mu, N), function(x){
                     my_sample <- rnorm(n, x)
                     t.test(my_sample)$p.value
                   })
                   mean(p_vector < alpha)
                 })

Sys.time() - now #ile czasu siê wykonywa³o

power_df <- bind_cols(params, power = powers)
glimpse(power_df)
View(power_df)

power_df %>%
  ggplot(aes(x = mu, y = power, color = factor(length))) +
  geom_line() + 
  facet_wrap(~ alpha, nrow = 2)
  

power_wide <-
  power_df %>%
  spread(length, power)
View(power_wide)

#jak dlugoœæ próby zale¿y od wartoœci oczekiwanej
power_wide %>%
  ggplot(aes(x = mu, y = `16`, col = factor(alpha))) +
  geom_line()

power_long <-
  power_wide %>%
  gather(length, power, -alpha, -mu) %>%
  mutate(length = as.integer(length))
