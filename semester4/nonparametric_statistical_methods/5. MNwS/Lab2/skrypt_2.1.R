library(dplyr)
library(tidyr)
library(ggplot2)
#iloœæ symulacji
N <- 1000
#rozpatrywane wart. oczek.
mus <- seq(.1, 1, by = .1)
#ropatrywane poziomy istotnoœci
alphas <- c(.001, .01, .05, .1)
#d³ugoœci próby
sample_l <- 5:15

params <- expand.grid(mus, alphas, sample_l)
str(params)
names(params) <- c("mu", "alpha", "length")
head(params)
View(params)

set.seed(111)
now <- Sys.time()
powers <- sapply(1:nrow(params), function(p){
  mu <- params[p, 1]
  alpha <- params[p, 2]
  l <- params[p, 3]
  p_sim <-sapply(rep(mu, N), function(x){
    my_sample <- rnorm(l, mu)
    t.test(my_sample)$p.value
  })
  mean(p_sim < alpha)
})
Sys.time() - now

power_df <- bind_cols(params, power = powers)
View(power_df)

power_df %>% ggplot(aes(x = length, 
                        y = power, 
                        col = factor(mu))) +
  geom_line() +
  facet_wrap(~alpha, ncol = 2)

power_wide <- 
  power_df %>%
  spread(key = mu, value = power)
View(power_wide)

power_wide %>%
  ggplot(aes(x = length, 
             y = `0.5`,
             col = factor(alpha))) +
  geom_line()

power_long <- 
  power_wide %>%
  gather(key = mu, value = power, -alpha, -length) %>%
  mutate(mu = as.numeric(mu))
View(power_long)
View(power_df)
glimpse(power_long)
