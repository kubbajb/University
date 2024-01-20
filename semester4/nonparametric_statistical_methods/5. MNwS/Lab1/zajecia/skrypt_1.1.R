library(dplyr)
library(ggplot2)

set.seed(1918)
tmp <- rnorm(100)

?t.test
t.test(tmp)

tmp <- rnorm(100, 1, 1)
t.test(tmp) 
t.test(tmp) %>% str()
t.test(tmp)$p.value

#d³ugoœæ próby
sample_l <- 10
#poziom istotnoœci
alpha <- 0.1
#wartoœæ oczekiwana
mu = 0.1
#liczba symulacji
N <- 1000

set.seed(1410)
p_sim <-sapply(rep(mu, N), function(x){
  my_sample <- rnorm(sample_l, mu)
  t.test(my_sample)$p.value
})

#histogram p.value
tibble(p = p_sim) %>% ggplot(aes(x = p)) +
  geom_histogram() + 
  scale_x_log10()

hist(log10(p_sim))

#moc testu
sum(p_sim < alpha) / N

#d³ugoœæ próby
sample_l <- 5:20
#poziom istotnoœci
alpha <- 0.1
#wartoœæ oczekiwana
mu = 1
#liczba symulacji
N <- 1000

#JAK D£UGOŒÆ PRÓBY WP£YWA NA MOC? 

p_samples <- tibble(p = 1, n = 1)
for(n in sample_l){
  p_sim <-sapply(rep(mu, N), function(x){
    my_sample <- rnorm(n, mu)
    t.test(my_sample)$p.value
  })
  samples <- tibble(p = p_sim, n = n)
  p_samples <- bind_rows(p_samples, samples)
}

p_samples <- p_samples %>% filter(n > 1)

glimpse(p_samples)

powers <- p_samples %>%
  group_by(n) %>%
  summarise(power = sum(p < alpha) / n())
powers

powers %>% ggplot(aes(x = n, y = power)) + 
  geom_line()
