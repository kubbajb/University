 
set.seed(1)
tmp <- rnorm(20)

?t.test
t.test(tmp)

tmp <- rnorm(20, 1)
t.test(tmp)
t.test(tmp) %>% str()
t.test(tmp)$p.value

#d³ugoœæ próby
sample_l <- 20
#poziom istotnoœci
alpha <- 0.05
#wartoœæ oczekiwana
mu <- .2
#iloœæ symulacji
N <- 1000

#wektor p-value dla N symulacji
set.seed(4)
p_vector <- sapply(rep(mu, N), function(x){
  my_sample <- rnorm(sample_l, mu)
  t.test(my_sample)$p.value
})

head(p_vector)
length(p_vector)

tibble(p = p_vector) %>%
  ggplot(aes(x = p)) + 
  geom_histogram() + 
  scale_x_log10() 

#obliczam moc
mean(p_vector < alpha)


#d³ugoœæ próby
sample_l <- 20
#poziom istotnoœci
alpha <- 0.05
#wartoœæ oczekiwana
mu <- seq(.1, 2, by = .1)
#iloœæ symulacji
N <- 1000

sample_df <- tibble(p = 1, mu = 0)
set.seed(9)


for(x in mu){
  p_vector <- sapply(rep(x, N), function(x){
    my_sample <- rnorm(sample_l, x)
    t.test(my_sample)$p.value
  })
  tmp_df <- tibble(p = p_vector, mu = x)
  sample_df <- bind_rows(sample_df, tmp_df)
}
sample_df <- sample_df %>% filter(mu > 0)

glimpse(sample_df)

power_df <- sample_df %>%
  group_by(mu) %>%
  summarise(power = mean(p < 0.05))

power_df

power_df %>%
  ggplot(aes(x = mu, y = power)) +
  geom_line()

