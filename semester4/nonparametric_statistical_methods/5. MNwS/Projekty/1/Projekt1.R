
library(dplyr)
library(ggplot2)

#d³ugoœæ próby
N <- 1000
#stopnie swobody
df <- 10
#poziom istotnoœci
alpha <- 0.05

sample <- rt(N, df)

shapiro.test(sample)$p.value

#H0: próbka ma rozk³ad normalny 
#H1: próbka nie ma rozk³adu normalnego <-

#wektor p-value dla N symulacji
p_vector <- sapply(rep(df, N), function(x){
  shapiro.test(rt(N, df))$p.value
})

tibble(p = p_vector) %>%
  ggplot(aes(x = p)) + 
  geom_histogram() + 
  scale_x_log10() 

#obliczam moc
mean(p_vector < alpha)
#przy tych parametrach moc testu wynosi 100%
#w tylu % odrzucamy hipoteze H0




#####JAK D£UGOSC PROBY WP£YWA NA MOC?
length <- 100:200
#stopnie swobody
df <- 30
#poziom istotnoœci
alpha <- 0.05
#liczba symulacji
N <- 1000

proba <- expand.grid(df = df, alpha = alpha, length = length)
p_samples <- tibble(p = 1, n = 1)

for(n in length){
  p_sim <-sapply(rep(df, N), function(x){
    my_sample <- rt(n, df)
    shapiro.test(my_sample)$p.value
  })
  samples <- tibble(p = p_sim, n = n)
  p_samples <- bind_rows(p_samples, samples)
}
p_samples <- p_samples %>% filter(n > 1)

View(p_samples)
glimpse(p_samples)

powers <- p_samples %>%
  group_by(n) %>%
  summarise(power = mean(p < alpha))
powers

powers %>% ggplot(aes(x = n, y = power)) + 
  geom_line()


######JAK STOPNIE SWOBODY WP£YWAJA NA MOC?

#d³ugoœæ próby
sample_l <- 500
#stopnie swobody
df <- 3:100
#poziom istotnoœci
alpha <- 0.05
#liczba symulacji
N <- 1000

p_samples <- tibble(p = 1, df = 1)

for(x in df){
  p_sim <- sapply(rep(x, N), function(x){
    my_sample <- rt(sample_l, x)
    shapiro.test(my_sample)$p.value
  })
  samples <- tibble(p = p_sim, df = x)
  p_samples <- bind_rows(p_samples, samples)
}
p_samples <- p_samples %>% filter(df > 1)

glimpse(p_samples)

powers <- p_samples %>%
  group_by(df) %>%
  summarise(power = mean(p < alpha))
powers

powers %>% ggplot(aes(x = df, y = power)) + 
  geom_line()


######JAK ALPHA WP£YWA NA MOC?

sample_l <- 100
#stopnie swobody
df <- 5
#poziom istotnoœci
alpha <- seq(.01, .2, by = .02)
#liczba symulacji
N <- 1000

p_samples <- tibble(p = 1, alpha = 1)

for(x in alpha){
  p_sim <- sapply(rep(x, N), function(x){
    my_sample <- rt(sample_l, x)
    shapiro.test(my_sample)$p.value
  })
  samples <- tibble(p = p_sim, alpha = x)
  p_samples <- bind_rows(p_samples, samples)
}
p_samples <- p_samples %>% filter(alpha > 1)

glimpse(p_samples)

powers <- p_samples %>%
  group_by(alpha) %>%
  summarise(power = mean(p < alpha))
powers

powers %>% ggplot(aes(x = alpha, y = power)) + 
  geom_line()





#testy ko³mogorowa

#####JAK D£UGOSC PROBY WP£YWA NA MOC?
sample_l <- 30:200
#stopnie swobody
df <- 30
#poziom istotnoœci
alpha <- 0.05
#liczba symulacji
N <- 1000


p_samples <- tibble(p = 1, n = 1)

for(n in sample_l){
  p_sim <-sapply(rep(n, N), function(x){
    my_sample <- rt(n, df)
    ks.test(my_sample, "pnorm")$p.value
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


######JAK STOPNIE SWOBODY WP£YWAJA NA MOC?

#d³ugoœæ próby
sample_l <- 300
#stopnie swobody
df <- 3:100
#poziom istotnoœci
alpha <- 0.05
#liczba symulacji
N <- 1000

p_samples <- tibble(p = 1, df = 1)

for(x in df){
  p_sim <- sapply(rep(x, N), function(x){
    my_sample <- rt(sample_l, x)
    shapiro.test(my_sample)$p.value
  })
  samples <- tibble(p = p_sim, df = x)
  p_samples <- bind_rows(p_samples, samples)
}
p_samples <- p_samples %>% filter(df > 1)

glimpse(p_samples)


powers <- p_samples %>%
  group_by(df) %>%
  summarise(power = mean(p < alpha))
powers

powers %>% ggplot(aes(x = df, y = power)) + 
  geom_line()

######JAK ALPHA WP£YWA NA MOC?

sample_l <- 100
#stopnie swobody
df <- 5
#poziom istotnoœci
alpha <- seq(.01, .2, by = .02)
#liczba symulacji
N <- 1000

p_samples <- tibble(p = 1, alpha = 1)

for(x in alpha){
  p_sim <- sapply(rep(x, N), function(x){
    my_sample <- rt(sample_l, x)
    shapiro.test(my_sample)$p.value
  })
  samples <- tibble(p = p_sim, alpha = x)
  p_samples <- bind_rows(p_samples, samples)
}
p_samples <- p_samples %>% filter(alpha > 1)

glimpse(p_samples)

powers <- p_samples %>%
  group_by(alpha) %>%
  summarise(power = mean(p < alpha))
powers

powers %>% ggplot(aes(x = alpha, y = power)) + 
  geom_line()






#JARQUE _ BERA

#####JAK D£UGOSC PROBY WP£YWA NA MOC?
sample_l <- 100:200
#stopnie swobody
df <- 30
#poziom istotnoœci
alpha <- 0.05
#liczba symulacji
N <- 1000


p_samples <- tibble(p = 1, n = 1)

for(n in sample_l){
  p_sim <-sapply(rep(df, N), function(x){
    my_sample <- rt(n, df)
    norm_sample <- rnorm()
    jarque.bera.test(my_sample, norm_sample)$p.value
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


######JAK STOPNIE SWOBODY WP£YWAJA NA MOC?

#d³ugoœæ próby
sample_l <- 300
#stopnie swobody
df <- 3:100
#poziom istotnoœci
alpha <- 0.05
#liczba symulacji
N <- 1000

p_samples <- tibble(p = 1, df = 1)

for(x in df){
  p_sim <- sapply(rep(x, N), function(x){
    my_sample <- rt(sample_l, x)
    jarque.bera.test(my_sample)$p.value
  })
  samples <- tibble(p = p_sim, df = x)
  p_samples <- bind_rows(p_samples, samples)
}
p_samples <- p_samples %>% filter(df > 1)

glimpse(p_samples)


powers <- p_samples %>%
  group_by(df) %>%
  summarise(power = mean(p < alpha))
powers

powers %>% ggplot(aes(x = df, y = power)) + 
  geom_line()

######JAK ALPHA WP£YWA NA MOC?

sample_l <- 100
#stopnie swobody
df <- 5
#poziom istotnoœci
alpha <- seq(.01, .2, by = .02)
#liczba symulacji
N <- 1000

p_samples <- tibble(p = 1, alpha = 1)

for(x in alpha){
  p_sim <- sapply(rep(x, N), function(x){
    my_sample <- rt(sample_l, x)
    jarque.bera.test(my_sample)$p.value
  })
  samples <- tibble(p = p_sim, alpha = x)
  p_samples <- bind_rows(p_samples, samples)
}
p_samples <- p_samples %>% filter(alpha > 1)

glimpse(p_samples)

powers <- p_samples %>%
  group_by(alpha) %>%
  summarise(power = mean(p < alpha))
powers

powers %>% ggplot(aes(x = alpha, y = power)) + 
  geom_line()
