
#rozklad t
library(ggplot2)
library(dplyr)

set.seed(1)
tmp <- rnorm(20)

t.test(tmp)
#EX, przedzial uf, p-value

#moc testu:
#jak czesto odrzucamy test gdy H0 jest nieprawdziwa

tmp <- rnorm(20, 1)
t.test(tmp)
#odrzucaone w wiekszosci przypadkow, bo EX = 1 
#aby zbadac moc trzeba wykonac opercje wyele razy i zbadac jaka proporcja jest mniejsza od zadanego poziomu istotnosci

#dlugosc proby
sample_l <- 20
#poziom istotnosci
alpha <- 0.05
#EX
mu <- 1#seq(.1, 2, by = .1)
#ilosc symulacji
N <- 1000

#tworzenie wektora p-value dla 1000 elementowych prob
#wyciaganie p-value z testu
t.test(tmp)$p.value

#wektor p-value dla N symulacji
set.seed(4)

p_vector <- sapply(rep(mu, N), function(x){
  my_sample <- rnorm(sample_l, mu)
  t.test(my_sample)$p.value
})

head(p_vector)
length(p_vector)

#histogram. tibble - ramka danych
#wizualizacja p-value zeby obliczyc moc
tibble(p = p_vector) %>%
  ggplot(aes(x=p)) +
  geom_histogram() +
  scale_x_log10() 

#obliczanie mocy
mean(p_vector < alpha)
#przy tych parametrach moc testu wynosi 98,6%
#w tylu % odrzucamy hipoteze H0


sample_df <- tibble(p = 1, mu = 0)



set.seed(5)
for(x in mu){
  p_vector <- sapply(rep(x, N), function(x){
    my_sample <- rnorm(sample_l, x)
    t.test(my_sample)$p.value
  })
  tmp_df <- tibble(p = p_vector, mu=x)
  sample_df <- bind_rows(sample_df, tmp_df)
}



sample_df <- sample_df %>% filter(mu > 0)

glimpse(sample_df) # to samo co str

#
power_df <- sample_df %>%
  group_by(mu) %>%
  summarise(power = mean(p < alpha))

power_df

power_df %>% ggplot(aes(x = mu, y=power)) +
  geom_line()

#analiza mocy w zaleznosci 