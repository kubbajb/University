install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#1
#pierwsza próba rozk³ad normalny N(1,2)
#druga próba pochodzi z mieszanki rozk³adów N(-1,1), N(1,1)
# z prawdopodobieñstwem 0.5/0.5 <- po³owa pochodzi z 1 rozk³adu po³owa z 2

#il symulacji
N <- 10^4
#dl próby
n <- 30

#pierwsza próba
set.seed(30)
sample_norm <- rnorm(N*n, 0, 2) %>%
  matrix(nrow = N)
#druga próba
set.seed(77)
index <- sample(c(1,2),
                N*n,
                prob = c(0.5, 0.5),
                replace = T)
sample_mix <- rnorm(N*n, c(-1,1)[index]) %>%
  matrix(nrow = N)

p_vals <- sapply(1:N, function(i) {
  wilcox.test(sample_norm[i, ], sample_mix[i, ],
              paired = TRUE)$p.value
})

tibble(p = p_vals) %>%
  ggplot(aes(x = p)) +
  geom_histogram() +
  scale_x_log10() +
  scale_y_log10()

alpha = .05
mean(p_vals < alpha)

#2
#pierwsza próba rozk³ad normalny N(1,2)
#druga próba pochodzi z mieszanki rozk³adów N(0,1), N(1,2)
# z prawdopodobieñstwem 0.5/0.5 <- po³owa pochodzi z 1 rozk³adu po³owa z 2

#il symulacji
N <- 10^4
#dl próby
n <- 30

set.seed(77)
index <- sample(c(1,2),
                N*n,
                prob = c(0.5, 0.5),
                replace = T)
sample_mix <- rnorm(N*n, c(1,2)[index]) %>%
  matrix(nrow = N)

p_vals <- sapply(1:N, function(i) {
  wilcox.test(sample_norm[i, ], sample_mix[i, ],
              paired = TRUE)$p.value
})

tibble(p = p_vals) %>%
  ggplot(aes(x = p)) +
  geom_histogram() +
  scale_x_log10() +
  scale_y_log10()

alpha = .05
mean(p_vals < alpha)


#3

#il symulacji
N <- 10^4
#dl próby
n <- 30

set.seed(77)
index <- sample(c(1,2),
                N,
                prob = c(0.5, 0.5),
                replace = T)
sample_mix <- rnorm(N, c(0,2)[index], c(1, 0.5)) 


tibble(x = sample_mix, indekx = index) %>%
  ggplot(aes(x = x)) +
  geom_density()

#4
set.seed(77)
index <- sample(c(1,2),
                N,
                prob = c(0.5, 0.5),
                replace = T)
sample_mix <- rnorm(N, c(0,1)[index], c(1, 1)) 


tibble(x = sample_mix, index = factor(index)) %>%
  ggplot(aes(x = x, col = index)) +
  geom_density(lxd = 2, alpha = 0.7)


tibble(x = sample_mix, index = factor(index)) %>%
  ggplot(aes(x = x, col = index)) +
  geom_density(aes(y = 0.5 * ..density..),
               lwd = 2, 
               alpha = 0.7) +
  geom_density(aes(x = x),
               alpha = 0.5,
               col = "gray60",
               lwd = 1.4)
  




