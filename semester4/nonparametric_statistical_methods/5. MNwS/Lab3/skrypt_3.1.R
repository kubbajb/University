library(dplyr)
library(ggplot2)
fun_name <- "mean"

my_mean <- mean

eval(parse(text = "my_mean <- mean"))
my_mean(c(1,4,11))

eval(parse(text = paste("my_mean <- ", fun_name)))
my_mean(c(1,4,11))

family <- "exp"
eval(parse(text = paste0("my_rfam <- r", family)))
set.seed(1)
my_rfam(3, 1)

#generujemy obserwacje z mieszanki rozk³adow normalnych
mu <- c(0, 4)
s <- c(1,2)
p <- 0.5
N <- 10 ^ 4

set.seed(111)
index <- sample(c(1,2), N, prob = c(p, 1-p),replace = T)
head(index)

sample_mix <- rnorm(N, mu[index], s[index])

tibble(x = sample_mix, index = factor(index)) %>% 
  ggplot(aes(x = x)) +
  geom_density(aes(x = x, 
                   col = index, 
                   y = .5 * ..density.. ), 
               lwd = 2, 
               alpha = .5) + 
  geom_density(aes(y = ..density.. ),
               alpha = .5, 
               lwd = 1.5, 
               col = "gray50") +
  geom_histogram(aes(y = ..density..), 
                 alpha = .5)

