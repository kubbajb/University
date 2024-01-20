library(ggplot2)
library(dplyr)

data <- read.csv("results_29_genetic_algorithm.csv")

data <- data %>%
  rename(break_parameter = combination_parameter)

data1 <- data %>%
  select(population_size, break_parameter, mutation_parameter, final_distance, calc_time) %>%
  group_by(population_size, break_parameter, mutation_parameter) %>%
  summarize(avg_final_distance = mean(final_distance),
            min_final_distance = min(final_distance),
            avg_calc_time = mean(calc_time))

data <- data %>%
  mutate(population_size = as.factor(population_size), 
         break_parameter = as.factor(break_parameter), 
         mutation_parameter = as.factor(mutation_parameter))

x <- lm(min_final_distance ~ population_size + break_parameter + mutation_parameter, data = data1)

# Distance

ggplot(data, aes(x = population_size, y = final_distance, fill = population_size)) + 
  geom_boxplot(alpha = 1) +
  theme(legend.position = "none")

ggplot(data, aes(x = break_parameter, y = final_distance, fill = break_parameter)) + 
  geom_boxplot(alpha = 1) +
  theme(legend.position = "none")

ggplot(data, aes(x = mutation_parameter, y = final_distance, fill = mutation_parameter)) + 
  geom_boxplot(alpha = 1) +
  theme(legend.position = "none")

# Time

ggplot(data, aes(x = population_size, y = calc_time, fill = population_size)) + 
  geom_boxplot(alpha = 1) +
  theme(legend.position = "none")

ggplot(data, aes(x = break_parameter, y = calc_time, fill = break_parameter)) + 
  geom_boxplot(alpha = 1) +
  theme(legend.position = "none")

ggplot(data, aes(x = mutation_parameter, y = calc_time, fill = mutation_parameter)) + 
  geom_boxplot(alpha = 1) +
  theme(legend.position = "none")

