library(ggplot2)
library(dplyr)

data <- read.csv("results_3.csv")

data <- data %>%
  mutate(start_a = as.factor(start_a), 
         start_T = as.factor(start_T)) %>%
  filter(calc_time <= 40)

# Distance

ggplot(data, aes(x = start_a, y = final_distance, fill = start_a)) + 
  geom_boxplot(alpha = 1) +
  theme(legend.position = "none")

ggplot(data, aes(x = start_T, y = final_distance, fill = start_T)) + 
  geom_boxplot(alpha = 1) +
  theme(legend.position = "none")

# Time

ggplot(data, aes(x = start_a, y = calc_time, fill = start_a)) + 
  geom_boxplot(alpha = 1) +
  theme(legend.position = "none")

ggplot(data, aes(x = start_T, y = calc_time, fill = start_T)) + 
  geom_boxplot(alpha = 1) +
  theme(legend.position = "none")

