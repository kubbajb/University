data("airquality")
?airquality

library(ggplot2)
library(dplyr)
library(tidyr)

glimpse(airquality)

airquality %>%
  ggplot(aes(x = Temp, y = Solar.R)) +
  geom_jitter() + 
  geom_smooth() + 
  geom_rug()

airquality %>%
  ggplot(aes(x = Ozone, y = Solar.R)) +
  geom_jitter() + 
  geom_smooth() + 
  geom_rug()

airquality %>%
  ggplot(aes(x = Wind, y = Solar.R)) +
  geom_jitter() + 
  geom_smooth() + 
  geom_rug()

airquality %>%
  ggplot(aes(x = factor(Month), y = Solar.R)) +
  geom_boxplot() + 
  geom_jitter(col = "grey30", width = .2)

model_temp <- lm(Solar.R ~ Temp, data = airquality)

summary(model_temp)

airquality %>%
  ggplot(aes(x = Temp, y = Solar.R)) +
  geom_jitter() + 
  geom_smooth(se = T) +
  geom_smooth(method = "lm", col = "red", se = F) 

model_ozone <- lm(Solar.R ~ Ozone, data = airquality)
summary(model_ozone)

airquality %>%
  ggplot(aes(x = Ozone, y = Solar.R)) +
  geom_jitter() + 
  geom_smooth(se = T) +
  geom_smooth(method = "lm", col = "red", se = F) +
  scale_x_log10()

model_logozone <- lm(Solar.R ~ log10(Ozone), data = airquality)
summary(model_logozone)

model_wind <- lm(Solar.R ~ Wind, data = airquality)
summary(model_wind)


coefs_logozone <- summary(model_logozone)$coefficients
##90% przedzia³ ufnoœci dla parametru przy log10(Ozone)
coefs_logozone[2,1] + 
  qnorm(c(.05, .95)) * coefs_logozone[2,2]
## 95%
coefs_logozone[2,1] + 
  qnorm(c(.025, .975)) * coefs_logozone[2,2]
## 99%
coefs_logozone[2,1] + 
  qnorm(c(.005, .995)) * coefs_logozone[2,2]

windows()
plot(model_logozone)

#parametr przy log10(Ozone)
par_logozone_vect <- rep(NA, 10 ^ 5)

data <- airquality %>% 
  mutate(log_ozone = log10(Ozone)) %>%
  select(Solar.R, log_ozone) %>%
  filter(!(is.na(log_ozone + Solar.R)))
glimpse(data)

set.seed(123)
for(i in 1:10^5){
  boot_sample <- sample(1:nrow(data), 
                        nrow(data), 
                        replace = T)
  model <- lm(Solar.R ~ log_ozone, 
              data = data[boot_sample, ])
  par_logozone_vect[i] <- coef(model)[2]
}

#90% bootstrapowy przedzial ufnosci 
##dla parametru przy log10(Ozone)
quantile(par_logozone_vect, c(0.05, 0.95))

#95%
quantile(par_logozone_vect, c(0.025, 0.975))

#99%
quantile(par_logozone_vect, c(0.005, 0.995))

set.seed(321)
par_logozone_df <- tibble(bootstrap = par_logozone_vect,
                          classic = 
                            coefs_logozone[2,1] + 
                            rnorm(10^) * 
                            coefs_logozone[2,2]) %>%
  gather(type, parameter)
  


par_logozone_df %>%
  ggplot(aes(x = parameter, col = type)) + 
  geom_density()
