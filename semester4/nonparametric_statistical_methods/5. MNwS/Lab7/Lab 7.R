
library(dplyr)
library(tidyr)
library(ggplot2)

View(airquality)

airquality %>%
  ggplot(aes(x = Wind, y = Solar.R)) +
  geom_jitter() +
  geom_smooth() +
  geom_rug()

airquality %>%
  ggplot(aes(x = factor(Month), y = Solar.R)) +
  geom_boxplot() +
  geom_jitter(col = "grey30", width = .2)

#temperatura 
model_temp <- lm(Solar.R ~ Temp, data = airquality)
summary(model_temp)

airquality %>%
  ggplot(aes(x = Temp, y = Solar.R)) +
  geom_jitter() +
  geom_smooth(se = T) +
  geom_smooth(method = "lm", col = "red", se = F) +
  scale_x_log10()

#ozon
model_ozone <- lm(Solar.R ~ Ozone, data = airquality)
summary(model_ozone)

airquality %>%
  ggplot(aes(x = Ozone, y = Solar.R)) +
  geom_jitter() +
  geom_smooth(se = T) +
  geom_smooth(method = "lm", col = "red", se = F) +
  scale_x_log10()

#ozon log
model_logozone <- lm(Solar.R ~ log10(Ozone), data = airquality)
summary(model_logozone)

confs_logozone <- summary(model_logozone)#$######
  
#95%
confs_logozone[2,1] +
  qnorm(c(.005, .975)) * confs_logozone[2,2]

#99%
confs_logozone[2,1] +
  qnorm(c(.005, .995)) * confs_logozone[2,2]

windows()
plot(model_logozone)

#parametr przy log10(Ozone)
par_logozone_vect <- rep(NA, 10 ^ 5)

data <- airquality %>%
  mutate(log_ozone = log10(Ozone))
#....


#próba bootstrapowa




