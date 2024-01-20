# - - - - - - - - - - - - - - - - Biblioteki - - - - - - - - - - - - - - - - - - 

library(tidyverse)
library(corrplot)
library(lmtest)
library(ggplot2)
library(car)

# - - - - - - - - - - - - - - - - Import danych - - - - - - - - - - - - - - - - - - 

data <- read.csv("rawdata/countries of the world.csv", dec = ",")

# Transformacja i podgląd danych
data %>% group_by(Region) %>% 
  summarize(mean_GDP = mean(GDP)) %>% 
  arrange(mean_GDP)

view(data)
summarize(data)

# Oczyszczanie danych: usuwanie pustych miejsc/spacji
sata <- data %>% mutate(Region = str_trim(Region),
                        Country = str_trim(Country))

# Oczyszczanie danych: usuwanie braków w danych
data <- data[complete.cases(data), ]

# Modele ekonometryczne:
#    y₁ = a₀ + a₁*x₁ + ɛ
#    y₂= a₀ + a₁*x₁ + b + ɛ
#    y₂ - y₁ = b

# Budowa pierwszego modelu:

model <- lm(GDP ~ Migration, data)
summary(model)

# - - - - - - - - - - - - - Wyjaśnienie wyników - - - - - - - - - - - - - - - - - 

# Residuals (reszty, e): 
# The section summarizes the residuals, the error between the prediction 
# of the model and the actual results.  Smaller residuals are better.

# Coefficients: For each variable and the intercept, a weight is produced 
# and that weight has other # attributes like the standard error, a t-test 
# value and significance.

# Estimate: This is the weight given to the variable.  
# In the simple regression case (one variable plus the intercept), for every
# one dollar increase in Spend, the model predicts an increase of $10.6222.

# Std. Error: Tells you how precisely was the estimate measured.  
# It’s really only useful for calculating the t-value.

# t-value and Pr(>[t]): 
# The t-value is calculated by taking the coefficient divided by the Std. Error.  
# It is then used to test whether or not the coefficient is significantly different 
# from zero. If it isn’t significant, then the coefficient really isn’t adding 
# anything to the model and could be dropped or investigated further. 
# Pr(>|t|) is the significance level.

# Performance Measures: Three sets of measurements are provided.

# Residual Standard Error: This is the standard deviation of the residuals. 
# Smaller is better.

# Multiple / Adjusted R-Square: For one variable, the distinction doesn’t really matter. 
# R-squared shows the amount of variance explained by the model. Adjusted R-Square takes
# into account the number of variables and is most useful for multiple-regression.

# F-Statistic: The F-test checks if at least one variable’s weight is significantly 
# different than zero. This is a global test to help asses a model.  If the p-value
# is not significant (e.g. greater than 0.05) than your model is essentially not 
# doing anything.

# - - - - - - - - - - - - - - - Koincydencja - - - - - - - - - - - - - - - - - 

cor(data$GDP, data$Migration)^2

model1 <- lm(GDP ~ Migration + Pop.Density + Literacy, data)
summary(model1)

# Kryterium odrzucenia: 
# Wartość t-value powinna być jak najbliższa 1, a p-value powinno być jak najbliższe 0.

data_num <- data[,3:20]
model2 <- lm(GDP ~ . -Literacy - Area - Population -Coastline - Birthrate - Climate, data_num)
summary(model2)

# Metoda Hellwiga
# Liczba kombinacji: 2^(liczba objaśniających minus objaśniana) - 1
# Liczba kombinacji: 2^17 - 

# - - - - - - - - - - - Macierz współczynników korelacji - - - - - - - - - - - 

cor_matrix <- cor(data_num)
R_0 <- cor_matrix[7,-7]
R <- cor_matrix[-7,-7]

comb <- expand.grid(rep(list(c(T,F)), 17))
  
tot_comb <- nrow(comb)-1
comb_capacity <- comb %>% mutate(capacity = 0)

#  - - - Obliczenie wskaźnika pojemności informacji dla każdej kombinacji - - -

for(row in 1:tot_comb)
{
  
  k <-c(1:17)[unlist(comb[row,])]
  H <- 0
  
  for(i in k)
  {
    H <- H + (R_0[i]^2)/sum(abs(R[k,i]))
  }
  
  comb_capacity[row, 18] = H
}

# Ostateczne wyniki

comb_capacity <- comb_capacity %>% arrange(desc(capacity))
max_capacity <- comb_capacity[1,]
x <-c(1:17)[unlist(comb_capacity[1,1:17])]

# - - - - - - - - - - - - - - - - DIAGNOSTYKA - - - - - - - - - - - - - - - - 
# x powinny być niezalezne

final_model <- lm(GDP ~ Phones, data)

plot(final_model)
plot(data$GDP, data$Phones)

# współliniowość
# x1 = 2*x2 - 5


data2 <- data_num %>% mutate(Migration2 = 2*Migration -5)
model <- lm(GDP ~ . -Industry -Area -Literacy -Population 
            -Coastline -Birthrate -Climate -Other -Agriculture, data[,-1])

summary(model)
vif(model)
k <- c(3,5,6,9:12,15,16,18)
R <- cor(data_num[,k])
corrplot(R)

summary(model)

library(car)
vif(model)

# liniowość
library(lmtest)
reset(model)

plot(data$Infant.Mortality, log(data$GDP))


# składniki losowe(mnk minimalizowanie reszt)
# autokorelacja - zmienna skorelowana sama ze sobą

dwtest(model)

# yt = a + c*yt-1 + b*x + E
# heteroskedastyczność

bptest(model)
plot(model$fitted.values, model$residuals)

shapiro.test(model$residuals) #gdy p value <5% odrzucamy
hist(model$residuals)


bptest(model)
plot(model$fitted.values, model$residuals)

# test normalności reszt (Shapiro-Wilk)

shapiro.test(model$residuals) #gdy p value <5% odrzucamy
hist(model$residuals)


# prognozowanie
prediction <- predict(model, newdata = data.frame())

set.seed(100)
train <- data %>% slice_sample(prop = 0.8)

test <- data %>% filter(!(Country %in% train$Country))

model_train <- lm(log(GDP) ~ . -Industry -Area -Literacy -Population 
            -Coastline -Birthrate -Climate -Other -Agriculture, train[,-1])

prediction <- predict(model, train)
error <- prediction[1] - data$GDP

MAE <- mean(abs(error))
mean(test$GDP)

# Wyestymować model dla train
# Zrobić prognozę na dane testowe
# Obliczyć błędy prognozy

# error = rzeczywista - prognoza
# MAE = mean(abs(error))
# RMSE = sqrt(mean(error^2))
# MAPE = mean(abs(error/rzeczywista))