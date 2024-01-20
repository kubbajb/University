PackageNames <- c("regclass", "tidyverse", "stargazer", "magrittr", "moments", "stringr", "dplyr",  "caret", "ggplot2", "lmtest", "corrplot")

for (i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}




data <- read.csv("miami-housing.csv")

data_num <- data %>%
  select_if(~all(!is.character(.)))

stargazer(data_num, type="text")

cor_matrix <- cor(data_num)

cor_matrix <- cor(data)
R_0 <- cor_matrix[4,-4]
R <- cor_matrix[-4,-4]

data$structure_quality

comb <- expand.grid(rep(list(c(T,F)), 16))

tot_comb <- nrow(comb)-1
comb_capacity <- comb %>% mutate(capacity = 0)

# - - - - - - - - - - - - - - - - - - - - - - - - - 


for(row in 1:tot_comb)
{
  
  k <-c(1:16)[unlist(comb[row,])]
  H <- 0
  
  for(i in k)
  {
    H <- H + (R_0[i]^2)/sum(abs(R[k,i]))
  }
  
  comb_capacity[row, 17] = H
}

# Ostateczne wyniki
x <- cor(data_num$SALE_PRC, data_num$structure_quality, method = c("pearson"))

comb_capacity <- comb_capacity %>% arrange(desc(capacity))
max_capacity <- comb_capacity[1,]
x <-c(1:16)[unlist(comb_capacity[1,1:16])]
y <- names(R_0[x])
y[4] <- "SALE_PRC"

final_model <- lm(SALE_PRC ~ .,data[y])
summary(model)

#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
  
plot(final_model)
summary(final_model)

k <- x
R <- cor(data_num[,x])
corrplot(R)

# liniowość
library(lmtest)
reset(final_model)

# składniki losowe(mnk minimalizowanie reszt)
# autokorelacja - zmienna skorelowana sama ze sobą

dwtest(final_model)

# yt = a + c*yt-1 + b*x + E
# heteroskedastyczność

bptest(final_model)
plot(final_model$fitted.values, final_model$residuals)
hist(final_model$residuals)
bptest(final_model)

# test 

# prognozowanie
prediction <- predict(final_model, newdata = data)

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
