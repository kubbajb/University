# ZADANIE 1
library('ggplot2')
library(nortest)

# H(0): x ~ N(*)
# H(1): ~H(0)

# Różne stopnie swobody w generowanych danych

sw_df_power <- function(df, n){
  mean(
    sapply(1:1000, 
           function(i) shapiro.test(rt(n,df))$p.val < 0.05
    )
  )
}
ks_df_power <- function(df, n){
  mean(
    sapply(1:1000, 
           function(i) ks.test(rt(n,df),"pnorm")$p.val < 0.05
    )
  )
}
chi_df_power <- function(df, n){
  mean(
    sapply(1:1000, 
           function(i) pearson.test(rt(n,df))$p.val < 0.05
    )
  )
}

data <- expand.grid(df = seq(1, 50, by=2), n = seq(5, 100, by=15))

data$sw_power <- mapply(sw_df_power, data$df, data$n)
data$ks_power <- mapply(ks_df_power, data$df, data$n)
data$chi_power <- mapply(chi_df_power, data$df, data$n)

sw <- ggplot(data, aes(x = df, y = sw_power, color = as.factor(n))) +
  geom_line() +
  labs(x = "df", y = "power") +
  scale_color_manual(values = rainbow(10)) +
  labs(color = "n", title = "Moc testu Shapiro-Wilka w zależności od n i df")
 
ks <- ggplot(data, aes(x = df, y = ks_power, color = as.factor(n))) +
  geom_line() +
  labs(x = "df", y = "power") +
  scale_color_manual(values = rainbow(10)) +
  labs(color = "n", title = "Moc testu Kołmogorowa-Smirnowa w zależności od n i df")

chi <- ggplot(data, aes(x = df, y = chi_power, color = as.factor(n))) +
  geom_line() +
  labs(x = "df", y = "power") +
  scale_color_manual(values = rainbow(10))  +
  labs(color = "n", title = "Moc testu Chi-kwadrat w zależności od n i df")