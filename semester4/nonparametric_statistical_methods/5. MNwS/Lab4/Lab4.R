library(ggplot2)
library(CARS)

#DIAMONDS

#Zad1
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() 

#Zad2
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() + geom_smooth(method='lm', formula = y~x)

#Zad3
ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_point()

#Zad4
ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_point(aes(shape = cut)) + 
  geom_smooth(se = F)

#*
library(hexbin)
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_hex()

#*
#do mtcars <- wyznaczyć z $names, rownames <-
ggplot(mtcars, aes(x = wt, y = apg, label = name)) +
  geom_text()


#IRIS

ggplot(iris, aes(x = Petal.Width, y = Petal.Length, col = Species)) +
  geom_point()

#wprowadza nowy klucz ze wszystkich danych i dzieli na dwie kolumny
iris.tidy <- iris %>%
  gather(key, Value, - Species) %>% 
  separate(key, c("Part", "Measure"), "\\.")

ggplot(iris.tidy, aes(x = Species, y = Value, col = Part)) +
  geom_jitter() + #"
  facet_grid(. ~ Measure)


#Zad1
library(dplyr)
library(hflights)
View(hflights)

hflights.tidy <- hflights %>%
  gather(key = DelayType, value = delay, ArrDelay, DepDelay) %>%
  ggplot(hflights, aes(x = delay, y = ActualElapsedTime, shape = DelayType, col = UniqueCarrier)) +
  geom_point()







