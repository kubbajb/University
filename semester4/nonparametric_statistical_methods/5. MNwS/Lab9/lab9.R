#zajęcia 25.05.18
library(hflights)
View(hflights)

#Zad 1.
#iris.tidy <- iris %>%
#  gather(key, Value, - Species) %>%
#  separate(key, c("Part", "Measure"), "\\.")

hf <- hflights[c(7, 11:13, 16)] %>%
  gather(key, Value, -UniqueCarrier, -AirTime, -Distance) %>%
  separate(key, c("type"))

hf %>%
  ggplot(aes(x=AirTime, y=Value, shape=type, size=Distance, col=UniqueCarrier)) +
  geom_point()
  

#Zad 3.
hflights %>%
  gather(TaxiType, TaxiTime, TaxiIn, TaxiOut) %>%
  unite(date, Year:DayofMonth, sep="-") %>%
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x=date, y=TaxiTime)) +
  geom_point(aes(shape=TaxiType, size=Distance)) +
  geom_smooth(aes(col=UniqueCarrier), se=F)

tf <- hflights[c(7, 117, 18)]