library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

#zajecia 06.04.2018r.

bmi <- read_csv("C:/Users/Artur/Dropbox/AGH/msad/data/bmi_clean.csv")

bmi_long <- gather(bmi, year, bmi, - Country)
glimpse(bmi_long)

bmi_wide <- spread(bmi_long, year, bmi)
glimpse(bmi_wide)

students <- read_csv("C:/Users/Artur/Dropbox/AGH/msad/data/students_with_dates.csv")

students_clean <- separate(students, 
                           Grades, 
                           c("Grade_1", "Grade_2", "Grade_3"),
                           convert = T)
glimpse(students_clean)


#zajecia 13.04.18r.

library(hflights)
View(hflights)

#------------
hflights_1 <- 
  select(hflights, Year:ArrTime, - DayOfWeek)

hflights_2 <- 
  filter(hflights, 
         ArrDelay > 60, 
         DepDelay > 60)

summarise(hflights,
          min_dist = min(Distance),
          max_dist = max(Distance),
          avg_dist = mean(Distance),
          median_dist = median(Distance),
          na_taxi = mean(is.na(TaxiIn)))
#-----------


#zad1
hflights %>%
  filter((Distance/(AirTime/60)) > 300) %>%
  summarize(n_flights = n(),
            n_unique_dest = n_distinct(Dest),
            min_dist = min(Distance),
            max_dist = max(Distance))

#zad2
hflights %>%
  filter(Distance > 1000) %>%
  group_by(UniqueCarrier) %>%
  summarise(n_long_flights = n()) %>%
  arrange(desc(n_long_flights))  #sortowanie
  
#zad3
hflights %>%
  group_by(UniqueCarrier, Dest) %>%
  summarise(n_flights = n()) %>%
  arrange(UniqueCarrier, desc(n_flights)) %>%
  summarise(most_frequent = first(Dest),
            n_flights = first(n_flights))



#zajecia 20.04.18r.

load(file.choose())
View(my_db[1])


#przy³aczenie postów do uzytkowników - left join
left_join(users, posts, by = c("id" = "user_id"))

#zad1
install.packages("Lahman")
library(Lahman)

View(Master)
View(Salaries)

tescik <- left_join(Master, Salaries, by = c("playerID" = "playerID"))

df1 <- anti_join(Master, Salaries, by = c("playerID" = "playerID"))
View(df1)

#alternatywnie
master_no_money <- Master %>% anti_join(Salaries)

#zad2

View(Appearances)

df1 %>% full_join(Appearances, by = c("playerID" = "playerID")) %>%
  full_join(comments, by = c("id.y" = "post_id")) %>% 
  group_by(playerID) %>%
  summarise(n_comments = sum(!is.na(message))) %>%
  arrange(desc(n_comments))

master_no_money %>%
  left_join(Appearances) %>%
  group_by(playerID) %>%
  summarise(all_app = sum(G_all, na.rm = T)) %>%
  summarise(n_no_app = sum(all_app == 0, na.rm = T),
            max_app = max(all_app, na.rm = T),
            avg_app = mean(all_app, na.rm = T))

#zad3


