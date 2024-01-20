library(stringr)
library(tidyr)
library(dplyr)
library(hflights)
library(ggplot2)
library(Lahman)

bmi_long <- bmi %>% 
  gather(year, bmi, -Country) %>% 
  mutate(year = year %>% 
           str_extract("([0-9]{4})") %>% 
           as.numeric())

head(bmi_long)

bmi_wide <- bmi_long %>%
  mutate(year = paste0("Y", year)) %>%
  spread(year, bmi)
head(bmi_wide)

students_clean <- students %>%
  separate(Grades, 
           paste0("exam", 1:3), 
           convert = TRUE)
str(students_clean)

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
          na_dist = mean(is.na(Distance)))


hflights %>% 
  filter(Distance / (AirTime / 60) > 300) %>%
  summarise(n_flights = n(),
            n_dest = n_distinct(Dest),
            min_dist = min(Distance),
            max_dist = max(Distance))

hflights %>%
  filter(Distance > 1000) %>%
  group_by(UniqueCarrier) %>%
  summarise(n_long_flights = n()) %>%
  arrange(desc(n_long_flights))

hflights %>%
  group_by(UniqueCarrier, Dest) %>%
  summarise(n_flights = n()) %>%
  arrange(UniqueCarrier, desc(n_flights)) %>% 
  summarise(most_frequent = first(Dest),
            n_flights = first(n_flights))

Master_no_money <- 
  Master %>% anti_join(Salaries)

Master_no_money %>%
  left_join(Appearances) %>%
  group_by(playerID) %>%
  summarise(all_app = sum(G_all, na.rm = T)) %>%
  summarise(n_no_app = sum(all_app == 0, na.rm = T),
          max_app = max(all_app, na.rm = T),
          avg_app = mean(all_app, na.rm = T))

Salaries %>% 
  left_join(HallOfFame %>% 
              arrange(playerID, inducted) %>%
              group_by(playerID) %>%
              summarise(inducted = last(inducted))
            ) %>%
  mutate(HoF = recode(inducted, Y = "awarded", N = "nominated"),
         HoF = as.character(HoF),
         HoF = ifelse(is.na(inducted), "none", HoF)
  ) %>%
  group_by(HoF) %>%
  filter(!is.na(salary)) %>%
  summarise(avg_salary = mean(salary),
            Q1_salary = quantile(salary, .25),
            median_salary = median(salary),
            Q3_salary = quantile(salary, .75))
