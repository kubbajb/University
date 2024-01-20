library(readr)
library(tidyr)
library(dplyr)

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






