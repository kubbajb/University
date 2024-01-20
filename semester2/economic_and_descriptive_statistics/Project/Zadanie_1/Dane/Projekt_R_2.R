# Libraries
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)
library(eurostat)
library(tibbletime)
library(plyr)
library(readxl)
library(data.table)



# Import religion data
# Source: https://correlatesofwar.org/data-sets/world-religion-data
# Direct original URL: https://correlatesofwar.org/data-sets/world-religion-data/wrp-national-data-1/@@download/file/WRP_national.csv
# Codebook and explanations: https://correlatesofwar.org/data-sets/world-religion-data/wrp-codebook/at_download/file
url_religion = 'https://correlatesofwar.org/data-sets/world-religion-data/wrp-national-data-1/@@download/file/WRP_national.csv'
download.file(url_religion, 'WRP_national.csv')
Religion = read_csv('WRP_national.csv')
write.csv(Religion, "WRP_national.csv")

# Import democracy data
# Source: https://freedomhouse.org/report/freedom-world
# Direct original URL: https://freedomhouse.org/sites/default/files/2022-02/Aggregate_Category_and_Subcategory_Scores_FIW_2003-2022.xlsx
url_democracy = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQGO3rY5a3v7ecCiDe9HXYLCKdQgDE_zipk5zz9OrqsfE4Yf1vYwT-WkNVKdrZGYIVf982I_Mcvmbj1/pub?output=csv'
download.file(url_democracy, 'Aggregate_Category_and_Subcategory_Scores_FIW_2003-2022.csv')
Democracy = read_csv('Aggregate_Category_and_Subcategory_Scores_FIW_2003-2022.csv')
write.csv(Democracy, 'Aggregate_Category_and_Subcategory_Scores_FIW_2003-2022.csv')



# Aggregate religion data
reqd <- as.vector(c("chrstgen", "judgen", "islmgen", "budgen", "zorogen", "hindgen", "sikhgen", "shntgen", "bahgen", "taogen", "jaingen", "confgen", "syncgen", "nonrelig", "othrgen"))
religion_aggregated_data <- Religion[,reqd]
religion_aggregated_data <- religion_aggregated_data %>% mutate(dom_relig = names(.)[max.col(.)])

reqd <- as.vector(c("year", "name", "pop", "chrstgen", "judgen", "islmgen", "budgen", "zorogen", "hindgen", "sikhgen", "shntgen", "bahgen", "taogen", "jaingen", "confgen", "syncgen", "nonrelig", "othrgen", "othrgenpct", "nonreligpct", "sumreligpct", "total"))
religion_aggregated_names <- Religion[,reqd]
religion_aggregated <- cbind(religion_aggregated_names, "dom_relig" = religion_aggregated_data$dom_relig)

religion_aggregated$dom_relig <- religion_aggregated$dom_relig %>% 
  ifelse(. == "chrstgen", "Chrzescijanstwo", .) %>%
  ifelse(. == "judgen", "Judaizm", .) %>% 
  ifelse(. == "islmgen", "Islam", .) %>% 
  ifelse(. == "budgen", "Buddyzm", .) %>% 
  ifelse(. == "zorogen", "Zoroastrianizm", .) %>% 
  ifelse(. == "hindgen", "Hinduizm", .) %>% 
  ifelse(. == "sikhgen", "Sikhizm", .) %>% 
  ifelse(. == "shntgen", "Shintoizm", .) %>% 
  ifelse(. == "bahgen", "Bahaizm", .) %>% 
  ifelse(. == "taogen", "Taoizm", .) %>% 
  ifelse(. == "jaingen", "Dzinizm", .) %>% 
  ifelse(. == "confgen", "Konfucjanizm", .) %>% 
  ifelse(. == "syncgen", "Synkretyzm", .) %>% 
  ifelse(. == "nonrelig", "Ateizm", .) %>% 
  ifelse(. == "othrgen", "Inne", .)

# Remove redundant data
rm(religion_aggregated_data, religion_aggregated_names)

# Aggregate and order democracy data
democracy_aggregated <- Democracy
colnames(democracy_aggregated)[3] = 'name'
colnames(democracy_aggregated)[5] = 'year'

democracy_aggregated$Region <- democracy_aggregated$Region %>% 
  ifelse(. == "Africa", "Afryka", .) %>%
  ifelse(. == "Americas", "Ameryki", .) %>% 
  ifelse(. == "Asia", "Azja", .) %>% 
  ifelse(. == "Eurasia", "Eurazja", .) %>% 
  ifelse(. == "Europe", "Europa", .) %>% 
  ifelse(. == "Middle East", "Bliski Wschód", .)

# Combine democracy and religion data into one table
religion_by_democracy_data <- merge(religion_aggregated, democracy_aggregated)
religion_by_democracy_data$nonreligious <- (religion_by_democracy_data$sumreligpct-religion_by_democracy_data$nonreligpct)
religion_by_democracy_data$nonreligious <- religion_by_democracy_data$nonreligious %>% 
  ifelse(. > 1, 1, .)

religion_by_democracy_data_avg_region <- aggregate(religion_by_democracy_data$PR, list(religion_by_democracy_data$Region), mean)
religion_by_freedom_data_avg_region <- aggregate(religion_by_democracy_data$CL, list(religion_by_democracy_data$Region), mean)
world_religious_makup <- aggregate(religion_by_democracy_data_long$population, list(religion_by_democracy_data_long$religion), sum)
religion_by_democracy_data_avg_relig <- aggregate(religion_by_democracy_data$PR, list(religion_by_democracy_data$dom_relig), mean)
religion_by_freedom_data_avg_relig <- aggregate(religion_by_democracy_data$CL, list(religion_by_democracy_data$dom_relig), mean)

religion_by_democracy_data_long <- gather(religion_by_democracy_data, religion, population, chrstgen:othrgen, factor_key=TRUE)


# Plots
political_rights_rating_plot <- ggplot(religion_by_democracy_data, aes(x = PR, y = nonreligious**10, colour = dom_relig, delta = Region)) + geom_point(stat = 'identity') + labs(title = 'Religijnosc a swobody polityczne', x = 'Laczny wynik dla kategorii swobody polityczne', y = 'Odsetek populacji deklarujacy religijnosc (10 potega)', colour = "Religia", shape = "Region") + scale_color_viridis(discrete=TRUE) + theme_bw()
civil_liberties_rating_plot <- ggplot(religion_by_democracy_data, aes(x = CL, y = nonreligious**10, colour = dom_relig, delta = Region)) + geom_point(stat = 'identity') + labs(title = 'Religijnosc a swobody obywatelskie', x = 'Laczny wynik dla kategorii swobody obywatelskie', y = 'Odsetek populacji deklarujacy religijnosc (10 potega)', colour = "Religia", shape = "Region") + scale_color_viridis(discrete=TRUE) + theme_bw()
total_freedom_rating_plot <- ggplot(religion_by_democracy_data, aes(x = Total, y = nonreligious**10, colour = dom_relig, delta = Region)) + geom_point(stat = 'identity') + labs(title = 'Religijnosc a swobody polityczne i wyborcze', x = 'Laczny wynik dla wszystkich kategorii', y = 'Odsetek populacji deklarujacy religijnosc (10 potega)', colour = "Religia", shape = "Region") + scale_color_viridis(discrete=TRUE) + theme_bw()

political_rights_rating__avg_region <- ggplot(religion_by_democracy_data_avg_region, aes(x = Group.1, y = x)) + geom_bar(stat="identity", fill="#5ba7b3") + labs(title = 'Swobody polityczne wedlug regionu', x = 'Region', y = 'Sredni wynik dla kategorii Swobody polityczne') + theme_bw()
civil_liberties_rating__avg_region <- ggplot(religion_by_freedom_data_avg_region, aes(x = Group.1, y = x)) + geom_bar(stat="identity", fill="#5ba7b3") + labs(title = 'Swobody obywatelskie wedlug regionu', x = 'Region', y = 'Sredni wynik dla kategorii Swobody obywatelskie') + theme_bw()
political_rights_rating__avg_religion <- ggplot(religion_by_democracy_data_avg_relig, aes(x = Group.1, y = x)) + geom_bar(stat="identity", fill="#5ba7b3") + labs(title = 'Swobody polityczne wedlug wyznania', x = 'Wyznanie', y = 'Sredni wynik dla kategorii Swobody polityczne') + theme_bw()
civil_liberties_rating__avg_religion <- ggplot(religion_by_freedom_data_avg_relig, aes(x = Group.1, y = x)) + geom_bar(stat="identity", fill="#5ba7b3") + labs(title = 'Swobody obywatelskie wedlug wyznania', x = 'Wyznanie', y = 'Sredni wynik dla kategorii Swobody obywatelskie') + theme_bw()

world_religious_makup_plot <- ggplot(world_religious_makup, aes(x = x, y = Group.1, fill = x)) + geom_bar(stat="identity", fill="#5ba7b3")
world_religious_structure_plot <- ggplot(religion_by_democracy_data_long, aes(x = reorder(Region, population), y = population, fill = religion)) + geom_col(position = "fill") + labs(title = 'Struktura religijna wedlug regionu', x = 'Region', y = '', colour = "Religia", shape = "Region")
world_religion_total_plot <- ggplot(religion_by_democracy_data, aes(y = reorder(name, Total), x = Total, , fill = dom_relig)) + geom_col() + theme_bw() + labs(title = 'Laczny wynik dla wszystkich kategorii wedlug panstwa', x = 'Wynik', y = '', colour = "Religia", shape = "Region")

plot(political_rights_rating_plot)
plot(civil_liberties_rating_plot)
plot(political_rights_rating__avg_region)
plot(civil_liberties_rating__avg_region)
plot(political_rights_rating__avg_religion)
plot(civil_liberties_rating__avg_religion)
plot(total_freedom_rating_plot)
plot(world_religious_structure_plot)
plot(world_religious_makup_plot)
png("my_plot.png",
    width = 600, height = 900,
    bg = "white")
plot(world_religion_total_plot)
dev.off()