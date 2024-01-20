Projekt_ESI <- expand.grid(gatunek = c("buk", "jodla","swierk", "sosna", "dab", "brzoza"),
                           gleba = c("bielicowa", "plowa", "brunatna", "czarnoziem"),
                           jakosc_powietrza = c(1, 2, 3),
                           temperatura = c(1, 2, 3),
                           nawodnienie = c(1, 2, 3),
                           wyksztalcenie_siewki = "")

# gleba = bielicowa, plowa, brunatna, czarnoziem
# jakosc_powietrza = 3-dobra, 2-srednia, 1-zla
# temperatura = 3-wysoka, 2-srednia, 1-niska
# nawodnienie = 3-silne, 2-srednie, 1-slabe
# wyksztalcenie_siewki = 1 lub 0

write.csv(Projekt_ESI, "D:/Dokumenty/Studia/Semestr_4/elementy_sztucznej_inteligencji/Projekt/projekt_ESI_new.csv")