# LIBRARIES --------------------------------------------------------------------

PackageNames <- c("downloader", "readxl", "tidyverse","dplyr","data.table","tidyverse","stringr","data.table","plyr","fuzzySim", "leaps")

for (i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

options(max.print=10000)
Sys.setenv(LANG = "en")
setwd("D:/Projekty/MacroeconomicIndicatorsAndElections")

# DICTIONARY -------------------------------------------------------------------

powiaty_dict <- read.csv("PKW_GUS_Dict.csv")

# PARLAMENT 2001 - POBIERANIE DANYCH -------------------------------------------

dir.create("DanePKW/Parlament2001")
parlament_2001_url <- "https://danewyborcze.kbw.gov.pl/dane/2001/sejm/sejm2001-lis-pow.xls"
parlament_2001_path <- "DanePKW/Parlament2001"
parlament_2001_dest <- "DanePKW/Parlament2001/2001-parlament-pow-listy.xls"
download(parlament_2001_url, destfile = parlament_2001_dest, mode = "wb") 

# PARLAMENT 2001 - CZYSZCZENIE I ŁĄCZENIE DANYCH -------------------------------

okregi_parlament_2001 <- list.files(path = parlament_2001_path, pattern = "*.xls", full.names = T)
parlament_2001 <- do.call("rbind",lapply(okregi_parlament_2001,FUN = function(files){ read_excel(files)}))
parlament_2001 <- parlament_2001 %>%
  dplyr::select(Powiat, `Ważne`, `L. upr.`, TERYT) %>%
  dplyr::rename(powiat = Powiat,
                glosy = `Ważne`,
                uprawnieni = `L. upr.`,
                kod.TERYT = TERYT) %>%
  mutate(frekwencja = glosy/uprawnieni,
         wybory = 'parlament_2001',
         kod.TERYT = as.integer(kod.TERYT)) %>%
  left_join(dplyr::select(powiaty_dict, c(kod.TERYT,wojewodztwo)), by = 'kod.TERYT') %>%
  filter(!is.na(wojewodztwo))

# PARLAMENT 2005 - POBIERANIE DANYCH -------------------------------------------

dir.create("DanePKW/Parlament2005")
parlament_2005_url <- "https://danewyborcze.kbw.gov.pl/dane/2005/sejm/1456225675_36797.xls"
parlament_2005_path <- "DanePKW/Parlament2005"
parlament_2005_dest <- "DanePKW/Parlament2005/2005-parlament-pow-listy.xls"
download(parlament_2005_url, destfile = parlament_2005_dest, mode = "wb") 

# PARLAMENT 2005 - CZYSZCZENIE I ŁĄCZENIE DANYCH -------------------------------

okregi_parlament_2005 <- list.files(path = parlament_2005_path, pattern = "*.xls", full.names = T)
parlament_2005 <- do.call("rbind",lapply(okregi_parlament_2005,FUN = function(files){ read_excel(files)}))
parlament_2005 <- parlament_2005 %>%
  dplyr::select(Powiat, `Głosy ważne`, `Uprawnieni do głosowania`, TERYT) %>%
  dplyr::rename(powiat = Powiat,
                glosy = `Głosy ważne`,
                uprawnieni = `Uprawnieni do głosowania`,
                kod.TERYT = TERYT) %>%
  mutate(frekwencja = glosy/uprawnieni,
         wybory = 'parlament_2005',
         kod.TERYT = as.integer(kod.TERYT)) %>%
  left_join(dplyr::select(powiaty_dict, c(kod.TERYT,wojewodztwo)), by = 'kod.TERYT') %>%
  filter(!is.na(wojewodztwo))

# PARLAMENT 2007 - POBIERANIE DANYCH -------------------------------------------

dir.create("DanePKW/Parlament2007")
parlament_2007_url <- "https://danewyborcze.kbw.gov.pl/dane/2007/sejm/sejm2007-pow-listy.xls"
parlament_2007_path <- "DanePKW/Parlament2007"
parlament_2007_dest <- "DanePKW/Parlament2007/2007-parlament-pow-listy.xls"
download(parlament_2007_url, destfile = parlament_2007_dest, mode = "wb") 

# PARLAMENT 2007 - CZYSZCZENIE I ŁĄCZENIE DANYCH -------------------------------

okregi_parlament_2007 <- list.files(path = parlament_2007_path, pattern = "*.xls", full.names = T)
parlament_2007 <- do.call("rbind",lapply(okregi_parlament_2007,FUN = function(files){ read_excel(files)}))
parlament_2007 <- parlament_2007 %>%
  dplyr::select(Powiat, `Ważne`, `Upr.`, `Kod pow.`) %>%
  dplyr::rename(powiat = Powiat,
                glosy = `Ważne`,
                uprawnieni = `Upr.`,
                kod.TERYT = `Kod pow.`) %>%
  mutate(frekwencja = glosy/uprawnieni,
         wybory = 'parlament_2007',
         kod.TERYT = as.integer(kod.TERYT)) %>%
  left_join(dplyr::select(powiaty_dict, c(kod.TERYT,wojewodztwo)), by = 'kod.TERYT') %>%
  filter(!is.na(wojewodztwo))

# PARLAMENT 2011 - POBIERANIE DANYCH -------------------------------------------

dir.create("DanePKW/Parlament2011")
parlament_2011_url <- "https://danewyborcze.kbw.gov.pl/dane/2011/sejmsenat/2011-sejm-pow-listy.xls"
parlament_2011_path <- "DanePKW/Parlament2011"
parlament_2011_dest <- "DanePKW/Parlament2011/2011-parlament-pow-listy.xls"
download(parlament_2011_url, destfile = parlament_2011_dest, mode = "wb") 

# PARLAMENT 2011 - CZYSZCZENIE I ŁĄCZENIE DANYCH -------------------------------

okregi_parlament_2011 <- list.files(path = parlament_2011_path, pattern = "*.xls", full.names = T)
parlament_2011 <- do.call("rbind",lapply(okregi_parlament_2011,FUN = function(files){ read_excel(files)}))
parlament_2011 <- parlament_2011 %>%
  dplyr::select(Powiat, `Głosy ważne`, `Liczba uprawnionych do głosowania`, TERYT) %>%
  dplyr::rename(powiat = Powiat,
                glosy = `Głosy ważne`,
                uprawnieni = `Liczba uprawnionych do głosowania`,
                kod.TERYT = TERYT) %>%
  mutate(frekwencja = glosy/uprawnieni,
         wybory = 'parlament_2011',
         kod.TERYT = as.integer(kod.TERYT)) %>%
  left_join(dplyr::select(powiaty_dict, c(kod.TERYT,wojewodztwo)), by = 'kod.TERYT') %>%
  filter(!is.na(wojewodztwo))

# PARLAMENT 2015 - POBIERANIE DANYCH -------------------------------------------

dir.create("DanePKW/Parlament2015")
parlament_2015_url <- "https://danewyborcze.kbw.gov.pl/dane/2015/sejm/2015-gl-lis-pow.zip"
parlament_2015_path <- "DanePKW/Parlament2015/2015-gl-lis-pow.zip"
parlament_2015_dest <- "DanePKW/Parlament2015"
download(parlament_2015_url, destfile = parlament_2015_path, mode = "wb") 
unzip(parlament_2015_path, exdir = parlament_2015_dest)
file.remove(parlament_2015_path)

# PARLAMENT 2015 - CZYSZCZENIE I ŁĄCZENIE DANYCH -------------------------------

okregi_parlament_2015 <- list.files(path = parlament_2015_dest, pattern = "*.xls", full.names = T)
parlament_2015 <- do.call("rbind",lapply(okregi_parlament_2015,FUN = function(files){ read_excel(files)}))
parlament_2015 <- parlament_2015 %>%
  dplyr::select(Powiat, `Głosy ważne`, `Liczba wyborców`, TERYT) %>%
  dplyr::rename(powiat = Powiat,
                glosy = `Głosy ważne`,
                uprawnieni = `Liczba wyborców`,
                kod.TERYT = TERYT) %>%
  mutate(frekwencja = glosy/uprawnieni,
         wybory = 'parlament_2015',
         kod.TERYT = as.integer(paste(kod.TERYT, "00", sep = ""))) %>%
  left_join(dplyr::select(powiaty_dict, c(kod.TERYT,wojewodztwo)), by = 'kod.TERYT') %>%
  filter(!is.na(wojewodztwo))

# PARLAMENT 2019 - POBIERANIE DANYCH -------------------------------------------

dir.create("DanePKW/Parlament2019")
parlament_2019_url <- "https://danewyborcze.kbw.gov.pl/dane/2019/sejmsenat/wyniki_gl_na_kand_po_powiatach_sejm_csv.zip"
parlament_2019_path <- "DanePKW/Parlament2019/wyniki_gl_na_kand_po_powiatach_parlament_csv.zip"
parlament_2019_dest <- "DanePKW/Parlament2019"
download(parlament_2019_url, destfile = parlament_2019_path, mode = "wb") 
unzip(parlament_2019_path, exdir = parlament_2019_dest)
file.remove(parlament_2019_path)

# PARLAMENT 2019 - CZYSZCZENIE I ŁĄCZENIE DANYCH -------------------------------

okregi_parlament_2019 <- list.files(path = parlament_2019_dest, pattern = "*.csv", full.names = T)
parlament_2019 <- do.call("rbind",lapply(okregi_parlament_2019,FUN = function(files){ read.csv(files, sep = ';')[,0:25]}))
parlament_2019 <- parlament_2019 %>%
  dplyr::select(Powiat, Województwo, Liczba.kart.ważnych, Liczba.wyborców.uprawnionych.do.głosowania, `Kod.TERYT`) %>%
  dplyr::rename(glosy = Liczba.kart.ważnych,
                uprawnieni = Liczba.wyborców.uprawnionych.do.głosowania,
                wojewodztwo = Województwo,
                powiat = Powiat,
                kod.TERYT = Kod.TERYT) %>%
  mutate(frekwencja = glosy/uprawnieni,
         wybory = 'parlament_2019') %>%
  filter(!is.null(wojewodztwo),
         wojewodztwo != "")

# PARLAMENT 2023 - POBIERANIE DANYCH -------------------------------------------

dir.create("DanePKW/Parlament2023")
parlament_2023_url <- "https://danewyborcze.kbw.gov.pl/dane/2023/sejmsenat/wyniki_gl_na_listy_po_powiatach_sejm_csv.zip"
parlament_2023_path <- "DanePKW/Parlament2023/wyniki_gl_na_kand_po_powiatach_parlament_csv.zip"
parlament_2023_dest <- "DanePKW/Parlament2023/"
download(parlament_2023_url, destfile = parlament_2023_path, mode = "wb") 
unzip(parlament_2023_path, exdir = parlament_2023_dest)
file.remove(parlament_2023_path)

# PARLAMENT 2023 - CZYSZCZENIE I ŁĄCZENIE DANYCH -------------------------------

plik_parlament_2023 <- list.files(path = parlament_2023_dest, pattern = "*.csv", full.names = T)[1]
parlament_2023 <- do.call("rbind",lapply(plik_parlament_2023,FUN = function(files){ read.csv(files, sep = ';')}))
parlament_2023 <- parlament_2023 %>%
  dplyr::select(Powiat, Województwo, Powiat, Województwo, Liczba.kart.ważnych, Liczba.wyborców.uprawnionych.do.głosowania, TERYT.Powiatu) %>%
  dplyr::rename(glosy = Liczba.kart.ważnych,
                uprawnieni = Liczba.wyborców.uprawnionych.do.głosowania,
                kod.TERYT = TERYT.Powiatu,
                wojewodztwo = Województwo,
                powiat = Powiat) %>%
  mutate(frekwencja = glosy/uprawnieni,
         wybory = 'parlament_2023') %>%
  filter(!is.null(wojewodztwo),
         wojewodztwo != "")

# ZAPIS  -----------------------------------------------------------------------

frekwencja_wyborcza <- rbindlist(list(parlament_2001, parlament_2005, parlament_2007, parlament_2011, parlament_2015, parlament_2019, parlament_2023), use.names = TRUE, fill = TRUE)
write.csv(frekwencja_wyborcza, "frekwencja_wyborcza_parlament_2001-2023.csv", row.names = FALSE)
