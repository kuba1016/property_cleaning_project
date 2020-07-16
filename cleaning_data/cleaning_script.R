#Cleaning script

library(tidyverse)

#Loding raw files.

new_homes <- read_csv("raw_data/nieruchomosci_otodom_developers.csv")
old_homes <- read_csv("raw_data/nieruchomosci_otodom.csv")

# BASIC CLEANING FOR new_homes
new_homes <- new_homes %>% 
  mutate(rooms = str_remove(rooms,"pokoje")) %>% 
  mutate(rooms = str_remove(rooms,"pokoi")) %>% 
  mutate(price = str_remove(price,"zł")) %>% 
  mutate(m2 = str_remove(m2,"m²")) %>% 
  mutate(plot_m2 = str_remove_all(plot_m2,"[A-z]+")) %>% 
  mutate(plot_m2 = str_remove_all(plot_m2,"ł" )) %>%
  mutate(plot_m2 = str_remove_all(plot_m2,"²")) %>% 
  mutate(price = str_remove_all(price, " ")) %>% 
  mutate(rooms = as.numeric(rooms)) %>% 
  mutate(price = as.numeric(price)) %>% 
  mutate(m2 = str_replace(m2,",",".")) %>% 
  mutate(m2 = as.numeric(m2)) %>% 
  mutate(plot_m2 = str_replace_all(plot_m2,fixed(" "),"")) %>% 
  mutate(plot_m2 = as.numeric(plot_m2))

# BASIC CLEANING FOR old_homes

old_homes <- old_homes %>% 
  mutate(rooms = str_remove(rooms,"pokoje")) %>% 
  mutate(rooms = str_remove(rooms,"pokoi")) %>% 
  mutate(price = str_remove(price,"zł")) %>% 
  mutate(m2 = str_remove(m2,"m²")) %>% 
  mutate(plot_m2 = str_remove_all(plot_m2,"[A-z]+")) %>% 
  mutate(plot_m2 = str_remove_all(plot_m2,"ł" )) %>%
  mutate(plot_m2 = str_remove_all(plot_m2,"²")) %>% 
  mutate(price = str_remove_all(price, " ")) %>% 
  mutate(rooms = as.numeric(rooms)) %>% 
  mutate(price = as.numeric(price)) %>% 
  mutate(m2 = str_replace(m2,",",".")) %>% 
  mutate(m2 = as.numeric(m2)) %>% 
  mutate(plot_m2 = str_replace_all(plot_m2,fixed(" "),"")) %>% 
  mutate(plot_m2 = as.numeric(plot_m2))

# Cleaning voivodeship new_homes

new_homes <- new_homes %>% 
  separate(location,into = c("town_vilage","rest_1","voivodeship"), sep = ",",remove=F) %>% 
  mutate(voivodeship = if_else(str_detect(rest_1,"kie") ,rest_1,voivodeship)) %>% 
  mutate(voivodeship = case_when(town_vilage =="Warszawa" ~ "mazowieckie",
                                 town_vilage =="Rzeszów" ~"podkarpackie",
                                 town_vilage =="Szczecin" ~"zachodniopomorskie",
                                 town_vilage =="Gdańsk" ~"pomorskie",
                                 town_vilage =="Łódź" ~"łódzkie",
                                 town_vilage =="Kraków" ~"malopolskie",
                                 town_vilage =="Wrocław" ~"dolnośląskie",
                                 town_vilage =="Białystok"~"podlaskie",
                                 town_vilage =="Lublin"~"lubelskie",
                                 town_vilage =="Częstochowa"~"śląskie",
                                 town_vilage =="Tarnowskie Góry"~"śląskie",
                                 town_vilage =="Ruda Śląska"~"śląskie",
                                 town_vilage =="Katowice"~"śląskie",
                                 town_vilage =="Gliwice"~"śląskie",
                                 town_vilage =="Leszno"~"wielkopolskie",
                                 town_vilage =="Zielona Góra"~"lubuskie",
                                 town_vilage =="Sosnowiec"~"śląskie",
                                 town_vilage =="Bielsko-Biała"~"śląskie",
                                 town_vilage =="Zabrze"~"śląskie",
                                 town_vilage =="Poznań"~"wielkopolskie",
                                 town_vilage =="Gdynia"~"pomorskie",
                                 town_vilage =="Mirków"~"dolnośląskie",
                                 town_vilage =="Jaworzno"~"śląskie",
                                 town_vilage =="Rybnik"~"śląskie",
                                 town_vilage =="Dąbrowa Górnicza"~"śląskie",
                                 town_vilage =="Bydgoszcz"~"kujawskopomorskie",
                                 town_vilage =="Żory"~"śląskie",
                                 town_vilage =="Opole"~"opolskie",
                                 town_vilage =="Tychy"~"śląskie",
                                 town_vilage =="Bytom"~"śląskie",
                                 town_vilage =="Toruń"~"kujawskopomorskie",
                                 town_vilage =="Kielce"~"świętokrzyskie",
                                 town_vilage =="Koszalin"~"zachodniopomorskie",
                                 town_vilage =="Konstancin-Jeziorna"~"mazowieckie",
                                 town_vilage =="Pruszków"~"mazowieckie",
                                 town_vilage =="Swarzędz"~"wielkopolskie",
                                 town_vilage =="Czeladź"~"śląskie",
                                 town_vilage =="Grodzisk Mazowiecki"~"mazowieckie",
                                 town_vilage =="Grudziądz"~"kujawsko-pomorskie",
                                 town_vilage =="Mysłowice"~"śląskie",
                                 town_vilage =="Jelenia Góra"~"śląskie",
                                 town_vilage =="Lubin"~"dolnośląskie",
                                 town_vilage =="Siemianowice Śląskie"~"śląskie",
                                 town_vilage =="Olsztyn"~"warmińsko-mazurskie",
                                 town_vilage =="Będzin"~"śląskie",
                                 town_vilage =="Mikołów"~"śląskie",
                                 town_vilage =="Gorzów Wielkopolski"~"lubuskie",
                                 town_vilage =="Stargard"~"zachodniopomorskie",
                                 town_vilage =="Żyrardów"~"mazowieckie",
                                 town_vilage =="Kalisz"~"wielkopolskie",
                                 town_vilage =="Łomianki"~"mazowieckie",
                                 town_vilage =="Piła"~"wielkopolskie",
                                 town_vilage =="Tarnów"~"małopolskie",
                                 town_vilage =="Włocławek"~"kujawsko-pomorskie",
                                 town_vilage =="Legionowo"~"mazowieckie",
                                 town_vilage =="Nowy Sącz"~"małopolskie",
                                 town_vilage =="Przemyśl"~"podkarpackie",
                                 town_vilage =="Chełm"~"lubelskie",
                                 town_vilage =="Jastrzębie-Zdrój"~"śląskie",
                                 town_vilage =="Marki"~"mazowieckie",
                                 town_vilage =="Pabianice"~"łódzkie",
                                 town_vilage =="Piekary Śląskie"~"śląskie",
                                 town_vilage =="Radom"~"mazowieckie",
                                 town_vilage =="Zgierz"~"łódzkie",
                                 town_vilage =="Gniezno"~"wielkopolskie",
                                 town_vilage =="Chorzów"~"śląskie",
                                 town_vilage =="Kędzierzyn-Koźle"~"opolskie",
                                 town_vilage =="Ostrów Wielkopolski"~"wielkopolskie",
                                 town_vilage =="Płock"~"mazowieckie",
                                 town_vilage =="Stalowa Wola"~"podkarpackie",
                                 town_vilage =="Ustroń"~"śląskie",
                                 town_vilage =="Wałbrzych"~"dolnośląskie",
                                 town_vilage =="Wieliczka"~"małopolskie",
                                 town_vilage =="Zakopane"~"małopolskie",
                                 town_vilage =="Piaseczno"~"mazowieckie",
                                 town_vilage =="Luboń"~"wielkopolskie",
                                 town_vilage =="Mrozów"~"dolnośląskie",
                                 town_vilage =="Jakubowice Konińskie"~"lubelskie",
                                 town_vilage =="Boguszów-Gorce"~"dolnośląskie",
                                 
                                 
                                 
                                 TRUE ~ voivodeship
  ))%>% 
  mutate(voivodeship =str_trim(voivodeship))

# Cleaning voivodeship old_homes

old_homes <- old_homes %>% 
  separate(location,into = c("town_vilage","rest_1","voivodeship"), sep = ",",remove=F) %>% 
  mutate(voivodeship = if_else(str_detect(rest_1,"kie") ,rest_1,voivodeship)) %>% 
  mutate(voivodeship = case_when(town_vilage =="Warszawa" ~ "mazowieckie",
                                 town_vilage =="Rzeszów" ~"podkarpackie",
                                 town_vilage =="Szczecin" ~"zachodniopomorskie",
                                 town_vilage =="Gdańsk" ~"pomorskie",
                                 town_vilage =="Łódź" ~"łódzkie",
                                 town_vilage =="Kraków" ~"malopolskie",
                                 town_vilage =="Wrocław" ~"dolnośląskie",
                                 town_vilage =="Białystok"~"podlaskie",
                                 town_vilage =="Lublin"~"lubelskie",
                                 town_vilage =="Częstochowa"~"śląskie",
                                 town_vilage =="Tarnowskie Góry"~"śląskie",
                                 town_vilage =="Ruda Śląska"~"śląskie",
                                 town_vilage =="Katowice"~"śląskie",
                                 town_vilage =="Gliwice"~"śląskie",
                                 town_vilage =="Leszno"~"wielkopolskie",
                                 town_vilage =="Zielona Góra"~"lubuskie",
                                 town_vilage =="Sosnowiec"~"śląskie",
                                 town_vilage =="Bielsko-Biała"~"śląskie",
                                 town_vilage =="Zabrze"~"śląskie",
                                 town_vilage =="Poznań"~"wielkopolskie",
                                 town_vilage =="Gdynia"~"pomorskie",
                                 town_vilage =="Mirków"~"dolnośląskie",
                                 town_vilage =="Jaworzno"~"śląskie",
                                 town_vilage =="Rybnik"~"śląskie",
                                 town_vilage =="Dąbrowa Górnicza"~"śląskie",
                                 town_vilage =="Bydgoszcz"~"kujawskopomorskie",
                                 town_vilage =="Żory"~"śląskie",
                                 town_vilage =="Opole"~"opolskie",
                                 town_vilage =="Tychy"~"śląskie",
                                 town_vilage =="Bytom"~"śląskie",
                                 town_vilage =="Toruń"~"kujawskopomorskie",
                                 town_vilage =="Kielce"~"świętokrzyskie",
                                 town_vilage =="Koszalin"~"zachodniopomorskie",
                                 town_vilage =="Konstancin-Jeziorna"~"mazowieckie",
                                 town_vilage =="Pruszków"~"mazowieckie",
                                 town_vilage =="Swarzędz"~"wielkopolskie",
                                 town_vilage =="Czeladź"~"śląskie",
                                 town_vilage =="Grodzisk Mazowiecki"~"mazowieckie",
                                 town_vilage =="Grudziądz"~"kujawsko-pomorskie",
                                 town_vilage =="Mysłowice"~"śląskie",
                                 town_vilage =="Jelenia Góra"~"śląskie",
                                 town_vilage =="Lubin"~"dolnośląskie",
                                 town_vilage =="Siemianowice Śląskie"~"śląskie",
                                 town_vilage =="Olsztyn"~"warmińsko-mazurskie",
                                 town_vilage =="Będzin"~"śląskie",
                                 town_vilage =="Mikołów"~"śląskie",
                                 town_vilage =="Gorzów Wielkopolski"~"lubuskie",
                                 town_vilage =="Stargard"~"zachodniopomorskie",
                                 town_vilage =="Żyrardów"~"mazowieckie",
                                 town_vilage =="Kalisz"~"wielkopolskie",
                                 town_vilage =="Łomianki"~"mazowieckie",
                                 town_vilage =="Piła"~"wielkopolskie",
                                 town_vilage =="Tarnów"~"małopolskie",
                                 town_vilage =="Włocławek"~"kujawsko-pomorskie",
                                 town_vilage =="Legionowo"~"mazowieckie",
                                 town_vilage =="Nowy Sącz"~"małopolskie",
                                 town_vilage =="Przemyśl"~"podkarpackie",
                                 town_vilage =="Chełm"~"lubelskie",
                                 town_vilage =="Jastrzębie-Zdrój"~"śląskie",
                                 town_vilage =="Marki"~"mazowieckie",
                                 town_vilage =="Pabianice"~"łódzkie",
                                 town_vilage =="Piekary Śląskie"~"śląskie",
                                 town_vilage =="Radom"~"mazowieckie",
                                 town_vilage =="Zgierz"~"łódzkie",
                                 town_vilage =="Gniezno"~"wielkopolskie",
                                 town_vilage =="Chorzów"~"śląskie",
                                 town_vilage =="Kędzierzyn-Koźle"~"opolskie",
                                 town_vilage =="Ostrów Wielkopolski"~"wielkopolskie",
                                 town_vilage =="Płock"~"mazowieckie",
                                 town_vilage =="Stalowa Wola"~"podkarpackie",
                                 town_vilage =="Ustroń"~"śląskie",
                                 town_vilage =="Wałbrzych"~"dolnośląskie",
                                 town_vilage =="Wieliczka"~"małopolskie",
                                 town_vilage =="Zakopane"~"małopolskie",
                                 town_vilage =="Piaseczno"~"mazowieckie",
                                 town_vilage =="Sopot"~"pomorskie",
                                 town_vilage =="Konin"~"wielkopolskie",
                                 town_vilage =="Józefów"~"mazowieckie",
                                 town_vilage =="Kołobrzeg"~"zachodniopomorskie",
                                 town_vilage =="Ostrowiec Świętokrzyski"~"świętokrzyskie",
                                 town_vilage =="Tomaszów Mazowiecki"~"mazowieckie",
                                 town_vilage =="Legnica"~"dolnośląskie",
                                 town_vilage =="Luboń"~"wielkopolskie",
                                 town_vilage =="Sulejówek"~"mazowieckie",
                                 town_vilage =="Inowrocław"~"kujawskopomorskie",
                                 town_vilage =="Koronowo"~"kujawskopomorskie",
                                 town_vilage =="Piotrków Trybunalski"~"łódzkie",
                                 town_vilage =="Świdnica"~"dolnośląskie",
                                 town_vilage =="Tczew"~"pomorskie",
                                 town_vilage =="Ząbki"~"mazowieckie",
                                 town_vilage =="Siedlce"~"mazowieckie",
                                 town_vilage =="Marysin"~"lubuskie",
                                 town_vilage =="Uraz"~"dolnośląskie",
                                 town_vilage =="Świdnik"~"lubelskie",
                                 town_vilage =="Świętochłowice"~"śląskie",
                                 town_vilage =="Ciechocinek"~"kujawskopomorskie",
                                 town_vilage =="Bieruń"~"śląskie",
                                 town_vilage =="Chwaszczyno"~"pomorskie",
                                 town_vilage =="Kwidzyn"~"pomorskie",
                                 town_vilage =="Boguszów-Gorce"~"dolnośląskie",
                                 TRUE ~ voivodeship
  )) %>% 
  mutate(voivodeship =str_trim(voivodeship))

# Follow-up cleaning of voivodeship column for new_homes.

new_homes <- new_homes %>% 
  
  mutate(voivodeship = recode(voivodeship,
                              "kielecki" = "świętokrzyskie",
                              "skierniewicki"="łódzkie",
                              "kujawsko-pomorskie"="kujawskopomorskie",
                              "Jagodno" = "dolnośląskie",
                              "warmińsko-mazurskie"="warmińskomazurskie",
                              "nakielski"="kujawskopomorskie",
                              "malopolskie"="małopolskie"))

# Follow-up cleaning of voivodeship column for old_homes.

old_homes <- old_homes %>% 
  
  mutate(voivodeship = recode(voivodeship,
                              "kielecki" = "świętokrzyskie",
                              "skierniewicki"="łódzkie",
                              "kujawsko-pomorskie"="kujawskopomorskie",
                              "Jagodno" = "dolnośląskie",
                              "warmińsko-mazurskie"="warmińskomazurskie",
                              "nakielski"="kujawskopomorskie",
                              "malopolskie"="małopolskie",
                              "Przedmieście Lubelskie" = "lubelskie",
                              "Ołtaszyn"= "dolnośląskie"))




#Selecting columns for the final clean files.

new_homes_clean <- new_homes %>% 
  select(-c("title","location","rest_1"))
old_homes_clean <- old_homes %>% 
  select(-c("title","location","rest_1"))



















