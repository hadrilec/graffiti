
library(eurostat)
library(tidyverse)
library(lubridate)

time_start = Sys.time()

countries_selected = c("EU28", "EA19", "FR", "DE", "IT", "ES", "UK", "NL", "PL")

# chargement des données du chomage depuis eurostat
df = get_eurostat("tps00203",
                  filters = list(geo = countries_selected,
                                 sex = "T",
                                 age = "Y15-74",
                                 unit = "PC_ACT"))

# la fonction label_eurostat peut être utilisée pour obtenir des descriptions de variables

# valeurs disponibles pour chaque colonne
list_age = df %>% distinct(age)
list_sex = df %>% distinct(sex)
list_unit = df %>% distinct(unit)

# selection des données
# les filtres ci-dessous ne sont nécessaires que si la table initiale n'est pas filtrée
# cad si df = get_eurostat("tps00203")

data = df %>% 
  filter(age == "Y15-74") %>% #population agée de 12 à 64 ans
  filter(sex == "T") %>% #hommes et femmes
  filter(unit == "PC_ACT") %>% 	
  filter(geo %in% countries_selected) %>% #sélection des pays/zones
  filter(time >= "2002-01-01") #sélection de la période

time_now = with_tz(now(), "Europe/Paris")
sous_titre = sprintf("Source : Eurostat, Fait le : %s \nDernier point : %s", time_now, max(data$time))
titre = "Taux de chômage en Europe"

gg_pop = 
ggplot(data, aes(x = time, y = values)) + 
  facet_wrap(~geo, scales = "free") +
  geom_line() + 
  # geom_point() + 
  ggtitle(titre) +
  labs(subtitle = sous_titre) 

time_end = Sys.time()
run_time = round(difftime(time_end, time_start, units = "secs"))

export_minio_graph(gg_pop,
                   folder_name = "chomage_europe",
                   # create_code_html =  TRUE,
                   perim = "ZE", update = T,
                   run_time = run_time)








