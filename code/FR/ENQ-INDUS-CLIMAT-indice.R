library(tidyverse)
library(insee)
library(lubridate)

# rechercher le dataset >> ENQ-CONJ-ACT-IND
insee_dataset = get_dataset_list()

# debut de la recherche de l'idbank
idbank_list = 
  get_idbank_list() %>% 
  filter(nomflow == "ENQ-CONJ-ACT-IND") %>% 
  add_insee_title(lang = "fr")

# on ajoute les filtres au fur et a mesure
idbank_list = 
  get_idbank_list() %>% 
  # dataset selection
  filter(nomflow == "ENQ-CONJ-ACT-IND") %>% 
  add_insee_title(lang = "fr") %>% 
  filter(dim8 == "CVS") %>% 
  filter(dim4 == "A10-CZ") %>% 
  filter(dim5 == "INDICE")

# extraction de la liste des idbanks
idbank_list_selected = idbank_list %>% pull(idbank)
  
# obtention des données
data = 
  get_insee_idbank(idbank_list_selected) %>%
  # on divise le titre en morceau
  split_title(lang = "fr")

gg_enq_indus =
  ggplot(data, aes(x = DATE, y = OBS_VALUE, colour = TITLE_FR3)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~TITLE_FR3, scales = "free") +
  ggtitle("Enquêtes dans l'industrie") +
  labs(subtitle = sprintf("Fait le : %s", now()))

export_minio_graph(gg_enq_indus, perim = "FR-ENQ-CONJ-ACT-IND",
                   folder_name = "enq_indus_indice")


