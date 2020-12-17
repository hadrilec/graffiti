
library(saqgetr)
library(tidyverse)
library(lubridate)
library(zoo)

time1 = Sys.time() 

# DATA SOURCE : AIR BASE
# https://www.eea.europa.eu/data-and-maps/data/airbase-the-european-air-quality-database-7

# Import site information
data_sites <- get_saq_sites()

data_sites_fr = data_sites %>%
  filter(date_end >= as.Date("2020-03-16")) %>%
  filter(country_iso_code == "FR") %>%  
  filter(str_detect(eoi_code, "^FR04")) 

data_sites_metada = data_sites %>%
  select(site, site_name, country_iso_code, latitude, longitude, site_area, site_type) 

data_sites_city_id = data_sites_fr %>% pull(site)

  data_all <- get_saq_observations(
    site = data_sites_city_id, 
    start = 2020,
    verbose = TRUE
  )
  
  data_no2 = data_all %>% 
    filter(variable == "no2") %>% 
    left_join(data_sites_metada, by = "site") %>% 
    filter(date >= as.Date("2019-01-01"))
  
date_confinement_ = as.POSIXct(c("2020-03-17 12:00:00",
                               "2020-05-11 12:00:00"))

date_confinement_ = as.Date(c("2020-03-17",
                                 "2020-05-11"))

list_site_selected = data_no2 %>% distinct(site_name)

data_plot = data_no2 %>% 
  filter(date >= as.POSIXct("2019-12-01 00:00:00")) %>% 
  mutate(jour = lubridate::date(date)) %>% 
  filter(site_area != "rural_regional") %>% 
  filter(str_detect(site_name, "PARIS|peripherique|Elysee|Saint-De|Opéra")) %>% 
  group_by(jour, site_name) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  arrange(site_name) %>% 
  group_by(site_name) %>% 
  mutate(rollmean = rollmean(value, 7, na.pad = TRUE, align = "center")) %>% 
  filter(jour >= as.Date("2020-01-01")) %>%
  select(value, rollmean, site_name, jour) %>% 
  pivot_longer(-c("jour", "site_name"), names_to = "variable", values_to = "value") %>% 
  mutate(variable_label = case_when(variable == "value" ~ "Moyenne journalière",
                                    variable == "rollmean" ~ "Moyenne mobile centrée sur 7 jours")) %>% 
  mutate(variable_label = factor(variable_label,
                                 levels = c("Moyenne mobile centrée sur 7 jours", "Moyenne journalière")))

last_values_date = data_plot %>% 
  ungroup() %>% 
  filter(jour == max(jour)) %>% 
  pull(jour) %>% 
  max()

time_now = with_tz(now(), "Europe/Paris")
comment_ = "les lignes verticales correspondent à la période de confinement"

titre = "Pollution à Paris, concentration de NO2 dans l'air"
sous_titre = sprintf("Source: EEA Agence Européenne de l'environnement/AirParif \nDernier point : %s, Fait le : %s,  Unité :%s\n%s",
                     last_values_date,  time_now, "µg/m^3", comment_)

gg_pollution_idf =
ggplot(data_plot, aes(x = jour, y = value,
                      colour = variable_label, linetype = variable_label)) +
  facet_wrap(~site_name, scales = "free_y") +
  geom_line(size = 0.8) +
  geom_vline(xintercept = date_confinement_, colour = "black", linetype = "dashed") +
  ggtitle(titre) +
  labs(subtitle = sous_titre) +
  ggthemes::theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8,
                                face = "bold", hjust = 0.5, size = 16),
    axis.text.x  = element_text(angle = 0),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    text = element_text(size = 12),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 

# gg_pollution_idf

time2 = Sys.time()
run_time = as.numeric(difftime(time2, time1, units = "secs"))

export_minio_graph(gg_pollution_idf,
             update = TRUE,
             run_time = run_time,
             # create_code_html = T,
             perim = "FR",
             folder_name = "pollution_idf")
