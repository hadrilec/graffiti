
library(rdbnomics)
library(rsdmx)
library(ggthemes)
library(grid)
library(lubridate)
library(eurostat)
library(fredr)
library(dplyr)
library(ggplot2)
library(tidyverse)

start_time = today() %m-% months(12)
runtime_start = Sys.time()

date = gsub("-","",Sys.Date())

# Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma (DCOILWTICO)
oil_us = fredr(series_id = "DCOILWTICO")

# Crude Oil Prices: Brent - Europe (DCOILBRENTEU)
oil_eu = fredr(series_id = "DCOILBRENTEU")


oil = rbind(oil_us, oil_eu)

oil = oil %>%
  mutate(name = case_when(series_id == "DCOILWTICO" ~ "WTI Texas",
                          series_id == "DCOILBRENTEU" ~ "Brent Europe")) %>% 
  mutate(name1 = name) %>% 
  # mutate(name = paste(name, "- dollars")) %>% 
  dplyr::filter(date >= "2001-01-01") %>% 
  mutate(currency = "dollar")

link = 'https://api.db.nomics.world/v22/series/BDF/EXR/EXR.D.USD.EUR.SP00.A?observations=1'
eur_usd <- rdbnomics::rdb_by_api_link(link) %>%
  mutate(type = "Dollar américain", label = "DOLLAR / EURO") %>%
  drop_na() %>% 
  mutate(eur_dollar = 1/value) %>% 
  dplyr::select(period, eur_dollar) %>% 
  dplyr::rename(date = period)

oil_eur = oil %>% 
  left_join(eur_usd) %>% 
  mutate(value_eur = value * eur_dollar) %>% 
  mutate(name1 = name) %>% 
  mutate(name = paste(name, "- euros")) %>% 
  dplyr::select(date, series_id, value_eur, name, name1) %>% 
  dplyr::rename(value = value_eur) %>% 
  dplyr::mutate(currency = "euro")

oil_ = oil %>% bind_rows(oil_eur)
  
max_oil_us = oil_us %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month)) %>%
  mutate(value = round(value,1))

max_oil_eu = oil_eu %>%
  arrange(desc(date)) %>%
  slice(1:3) %>%
  mutate(month = month(date, label = T, abbr = T)) %>%
  mutate(month = gsub("\\\\.","", month)) %>%
  mutate(value = round(value,1))

year_value = year(today())
last_month_value = month(today() %m-% months(1))

avg_last_month = 
  oil_ %>% 
  mutate(month_ = month(date)) %>% 
  mutate(month_abb = month(date, label = TRUE)) %>% 
  mutate(year_ = year(date)) %>% 
  filter(year_ == year_value & month_ %in% c(last_month_value, last_month_value-1) ) %>% 
  filter(str_detect(name, "Brent")) %>% 
  group_by(name, month_, year_, month_abb, currency) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  mutate(month_label = gsub("\\\\.", "", month_abb))

year_current = avg_last_month %>% 
  pull(year_) %>% unique()

month_current = avg_last_month %>% 
  pull(month_label) %>% unique()

euro_brent_last_month = avg_last_month %>% 
  filter(currency == "euro") %>% 
  pull(value) %>% 
  round(1)

dollar_brent_last_month = avg_last_month %>% 
  filter(currency == "dollar") %>% 
  pull(value) %>% 
  round(1)

last_month_value_brent = sprintf("Brent %s %s : %s$ %s€",
                                 year_current, month_current,
                                 dollar_brent_last_month, euro_brent_last_month)

subtitle_day = lubridate::day(max(oil$date))
subtitle_month = gsub("\\\\.","",
                      lubridate::month(max(oil$date), label = TRUE))
subtitle_year = lubridate::year(max(oil$date)) 
xaxis_breaks = seq.Date(from = min(oil$date), to = max(oil$date), by = "1 months")

graph_subtitle_bond = sprintf("Texas: %s, Brent: %s, Unité : $/baril \n source : Agence américaine d'information sur l'énergie EIA",
                               max_oil_us[1,"value"],
                               max_oil_eu[1,"value"])

time_now = with_tz(now(), "Europe/Paris")

graph_subtitle = sprintf("Dernier point : %s %s %s, Fait le : %s \n %s\n%s",
                         subtitle_day, subtitle_month, subtitle_year, time_now,
                         graph_subtitle_bond, last_month_value_brent)
coeff = 10
yaxis_breaks = seq(from = floor(min(oil$value, na.rm = TRUE) / coeff) * coeff,
                   to = ceiling(max(oil$value, na.rm = TRUE)/ coeff) * coeff, by = coeff)
# scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks, labels = function(x) paste0(x, "%")) +


oil_2 = oil_ %>%
  filter(date > "2020-01-01") %>%
  # filter(str_detect(name, "Brent")) %>% 
  drop_na() %>% 
  filter(value != 0)

graph_oil = 
  ggplot(data = oil_2, aes(x = date, y = value, colour = name )) +
  geom_line(size = 1) +
  facet_grid(name1~currency) +
  ggtitle("Prix du pétrole") +
  labs(subtitle = graph_subtitle) + 
  scale_y_continuous(sec.axis = dup_axis(), breaks = yaxis_breaks) +
  scale_x_date(breaks = xaxis_breaks, date_labels = "%b %y", expand = c(0.01, 0.01)) +
  theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 

runtime_end = Sys.time()
run_time = as.numeric(difftime(runtime_end, runtime_start), units = "secs")


export_graph(graph_oil, perim = "OIL", run_time = run_time,
             # create_code_html = TRUE,
             folder_name = "OIL_price", update = F)

