library(tidyverse)
library(lubridate)
library(highcharter)
library(htmlwidgets)
library(xts)
library(ggthemes)

time_now = with_tz(now(), "Europe/Paris")
runtime_start = Sys.time()

# source
# https://opendata.paris.fr/explore/dataset/comptage-velo-donnees-compteurs/information/?disjunctive.id_compteur&disjunctive.nom_compteur&disjunctive.id&disjunctive.name

link = "https://opendata.paris.fr/api/records/1.0/analyze/?output_timezone=UTC&disjunctive.id_compteur=true&disjunctive.nom_compteur=true&disjunctive.id=true&disjunctive.name=true&timezone=Europe%2FParis&dataset=comptage-velo-donnees-compteurs&x=date.year&x=date.month&x=date.day&x=date.hour&sort=x.date.year,x.date.month,x.date.day,x.date.hour&maxpoints=&y.serie1-1.expr=sum_counts&y.serie1-1.func=AVG&y.serie1-1.cumulative=false&lang=fr"

df = jsonlite::fromJSON(link, flatten = T)

data = df %>% 
  mutate(year = x.year) %>% 
  mutate(hour = case_when(nchar(x.hour) == 1 ~ paste0("0", x.hour),
                           TRUE ~ as.character(x.hour))) %>% 
  mutate(month = case_when(nchar(x.month) == 1 ~ paste0("0", x.month),
                           TRUE ~ as.character(x.month))) %>% 
  mutate(day = case_when(nchar(x.day) == 1 ~ paste0("0", x.day),
                           TRUE ~ as.character(x.day))) %>% 
  mutate(time = as_datetime(sprintf("%s-%s-%s %s:00:00", year, month, day, hour))) %>% 
  mutate(variable1 = `serie1-1`) %>% 
  select(time, variable1) %>% 
  pivot_longer(-time, names_to = "variable", values_to = "value") %>% 
  mutate(label = case_when(variable == "variable1" ~ "Moyenne horaire du comptage des vélos"))

subtt3 = sprintf("Fait le : %s, Source: Marie de Paris/open data", time_now)

# 
# GRAPHIQUE GGPLOT
# 

data_plot = data

gg_velo_paris =
  ggplot(data_plot, aes(x = time, y = value)) +
  facet_wrap(~label, scales = "free") +
  geom_line(show.legend = F) +
  ggtitle("Trafic cycliste dans Paris") +
  labs(subtitle = sprintf("%s", subtt3) )

# 
# GRAPHIQUE HIGHCHART (html/javascript)
# 

data_hc = data %>% 
  select(-label) %>% 
  pivot_wider(., names_from = "variable", values_from = "value") %>% 
  as.data.frame()

# file_html = file.path(Sys.getenv("USERPROFILE"), "Desktop", "hc_chart_velo.html")

xdata = xts(data_hc[,-1], order.by = data_hc[,1])
names(xdata) = "variable1"

hc_velo_paris = 
  highchart(type = "stock") %>% 
  hc_title(text = "Trafic cycliste dans Paris") %>% 
  hc_subtitle(text = sprintf("%s", subtt3)) %>% 
  hc_add_series(xdata$variable1, name = "Débit horaire moyen", showInLegend = T) 

runtime_end = Sys.time()

run_time = as.numeric(difftime(runtime_end, runtime_start, units = "secs"))

export_graph(plot = hc_velo_paris,
             folder_name = "velo_paris",
             create_code_html = T,
             run_time = run_time,
             perim = "FR",
             update = T )

# saveWidget(hc_velo_paris, file = file_html)


