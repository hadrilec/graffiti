library(tidyverse)
library(lubridate)
library(highcharter)
library(htmlwidgets)
library(xts)
library(ggthemes)

time_now = with_tz(now(), "Europe/Paris")
runtime_start = Sys.time()

start_day = "2019-12-31"
start_time = "23:00:00"
start_time = paste0("%5B", start_day, "T", start_time, "Z")

# end_day = "2020-06-14"
end_day = date(time_now)
# end_time = "21:59:59"
end_time = gsub(end_day, "", time_now)
end_time = gsub("\\s+", "", end_time)
end_time = paste0(end_day, "T", end_time, "Z%5D")

link = sprintf("https://parisdata.opendatasoft.com/api/records/1.0/analyze/?output_timezone=UTC&disjunctive.libelle=true&disjunctive.etat_trafic=true&disjunctive.libelle_nd_amont=true&disjunctive.libelle_nd_aval=true&rows=1000000000000000000&dataChart=eyJxdWVyaWVzIjpbeyJjaGFydHMiOlt7InR5cGUiOiJzcGxpbmUiLCJmdW5jIjoiQVZHIiwieUF4aXMiOiJxIiwic2NpZW50aWZpY0Rpc3BsYXkiOnRydWUsImNvbG9yIjoiI2ZmNWMzMyJ9XSwieEF4aXMiOiJ0XzFoIiwibWF4cG9pbnRzIjoiIiwidGltZXNjYWxlIjoiaG91ciIsInNvcnQiOiIiLCJjb25maWciOnsiZGF0YXNldCI6ImNvbXB0YWdlcy1yb3V0aWVycy1wZXJtYW5lbnRzIiwib3B0aW9ucyI6eyJkaXNqdW5jdGl2ZS5saWJlbGxlIjp0cnVlLCJkaXNqdW5jdGl2ZS5ldGF0X3RyYWZpYyI6dHJ1ZSwiZGlzanVuY3RpdmUubGliZWxsZV9uZF9hbW9udCI6dHJ1ZSwiZGlzanVuY3RpdmUubGliZWxsZV9uZF9hdmFsIjp0cnVlLCJzb3J0IjoidF8xaCIsInJvd3MiOiIxMDAwMDAwMDAwMDAwMDAwMDAwIiwicS50aW1lcmFuZ2UudF8xaCI6InRfMWg6WzIwMTktMTItMzFUMjM6MDA6MDBaIFRPIDIwMjAtMDYtMTRUMjE6NTk6NTlaXSJ9fX1dLCJkaXNwbGF5TGVnZW5kIjp0cnVlLCJhbGlnbk1vbnRoIjp0cnVlLCJ0aW1lc2NhbGUiOiIifQ%s&timezone=Europe%sFParis&dataset=comptages-routiers-permanents&x=t_1h.year&x=t_1h.month&x=t_1h.day&x=t_1h.hour&sort=x.t_1h.year,x.t_1h.month,x.t_1h.day,x.t_1h.hour&maxpoints=&y.serie1-1.expr=q&y.serie1-1.func=AVG&y.serie1-1.cumulative=false&y.serie1-2.expr=k&y.serie1-2.func=AVG&y.serie1-2.cumulative=false&q=t_1h:%s+TO+%s&lang=fr",
               "%3D%3D", "%2", start_time, end_time)

df = jsonlite::fromJSON(link, flatten = T)

data = df %>% 
  mutate(year = df$x.year) %>% 
  mutate(hour = case_when(nchar(x.hour) == 1 ~ paste0("0", x.hour),
                           TRUE ~ as.character(x.hour))) %>% 
  mutate(month = case_when(nchar(x.month) == 1 ~ paste0("0", x.month),
                           TRUE ~ as.character(x.month))) %>% 
  mutate(day = case_when(nchar(x.day) == 1 ~ paste0("0", x.day),
                           TRUE ~ as.character(x.day))) %>% 
  mutate(time = as_datetime(sprintf("%s-%s-%s %s:00:00", year, month, day, hour))) %>% 
  mutate(variable1 = `serie1-1`) %>% 
  mutate(variable2 = `serie1-2`) %>% 
  select(time, variable1, variable2) %>% 
  pivot_longer(-time, names_to = "variable", values_to = "value") %>% 
  mutate(label = case_when(variable == "variable1" ~ "Débit horaire moyen",
                           variable == "variable2" ~ "Taux d'occupation horaire moyen"))

subtt1 = paste0("Le débit qui correspond au nombre de véhicules ayant passé le point de comptage pendant un intervalle de temps fixe (une heure pour les données fournies).")
subtt2 = "Le taux d’occupation, qui correspond au temps de présence de véhicules sur la boucle en pourcentage d’un intervalle de temps fixe (une heure pour les données fournies). Ainsi, 25% de taux d’occupation sur une heure signifie que des véhicules ont été présents sur la boucle pendant 15 minutes. Le taux fournit une information sur la congestion routière."
subtt3 = sprintf("Fait le : %s, Source: Marie de Paris/open data", time_now)

# 
# GRAPHIQUE GGPLOT
# 

data_plot = data %>% 
  mutate(colour_new = case_when(time >= "2020-03-17" ~ "ok",
                                TRUE ~ "other"))

gg_route_paris =
  ggplot(data_plot, aes(x = time, y = value, colour = colour_new)) +
  facet_wrap(~label, scales = "free") +
  geom_line(show.legend = F) +
  ggtitle("Trafic routier dans Paris") +
  labs(subtitle =  sprintf("%s\n%s\n%s", subtt1, subtt2, subtt3))

# 
# GRAPHIQUE HIGHCHART (html/javascript)
# 

data_hc = data %>% 
  select(-label) %>% 
  pivot_wider(., names_from = "variable", values_from = "value") %>% 
  as.data.frame()

# file_html = file.path(Sys.getenv("USERPROFILE"), "Desktop", "hc_chart.html")

xdata = xts(data_hc[,-1], order.by = data_hc[,1])

hc_route_paris = 
  highchart(type = "stock") %>% 
  hc_title(text = "Trafic routier dans Paris en 2020") %>% 
  hc_subtitle(text = sprintf("%s\n%s\n%s", subtt1, subtt2, subtt3)) %>% 
  hc_yAxis_multiples(
    create_yaxis(2, height = c(2, 1), turnopposite = TRUE)
  ) %>%
  hc_add_series(xdata$variable1, yAxis = 0, name = "Débit horaire moyen", showInLegend = T) %>% 
  hc_add_series(xdata$variable2, yAxis = 1, name = "Taux d'occupation horaire moyen", showInLegend = T) 

runtime_end = Sys.time()

run_time = as.numeric(difftime(runtime_end, runtime_start, units = "secs"))

export_graph(plot = hc_route_paris,
             folder_name = "route_paris",
             # create_code_html = T,
             run_time = run_time,
             perim = "FR",
             update = T )


# saveWidget(hc, file = file_html)


