
library(eia)
library(tidyverse)
library(ggplot2)
library(lubridate)

time_start = Sys.time()

monthweeks <- function(x) {
  UseMethod("monthweeks")
}
monthweeks.Date <- function(x) {
  ceiling(as.numeric(format(x, "%d")) / 7)
}

group_mean_years = 2011:2014
# group_mean_years2 = 2016:2018
group_mean_years2 = 0

# 
# TROUVER UNE SERIE PARMI CELLES DISPONIBLES
# 

# series = eia_cats()
# series_oil = eia_child_cats(714755)
# series_oil_stocks = eia_child_cats(714802)
# series_oil_stocks_type = eia_child_cats(388426)
# series_oil_stocks_type2 = eia_child_cats(395534)
# series_oil_stocks_type2_tot = eia_child_cats(395535)
# series_oil_stocks_type2_tot_product = eia_child_cats(395885)
# series_crude_oil_stock = eia_child_cats(395886)
# 
# series_tot_stock = eia_child_cats(387912)
# series_tot_stock_product = eia_child_cats(388153)

# PET.MCESTUS1.M
# U.S. Ending Stocks excluding SPR of Crude Oil, Monthly

# U.S. Ending Stocks excluding SPR of Crude Oil, Weekly
# PET.WCESTUS1.W

month_list = gsub("\\\\.","", month(1:12, label = T))
week_list = unlist(lapply(month_list, function(x){paste(x, 1:5)}))

# 
# TELECHARGEMENT ET TRAITEMENY DES DONNEES ####
# 

PET.WCESTUS1.W = eia_series("PET.WCESTUS1.W")

PET.WCESTUS1.W_data = data.frame(PET.WCESTUS1.W$data, stringsAsFactors = F) %>% 
  mutate(date_ = as.Date(paste0("2020-",week, "-1"), "%Y-%U-%u")) %>% 
  filter(year >= 2011) %>% 
  mutate(year_ = case_when(year %in% group_mean_years ~
                             sprintf("%s-%s", min(group_mean_years), max(group_mean_years)),
                           year %in% group_mean_years2 ~
                             sprintf("%s-%s", min(group_mean_years2), max(group_mean_years2)),
                           TRUE ~ as.character(year))) %>% 
  mutate(year_ = factor(year_)) %>% 
  group_by(year_, date_, week) %>% 
  summarise(mean_ = mean(value, na.rm = TRUE) / 1000) %>% 
  mutate(month_week = monthweeks(date_)) %>% 
  mutate(month_ = gsub("\\\\.", "", month(date_, label = T))) %>% 
  mutate(date2 = paste(month_, month_week)) %>% 
  mutate(date2 = factor(date2, levels = week_list)) %>% 
  drop_na()
  
# weeks = seq(from = 1, to = 60, by = 4)

xaxis_breaks = PET.WCESTUS1.W_data %>% 
  filter(month_week %in% 1) %>% 
  drop_na() %>% 
  pull(date2) %>% 
  unique()

# 
# CREATION DU GRAPHIQUE ####
# 

last_value = PET.WCESTUS1.W_data %>% 
  ungroup() %>% 
  mutate(year_ = as.numeric(as.character(year_))) %>% 
  filter(year_ == max(year_, na.rm = T)) %>% 
  filter(week == max(week)) %>% 
  as.data.frame()

time_now = with_tz(now(), "Europe/Paris")

subtitle_unit = "Unité : millions de barils de pétrole"
subtitle_source = "Source: Energy Information Agency EIA"
substitle_last_value = sprintf("Dernier point : %s semaine %s, stock : %s\n Fait le :%s",
                               last_value[1,"month_"],
                               last_value[1,"month_week"],
                               round(last_value[1,"mean_"]),
                               time_now)

gg_us_stock_oil_weekly =
ggplot(PET.WCESTUS1.W_data, aes(x = date2, y = mean_, colour = year_, group = year_)) +
  geom_line(size = 1) +
  ggthemes::theme_stata() +
  ggtitle("Stocks commerciaux de pétrole brut aux Etats-Unis") +
  labs(subtitle = sprintf("%s\n%s\n%s", subtitle_unit, subtitle_source, substitle_last_value)) +
  scale_y_continuous(sec.axis = dup_axis()) +
  scale_x_discrete(breaks = xaxis_breaks, expand = c(0.01, 0.01)) +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 


time_end = Sys.time()
run_time = round(difftime(time_end, time_start, units = "secs"))


export_graph(
  gg_us_stock_oil_weekly,
  perim = "OIL",
  folder_name = "us_stock_oil_weekly",
  update = TRUE,
  # create_code_html = TRUE,
  run_time = run_time
)


