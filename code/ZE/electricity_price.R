
# library(tidyverse)
library(dplyr)
library(stringr)
library(xml2)
library(rvest)
library(XML)
library(purrr)
library(lubridate)
library(ggplot2)

time1 = Sys.time()

# 
# REQUETE SUR ECO2MIX POUR OBTENIR LES CHIFFRES :
# https://www.rte-france.com/en/eco2mix/donnees-de-marche-en

link_data = "./data/resultats/ZE"
dir.create(file.path(link_data, "RTE_price"))
dir.create(file.path(link_data, "RTE_price", "data"))

link_envir_RTE_price_ = file.path(link_data, "RTE_price", "data")

# link_envir_RTE_price_ = file.path("D:/XLAPDO", "RTE_price")

get_RTE_price = function(target_date_start,
                         target_date_end,
                         link_envir_RTE_price = link_envir_RTE_price_ ){
  
  
  target_date = target_date_start
  year = substr(target_date, 1, 4)
  month = substr(target_date, 6, 7)
  day = substr(target_date, 9, 10)
  target_date2 = paste(day, month, year, sep = "/")
  
  year_end = substr(target_date_end, 1, 4)
  month_end = substr(target_date_end, 6, 7)
  day_end = substr(target_date_end, 9, 10)
  target_date_end2 = paste(day_end, month_end, year_end, sep = "/")
  
  # url = "https://www.rte-france.com/getEco2MixXml.php?type=donneesMarche&&dateDeb=05/02/2020&dateFin=05/02/2020&mode=NORM"
  
  url = sprintf("https://www.rte-france.com/getEco2MixXml.php?type=donneesMarche&&dateDeb=%s&dateFin=%s&mode=NORM",
                target_date2, target_date_end2)
  
  price_data_file = file.path(link_envir_RTE_price, sprintf("RTE_price_%s_%s.xml", target_date, target_date_end))
  if(!file.exists(price_data_file)){
    download.file(url, price_data_file, mode = "wb")
  }
  
  print(price_data_file)
  # df = xml2::read_xml(t_file)
  df = try(xml2::read_xml(price_data_file), silent = TRUE)
  
  list_df = list()
  
  i = 6
  data = try(xml2::xml_child(df, i), silent = T)
  data = xml2::as_list(data)
  
  while(class(data) != "try-error"){
    
    data = try(xml_child(df, i), silent = T)
    
    if(class(data) != "try-error"){
      
      date_found = gsub('"', "", gsub("<donneesMarche date=|>", "", format(data)))
      data = as_list(data)
      
      # data = xml_child(df, 6)
      # data = as_list(data)
      
      list_id_areas = which(names(data) == "type")
      
      for(id in list_id_areas){
        perim = attr(data[[id]], "perimetre")
        n = length(data[[id]])
        for(hour in 1:n){
          values = data.frame(perim = perim,
                              value = as.numeric(data[[id]][[hour]][[1]]),
                              date = date_found,
                              hour = attr(data[[id]][[hour]], "periode"))
          
          list_df[[length(list_df)+1]] = values
        } 
      }
    }
    i = i + 1
  }
  
  RTE_price = bind_rows(list_df)
  return(RTE_price)
}
today_date = today()
last_year_date = as.Date(gsub(year(today_date), year(today_date)-1, today_date))

RTE_price_mars19 = get_RTE_price(as.Date("2019-03-01"), as.Date("2019-03-31"))
RTE_price_avril19 = get_RTE_price(as.Date("2019-04-01"), as.Date("2019-04-30"))
# RTE_price_mai19 = get_RTE_price(as.Date("2019-05-01"), as.Date("2019-05-31"))
RTE_price_today19 = get_RTE_price(as.Date("2019-05-01"), last_year_date)


RTE_price_mars20 = get_RTE_price(as.Date("2020-03-01"), as.Date("2020-03-31"))
RTE_price_avril20 = get_RTE_price(as.Date("2020-04-01"), as.Date("2020-04-30"))
# RTE_price_mai20 = get_RTE_price(as.Date("2020-05-01"), as.Date("2020-05-31"))
RTE_price_today20 = get_RTE_price(as.Date("2020-05-01"), today_date)


RTE_price = bind_rows(RTE_price_mars19, RTE_price_mars20,
                      RTE_price_avril19, RTE_price_avril20,
                      RTE_price_today19, RTE_price_today20)

RTE_price_ = RTE_price %>% 
  mutate(hour_ = case_when(nchar(hour) == 1 ~ paste0("0", hour), TRUE ~ as.character(hour))) %>% 
  mutate(day = day(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(year_ = as.character(year)) %>% 
  mutate(month = month(date)) %>% 
  mutate(day_ = case_when(nchar(day) == 1 ~ paste0("0", day), TRUE ~ as.character(day))) %>%
  mutate(month_ = case_when(nchar(month) == 1 ~ paste0("0", month), TRUE ~ as.character(month))) %>%
  mutate(time = as_datetime(paste(date, paste0(hour_, ":00:00")))) %>% 
  mutate(time_ = as_datetime(paste(sprintf("2020-%s-%s", month_, day_), paste0(hour_, ":00:00")))) %>% 
  filter(time_ < today()) %>%
  filter(!perim %in% c("DE", "CH")) %>% 
  mutate(perim = replace(perim, perim == "DL", "DE"))

confin_dates = data.frame(perim = c("FR", "GB", "IT", 
                                    # "IT",
                                    "ES", "BE", "DE"),
                          time_ = c(as_datetime("2020-03-17 12:00:00"),
                                    as_datetime("2020-03-23 12:00:00"),
                                    as_datetime("2020-03-09 20:00:00"), #all country
                                    # as_datetime("2020-03-08 20:00:00"), #northern part
                                    as_datetime("2020-03-14 08:00:00"),
                                    as_datetime("2020-03-18 12:00:00"),
                                    as_datetime("2020-03-22 12:00:00")
                                    ))

time_now = with_tz(now(), "Europe/Paris")

last_point_date = RTE_price_ %>% pull(time) %>% max()
last_point = sprintf("Dernier point : %s\nFait le : %s", last_point_date, time_now)
comment = "les lignes verticales correspondent à la date de début de confinement sur l'ensemble du territoire"

gg_RTE_price = 
ggplot(RTE_price_, aes(x = time_, y = value, colour = year_)) +
  geom_line(size = 1) +
  geom_vline(data = confin_dates, aes(xintercept = time_), linetype = "dashed") +
  facet_wrap(~perim) +
  ggtitle("Prix spot de l'éctricité en euros du Mwh sur le marché EPEX") +
  labs(subtitle = sprintf("Source : RTE, données disponibles heure par heure\n %s\n%s", comment, last_point)) +
  ggthemes::theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) 

time2 = Sys.time()
run_time = as.numeric(difftime(time2, time1, units = "secs"))

export_graph(gg_RTE_price, perim = "ZE",
             # create_code_html = T,
             run_time = run_time,
             folder_name = "RTE_price", update = F)







