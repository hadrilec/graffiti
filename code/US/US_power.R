time_start = Sys.time()

electr_ = eia_child_cats(2122628)
series_california = eia_cats(3389936)

list_perim = c("US48", "CAL", "NY")
list_perim_label = c("Etats-Unis", "Californie", "New York")
list_lockdown_dates = c(as_datetime("2020-03-20 21:00:00"),
                        as_datetime("2020-03-19 21:00:00"),
                        as_datetime("2020-03-20 21:00:00"))

list_utc_adj = c(5, 7, 5)

# time_now = Sys.time()
time_now = with_tz(now(), "Europe/Paris")
  
source_ = paste("Source : EIA, ", sprintf("Fait le : %s", time_now))

list_data = list()

for(perim in list_perim){
  print(perim)
  perim_id = which(list_perim[] == perim)
  
  perim_label = list_perim_label[perim_id]
  intercept_time = list_lockdown_dates[perim_id]
  utc_adj = list_utc_adj[perim_id]
  
  subtt_perim = sprintf("Début du confinement : %s\n%s",
                        intercept_time, source_)
  
  series = sprintf("EBA.%s-ALL.D.H", perim)
  
  EBA.US48_ALL.D.H = eia_series(series)
  
  EBA.US48_ALL.D.H_data = EBA.US48_ALL.D.H %>%
    pull(data) %>% 
    as.data.frame()
  
  today_week = week(today())
  first_week = today_week - 11
  
  EBA.US48_ALL.D.H_data4 = EBA.US48_ALL.D.H_data %>% 
    mutate(date = date %m-% hours(utc_adj)) %>% 
    mutate(date_ = as.Date(date, "%Y-%m-%d")) %>% 
    mutate(weekend = wday(date_)) %>% 
    mutate(weekend = case_when(weekend %in% c(1,7) ~ TRUE,
                               TRUE ~ FALSE)) %>% 
    mutate(hour = hour(date)) %>% 
    mutate(value_no2020 = case_when(year == 2020 ~ NA_real_, 
                                    TRUE ~ value)) %>% 
    group_by(hour, week, weekend) %>% 
    mutate(mean_hour_week = mean(value_no2020, na.rm = T),
           min_hour_week = min(value_no2020, na.rm = T),
           max_hour_week = max(value_no2020, na.rm = T)) %>% 
    ungroup() %>% 
    select(value, mean_hour_week, date, week, hour, weekend) %>% #min_hour_week, max_hour_week,
    dplyr::rename(demand = value) %>% 
    pivot_longer(-c("date", "week", "hour", "weekend"), values_to = "value", names_to = "label") %>% 
    filter(week >= first_week & week <= today_week) %>% 
    mutate(year = year(date)) %>% 
    filter(year == 2020) %>% 
    mutate(label_ = case_when(label == "demand" ~ "Demande 2020",
                              label == "mean_hour_week" ~ "Demande moyenne 2015-2019")) %>% 
    mutate(week_label = factor(paste("semaine", week), levels = paste("semaine", 1:55))) %>% 
    mutate(perim = perim_label)
  
  list_data[[length(list_data)+1]] = EBA.US48_ALL.D.H_data4
  
  gg_focus = 
  ggplot(EBA.US48_ALL.D.H_data4, aes(x = date, y = value, colour = label_)) +
    facet_wrap(~week_label, scales = "free_x") +
    geom_line(size = 1) +
    ggthemes::theme_stata() +
    ggtitle(sprintf("Consommation éléctrique hebdomadaire - %s", perim_label)) +
    theme(
      plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
      axis.text.x  = element_text(angle = 15, hjust = 1),
      axis.text.y  = element_text(angle = 0, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    ) 
  if(perim != "US48"){
    gg_focus = gg_focus +
      geom_vline(xintercept = intercept_time,linetype = "dashed") +
      labs(subtitle = subtt_perim) 
  }else{
    gg_focus = gg_focus +
      labs(subtitle = source_) 
  }
  assign(sprintf("gg_conso_electr_%s", perim), gg_focus)
}

# gg_conso_electr_NY
# gg_conso_electr_CAL
# gg_conso_electr_US48

time_end = Sys.time()
run_time = as.numeric(difftime(time_end, time_start, units = "secs"))

export_graph(gg_conso_electr_US48, perim = "US", run_time = run_time, 
             # create_code_html =  TRUE,
             folder_name = "us_electr_consumption_US", update = T)

export_graph(gg_conso_electr_CAL, perim = "US", run_time = run_time,
             # create_code_html =  TRUE,
             folder_name = "us_electr_consumption_CAL", update = T)

export_graph(gg_conso_electr_NY, perim = "US", run_time = run_time,
             # create_code_html =  TRUE,
             folder_name = "us_electr_consumption_NY", update = T)

