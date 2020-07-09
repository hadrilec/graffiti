
library(highcharter)
library(dplyr)

mapdata <- get_data_from_map(download_map_data("countries/us/us-all"))
# glimpse(mapdata)

data_fake <- mapdata %>% 
  select(code = `hc-a2`) %>% 
  mutate(value = 1e5 * abs(rt(nrow(.), df = 10)))

map_us = 
hcmap("countries/us/us-all", data = data_fake, value = "value",
      joinBy = c("hc-a2", "code"), name = "Fake data",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2, valuePrefix = "$", valueSuffix = " USD")) %>% 
  hc_title(text = "Carte des Etats-Unis")

export_minio_graph(map_us, perim = "US", folder_name = "map_us")
