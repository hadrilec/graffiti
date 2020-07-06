
library(eia)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

time_start = Sys.time()

# 
# TROUVER UNE SERIE PARMI CELLES DISPONIBLES
# 
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
# series_nrj_outlook = eia_child_cats(829714)
# series_world_oil = eia_child_cats(829716)
# series_world_supply = eia_child_cats(829746)

series_world_opec = eia_cats(1039874)
# series_world_non_opec = eia_cats(829751)

series_world_opec_df = series_world_opec[["childseries"]] %>% 
  filter(str_detect(name, "Crude Oil Production") & str_detect(name, "Monthly"))

series_download_opec = series_world_opec_df %>% pull(series_id)

data = eia_series(series_download_opec)

list_df = list()

for(i in 1:nrow(data)){
  df = data$data[[i]]
  df[,"series_id"] = data[i,"series_id"]
  df[,"name"] = data[i,"name"]
  list_df[[length(list_df)+1]] = df
}

OIL_prod_opec = bind_rows(list_df) %>% 
  mutate(type = "OPEC") %>% 
  filter(!str_detect(name, "Total")) %>% 
  mutate(country = gsub("Crude Oil Production|Monthly|,","", name))

country_order_df = OIL_prod_opec %>% 
  group_by(country) %>% 
  filter(date == max(date)) %>% 
  arrange(desc(value)) 

country_order = country_order_df %>% pull(country)
last_date = country_order_df %>% pull(date) %>% min()

OIL_prod_opec = OIL_prod_opec %>% 
  mutate(country = factor(country, levels = country_order))

mycolors = c(brewer.pal(7, "Set1"), brewer.pal(8, "Set2"), brewer.pal(8, "Set3"))

time_now = with_tz(now(), "Europe/Paris")

gg_OIL_prod_opec = 
ggplot(OIL_prod_opec, aes(x = date, y = value, fill = country)) +
  geom_area() +
  scale_y_continuous(sec.axis = dup_axis()) +
  scale_x_date(expand = c(0.01, 0.01)) +
  scale_fill_manual(values = mycolors) +
  ggtitle("Production de pétrole brut par les pays de l'OPEP") +
  labs(subtitle = sprintf("Unité : millions de barils de pétrole par jour\n Source : EIA, Dernier point : %s, Fait le : %s",
                          last_date, time_now)) +
  ggthemes::theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold",
                                hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y  = element_text(angle = 0, hjust = 1, size = 12),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  guides(fill=guide_legend(nrow = 2, byrow = TRUE)) 

time_end = Sys.time()
run_time = round(difftime(time_end, time_start, units = "secs"))

export_minio_graph(gg_OIL_prod_opec,
             folder_name = "OIL_prod_opec",
             # create_code_html = TRUE,
             perim = "OIL",
             update = TRUE, 
             run_time = run_time)


