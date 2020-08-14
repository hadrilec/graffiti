library(BIS)
library(tidyverse)
library(insee)

dt_list = BIS::get_datasets()

data = BIS::get_bis(dt_list$url[7])

data_plot  = data %>% 
  filter(str_detect(unit_of_measure, "Euro")) %>% 
  filter(type_of_instruments == "Credit (loans & debt securities)") %>% 
  filter(borrowers_country == "All countries excluding residents") %>% 
  mutate(time = insee:::get_date(toupper(date), freq = "T")) %>% 
  arrange(time) %>% 
  mutate(growth = 100*(obs_value/dplyr::lag(obs_value)-1)) %>% 
  select(time, obs_value, growth) %>% 
  pivot_longer(-time, names_to = "variable", values_to = "value") %>% 
  mutate(label = replace(variable, variable == "growth", "Taux de croissance")) %>% 
  mutate(label = replace(label, variable == "obs_value", "Volume"))

time_now = with_tz(now(), "Europe/Paris")
subtt = sprintf("Source: BIS, Fait le : %s", time_now)

gg_bis_credit_non_resident =
ggplot(data_plot, aes(x = time, y = value)) +
  geom_col() +
  facet_wrap(~label, scales = "free") +
  ggtitle("Crédit octroyé en euro (prêts et obligations) des non résidents de la zone euro") + 
  labs(subtitle = subtt) +
  ggthemes::theme_stata() +
  theme(
    plot.title   = element_text(lineheight = 0.8, face = "bold", hjust = 0.5, size = 18),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(angle = 0, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 

export_minio_graph(gg_bis_credit_non_resident,
                   folder_name = "bis_credit_euro_non_resident",
                   perim = "FI", update = TRUE)
