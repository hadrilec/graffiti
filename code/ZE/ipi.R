
library(rsdmx)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(eurostat)

# ipi EUROSTAT ####
ipiEU = get_eurostat('sts_inpr_m')

ipiEU_ = ipiEU %>% 
  mutate(indic_bt_label = label_eurostat(indic_bt, 'indic_bt', lang = "fr"),
         unit_label = label_eurostat(unit, 'unit', lang = "fr"),
         s_adj_label = label_eurostat(s_adj, 's_adj', lang = "fr")) %>% 
  mutate(nace_r2_label = label_eurostat(nace_r2, 'nace_r2', fix_duplicated = TRUE, lang = "fr")) %>% 
  filter(unit == 'I15') %>% 
  filter(s_adj == 'SCA') 

list_nace = ipiEU_ %>% 
  distinct(nace_r2, nace_r2_label)

ipiEU_ = ipiEU_ %>% 
  filter(nace_r2 %in% c('C29', "B-D_F"))

list_countries =  c('FR','DE', 'UK', 'ES','IT', 'EU28')

ipiEU_ = ipiEU_ %>% 
  filter(geo %in% list_countries) %>% 
  mutate(geo = factor(geo, levels = list_countries))

ipiEU2 = ipiEU_ %>% 
  filter(time >= "2015-01-01")

xaxis_breaks = seq.Date(from = min(ipiEU2$time),
                        to = max(ipiEU2$time),
                        by = '3 months')

last_values_ipi = ipiEU2 %>% 
  group_by(geo, nace_r2) %>% 
  filter(time == max(time)) %>% 
  filter(nace_r2 == "B-D_F") %>% 
  arrange(geo) %>% 
  as.data.frame()

last_values_auto = ipiEU2 %>% 
  group_by(geo, nace_r2) %>% 
  filter(time == max(time)) %>% 
  filter(nace_r2 == "C29" ) %>% 
  arrange(geo) %>% 
  as.data.frame()

subtt = sprintf("Dernier point : %s", max(last_values_auto$time))
subtt_ipi = "Industrie > "
subtt_auto = "Automobile > "

for(i in 1:nrow(last_values_ipi)){
  subtt_ipi = paste(subtt_ipi, sprintf("%s : %s, ", last_values_ipi[i,"geo"], last_values_ipi[i,"values"]))
  subtt_auto = paste(subtt_auto, sprintf("%s : %s, ", last_values_auto[i,"geo"], last_values_auto[i,"values"]))
}

subtt2 = sprintf("%s\n%s\n%s", subtt, subtt_ipi, subtt_auto)

gg_ipi = 
ggplot(ipiEU2, aes(x = time, y = values, colour = nace_r2_label)) +
  geom_line(size = 1) +
  scale_x_date(breaks = xaxis_breaks, date_labels = '%b %Y') +
  geom_hline(yintercept = 100, linetype = 'dashed') +
  facet_wrap(~geo) +
  labs(subtitle = subtt2) +
  ggtitle('Indice de production industrielle en Europe') +
  ggthemes::theme_stata() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 0),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_blank())


export_minio_graph(gg_ipi, folder_name = "ipi_eu", perim = "ZE")


prod_drop = ipiEU2 %>%
  group_by(geo) %>% 
  filter(nace_r2 == "C29" ) %>% 
  summarise(prod = 100*(values[time == max(time)] - values[time == "2018-01-01"] )/values[time == "2018-01-01"])






