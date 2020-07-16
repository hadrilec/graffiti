
library(rdbnomics)
library(tidyverse)
library(lubridate)

time_start = Sys.time()

IPC_data = rdbnomics::rdb_by_api_link("https://api.db.nomics.world/v22/series/INSEE/IPCH-2015?limit=1000&offset=0&q=&observations=1&align_periods=1&dimensions=%7B%7D")

list_NATURE = IPC_data %>% distinct(NATURE)
list_CORRECTION = IPC_data %>% distinct(CORRECTION)
list_INDICATEUR = IPC_data %>% distinct(INDICATEUR)
list_BASIND = IPC_data %>% distinct(BASIND)
list_ipc_global_coicop = IPC_data %>%  distinct(COICOP2016)

IPC = IPC_data %>% 
  dplyr::rename(freq = `@frequency`) %>% 
  dplyr::filter(freq == "monthly") %>% 
  dplyr::filter(NATURE == "INDICE") %>% 
  dplyr::filter(nchar(COICOP2016) <= 3) %>% 
  drop_na() %>% 
  arrange(period) %>% 
  mutate(month = month(period)) %>% 
  group_by(COICOP2016) %>% 
  mutate(IPC_growth_mensuel = 100 * (value/dplyr::lag(value)-1)) %>% 
  group_by(COICOP2016, month) %>% 
  mutate(IPC_growth_annuel = 100 * (value/dplyr::lag(value)-1)) %>% 
  filter(period >= "2015-01-01") %>% 
  ungroup() %>% 
  mutate(title = substr(`COICOP classification, 2016`, 1, 52))

last_values = IPC %>% 
  group_by(COICOP2016) %>% 
  filter(period == max(period)) %>% 
  select(period, IPC_growth_mensuel, IPC_growth_annuel, COICOP2016) %>% 
  mutate(sign_mensuel = case_when(IPC_growth_mensuel >= 0 ~ "+",
                                  TRUE ~ "")) %>% 
  mutate(sign_annuel = case_when(IPC_growth_annuel >= 0 ~ "+",
                                  TRUE ~ "")) %>% 
  mutate(growth_mensuel = paste0(sign_mensuel, round(IPC_growth_mensuel, 1), "%")) %>% 
  mutate(growth_annuel = paste0(sign_annuel, round(IPC_growth_annuel, 1), "%")) %>% 
  select(COICOP2016, growth_mensuel, growth_annuel)

IPC_ = IPC %>%
  left_join(last_values, by = "COICOP2016") %>% 
  mutate(title_annuel = substr(paste(growth_annuel, " - ", title), 1, 52)) %>% 
  mutate(title_mensuel = substr(paste(growth_mensuel, " - ", title), 1, 52))

list_ipc_coicop = IPC %>% distinct(COICOP2016)

ncoicop = nrow(list_ipc_coicop)
nplot = 12

list_seq = lapply(1:100, function(x){
  if(x == 1){
    return(1:nplot)
    }else{
    return(((x-1)*nplot+1):(x*nplot))
  }
})

i = 1
while(!(ncoicop %in% list_seq[[i]])){
  i = i + 1
}

for(j in 1:i){
  list_coicop_selected = 
    list_ipc_coicop %>% 
    ungroup() %>% 
    dplyr::slice(list_seq[[j]]) %>%
    pull(COICOP2016)
  
  df = IPC_ %>%
    filter(COICOP2016 %in% list_coicop_selected)
  
  df_order  = df %>% 
    mutate(COICOP2016 = factor(COICOP2016, levels = list_coicop_selected)) %>% 
    arrange(COICOP2016) 
  
  order_title_annuel = df_order %>% 
    pull(title_annuel) %>% 
    unique()
  
  order_title_mensuel = df_order %>% 
    pull(title_mensuel) %>% 
    unique()
  
  df = df %>% 
    mutate(title_mensuel = factor(title_mensuel, levels = order_title_mensuel)) %>% 
    mutate(title_annuel = factor(title_annuel, levels = order_title_annuel))
  
  assign(paste0("IPC", j), df)
}

# rm(list = ls(pattern = "^IPC[0-9][0-9]$"))

list_df = ls(pattern = "^IPC[0-9]$")

time_now = with_tz(now(), "Europe/Paris")
source_ = paste("Source : INSEE, ", sprintf("Fait le : %s", time_now))


for(df_id in list_df){
  
  df = get(df_id)
  
  title = paste("Glissement mensuel", df_id)
  
  gg_IPC = 
    ggplot(df, aes(x = period, y = IPC_growth_mensuel)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~title_mensuel, scales = "free") +
    ggtitle(title) +
    labs(subtitle = source_)
  
  assign(paste0("gg_", df_id, "_mensuel"), gg_IPC)
}

for(df_id in list_df){
  
  df = get(df_id)
  
  title = paste("Glissement annuel", df_id)
  
  gg_IPC = 
    ggplot(df, aes(x = period, y = IPC_growth_annuel)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~title_annuel, scales = "free") +
    ggtitle(title) +
    labs(subtitle = source_)
  
  assign(paste0("gg_", df_id, "_annuel"), gg_IPC)
}

time_end = Sys.time()
run_time = as.numeric(difftime(time_end, time_start, units = "secs"))

list_df_mensuel = paste0(list_df, "_mensuel")
list_gg_mensuel = paste0("gg_", list_df_mensuel)

list_df_annuel = paste0(list_df, "_annuel")
list_gg_annuel = paste0("gg_", list_df_annuel)

for(i in 1:length(list_df_mensuel)){
  
  gg = get(list_gg_mensuel[i])
  
  export_minio_graph(gg,
               update = TRUE,
               run_time = run_time,
               # create_code_html = TRUE,
               folder_name = list_df_mensuel[i],
               perim = "INF")
}

for(i in 1:length(list_df_annuel)){
  
  gg = get(list_gg_annuel[i])
  
  export_minio_graph(gg,
               update = TRUE,
               run_time = run_time,
               # create_code_html = TRUE,
               folder_name = list_df_annuel[i],
               perim = "INF")
}


# gg_IPC1_mensuel
# gg_IPC2_mensuel
# gg_IPC3_mensuel
# gg_IPC4_mensuel
# gg_IPC5_mensuel
# 
# gg_IPC1_annuel
# gg_IPC2_annuel
# gg_IPC3_annuel
# gg_IPC4_annuel
# gg_IPC5_annuel




