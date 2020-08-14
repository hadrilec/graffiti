# rm(list = ls())

library(rtsdata)
library(xml2)
library(rvest)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(stringr)

time1 = Sys.time()
time_now = with_tz(now(), "Europe/Paris")
today_date = today()

year_current = year(today_date)
start_date = as.Date(paste0(substr(today_date,1,7), '-01')) %m-% months(13)

start_date2 = start_date %m+% months(1)

index_code = "FCHI"
index_code2 = '^FCHI'
index_label = 'CAC40'
currency = 'euros'
company_number_plotted = 40


url = 'https://en.wikipedia.org/wiki/CAC_40'

table = read_html(url) %>% html_table(fill = TRUE)
table = table[[4]]

names(table) = tolower(names(table))
table = table[,c(3,1,2)]
names(table) = c('ticker', 'company', 'sector')
table[,1] = gsub('\\.$','',table[,1])
table[nrow(table)+1,] = c(index_code2,index_label, '')

env_ = new.env()

table[which(table[,'company'] == 'AXA'),'sector'] = 'banks'
table[which(table[,'company'] == 'Michelin'),'sector'] = 'automobiles'
table[which(table[,'company'] == 'Dassault Systèmes'),'sector'] = 'IT services'
table[str_detect(table[,'company'], 'Safran|Airbus|Thales'),'sector'] = 'aerospace & defense'
table[str_detect(table[,'company'], 'Herm|LVMH|Kering'),'sector'] = 'luxury goods'
table[str_detect(table[,'company'], 'Total|Engie|Technip|Veolia'),'sector'] = 'energy'
table[str_detect(table[,'company'], 'Carrefour|Danone|Sodexo'),'sector'] = 'food products, retailers & services'
table[str_detect(table[,'company'], 'Unibail'),'sector'] = 'real estate'
table[str_detect(table[,'company'], 'Unibail'),'company'] = 'Unibail'
table[str_detect(table[,'company'], 'Orange|Publicis|Vivendi'),'sector'] = 'telecommunications & media'
table[str_detect(table[,'company'], 'Arcelor|Air Liquide'),'sector'] = 'steel & chemical industry'
table[str_detect(table[,'company'], 'STMicro|Legrand|Schneider'),'sector'] = 'electrical components & semiconductor'
table[str_detect(table[,'company'], 'Bouygues|Vinci|Saint-Gobain'),'sector'] = 'construction'
table[str_detect(table[,'company'], 'Essilor|Sanofi'),'sector'] = 'pharmaceuticals & medical supplies'

table = table %>% drop_na()

Symbols = c(table[,1])

if(length(Symbols) < company_number_plotted){
  
  Symbols = c( "AC.PA",   "AI.PA",   "AIR.PA",  "MT.AS",   "ATO.PA",  "CS.PA",   "BNP.PA",  "EN.PA",   "CAP.PA" ,
               "CA.PA",   "ACA.PA",  "BN.PA",   "DSY.PA",  "ENGI.PA", "EL.PA",   "RMS.PA",  "KER.PA",  "OR.PA" , 
               "LR.PA",  "MC.PA",   "ML.PA",   "ORA.PA",  "RI.PA",   "UG.PA",   "PUB.PA",  "RNO.PA",  "SAF.PA" ,
               "SGO.PA",  "SAN.PA",  "SU.PA",   "GLE.PA",  "SW.PA",   "STM.PA",  "HO.PA",   "FP.PA" ,  "URW.AS" ,
               "VIE.PA", "DG.PA",   "VIV.PA",  "WLN.PA",  "^FCHI" )
  
  company_vector = c( "Accor" ,             "Air Liquide",    "Airbus",             "ArcelorMittal",
                      "Atos" ,              "AXA",      "BNP Paribas",        "Bouygues" , 
                      "Capgemini",          "Carrefour",       "Crédit Agricole",    "Danone" ,  
                      "Dassault Systèmes",  "Engie" ,        "Essilor"         ,   "Hermès" ,   
                      "Kering",             "L'Oréal",         "Legrand"          ,  "LVMH" ,    
                      "Michelin",           "Orange",          "Pernod Ricard"     , "PSA"  ,     
                      "Publicis" ,          "Renault" ,           "Safran"             ,"Saint-Gobain",      
                      "Sanofi"    ,         "Schneider Electric", "Société Générale"   ,"Sodexo",        
                      "STMicroelectronics", "Thales",             "Total"              ,"Unibail"  ,         
                      "Veolia",             "Vinci",              "Vivendi"            ,"Worldline (fr)",
                      "CAC40"   )
  
  
  sector_vector = c("hotels" ,                               "steel & chemical industry"            
                    , "aerospace & defense"  ,                 "steel & chemical industry"            
                    , "IT services"       ,                    "banks"                                
                    , "banks"            ,                     "construction"                         
                    , "IT services"        ,                   "food products, retailers & services"  
                    , "banks"               ,                  "food products, retailers & services"  
                    , "IT services"          ,                 "energy"                               
                    , "pharmaceuticals & medical supplies",    "luxury goods"                         
                    , "luxury goods"                       ,   "personal products"                    
                    , "electrical components & semiconductor", "luxury goods"                         
                    , "automobiles"                           ,"telecommunications & media"           
                    , "distillers and vintners"               ,"automobiles"                          
                    , "telecommunications & media"            ,"automobiles"                          
                    , "aerospace & defense"                   ,"construction"                         
                    , "pharmaceuticals & medical supplies"    ,"electrical components & semiconductor"
                    , "banks"                                 ,"food products, retailers & services"  
                    , "electrical components & semiconductor" ,"aerospace & defense"                  
                    , "energy"                                ,"real estate"                          
                    , "energy"                                ,"construction"                         
                    , "telecommunications & media"            ,"IT services"                          
                    , ""                                     )
  
  table = data.frame(ticker = Symbols, company = company_vector, sector =  sector_vector )
}

shares_ticker_default = data.frame(
  ticker = Symbols[!str_detect(Symbols, index_code)],
  shares = c(150225080,  255465506,  782776618,  263048823, 2600979263, 1142921755, 2402201431,  439291094,  144870391,
             552357347,  263765823,  539794566,  468035498,  556777996,  181391147, 2458310121,  429260342,  109753623,
             182561228,  246795982,  125100616,  556543257,  930471771,  217410197, 2657610790, 1249293880,  867202797,
             298688525, 1256978417,  883043478,  642369867,  559635036,  509037590,  801789549,  237924230, 2905955335,
             172798265,  103318560,  386078287, 1141885965)
)

  env_ = new.env()
  
  # download data
  for(i in 1:length(Symbols)){
    
    print(Symbols[[i]])
    
    getSymbols(Symbols[[i]], env_, src = 'yahoo', from = start_date,
               to = today_date, verbose = TRUE)
    
  }
  
  
  data = list()
  list_ticker_ = names(env_)
  if(any(grepl('getSymb', list_ticker_))){
    list_ticker_ = list_ticker_[-grep('getSymb', list_ticker_)]
  }
  
  
  for(ticker in list_ticker_){
    df = env_[[ticker]]
    if(length(df)>0){
      data[[length(data)+1]] = data.frame(time = time(env_[[ticker]][,1]),
                                          Open = as.numeric(env_[[ticker]][,1]),
                                          High = as.numeric(env_[[ticker]][,2]),
                                          Low  = as.numeric(env_[[ticker]][,3]),
                                          Close = as.numeric(env_[[ticker]][,4]),
                                          Volume = as.numeric(env_[[ticker]][,5]),
                                          Adjusted = as.numeric(env_[[ticker]][,6]),
                                          ticker = ticker)
    }

  }
  
  dfIndex = dplyr::bind_rows(data)
  dfIndex = dfIndex %>% left_join(table, by = 'ticker')
  
  shares_ticker = list()
  
  for(ticker in list_ticker_){
    print(ticker)
    url = sprintf('https://finance.yahoo.com/quote/%s?p=%s',ticker, ticker)
    
    df =  try(read_html(url))
    if(class(df) == "try-error"){
      next
    }
    
    if(!ticker %in% paste0(c('^',''),index_code)){
      
      df = df %>% html_table(fill=TRUE)
      
      if(str_detect(df[[2]][1, 2],'T')){
        markt_cap = gsub('T', '', df[[2]][1, 2])
        markt_cap = as.character(markt_cap)
        markt_cap = as.numeric(markt_cap) * 1000
      }else{
        markt_cap = gsub('B', '', df[[2]][1, 2])
        markt_cap = as.character(markt_cap)
        markt_cap = as.numeric(markt_cap) 
      }
      price = as.numeric(gsub(',','',df[[1]][1, 2]))
      
      shares_ticker[[length(shares_ticker) + 1]] =
        data.frame(ticker = ticker,
                   price = price,
                   markt_cap = markt_cap)
      
    }
    
  }
  
  if(length(shares_ticker) + 1 == length(Symbols)){
    
    shares_ticker_ = shares_ticker
    shares_ticker = bind_rows(shares_ticker)
    
    shares_ticker = shares_ticker %>% 
      mutate(shares = 1000000000 * markt_cap / price) %>% 
      select(ticker, shares)
    
  }else{
    shares_ticker = shares_ticker_default
  }
    
    
    dfIndex_2 = dfIndex %>% 
      left_join(shares_ticker) %>% 
      mutate(cap = shares * Adjusted) %>% 
      mutate(week = week(time)) %>% 
      mutate(year = year(time)) %>%
      mutate(month = month(time, label = TRUE)) %>% 
      mutate(day_ = as.Date(time)) %>% 
      mutate(time = paste0(substr(as.character(time), 1, 7), '-01')) %>% 
      mutate(time = as.Date(time))
    
    dfIndex_ = dfIndex_2 %>% 
      group_by(ticker, time, company) %>% 
      filter(day_ == max(day_)) %>% 
      ungroup() %>% 
      group_by(ticker, time, company) %>% 
      dplyr::summarise(cap = mean(cap, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(time) %>% 
      mutate(cap_tot = sum(cap, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(ticker) %>% 
      mutate(cap_growth = (cap / dplyr::lag(cap)-1)*100) %>% 
      mutate(contrib = cap_growth * dplyr::lag(cap) / dplyr::lag(cap_tot))
    
    contribs = dfIndex_ %>% 
      group_by(time) %>% 
      dplyr::summarise(contribs = sum(contrib, na.rm = TRUE)) %>% 
      filter(time >= start_date) %>% 
      mutate(label = 'Capitalisation des entreprises')
    
    cap_tot = dfIndex_ %>% 
      ungroup() %>% 
      distinct(time, cap_tot) %>% 
      mutate(cap_tot_growth = (cap_tot / dplyr::lag(cap_tot)-1)*100) #%>% 
    # mutate(cap_tot_growth = case_when(cap_tot_growth > 10 ~ NA_real_,
    #                                   cap_tot_growth < -10 ~ NA_real_,
    #                                   TRUE ~ as.numeric(cap_tot_growth)))
    
    
    index = data.frame(date = time(env_[[index_code2]]),
                       value = as.numeric(env_[[index_code2]][,6]))
    
    index = index %>% 
      drop_na() %>% 
      mutate(month = month(as.Date(date), label = TRUE)) %>% 
      mutate(year = year(date)) %>% 
      mutate(time = as.Date(paste0(substr(date,1,7), '-01'))) %>% 
      group_by(time) %>% 
      filter(date == max(date)) %>% 
      dplyr::summarise(value = mean(value)) %>% 
      mutate(index_growth = (value / dplyr::lag(value) - 1) * 100) %>% 
      mutate(label = sprintf('Indice %s', index_label))
    
    colors_ = c(brewer.pal(7,'Set1'), brewer.pal(8,'Set2'), brewer.pal(11,'Set3'),
                brewer.pal(9, 'Pastel1'), brewer.pal(8, 'Pastel2'),
                brewer.pal(8, 'Dark2'), brewer.pal(12, 'Paired'))
    
    while(length(colors_) < length(unique(dfIndex_$company))){
      colors_ = c(colors_, colors_)
    }
    
    dfIndex_ = dfIndex_ %>% 
      mutate(company = case_when(str_detect(company, 'Unibail-Rodamco-Westfield') ~ 'Unibail', 
                                 TRUE ~ as.character(company))) %>% 
      drop_na(contrib)
    
    contrib_avg = dfIndex_ %>% 
      group_by(company) %>% 
      dplyr::summarise(contrib_ = mean(abs(contrib), na.rm = TRUE)) %>% 
      arrange(desc(contrib_))
    
    dfIndex_$company = factor(dfIndex_$company, levels = contrib_avg$company)
    
    contribs_ = contribs %>% dplyr::rename(value = contribs)
    
    data_point = index %>% 
      select(time, index_growth, label) %>% 
      dplyr::rename(value = index_growth) %>% 
      bind_rows(contribs_) %>% 
      filter(time >= start_date2)
    
    dfIndex_price_100 = dfIndex %>% 
      group_by(ticker) %>% 
      filter(time >= start_date2) %>% 
      mutate(price_100 = 100 * Adjusted/Adjusted[time == min(time)],
             price_growth = round(price_100 - 100)) %>% 
      mutate(price_growth = case_when(price_growth >= 0 ~ paste0('+',price_growth),
                                      TRUE ~ as.character(price_growth)))
    
    dfIndex_growth_arrange = dfIndex_price_100 %>% 
      filter(time == max(time)) %>% 
      mutate(label = paste0(substr(company,1,12), ' ', price_growth, '%')) %>% 
      arrange(desc(price_100)) 
    
    label_dfIndex = dfIndex_growth_arrange %>% 
      pull(company) %>% 
      unique()
    
    label_dfIndex_growth = dfIndex_growth_arrange %>% 
      pull(label) %>% 
      unique()
    
    order_ = grepl(index_label, label_dfIndex)
    
    label_dfIndex = label_dfIndex[c(which(order_), which(!order_))]
    label_dfIndex_growth = label_dfIndex_growth[c(which(order_), which(!order_))]
    
    label_dfIndex_growth = str_replace(label_dfIndex_growth, "Unibail-Rodamco-Westfield", "Unibail" )
    label_dfIndex_growth = str_replace(label_dfIndex_growth, "Schneider Electric", "Schneider Electr" )
    label_dfIndex_growth = str_replace(label_dfIndex_growth, "STMicroelectronics", "STMicro" )
    label_dfIndex_growth = str_replace(label_dfIndex_growth, "Dassault Systèmes", "Dassault Syst" )
    
    dfIndex_bis = dfIndex
    dfIndex_bis$company = factor(dfIndex_bis$company,
                                 levels = label_dfIndex, labels = label_dfIndex_growth)
    
    company_colors = data.frame(company = levels(dfIndex_$company),
                                colors = colors_[1:length(unique(dfIndex_$company))])
    
    dfIndex_market_val = dfIndex_ %>% 
      ungroup() %>% 
      group_by(ticker) %>% 
      filter(time == max(time)) %>% 
      ungroup() %>% 
      mutate(cap_share = 100 * cap/sum(cap)) %>% 
      mutate(time = 1) %>% 
      mutate(cap_share_ = round(cap_share,1)) %>% 
      arrange(desc(cap_share))
    
    company_order = dfIndex_market_val %>% 
      select(company, ticker) %>% 
      left_join(company_colors) %>% 
      mutate(colors = as.character(colors)) %>% 
      slice(1:company_number_plotted)
    
    company_order[nrow(company_order)+1,] = c('OTHER', 'OTHER', brewer.pal(8, 'Pastel2')[8])
    
    dfIndex_market_val_ = dfIndex_market_val %>% 
      select(time, company, cap_share) %>% 
      mutate(company = case_when(company %in% company_order$company ~ as.character(company), 
                                 TRUE ~ 'OTHER')) %>% 
      group_by(company) %>% 
      filter(company != 'OTHER') %>% 
      dplyr::summarise(cap_share = sum(cap_share))
    
    pct_market_val_select = sum(dfIndex_market_val_$cap_share)
    
    dfIndex_bis = dfIndex_bis %>% 
      filter(time >= start_date2) %>%
      filter(ticker %in% c(company_order$ticker, index_code, index_code2))
    
    dfIndex_contrib = dfIndex_ %>% 
      select(time, company, contrib, ticker) %>% 
      mutate(company = case_when(company %in% company_order$company ~ as.character(company),
                                 TRUE ~ 'OTHER')) %>% 
      ungroup() %>% 
      group_by(time, company) %>% 
      dplyr::summarise(contrib = sum(contrib))
    
    dfIndex_contrib$company = factor(dfIndex_contrib$company,
                                     levels = company_order$company)
    
    
    dfIndex_market_val$company = factor(dfIndex_market_val$company,
                                        levels = company_order$company)
    
    
    
    cap_tot_dfIndex = dfIndex_market_val %>%
      pull(cap_tot) %>%
      unique() %>% 
      as.numeric() %>% 
      round() %>% 
      max()
    
    dfIndex_market_val = dfIndex_market_val %>% drop_na()
    
    subtt = sprintf("la capitalisation totale est de %s milliards %s \n les entreprises représentées représentent %s%% du total\n Fait le : %s",
                    round(cap_tot_dfIndex/1000000000), currency, round(pct_market_val_select), time_now)
    
    title_market_val = sprintf("Poids des entreprises du %s dans la capitalisation totale", index_label)
    
    dfIndex_contrib_sector = data.frame()
    if('sector' %in% names(table)){
      
      dfIndex_contrib_sector = dfIndex_ %>% 
        select(time, company, contrib, ticker) %>% 
        group_by(time, company) %>% 
        dplyr::summarise(contrib = sum(contrib)) %>% 
        left_join(table) %>% 
        ungroup() %>% 
        group_by(time, sector) %>% 
        dplyr::summarise(contrib = sum(contrib))
      
      contrib_sector_arr = dfIndex_contrib_sector %>% 
        ungroup() %>% 
        group_by(sector) %>% 
        dplyr::summarise(contrib_ = mean(abs(contrib))) %>% 
        arrange(desc(contrib_))
      
      dfIndex_contrib_sector$sector = factor(dfIndex_contrib_sector$sector , 
                                             levels = contrib_sector_arr$sector)
    }
    
    
    # 
    # plot market valuation share of total ####
    # 
    
    gg_company_weight_index =
    ggplot(data = dfIndex_market_val,
           aes(x = time, y = cap_share, fill = company)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      scale_y_continuous(breaks = 0:100, labels = function(x) paste0(x, "%")) +
      scale_fill_manual(values = as.character(company_order$colors)) +
      theme(
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank()
      ) +
      ggtitle(title_market_val) +
      labs(subtitle = subtt) +
      guides(fill = guide_legend(ncol = 2))
  
    
    # 
    # plot contribution to index ####
    # 
    
    title = sprintf("%s - variation de la capitalisation des entreprises et de l'indice",index_label)
    
    xaxis_breaks = seq.Date(from = start_date2,
                            to = max(dfIndex_$time), by = '1 month')
    
    yaxis_breaks = -50:50
    
    source_ = "Source : Yahoo Finance"
    hypoth = "Hypothèses de calcul de la capitalisation :"
    hypoth_1 = "- nombre d'actions constant par entreprise sur la période"
    hypoth_2 = "- même les actions non flottantes sont comprises"
    subtitle = sprintf("%s", source_)
    caption = sprintf("%s %s / %s\n Fait le : %s",  hypoth, hypoth_1, hypoth_2, time_now)
    
    gg_index_company = 
    ggplot() +
      geom_bar(data = dfIndex_contrib,
               aes(x = time, y = contrib, fill = company),
               stat = 'identity', position = 'stack') +
      geom_point(data = data_point, aes(x = time, y = value, shape = label)) +
      geom_line(data = index, aes(x = time, y = index_growth)) +
      ggthemes::theme_stata() +
      scale_fill_manual(values = company_order$colors) +
      scale_shape_manual(values = c(16, 17)) +
      scale_linetype_manual(values = c('dashed')) +
      scale_y_continuous(labels = function(x) paste0(x, '%'),
                         breaks = yaxis_breaks,
                         sec.axis = dup_axis()) +
      scale_x_date(breaks = xaxis_breaks,
                   limits = c(start_date2 %m-% days(15),
                              max(dfIndex_$time) %m+% days(15)),
                   date_labels =  "%b %Y") +
      ggtitle(title) + 
      labs(subtitle = subtitle, caption = caption) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 90),
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        legend.position = 'right'
      ) +   guides(fill = guide_legend(ncol = 2)) 
    
    
    # plot contribution by sector to index ####
    if('sector' %in% names(dfIndex_contrib_sector)){
      
      title_sector = sprintf("%s - variation de l'indice et contribution par secteur",index_label)
      
      gg_index_sector =
      ggplot() +
        geom_bar(data = dfIndex_contrib_sector,
                 aes(x = time, y = contrib, fill = sector),
                 stat = 'identity', position = 'stack') +
        geom_point(data = data_point, aes(x = time, y = value, shape = label)) +
        geom_line(data = index, aes(x = time, y = index_growth)) +
        scale_fill_manual(values = colors_) +
        ggthemes::theme_stata() +
        scale_shape_manual(values = c(16, 17)) +
        scale_linetype_manual(values = c('dashed')) +
        scale_y_continuous(labels = function(x) paste0(x, '%'),
                           breaks = yaxis_breaks,
                           sec.axis = dup_axis()) +
        scale_x_date(breaks = xaxis_breaks,
                     limits = c(start_date2 %m-% days(15),
                                max(dfIndex_$time) %m+% days(15)),
                     date_labels =  "%b %Y") +
        ggtitle(title_sector) + 
        labs(subtitle = subtitle, caption = caption) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(angle = 0),
          axis.text.x = element_text(angle = 90),
          legend.text = element_text(size = 9),
          legend.title = element_blank(),
          legend.position = 'right'
        ) +
        guides(fill = guide_legend(ncol = 2)) 
      
    }
    
  title_facet = sprintf('%s - Cours de bourse des entreprises',
                        index_label)
  
  subtitle = sprintf("Dernier point : %s, Fait le : %s, Evolution du cours entre le %s et le %s",
                     today_date, time_now, start_date2, today_date)
  
  gg_company_price_growth =
  ggplot(dfIndex_bis, aes(x = time, y = Adjusted, colour = company)) +
    geom_line(show.legend = FALSE) +
    facet_wrap(~company, scales = 'free_y') +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    ) +
    labs(subtitle = subtitle) +
    ggtitle(title_facet) 

  
  # 
  # EXPORT GRAPH #### 
  # 
  
  # gg_company_price_growth
  # gg_company_weight_index
  # gg_index_company
  # gg_index_sector
  
  
  time2 = Sys.time()
  run_time = as.numeric(difftime(time2, time1, units = "secs"))
  
  export_minio_graph(gg_company_price_growth, perim = "FI",
               # create_code_html = TRUE,
               run_time = run_time,
               folder_name = "cac40_company_price_growth", update = TRUE)
  
  export_minio_graph(gg_company_weight_index, perim = "FI",
               # create_code_html = TRUE,
               run_time = run_time,
               folder_name = "cac40_company_weight_index", update = TRUE)
  
  export_minio_graph(gg_index_company, perim = "FI",
               # create_code_html = TRUE,
               run_time = run_time,
               folder_name = "cac40_index_company", update = TRUE)
  
  export_minio_graph(gg_index_sector, perim = "FI",
               # create_code_html = TRUE,
               run_time = run_time,
               folder_name = "cac40_index_sector", update = TRUE)
  


