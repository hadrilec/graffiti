
# lib_loc = "N:/GDCJ/N-GDCJ/Echanges.DCJ/DSC/R/R-3.6.1/library"
# .libPaths("N:/GDCJ/N-GDCJ/Echanges.DCJ/DSC/R/R-3.6.1/library")

#version of R > 3.5

# library(shiny, lib.loc = lib_loc)
# library(shinydashboard, lib.loc = lib_loc)
# library(shinydashboardPlus, lib.loc = lib_loc)
# library(shinyWidgets, lib.loc = lib_loc)
# library(tidyr, lib.loc = lib_loc)
# library(lubridate, lib.loc = lib_loc)
# library(stringr, lib.loc = lib_loc)
# library(dplyr, lib.loc = lib_loc)


Print = function(x){
    obj_name = deparse(substitute(x))
    # print(obj_name);print(x)
    cat(file = stderr(), obj_name, ":", x, "\n")
}

link_app =  "C:/Users/XLAPDO/Desktop/app/dataviz_conj"

if(file.exists(link_app)){
  setwd(link_app)
}

# source(file.path(link_app, "pRev_DB_var_names.R"))
source("./librairies.R")
source("./function/pRev_DB_var_names.R")
source("./function/export_graph.R")
source("./function/readSDMX2.R")

# options(shiny.reactlog=TRUE)
# shiny::runApp(link_app, display.mode="showcase")
# setwd(link_app)
chemin_prev = ""
link_results =  "./data/resultats"


cahier_file = "./function/cahier.Rmd"
# rmd_file = "read_code.Rmd"
# rmd_output_file  = "read_code.html"
 
# DB_var_file = file.path(".","resultats","_files", "DB_variables.RData") 
DB_var_file = file.path(".", "data", "resultats", "_files", "DB_variables.rds")

# last_db_var_update =  file.info(DB_var_file)[1,"mtime"]
# last_db_var_update_min_date = last_db_var_update %m+% days(7)



# update_DB_variable()

# MAJ de la base de données des noms des variables toutes les heures
if(FALSE){
  if(Sys.time() > last_db_var_update_min_date){
    update_DB_variable()
  }
}

# 
# API KEY POUR TOUS LES SCRIPTS
# 

# BANQUE DE FRANCE
webstat_client_ID <- 'd85fb7da-a306-469f-9b60-2e35d251611b'

# EIA agence americaine energie
api_key_data_gov_us = "sudqWb4gHCNBIo00XXxrhkB1iSe6jRKod08vXyKc"
api_key_eia = "423b693a2da4d6e9f5b0a1957eff0ba8"
eia_set_key(api_key_eia)

# Federal Reserve API key
#fredr_set_key("1e1376b050a44076281adda2fe2e1a32")

# AQICN
api_token_aqicn = "d2cec6e4da536c80809e27cfc169962408f18642"

# QUANDL
Quandl.api_key('ZQv7EfJg6xnzC-by17Kn')

# Sys.setenv(http_proxy="proxy-rie.http.insee.fr:8080")
# Sys.setenv(https_proxy="proxy-rie.http.insee.fr:8080")

# DB_var_file = file.path(link_results, "_files", "variables_DB.RData")
# load(DB_var_file)
DB_variables = readRDS(DB_var_file)

# Print(DB_variables)

perimetre_list = DB_variables %>% 
  pull(perim) %>% 
  unique() %>% 
  sort()

countries <- c("FR", "DE", "UK", "ES", "IT", "CN", "JP", "ZE", "US", "OIL", "FI", "COM", "INF")
# countries <- c("FR", "DE", "UK", "ES", "IT", "CN", "JP", "ZE", "US")

perimetre_added = which(!perimetre_list %in% countries)
if(length(perimetre_added) > 0){
  countries = c(countries, perimetre_list[perimetre_added])
}

# link_flags = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3"
# 
# flags <- c("fr.svg", "de.svg", "gb.svg", "es.svg", "it.svg",
#            "cn.svg", "jp.svg", "eu.svg", "us.svg")

# flags_names = c(gsub(".svg", "", flags))

flags_id_list = c("fr", "de", "gb", "es", "it",
                         "cn", "jp", "eu", "us")

flags_label_list = c("France", "Allemagne", "Royaume-Uni", "Espagne", "Italie",
                  "Chine", "Japon", "Zone Euro", "Etats-Unis")

icons_perims = list()

for(pays in 1:length(flags_id_list)){
  pays_name_id = flags_id_list[pays]
  pays_name_label = flags_label_list[pays]
  icons_perims[[length(icons_perims)+1]] = paste(flag(pays_name_id, size = 15),
                                                 " ", pays_name_label)
}
icons_perims = unlist(icons_perims)

icons_perims = c(icons_perims,
                 paste(as.character(icon("gas-pump")), " " ,"Pétrole"),
                 paste(as.character(icon("money-check-alt")), " " ,"Finance"),
                       paste(as.character(icon("globe-americas")), " " ,"Commerce"),
                 paste(as.character(icon("shopping-cart")), " " ,"Inflation"))


# output$dico_home_page <- DT::renderDT(dico_table,
#                                       filter = "top")

# 
# 
# dico_table = DB_variables %>%
#   select(perim, var, title) %>%
#   dplyr::rename(`Périmètre` = perim,
#                 Variable = var,
#                 Titre = title)

# gg = readRDS("./data/resultats/ZE/population_europe/population_europe_gg_plot.rds")

# plot_ex = plotly::ggplotly(gg)
