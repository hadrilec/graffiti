
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

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
library(shinyflags)
# library(shinycssloaders)
# library(rhandsontable)
library(DT)

library(tidyverse)
library(lubridate)
library(zoo)

library(rmarkdown)
library(plotly)
library(RColorBrewer)
library(ggthemes)

library(fredr)
library(eia)
library(saqgetr)
library(eurostat)
library(Quandl)
library(rsdmx)
library(pdfetch)
library(RJSONIO)
library(xml2)
library(rvest)

Print = function(x){print(deparse(substitute(x)));print(x)}

link_app =  "N:/GDCJ/N-GDCJ/Echanges.DCJ/DSC/Rshiny boite a outils"

if(file.exists(link_app)){
  setwd(link_app)
}

# options(shiny.reactlog=TRUE)
# shiny::runApp(link_app, display.mode="showcase")
# setwd(link_app)
chemin_prev = ""
link_results =  "./data/resultats"


cahier_file = "cahier.Rmd"
rmd_file = "read_code.Rmd"
rmd_output_file  = "read_code.html"
 
# DB_var_file = file.path(".","resultats","_files", "DB_variables.RData") 
DB_var_file = file.path(".","data","resultats","_files", "DB_variables.rds")
# last_db_var_update =  file.info(DB_var_file)[1,"mtime"]
# last_db_var_update_min_date = last_db_var_update %m+% days(7)

# source(file.path(link_app, "pRev_DB_var_names.R"))
source("./pRev_DB_var_names.R")
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
fredr_set_key("1e1376b050a44076281adda2fe2e1a32")

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

link_flags = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3"

flags <- c("fr.svg", "de.svg", "gb.svg", "es.svg", "it.svg",
           "cn.svg", "jp.svg", "eu.svg", "us.svg")

# flags_names = c(gsub(".svg", "", flags))

flags_id_list = c("fr", "de", "gb", "es", "it",
                         "cn", "jp", "eu", "us")

flags_label_list = c("France", "Allemagne", "Royaume-Uni", "Espagne", "Italie",
                  "Chine", "Japon", "Zone Euro", "Etats-Unis")

icons_perims = list()

for(pays in 1:length(flags_id_list)){
  pays_name_id = flags_id_list[pays]
  pays_name_label = flags_label_list[pays]
  icons_perims[[length(icons_perims)+1]] = paste(flag(pays_name_id, size = 15), " ", pays_name_label)
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
dico_table = DB_variables %>%
  select(perim, var, title) %>%
  dplyr::rename(`Périmètre` = perim,
                Variable = var,
                Titre = title)
# 
# dico_home_page <- renderDataTable(dico_table,
#                                         filter = "top")

# gg = readRDS("./data/resultats/ZE/population_europe/population_europe_gg_plot.rds")

# plot_ex = plotly::ggplotly(gg)

# flag("fr")


# create_code_html = TRUE

export_graph = function (plot, folder_name, perim = "_autre", run_time = NULL, 
                         rmd_file = "./read_code.Rmd", create_code_html = FALSE,
                         width = 15, height = 10, update = FALSE) 
{
  if (missing(folder_name)) {
    folder_name = deparse(substitute(plot))
  }
  if (!any(class(plot) == "ggplot")) {
    cat("plot doit etre un graphique de class ggplot")
    stop()
  }
  error_message = FALSE
  if (exists("chemin_prev")) {
    path_resultats = file.path(".", "data","resultats")
    if (!file.exists(path_resultats)) {
      dir.create(path_resultats, "0777")
    }
  }else {
    error_message = TRUE
  }
  if (error_message) {
    text1 = "creer une variable globale chemin_prev contenant le lien vers le dossier ou exporter les resultats, exemple : "
    text2 = "N:/G140/M-G140/Usuels.dsc/pRev"
    error_message_text = sprintf("%s \n%s", text1, 
                                 text2)
    cat(error_message_text)
    stop()
  }
  path_resultats_perim = file.path(".","data", "resultats", 
                                   perim)
  path_resultats_perim_folder = file.path(".","data", "resultats", 
                                          perim, folder_name)
  for (link in c(path_resultats_perim, path_resultats_perim_folder)) {
    if (!file.exists(link)) {
      dir.create(link, "0777")
    }
  }
  file_name = paste0(folder_name, "_gg_plot.rds")
  # file_name = paste0(folder_name, "_gg_plot.RData")
  file_name_pdf = paste0(folder_name, "_gg_plot.pdf")
  file_path = file.path(path_resultats_perim_folder, file_name)
  file_path_pdf = file.path(path_resultats_perim_folder, file_name_pdf)
  timenow = gsub("-| |:|CET", "", Sys.time())
  # if (file.exists(file_path_pdf)) {
  #   if (file.opened(file_path_pdf)) {
  #     file_path_pdf = file.path(path_resultats_perim_folder, 
  #                               paste0(folder_name, "_", timenow, "_gg_plot.pdf"))
  #   }
  # }
  
  link_used_file = ""
  link_used_file <- try(rstudioapi::getSourceEditorContext()$path)
  if (class(link_used_file) == "try-error") {
    link_used_file = ""
  }
  
  # change_link_file = FALSE
  # if (is.null(link_used_file)) {
  #   change_link_file = TRUE
  # }
  # if (exists("chemin_prev")) {
  #   if (!str_detect(link_used_file, chemin_prev)) {
  #     change_link_file = TRUE
  #   }
  # }

      if (file.exists(file_path)) {
        gg = readRDS(file_path)
        if (!is.null(gg$link_code_file)) {
          if(gg$link_code_file != ""){
            if (!str_detect(gg$link_code_file, "server")) {
              link_used_file <- gg$link_code_file
            }
          }
        }
      }
  
  # 
  # html code file creation
  # 
  file_name_html = paste0(folder_name, "_code.html")
  rmd_output_file = file.path(path_resultats_perim_folder, file_name_html)
  
  code_file = link_used_file
  
  if(create_code_html){
    rmarkdown::render(input = rmd_file,
                      runtime = "shiny",
                      output_file = rmd_output_file,
                      params = list(code_file = link_used_file))
  }
  
  plot + ggsave(file_path_pdf, width = width, height = height)
  plot$update <- update
  plot$link_code_file <- link_used_file
  plot$run_time <- run_time
  gg = plot
  # save(gg, file = file_path)
  saveRDS(gg, file = file_path)
}



readSDMX2 <- function(link, name = "dfSDMX"){
  
  tfile <- tempfile()
  on.exit(unlink(tfile))
  options(warn = -1)
  dwn = try(utils::download.file(link, tfile, mode = "wb", quiet = TRUE), silent = TRUE)
  
  if(class(dwn) != "try-error"){
    sdmx <- rsdmx::readSDMX(tfile, isURL = FALSE)
    df <- as.data.frame(sdmx)
    return(df)
  }else if(stringr::str_detect(link, "insee")){
    options(warn = 1)
    warning("backup solution")
    series_plus = basename(link)
    list_series = stringr::str_split(series_plus, pattern = "\\+")[[1]]
    list_df = list()
    for(series in list_series){
      df = data.frame(pdfetch::pdfetch_INSEE(series))
      names(df) = "value"
      df[,"obsTime"] = rownames(df)
      df[,"idbank"] = series
      rownames(df) = NULL
      link2 = sprintf("https://www.insee.fr/en/statistiques/serie/ajax/%s", series)
      metadata = RJSONIO::fromJSON(link2)
      df[,"TITLE_FR"] = metadata[["series"]][[1]][["titreFR"]]
      df[,"TITLE_EN"] = metadata[["series"]][[1]][["titreEN"]]
      df[,"freq"] = metadata[["series"]][[1]][["frequence"]]
      df[,"lastUpdate"] = metadata[["series"]][[1]][["lastUpdate"]]
      df[,"unit"] = metadata[["series"]][[1]][["uniteMesure"]]
      list_df[[length(list_df)+1]] = df
    }
    data = dplyr::bind_rows(list_df)
    return(data)
  }else{
    return(NULL)
  }
}
