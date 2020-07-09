

readSDMX2 <- function(link){
  
  tfile <- tempfile()
  on.exit(unlink(tfile))
  options(warn = -1)
  dwn = try(utils::download.file(link, tfile, mode = "wb", quiet = TRUE), silent = TRUE)
  
  if(class(dwn) != "try-error"){
    sdmx <- rsdmx::readSDMX(tfile, isURL = FALSE)
    df <- as.data.frame(sdmx)
    return(df)
  }else if(stringr::str_detect(link, "insee") & stringr::str_detect(link, "SERIES_BDM")){
    options(warn = 1)
    warning("backup solution : 1")
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
  }else if(stringr::str_detect(link, "insee") &
           !stringr::str_detect(link, "SERIES_BDM")){
    
    options(warn = 1)
    warning("backup solution : 2")
    
    dt = httr::GET(link)
    data = httr::content(dt)
    df = xml2::as_list(data)
    df2 = tibble::as_tibble(df)
    
    n_line = length(df2[[1]])
    
    if(n_line > 1){
      
      n_series = length(df2[[1]][[2]])
      list_df = list()
      
      for(i in 1:n_series){
        # print(i)
        series_title = attr(df2[[1]][[2]][[i]], "TITLE_FR")
        series_id = attr(df2[[1]][[2]][[i]], "IDBANK")
        series_correction = attr(df2[[1]][[2]][[i]], "CORRECTION")
        res = lapply(df2[[1]][[2]][[i]], attributes)
        
        res2 = dplyr::bind_rows(res) %>% 
          mutate(idbank = series_id) %>% 
          mutate(TITLE_FR = series_title) %>% 
          mutate(correction = series_correction)
        
        list_df[[length(list_df)+1]] = res2
      }
      
      data_final = dplyr::bind_rows(list_df)
      return(data_final)
    }else{
      print(df2[[1]][[1]][["Text"]][[1]])
      return(NULL)
    }
    
  }else{
    return(NULL)
  }
}

get_insee <- function(...){
  if(length(list(...)) == 1){
    list_idbank = paste0(list(...)[[1]], collapse = "+")
  }else{
    list_idbank = paste0(list(...), collapse = "+")
  }
  
  link = sprintf("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/%s", list_idbank)
  df = readSDMX2(link)
  df = data.frame(df, stringsAsFactors = FALSE)
  df[,"TITLE_FR"] = as.character(df[,"TITLE_FR"])
  Encoding(df[,"TITLE_FR"]) = "UTF-8"
  return(df)
}

get_title <- function(idbank){
  print(idbank)
  link = sprintf("https://www.insee.fr/en/statistiques/serie/ajax/%s", as.character(idbank))
  metadata = RJSONIO::fromJSON(link)
  title =  metadata[["series"]][[1]][["titreFR"]]
  Encoding(title) <- "UTF-8"
  return(title)
}

# 
# EXAMPLE
# 

# data = get_insee("001558315", "010540726")


# path_ECB_ILM = "https://sdw-wsrest.ecb.europa.eu/service/data/ILM/"
# myUrl <- paste0(path_ECB_ILM,"W.U2.C..U2.EUR")
# df = readSDMX2(myUrl)