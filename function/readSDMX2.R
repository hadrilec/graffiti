
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

get_insee <- function(...){
  list_idbank = paste0(list(...), collapse = "+")
  link = sprintf("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/%s", list_idbank)
  df = readSDMX2(link)
  df = data.frame(df, stringsAsFactors = FALSE)
  df[,"TITLE_FR"] = as.character(df[,"TITLE_FR"])
  Encoding(df[,"TITLE_FR"]) = "UTF-8"
  return(df)
}

# data = get_insee("001558315", "010540726")