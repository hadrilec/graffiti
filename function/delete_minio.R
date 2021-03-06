delete_minio = function (variable, perim){
  
  file_minio_credentials = "M:/Usuels.dsc/pRev/fonctions/minio_aws_access.R"
  if(file.exists(file_minio_credentials)){
    source(file_minio_credentials)
  }
  
  data_in_DB = aws.s3::get_bucket(Sys.getenv("AWS_BUCKET"), use_https = T, region = "")
  dataf = do.call(c, unlist(data_in_DB, recursive = FALSE))
  contents_key = as.character(unlist(dataf[which(names(dataf) == "Contents.Key")]))
  
  minio_path_selected = contents_key[stringr::str_detect(contents_key, "^dataviz/")]
  minio_path_selected = minio_path_selected[!stringr::str_detect(minio_path_selected, "DB_variable\\/DB_variable")]
  
  
  minio_file_path_ggplot = file.path("dataviz", perim, variable, paste0(variable, "_gg_plot"))
  minio_file_path_png = file.path("dataviz", perim, variable, paste0(variable, "_png"))
  minio_file_path_jpg = file.path("dataviz", perim, variable, paste0(variable, "_jpg"))
  minio_file_path_html = file.path("dataviz", perim, variable, paste0(variable, "_html"))
  minio_file_path_code = file.path("dataviz", perim, variable, paste0(variable, "_code"))
  
  
  list_file = c(minio_file_path_ggplot, minio_file_path_png, minio_file_path_jpg, minio_file_path_html, minio_file_path_code)
  
  for(file_ in list_file){
    if(file_ %in% minio_path_selected){
     
                            object = file_,
                            use_https = T,
                            region = "")
      
      print(sprintf("file deleted : %s", file_))
    }
  }
}

# delete_minio("chomage_europe2", "ZE")
