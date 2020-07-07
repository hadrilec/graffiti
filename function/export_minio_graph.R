export_minio_graph = function (plot, folder_name, perim = "_autre", run_time = NULL, 
                         rmd_file = "M:/Usuels.dsc/pRev/fonctions/read_code.Rmd", create_code_html = FALSE,
                         export_code = TRUE,
                         update = FALSE, data_format = "rds") 
{
  file_minio_credentials = "M:/Usuels.dsc/pRev/fonctions/minio_aws_access.R"
  if(file.exists(file_minio_credentials)){
    source(file_minio_credentials)
  }
  
  
  if (missing(folder_name)) {
    folder_name = deparse(substitute(plot))
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
  
  if(data_format == "rds"){
    file_name = paste0(folder_name, "_gg_plot.rds")
  }else if (data_format == "rdata"){
    file_name = paste0(folder_name, "_gg_plot.RData")
  }else{
    stop("wrong data format")
  }
  
  # 
  file_name_pdf = paste0(folder_name, "_gg_plot.pdf")
  
  file_path = file.path(path_resultats_perim_folder, file_name)
  file_path_pdf = file.path(path_resultats_perim_folder, file_name_pdf)
  timenow = gsub("-| |:|CET", "", Sys.time())
  
  link_used_file = ""
  link_used_file <- try(rstudioapi::getSourceEditorContext()$path)
  
  if (class(link_used_file) == "try-error") {
    link_used_file = ""
  }
  if(exists("link_app")){
    link_used_file = gsub(link_app, ".", link_used_file)
  }
  
  # 
  # create and export html code file 
  # 
  
  file_name_html = paste0(folder_name, "_code.html")
  rmd_output_file = file.path(path_resultats_perim_folder, file_name_html)
  
  rmd_output_file_ = file.path(getwd(), substr(rmd_output_file, 3, nchar(rmd_output_file)))
  link_used_file_ = file.path(getwd(), substr(link_used_file, 3, nchar(link_used_file)))
  
  code_file = link_used_file_
  code_file = try(rstudioapi::getSourceEditorContext()$path)
  link_used_file_ = code_file
  
  if(!file.exists(rmd_output_file_) || create_code_html){
    
    if(file.exists(rmd_file)){
      if(!"try-error" %in% class(code_file)){
        
        t_file = tempfile()
        
        try(
          rmarkdown::render(input = rmd_file,
                            runtime = "shiny",
                            output_file = t_file,
                            params = list(code_file = link_used_file_))
        )
        
        # file.copy(from = t_file, to = rmd_output_file_)
        
        minio_file_html_path = file.path("dataviz", perim, folder_name, paste0(folder_name, "_html"))
        
        aws.s3::s3write_using(paste0(t_file,".html"), FUN = file.copy,
                              bucket = "groupe-1360", object = minio_file_html_path,
                              opts = list("use_https" = F, "region" = ""))
        
        print(minio_file_html_path)
      }
      


    }

  }

  plot$update <- update
  plot$link_code_file <- link_used_file
  plot$run_time <- run_time
  gg = plot
  
  if(file.exists(path_resultats_perim_folder)){
    if(data_format == "rds"){
      # saveRDS(gg, file = file_path)
    }else if (data_format == "rdata"){
      # save(gg, file = file_path)
    }
  }
  
  minio_file_path = file.path("dataviz", perim, folder_name, paste0(folder_name, "_gg_plot"))
  
  print(minio_file_path)
  
  # 
  # export graph data
  # 
  
  aws.s3::s3write_using(gg, FUN = saveRDS,
                bucket = "groupe-1360", object = minio_file_path,
                opts = list("use_https" = F, "region" = ""))
  
  
  # 
  # export code file
  # 
  
  if(export_code){
    
    minio_file_code_path = file.path("dataviz", perim, folder_name, paste0(folder_name, "_code"))

    save_code_file = try(rstudioapi::documentSave(rstudioapi::getActiveDocumentContext()$id))
    link_used_file = try(rstudioapi::getSourceEditorContext()$path)
    
    if(class(save_code_file) != "try-error" & class(link_used_file) != "try-error"){
      
      aws.s3::s3write_using(link_used_file, FUN = file.copy,
                            bucket = "groupe-1360", object = minio_file_code_path,
                            opts = list("use_https" = F, "region" = ""))
      
      print(minio_file_code_path)
    }
  }
  
}

