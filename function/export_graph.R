export_graph = function (plot, folder_name, perim = "_autre", run_time = NULL, 
                         rmd_file = "./function/read_code.Rmd", create_code_html = FALSE,
                         width = 15, height = 10, update = FALSE, data_format = "rds") 
{
  
  if (missing(folder_name)) {
    folder_name = deparse(substitute(plot))
  }
  # if (!any(class(plot) == "ggplot")) {
  #   cat("plot doit etre un graphique de class ggplot")
  #   stop()
  # }
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
  if(exists("link_app")){
    link_used_file = gsub(link_app, ".", link_used_file)
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
  
  # if (file.exists(file_path)) {
  #   gg = readRDS(file_path)
  #   if (!is.null(gg$link_code_file)) {
  #     if(gg$link_code_file != ""){
  #       # if (!str_detect(gg$link_code_file, "server")) {
  #       link_used_file <- gg$link_code_file
  #       # }
  #     }
  #   }
  # }
  
  # 
  # html code file creation
  # 
  file_name_html = paste0(folder_name, "_code.html")
  rmd_output_file = file.path(path_resultats_perim_folder, file_name_html)
  
  rmd_output_file_ = file.path(getwd(), substr(rmd_output_file, 3, nchar(rmd_output_file)))
  link_used_file_ = file.path(getwd(), substr(link_used_file, 3, nchar(link_used_file)))
  
  code_file = link_used_file_
  
  
  if(!file.exists(rmd_output_file_) || create_code_html){
    # if(create_code_html){
    
    # render_code_html = try(
    #   rmarkdown::render(input = rmd_file,
    #                     runtime = "shiny",
    #                     output_file = rmd_output_file,
    #                     params = list(code_file = link_used_file))
    # )
    # 
    # if(class(render_code_html) == "try-error"){
    
    rmarkdown::render(input = rmd_file,
                      runtime = "shiny",
                      output_file = rmd_output_file_,
                      params = list(code_file = link_used_file_))
    # }
    
    # }
  }

  
  # plot + ggsave(file_path_pdf, width = width, height = height)
  plot$update <- update
  plot$link_code_file <- link_used_file
  plot$run_time <- run_time
  gg = plot
  # save(gg, file = file_path)
  
  
  if(data_format == "rds"){
    saveRDS(gg, file = file_path)
  }else if (data_format == "rdata"){
    save(gg, file = file_path)
  }
  
  minio_file_path = file.path(perim, folder_name, paste0(folder_name, "_gg_plot"))
  
  print(minio_file_path)
  
  aws.s3::s3write_using(gg, FUN = saveRDS,
                bucket = "groupe-1360", object = minio_file_path,
                opts = list("use_https" = F, "region" = ""))
  
}

