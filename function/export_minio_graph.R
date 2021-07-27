export_minio_graph = function (plot, folder_name,
                               perim = "_autre",
                               run_time = NULL,
                               rmd_file = "M:/Usuels.dsc/pRev/fonctions/read_code.Rmd",
                               create_code_html = TRUE,
                               export_code = TRUE,
                               update = TRUE)
{
  if(!("ggplot" %in% class(plot) | "highchart" %in% class(plot))){
    warning("!!! plot is not a ggplot or highchart plot !!!")
  }
  
  pkg = installed.packages()
  
  if(!"rstudioapi" %in% pkg[,1]){
    stop("Please download rstudioapi R package")
  }
  if(!"lubridate" %in% pkg[,1]){
    stop("Please download lubridate R package")
  }
  
  time_now = sprintf("Fait le : %s", lubridate::with_tz(lubridate::now(), "Europe/Paris"))
    
  subtt = plot$labels$subtitle
  if(is.null(subtt)){
    plot$labels$subtitle = time_now
  }else{
    plot$labels$subtitle = sprintf("%s\n%s", subtt, time_now)
  }

  file_minio_credentials = "M:/Usuels.dsc/pRev/fonctions/minio_aws_access.R"
  if(file.exists(file_minio_credentials)){
    source(file_minio_credentials)
  }

  if (missing(folder_name)) {
    folder_name = deparse(substitute(plot))
  }

  timenow = gsub("-| |:|CET", "", Sys.time())


  #
  # create and export html code file
  #

  file_name_html = paste0(folder_name, "_code.html")


  code_file = try(rstudioapi::getSourceEditorContext()$path, silent = TRUE)
  link_used_file_ = code_file

  if(create_code_html){
    if(file.exists(rmd_file)){
      if(!"try-error" %in% class(code_file)){
        if(!basename(code_file) %in% c("server", "server.R", "data_update", "data_update.R")){
          t_file = tempfile()

          html_file_rmd =
            try(
              rmarkdown::render(input = rmd_file,
                                runtime = "shiny",
                                output_file = t_file,
                                params = list(code_file = link_used_file_))
            )

          minio_file_html_path = file.path("dataviz", perim, folder_name, paste0(folder_name, "_html"))

          if(!"try-error" %in% class(html_file_rmd)){
            if(file.exists(paste0(t_file,".html"))){

              aws.s3::s3write_using(paste0(t_file, ".html"), FUN = file.copy,
                                    bucket = Sys.getenv("AWS_BUCKET"), object = minio_file_html_path,
                                    opts = list( "region" = ""))

              print(minio_file_html_path)

            }
          }
        }
      }
    }
  }

  plot$update <- update
  plot$link_code_file <- link_used_file_
  plot$run_time <- run_time
  gg = plot

  minio_file_path = file.path("dataviz", perim, folder_name, paste0(folder_name, "_gg_plot"))

  print(minio_file_path)

  #
  # export graph data
  #

  aws.s3::s3write_using(gg, FUN = saveRDS,
                bucket = Sys.getenv("AWS_BUCKET"), object = minio_file_path,
                opts = list( "region" = ""))


  #
  # export code file
  #

  if(export_code){

    minio_file_code_path = file.path("dataviz", perim, folder_name, paste0(folder_name, "_code"))

    save_code_file = try(rstudioapi::documentSave(rstudioapi::getActiveDocumentContext()$id), silent = TRUE)
    link_used_file = try(rstudioapi::getSourceEditorContext()$path, silent = TRUE)

    if(class(save_code_file) != "try-error" & class(link_used_file) != "try-error"){
      if(!basename(code_file) %in% c("server", "server.R", "data_update", "data_update.R")){

        aws.s3::s3write_using(link_used_file, FUN = file.copy,
                              bucket = Sys.getenv("AWS_BUCKET"), object = minio_file_code_path,
                              opts = list( "region" = ""))

        print(minio_file_code_path)
      }
    }
  }
}

