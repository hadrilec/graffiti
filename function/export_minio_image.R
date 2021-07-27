export_minio_image <- function(link_image, perim, folder_name, title){
  
  file_minio_credentials = "M:/Usuels.dsc/pRev/fonctions/minio_aws_access.R"
  if(file.exists(file_minio_credentials)){
    source(file_minio_credentials)
  }
  
  if(!file.exists(link_image)){
    stop("le fichier link_image n'existe pas")
  }
  
  # path_resultats_perim = file.path(".", "data", "resultats", perim)
  # 
  # path_resultats_perim_folder = file.path(".", "data", "resultats", perim, folder_name)
  # 
  # for (link in c(path_resultats_perim, path_resultats_perim_folder)) {
  #   if (!file.exists(link)) {
  #     dir.create(link, "0777")
  #   }
  # }
  
  df = data.frame(perim = perim, folder_name = folder_name, title = title, file = link_image)
  
  # saveRDS(df, file = file.path(path_resultats_perim_folder, paste0(folder_name, "_png" ,".rds")))
  
  image_type = ""
  if(stringr::str_detect(link_image, ".png$")){image_type = "png"}
  if(stringr::str_detect(link_image, ".jpg$")){image_type = "jpg"}
  
  minio_file_path = file.path("dataviz", perim, folder_name, paste0(folder_name, "_", image_type))
  
  print(minio_file_path)
  # export image
  aws.s3::s3write_using(link_image, FUN = file.copy,
                        bucket = Sys.getenv("AWS_BUCKET"), object = minio_file_path,
                        opts = list( "region" = ""))
  
  minio_title_path = file.path("dataviz", perim, folder_name, paste0(folder_name, "_", image_type, "_title"))
 
  # export image title
  aws.s3::s3write_using(df, FUN = saveRDS,
                        bucket = Sys.getenv("AWS_BUCKET"), object = minio_title_path,
                        opts = list( "region" = ""))
  
}

