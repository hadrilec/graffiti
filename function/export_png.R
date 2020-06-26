export_png <- function(link_image, perim, folder_name, title){
  
  if(!file.exists(link_image)){
    stop("le fichier link_image n'existe pas")
  }
  
  path_resultats_perim = file.path(".", "data", "resultats", perim)
  
  path_resultats_perim_folder = file.path(".", "data", "resultats", perim, folder_name)
  
  for (link in c(path_resultats_perim, path_resultats_perim_folder)) {
    if (!file.exists(link)) {
      dir.create(link, "0777")
    }
  }
  
  df = data.frame(perim = perim, folder_name = folder_name, title = title, file = link_image)
  
  saveRDS(df, file = file.path(path_resultats_perim_folder, paste0(folder_name, "_png" ,".rds")))
}

