library(tidyverse)
library(aws.s3)


update_DB_variable <- function(){
  
  data_in_DB = aws.s3::get_bucket("groupe-1360", use_https = F, region = "")
  
  dataf = do.call(c, unlist(data_in_DB, recursive = FALSE))
  contents_key = as.character(unlist(dataf[which(names(dataf) == "Contents.Key")]))
  
  minio_path_selected = contents_key[stringr::str_detect(contents_key, "^dataviz/")]
  minio_path_selected = minio_path_selected[!stringr::str_detect(minio_path_selected, "DB_variable\\/DB_variable")]
  
  contents_key_dataviz = 
    data.frame(minio_path = minio_path_selected,
               stringsAsFactors = F) %>%
    tidyr::separate(minio_path,
                    into = c("main", "perim", "var", "object"),
                    sep = "/", remove = F)
  
  DB_variable = contents_key_dataviz
  
  for(row_id in 1:nrow(contents_key_dataviz)){
    
    obj_name = contents_key_dataviz %>% 
      slice(row_id) %>% 
      pull(minio_path) %>% 
      as.character()
    
    if(stringr::str_detect(obj_name, "_gg_plot$|_png_title$|_jpg_title$")){
      minio_obj = try(aws.s3::s3read_using(FUN = readRDS,
                                           bucket = "groupe-1360",
                                           object = obj_name,
                                           opts = list("use_https" = F, "region" = "")))
      var_title <- ""
      gg_run_time <- ""
      gg_code_file <- ""
      
      if(!"try-error" %in% class(minio_obj)){
        
        if("ggplot" %in% class(minio_obj) | "highchart" %in% class(minio_obj)){
          gg = minio_obj
          gg_run_time <- gg$run_time
          
          if(is.null(gg_run_time)){gg_run_time <- ""}
          
          gg_code_file <- gg$link_code_file
          if(is.null(gg_code_file)){gg_code_file <- ""}
          
          if("ggplot" %in% class(gg)){
            var_title = gg$labels$title
          }
          
          if("highchart" %in% class(gg)){
            var_title = gg[["x"]][["hc_opts"]][["title"]][["text"]]
          }
          
          if(is.null(var_title)){
            var_title = ""
          }
          DB_variable[row_id, "title"] = var_title
          DB_variable[row_id, "run_time"] = as.numeric(as.character(gg_run_time))
          DB_variable[row_id, "file"] = as.character(gg_code_file)
        }
        
        if(stringr::str_detect(obj_name, "_png_title$|_jpg_title$")){
          if("data.frame" %in% class(minio_obj)){
            row_id_targeted = which(DB_variable[, "minio_path"] == gsub("_title", "", obj_name))
            if(length(row_id_targeted) > 0){
              DB_variable[row_id_targeted, "title"] = as.character(minio_obj[1,"title"])
              DB_variable[row_id_targeted, "run_time"] = 0
              DB_variable[row_id_targeted, "file"] = ""
            }
          }
        }
      }
    }
  }
  
  DB_variable = DB_variable %>% 
    tidyr::drop_na()
  
  aws.s3::s3write_using(DB_variable, FUN = saveRDS,
                bucket = "groupe-1360", object = "dataviz/DB_variable/DB_variable",
                opts = list("use_https" = F, "region" = ""))
  
  return(DB_variable)
  
}

# DB_variable = update_DB_variable()
