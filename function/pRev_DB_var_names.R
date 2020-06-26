library(dplyr)
library(stringr)

link_results_ =  "./data/resultats"
# file_DB_path_ = file.path(link_results_, "_files", "DB_variables.RData")
file_DB_path_ = file.path(link_results_, "_files", "DB_variables.rds")
# load(file_DB_path)
update_DB_variable <- function(perim = 'all', 
                               file_DB_path = file_DB_path_,
                               link_results = link_results_){
  print("update DB")
  
  list_folders = list.files(link_results)
  if(length(grep("_" , list_folders))>0){
    list_folders = list_folders[-grep("_" , list_folders)]
  }
  
  if(perim != "all" & (perim %in% list_folders)){
    list_folders = c(perim)
    if(file.exists(file_DB_path)){
      load(file_DB_path)
      DB_variables_old = DB_variables %>% 
        filter(perim == perim)
    }
  }
  
  list_df = list()
  
  for(folder in list_folders){
    print(folder)
    path_folder = file.path(link_results, folder)
    list_variables = list.files(path_folder)
    
    for(var in list_variables){
      print(var)
      path_folder_var = file.path(path_folder, var)
      var_title = ""
      gg_run_time <- ""
      gg_code_file <- ""
      
      if(file.exists(path_folder_var)){
        var_ = str_replace_all(var,"e\\(" ,"")
        patt =  paste0(c(paste0(var_, "_estim.RData"), paste0(var_, ")_estim.RData")) ,collapse = "|")
        
        estim_file = list.files(path_folder_var, pattern = patt)
        prl_file = list.files(path_folder_var, "_prl.RData")
        # gg_plot_file = list.files(path_folder_var,  "_gg_plot.RData")
        gg_plot_file = list.files(path_folder_var, 
                                  pattern = paste0(c("_gg_plot.rds","_gg_html.rds"),  collapse = "|"))
        
        png_metadata_file = list.files(path_folder_var,  pattern = "_png.rds$")
        
        if(length(estim_file) > 0){
          estim_file_path = file.path(path_folder_var, estim_file)
          
          if(length(estim_file_path) > 1){
            estim_file_path = estim_file_path[1]
          }
          
          if(file.exists(estim_file_path)){
            load(estim_file_path) 
            
            if(exists("reslist")){
              if(!is.null( reslist$title)){
                if(reslist$title != ""){
                  var_title = reslist$title
                }
              }
            }
          }
        }else if(length(prl_file) > 0){
          prl_file_path = file.path(path_folder_var, prl_file)
          if(length(prl_file_path) > 1){
            prl_file_path = prl_file_path[1]
          }
          if(file.exists(prl_file_path)){
            load(prl_file_path) 
            var_title <- gg$plot_env$title
            
            gg_run_time <- gg$run_time
            if(is.null(gg_run_time)){gg_run_time <- ""}
            
            gg_code_file <- gg$link_code_file
            if(is.null(gg_code_file)){gg_code_file <- ""}
              
            if(is.null(var_title)){
              var_title = gg$labels$title
              if(is.null(var_title)){
                var_title = ""
              }
            }
          }
        }else if(length(gg_plot_file) > 0){
          gg_plot_file_path = file.path(path_folder_var, gg_plot_file)
          
          if(length(gg_plot_file_path) > 1){
            gg_plot_file_path = gg_plot_file_path[1]
          }
          
          if(file.exists(gg_plot_file_path)){
            # load(gg_plot_file_path) 
            gg = readRDS(gg_plot_file_path)
            gg_run_time <- gg$run_time
            if(is.null(gg_run_time)){gg_run_time <- ""}
            
            gg_code_file <- gg$link_code_file
            if(is.null(gg_code_file)){gg_code_file <- ""}
            
            var_title = gg$labels$title
            
            if("highchart" %in% class(gg)){
              var_title = gg[["x"]][["hc_opts"]][["title"]][["text"]]
            }
            
            if(is.null(var_title)){
              var_title = ""
            }
            
          }
          
        }
        
        if(length(png_metadata_file) > 0){
          png_metadata = readRDS(file.path(path_folder_var, png_metadata_file))
          var_title = png_metadata[1, "title"]
          gg_code_file = png_metadata[1, "file"]
        }
        
      }
      list_df[[length(list_df)+1]] = data.frame(perim = as.character(folder),
                                                var = as.character(var),
                                                title = as.character(var_title),
                                                run_time = as.numeric(as.character(gg_run_time)),
                                                file = as.character(gg_code_file),
                                                stringsAsFactors = FALSE)
    }
  }
  
  if(perim != "all" & (perim %in% list_folders) & file.exists(file_DB_path)){
    DB_variables = DB_variables_old %>% bind_rows(list_df)
  }else{
    DB_variables = bind_rows(list_df)
  }
  
  
  DB_variables_notitle = DB_variables %>% 
    filter(!str_detect(var, "\\(")) %>% 
    filter(sapply(1:nrow(.), function(i) grepl(var[i], title[i]) | title[i] == "")) %>% 
    mutate(title = var) %>% 
    arrange(title)
  
  DB_variables = DB_variables %>% 
    filter(!str_detect(var, "\\(")) %>% 
    filter(!sapply(1:nrow(.), function(i) grepl(var[i], title[i]) | title[i] == "")) %>% 
    arrange(title) %>% 
    bind_rows(DB_variables_notitle) 
  
  # save(DB_variables , file = file_DB_path)
  
  saveRDS(DB_variables , file = file_DB_path)
  # return(DB_variables)
}


# update_DB_variable()
# DB_variable = readRDS(file_DB_path_)
