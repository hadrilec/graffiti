
link_app =  "C:/Users/XLAPDO/Desktop/app/dataviz_conj"

if(file.exists(link_app)){
  setwd(link_app)
  link_app_code = file.path(link_app, "code")
}else{
  link_app_code = file.path("./", "code")
}

link_results_ =  "./data/resultats"

file_data_update = file.path(link_results_, "_files",
                                  paste0("data_update", ".rds"))

file_data_update_date = file.path(link_results_, "_files",
                             paste0("data_update_", gsub("-|:| |CET","", Sys.time()),".rds"))

source("./function/librairies.R")
source("./function/api_key.R")
source("./function/export_graph.R")
source("./function/pRev_DB_var_names.R")

list_R_files = list.files(link_app_code, recursive = T)

df_data_update = 
  data.frame(file_ = list_R_files) %>% 
  separate(file_, into = c("perim", "file"), sep = "/") %>% 
  as.data.frame()

# 
# run all scripts
# 

for (file_ in list_R_files){
  
  file_short = basename(file_)
  link_file = file.path(link_app_code, file_)
  
  run_file = try(source(link_file))
  
  if(class(run_file) != "try-error"){
    check = "OK"
  }else{
    check = run_file
  }
  i_file = which(df_data_update[,"file"] == file_short)
  df_data_update[i_file, "check"] = check
}

saveRDS(df_data_update , file = file_data_update)
saveRDS(df_data_update , file = file_data_update_date)

# MAJ catalogue/dico variables
update_DB_variable()


