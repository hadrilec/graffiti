print(commandArgs())

cat(getwd(), file = stderr())

link_app =  "C:/Users/XLAPDO/Desktop/app/dataviz_conj"

if(file.exists(link_app)){
  setwd(link_app)
  link_app_code = file.path(link_app, "code")
}else{
  link_app_code = file.path("./", "code")
}

try(source("./function/librairies.R"))
try(source("./function/api_key.R"))

source("./function/readSDMX2.R")
source("./function/export_minio_graph.R")
source("./function/export_minio_image.R")
source("./function/update_DB_variable.R")

list_R_files = data.frame(file = unlist(get_minio_all()), stringsAsFactors = F)
list_R_files = list_R_files %>% 
  filter(str_detect(file, "_code$"))

tempfile_ <- tempfile()

for(i in 1:nrow(list_R_files)){
  file_dwn = paste0(tempfile_, i)
  
  aws.s3::save_object(list_R_files[i,1], 
                      file = file_dwn,
                      bucket = "groupe-1360", use_https = F, region = "")
  
}

list_downloaded_file = paste0(tempfile_, 1:nrow(list_R_files))

df_downloaded_file = data.frame(downloaded_file = list_downloaded_file,
                                file = list_R_files,
                                twin_exist = NA,
                                stringsAsFactors = F)

for (ifile in 1:length(list_downloaded_file)){
  # print(ifile)
  file_to_check = list_downloaded_file[ifile]
  
  twin_file_exist = FALSE
  
  if(ifile != length(list_downloaded_file)){
    for(ifile2 in (ifile+1):length(list_downloaded_file)){
      
      file_to_compare = list_downloaded_file[ifile2]
      
      if(md5sum(file_to_check) == md5sum(file_to_compare)){
        twin_file_exist = TRUE
        break
      }
    }
  }
  df_downloaded_file[ifile,"twin_exist"] = twin_file_exist
}


# 
# run all scripts
# 

for (ifile in 1:nrow(df_downloaded_file)){
  
  file_run = df_downloaded_file[ifile,"downloaded_file"]
  file_run_name = df_downloaded_file[ifile,"file"]
  twin_exist = df_downloaded_file[ifile,"twin_exist"]
  print(file_run_name)
  # cat(file_run_name, file = stderr())
  
  if(!twin_exist){

    run_message = try(source(file_run, encoding = "UTF-8"))
    
    if(class(run_message) != "try-error"){
      check = "OK"
    }else{
      check = as.character(run_message)
    }
    
  }else{
    check = "no run"
  }
    
  df_downloaded_file[ifile,"check"] = check
}

# TODO : exporter dans minio df_downloaded_file

# MAJ catalogue/dico variables
dbVar = update_DB_variable()


