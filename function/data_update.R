# print(commandArgs())

Sys.setenv(AWS_ACCESS_KEY_ID = commandArgs()[8])
Sys.setenv(AWS_SECRET_ACCESS_KEY = commandArgs()[9])
Sys.setenv(AWS_S3_ENDPOINT = "minio.stable.innovation.insee.eu")
Sys.setenv(AWS_DEFAULT_REGION = "us-east-1")
Sys.setenv(no_proxy = "minio.stable.innovation.insee.eu")

file_minio_credentials = "M:/Usuels.dsc/pRev/fonctions/minio_aws_access.R"
if(file.exists(file_minio_credentials)){
  source(file_minio_credentials)
}

cat(getwd(), file = stderr())
list_file_wd = list.files()
cat(list_file_wd, file = stderr())

link_app =  "C:/Users/XLAPDO/Desktop/app/dataviz_conj"

if(file.exists(link_app)){
  setwd(link_app)
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
                      bucket = Sys.getenv("AWS_BUCKET"), use_https = T, region = "")

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

      if(tools::md5sum(file_to_check) == tools::md5sum(file_to_compare)){
        twin_file_exist = TRUE
        break
      }
    }
  }
  df_downloaded_file[ifile,"twin_exist"] = twin_file_exist
}


#
# download all scripts
#

for (ifile in 1:nrow(df_downloaded_file)){

  file_run_name = df_downloaded_file[ifile,"file"]
  dwn_minio_file(file_run_name)

}

#
# run all scripts
#
# app_directory = getwd()

for (ifile in 1:nrow(df_downloaded_file)){

  file_run = df_downloaded_file[ifile,"downloaded_file"]
  file_run_name = df_downloaded_file[ifile,"file"]
  twin_exist = df_downloaded_file[ifile,"twin_exist"]
  # dwn_minio_file(file_run_name)
  print(sprintf("Exec : %s",file_run_name))
  # cat(file_run_name, file = stderr())



  if(!twin_exist){

    # eviter les erreurs sur le serveur, pas de liens vers des fichiers externes
    con <- file(file_run, open = 'r')
    while(TRUE) {
      line <- readLines(con, n = 1)
      
      if(length(line) == 0) break
      else if(!str_detect(line, "rm\\(|setwd\\(|source\\(")){
        write(line, file = paste0(file_run, "_clean"), append = TRUE)
      }else{
        print(sprintf("line skipped : %s", line))
      }
    }
    
    run_message = try(source(paste0(file_run, "_clean"), encoding = "UTF-8"))
    # setwd(app_directory)

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


