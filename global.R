link_app =  "C:/Users/XLAPDO/Desktop/app/dataviz_conj"

if(file.exists(link_app)){
  setwd(link_app)
}

Print = function(x){
  obj_name = deparse(substitute(x))
  # print(obj_name);print(x)
  cat(file = stderr(), obj_name, ":", x, "\n")
}

source("./function/librairies.R")
source("./function/api_key.R")
# source("./function/pRev_DB_var_names.R")
source("./function/update_DB_variable.R")
source("./function/export_graph.R")
source("./function/readSDMX2.R")

pkg = installed.packages()

Print(getwd())
Print(sessionInfo()$R.version$version.string)

var_env = as.data.frame(t(as.data.frame(as.list(Sys.getenv()))))
var_env[,"var"] = as.character(rownames(var_env))
rownames(var_env) = NULL
names(var_env) = c("value", "var")
var_env = var_env[,c("var", "value")]
var_env_tbl = as_tibble(var_env)
print(var_env)
print(var_env_tbl)
#bucketlist()

bucket_data = try(get_bucket("groupe-1360", use_https = F, region = ""))
if(!"try-error" %in% class(bucket_data)){
  cat("app connected to minio", file = stderr())
}

# data_format = "RData"
data_format = "rds"

chemin_prev = ""
link_results =  "./data/resultats"

cahier_file = "./function/cahier.Rmd"
 
# DB_var_file = file.path(".", "data", "resultats", "_files", "DB_variables.rds")

# MAJ de la base de données des noms des variables toutes les heures
# if(FALSE){
#   update_DB_variable()
# }

# Sys.setenv(http_proxy="proxy-rie.http.insee.fr:8080")
# Sys.setenv(https_proxy="proxy-rie.http.insee.fr:8080")

# DB_variables = readRDS(DB_var_file)
DB_variables = update_DB_variable()

perimetre_list = 
  DB_variables %>% 
  pull(perim) %>% 
  unique() %>% 
  sort()

countries <- c("FR", "DE", "UK", "ES", "IT", "CN", "JP", "ZE", "US", "OIL", "FI", "COM", "INF")

perimetre_added = which(!perimetre_list %in% countries)
if(length(perimetre_added) > 0){
  countries = c(countries, perimetre_list[perimetre_added])
}

# 
# COUNTRI FLAG
# 

flags_id_list = c("fr", "de", "gb", "es", "it",
                         "cn", "jp", "eu", "us")

flags_label_list = c("France", "Allemagne", "Royaume-Uni", "Espagne", "Italie",
                  "Chine", "Japon", "Zone Euro", "Etats-Unis")

icons_perims = list()

for(pays in 1:length(flags_id_list)){
  pays_name_id = flags_id_list[pays]
  pays_name_label = flags_label_list[pays]
  icons_perims[[length(icons_perims)+1]] = paste(flag(pays_name_id, size = 15),
                                                 " ", pays_name_label)
}
icons_perims = unlist(icons_perims)

icons_perims = c(icons_perims,
                 paste(as.character(icon("gas-pump")), " " ,"Pétrole"),
                 paste(as.character(icon("money-check-alt")), " " ,"Finance"),
                       paste(as.character(icon("globe-americas")), " " ,"Commerce"),
                 paste(as.character(icon("shopping-cart")), " " ,"Inflation"))

