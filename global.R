link_app =  "C:/Users/XLAPDO/Desktop/app/dataviz_conj"

options(warn = -1)

if(file.exists(link_app)){
  setwd(link_app)
}

Print = function(x){
  obj_name = deparse(substitute(x))
  # print(obj_name);print(x)
  cat(file = stderr(), obj_name, ":", x, "\n")
}

Sys.unsetenv("AWS_SESSION_TOKEN")

Print(Sys.getenv("AWS_ACCESS_KEY_ID"))
Print(Sys.getenv("AWS_S3_ENDPOINT"))

# stop("test")

source("./function/librairies.R")
source("./function/api_key.R")
source("./function/update_DB_variable.R")
source("./function/export_minio_graph.R")
source("./function/export_minio_image.R")
source("./function/readSDMX2.R")
source("./function/add_style.R")
source("./function/gg_plotly.R")

main_folder = rappdirs::user_data_dir()
insee_main_folder = file.path(main_folder, "insee")
insee_folder = file.path(insee_main_folder, "insee")

list_folders = c(main_folder, insee_main_folder, insee_folder)
for(f in list_folders){
  if (!file.exists(f)){
    dir.create(f)
    cat(sprintf("folder creation : %s", f), file = stderr())
  }else{
    cat(sprintf("folder exists : %s", f), file = stderr())
  }
}
# stop("test")

# idbank_list_title_file = "./data/idbank_list_title.RData"
# idbank_list_title = readRDS(idbank_list_title_file)

pkg = installed.packages()

Print(getwd())
Print(sessionInfo()$R.version$version.string)

var_env = as.data.frame(t(as.data.frame(as.list(Sys.getenv()))))
var_env[,"var"] = as.character(rownames(var_env))
rownames(var_env) = NULL
names(var_env) = c("value", "var")
var_env = var_env[,c("var", "value")]
var_env_tbl = as_tibble(var_env)
# print(var_env)
# print(var_env_tbl)

bucket_data = try(get_bucket(Sys.getenv("AWS_BUCKET"), use_https = T, region = ""))

if(!"try-error" %in% class(bucket_data)){
  cat("app connected to minio", file = stderr())
}

# data_format = "RData"
data_format = "rds"

chemin_prev = ""
link_results =  "./data/resultats"

if(!file.exists(link_app)){
  path_global = Sys.getenv("PATH")
  if(!str_detect(path_global, "pandoc")){
    Sys.setenv(PATH = paste0(path_global, ";", "usr/bin/pandoc/"))
  }
  Sys.setenv(RSTUDIO_PANDOC = "usr/bin/pandoc/")
}

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

if("insee" %in% pkg[1,]){
  insee_dataset =
    get_dataset_list() %>%
    drop_na() %>%
    mutate(name = paste(paste0("FR-",id), ":", Name.fr)) %>%
    pull(name) %>%
    gsub("'|,", "", .) %>%
    paste0(collapse = "','")

  insee_dataset_id =
    get_dataset_list() %>%
    drop_na() %>%
    mutate(name = paste(paste0("FR-",id))) %>%
    pull(name) %>%
    gsub("'|,", "", .) %>%
    paste0(collapse = "','")

}


id = insee:::idbank_list_internal

idbank_list = insee::get_idbank_list()
idbank_list_all = id$idbank

id_fr = id %>%
  select(nomflow, idbank, cleFlow, title_fr, dplyr::starts_with("dim")) %>% 
  dplyr::rename(title = title_fr)

idbank_list_all_label_fr = paste(id$idbank, ":", id$title_fr)

dataset_list = insee::get_dataset_list()

dataset_list_selectize =
  dataset_list %>%
  dplyr::filter(id %in% unique(idbank_list$nomflow))

dataset_list_id = dataset_list_selectize$id

dataset_list_selectize_fr = paste(dataset_list_selectize$id, ":", dataset_list_selectize$Name.fr)


insee_dt_id =
  c(
    'FR-BALANCE-PAIEMENTS',
    'FR-CHOMAGE-TRIM-NATIONAL',
    'FR-CLIMAT-AFFAIRES',
    'FR-CNA-2010-CONSO-MEN',
    'FR-CNA-2010-CONSO-SI',
    'FR-CNA-2010-CPEB',
    'FR-CNA-2010-CSI',
    'FR-CNA-2010-DEP-APU',
    'FR-CNA-2010-DETTE-APU',
    'FR-CNA-2010-EMPLOI',
    'FR-CNA-2010-ERE',
    'FR-CNA-2010-FBCF-BRANCHE',
    'FR-CNA-2010-FBCF-SI',
    'FR-CNA-2010-PAT-NF',
    'FR-CNA-2010-PIB',
    'FR-CNA-2010-RDB',
    'FR-CNA-2010-TEI',
    'FR-CNA-2010-TOF',
    'FR-CNA-2014-CONSO-MEN',
    'FR-CNA-2014-CONSO-SI',
    'FR-CNA-2014-CPEB',
    'FR-CNA-2014-CSI',
    'FR-CNA-2014-DEP-APU',
    'FR-CNA-2014-DETTE-APU',
    'FR-CNA-2014-EMPLOI',
    'FR-CNA-2014-ERE',
    'FR-CNA-2014-FBCF-BRANCHE',
    'FR-CNA-2014-FBCF-SI',
    'FR-CNA-2014-PAT-NF',
    'FR-CNA-2014-PIB',
    'FR-CNA-2014-RDB',
    'FR-CNA-2014-TEI',
    'FR-CNA-2014-TOF',
    'FR-CNT-2010-CB',
    'FR-CNT-2010-CSI',
    'FR-CNT-2010-OPERATIONS',
    'FR-CNT-2010-PIB-EQB-RF',
    'FR-CNT-2014-CB',
    'FR-CNT-2014-CSI',
    'FR-CNT-2014-OPERATIONS',
    'FR-CNT-2014-PIB-EQB-RF',
    'FR-COEFF-EURO-FRANC',
    'FR-COM-EXT',
    'FR-COMPTES-ETAT',
    'FR-CONSO-MENAGES-2010',
    'FR-CONSO-MENAGES-2014',
    'FR-CONSTRUCTION-LOCAUX',
    'FR-CONSTRUCTION-LOGEMENTS',
    'FR-CREATIONS-ENTREPRISES',
    'FR-DECES-MORTALITE',
    'FR-DEFAILLANCES-ENTREPRISES',
    'FR-DEMANDES-EMPLOIS-NATIONALES',
    'FR-DETTE-NEGOCIABLE-ETAT',
    'FR-DETTE-TRIM-APU',
    'FR-DETTE-TRIM-APU-2014',
    'FR-EMPLOI-BIT-TRIM',
    'FR-EMPLOI-SALARIE-TRIM-NATIONAL',
    'FR-ENQ-CONJ-ACT-IND',
    'FR-ENQ-CONJ-ART-BAT',
    'FR-ENQ-CONJ-COM-DET',
    'FR-ENQ-CONJ-COM-GROS',
    'FR-ENQ-CONJ-IND-BAT',
    'FR-ENQ-CONJ-INV-IND',
    'FR-ENQ-CONJ-MENAGES',
    'FR-ENQ-CONJ-PROMO-IMMO',
    'FR-ENQ-CONJ-SERV',
    'FR-ENQ-CONJ-TP',
    'FR-ENQ-CONJ-TRES-IND',
    'FR-ERI-ACTIVITE-PARTIELLE',
    'FR-IC-PROD-CONS-2010',
    'FR-IC-PROD-CONS-2015',
    'FR-ICA-2005-COM-SERV',
    'FR-ICA-2005-EMAGSA',
    'FR-ICA-2005-IND-CONS',
    'FR-ICA-2010-COMMERCE',
    'FR-ICA-2010-EMAGSA',
    'FR-ICA-2010-IND-CONS',
    'FR-ICA-2010-SERVICE',
    'FR-ICA-2015-COMMERCE',
    'FR-ICA-2015-EMAGSA',
    'FR-ICA-2015-IND-CONS',
    'FR-ICA-2015-SERVICES',
    'FR-ICHT-2008',
    'FR-ICT-2012',
    'FR-ICT-2016',
    'FR-ILC-ILAT-ICC',
    'FR-INDEX-BT-TP-DIV-ANCIENNES-BASES',
    'FR-INDEX-BT-TP-DIV-IM-2010',
    'FR-INDICE-TRAITEMENT-FP',
    'FR-INDICES_LOYERS',
    'FR-IP-PROD-CONS-N-HAB-2010',
    'FR-IP-PROD-CONS-N-HAB-2015',
    'FR-IPAGRI',
    'FR-IPAGRI-BASE-2015',
    'FR-IPC-1970-1980',
    'FR-IPC-1990',
    'FR-IPC-1998',
    'FR-IPC-2015',
    'FR-IPC-PM-2015',
    'FR-IPCH-2005',
    'FR-IPCH-2015',
    'FR-IPEA-2010',
    'FR-IPEA-2015',
    'FR-IPGD-1998',
    'FR-IPGD-2015',
    'FR-IPI-1990',
    'FR-IPI-2005',
    'FR-IPI-2010',
    'FR-IPI-2015',
    'FR-IPLA-IPLNA-2010',
    'FR-IPLA-IPLNA-2015',
    'FR-IPPI-2005',
    'FR-IPPI-2010',
    'FR-IPPI-2015',
    'FR-IPPMP',
    'FR-IPPMP-NF',
    'FR-IPPS-2010',
    'FR-IPPS-2015',
    'FR-IPS-2015-SERVICES',
    'FR-IRL',
    'FR-MARIAGES-NUPTIALITE-PACS-DIVORCES',
    'FR-NAISSANCES-FECONDITE',
    'FR-ODD-CONSOMMATION-PRODUCTION',
    'FR-ODD-EAU-PROPRE-ASSAINISSEMENT',
    'FR-ODD-EDUCATION-QUALITE',
    'FR-ODD-EGALITE-SEXES',
    'FR-ODD-ENERGIE-PROPRE-COUT-ABORDABLE',
    'FR-ODD-ERADICATION-PAUVRETE',
    'FR-ODD-INFRASTRUCTURES-RESILIENTES-INNOVATION',
    'FR-ODD-LUTTE-CHANGEMENT-CLIMATIQUE',
    'FR-ODD-PAIX-JUSTICE-INSTITUTIONS-EFFICACES',
    'FR-ODD-PARTENARIATS-REALISATION',
    'FR-ODD-REDUCTION-INEGALITES',
    'FR-ODD-SANTE-BIEN-ETRE',
    'FR-ODD-SECURITE-ALIMENTAIRE-AGRICULTURE-DURABLE',
    'FR-ODD-TRAVAIL-CROISSANCE',
    'FR-ODD-VIE-AQUATIQUE',
    'FR-ODD-VIE-TERRESTRE',
    'FR-ODD-VILLES-COMMUNAUTES-DURABLES',
    'FR-PARC-LOGEMENTS',
    'FR-POPULATION-STRUCTURE',
    'FR-REVALORISATION-PENSIONS',
    'FR-SALAIRES-ACEMO',
    'FR-SALAIRES-ACEMO-2017',
    'FR-SALAIRES-ANNUELS',
    'FR-SERIES_BDM',
    'FR-SERIES_LOYERS',
    'FR-SMIC-COTISATIONS',
    'FR-TAUX-CHOMAGE',
    'FR-TCRED-AGRICULTURE-CHEP-AGRI',
    'FR-TCRED-AGRICULTURE-EXP-AGRI',
    'FR-TCRED-AGRICULTURE-OCC-SOL',
    'FR-TCRED-AGRICULTURE-PROD-VEG',
    'FR-TCRED-CONDITIONSDEVIE-APE-PEC',
    'FR-TCRED-CONDITIONSDEVIE-LICENCES-SPORTIVES',
    'FR-TCRED-CONDITIONSDEVIE-LOG-SOC',
    'FR-TCRED-CONDITIONSDEVIE-NMU-MOH',
    'FR-TCRED-CONDITIONSDEVIE-TXP-CDE',
    'FR-TCRED-CONSTRUCTION-LOCAUX',
    'FR-TCRED-ECONOMIE-DEP-REC',
    'FR-TCRED-ECONOMIE-PIB-REG',
    'FR-TCRED-ECONOMIE-VAB-REG',
    'FR-TCRED-EDUCATION-APP-JEN',
    'FR-TCRED-EDUCATION-DIPLOMES-TECHNIQUES',
    'FR-TCRED-EDUCATION-EFF-PSS',
    'FR-TCRED-EDUCATION-ENS-PSD',
    'FR-TCRED-EDUCATION-ETAB-SCOL',
    'FR-TCRED-EDUCATION-REU-BAC',
    'FR-TCRED-EMPLOI-31-DECEMBRE',
    'FR-TCRED-EMPLOI-SALARIE-TRIM',
    'FR-TCRED-ENTREPRISES-EFF-OPE',
    'FR-TCRED-ENTREPRISES-EMP-SAL-AN-TAILLE',
    'FR-TCRED-ESTIMATIONS-POPULATION',
    'FR-TCRED-SALAIRES-REVENUS-MEN',
    'FR-TCRED-SALAIRES-REVENUS-RET-BEN',
    'FR-TCRED-SALAIRES-REVENUS-REV-SAL-SEXE-CS',
    'FR-TCRED-SALAIRES-REVENUS-SAM-SEX-CSP',
    'FR-TCRED-SALAIRES-REVENUS-STRUCTURE-RDB',
    'FR-TCRED-SALAIRES-REVENUS-TAUX-PAUVRETE-AGE',
    'FR-TCRED-SALAIRES-REVENUS-TNB',
    'FR-TCRED-SANTE-ACCUEIL-PERS-AGEES',
    'FR-TCRED-SANTE-CAUSES-DECES',
    'FR-TCRED-SANTE-EQUIP-HOP',
    'FR-TCRED-SANTE-EQUIP-SPE',
    'FR-TCRED-SANTE-PERSONNELS',
    'FR-TCRED-SERVICES-TOURISME-TRANSPORTS-IMM-VN',
    'FR-TCRED-SERVICES-TOURISME-TRANSPORTS-RR',
    'FR-TCRED-TRAVAIL-EMPLOI-EFF-FP-RATIO',
    'FR-TCRED-TRAVAIL-EMPLOI-EFF-FPE',
    'FR-TCRED-TRAVAIL-EMPLOI-EFF-FPH',
    'FR-TCRED-TRAVAIL-EMPLOI-EFF-FPT',
    'FR-TCRED-TRAVAIL-EMPLOI-TCHOMA-SA',
    'FR-TOURISME-FRANCE-METHODE-REDRESSEMENT-2019',
    'FR-TRANSPORTS',
    'FR-VOV-2015-COMMERCE'
  )

insee_dt = c(
  'FR-BALANCE-PAIEMENTS : Balance des paiements',
  'FR-CHOMAGE-TRIM-NATIONAL : Chômage taux de chômage par sexe et âge (sens BIT)',
  'FR-CLIMAT-AFFAIRES : Indicateurs synthétiques du climat des affaires',
  'FR-CNA-2010-CONSO-MEN : Consommation des ménages',
  'FR-CNA-2010-CONSO-SI : Dépenses de consommation finale par secteur institutionnel',
  'FR-CNA-2010-CPEB : Comptes de production et d’exploitation par branche',
  'FR-CNA-2010-CSI : Comptes des secteurs institutionnels',
  'FR-CNA-2010-DEP-APU : Dépenses des administrations publiques',
  'FR-CNA-2010-DETTE-APU : Dette et déficit des administrations publiques au sens de Maastricht',
  'FR-CNA-2010-EMPLOI : Emploi intérieur durée effective travaillée et productivité horaire',
  'FR-CNA-2010-ERE : Équilibre ressources-emplois (ERE)',
  'FR-CNA-2010-FBCF-BRANCHE : Formation brute de capital fixe (FBCF) par branche',
  'FR-CNA-2010-FBCF-SI : Formation brute de capital fixe (FBCF) par secteur institutionnel',
  'FR-CNA-2010-PAT-NF : Comptes de patrimoine non financier',
  'FR-CNA-2010-PIB : Produit intérieur brut (PIB) et ses composantes',
  'FR-CNA-2010-RDB : Revenu et pouvoir d’achat des ménages',
  'FR-CNA-2010-TEI : Tableau des entrées intermédiaires',
  'FR-CNA-2010-TOF : Comptes financiers',
  'FR-CNA-2014-CONSO-MEN : Consommation des ménages',
  'FR-CNA-2014-CONSO-SI : Dépenses de consommation finale par secteur institutionnel',
  'FR-CNA-2014-CPEB : Comptes de production et d’exploitation par branche',
  'FR-CNA-2014-CSI : Comptes des secteurs institutionnels',
  'FR-CNA-2014-DEP-APU : Dépenses des administrations publiques',
  'FR-CNA-2014-DETTE-APU : Dette et déficit des administrations publiques au sens de Maastricht',
  'FR-CNA-2014-EMPLOI : Emploi intérieur durée effective travaillée et productivité horaire',
  'FR-CNA-2014-ERE : Équilibre ressources-emplois (ERE)',
  'FR-CNA-2014-FBCF-BRANCHE : Formation brute de capital fixe (FBCF) par branche',
  'FR-CNA-2014-FBCF-SI : Formation brute de capital fixe (FBCF) par secteur institutionnel',
  'FR-CNA-2014-PAT-NF : Comptes de patrimoine non financier',
  'FR-CNA-2014-PIB : Produit intérieur brut (PIB) et ses composantes',
  'FR-CNA-2014-RDB : Revenu et pouvoir d’achat des ménages',
  'FR-CNA-2014-TEI : Tableau des entrées intermédiaires',
  'FR-CNA-2014-TOF : Comptes financiers',
  'FR-CNT-2010-CB : Comptes des branches',
  'FR-CNT-2010-CSI : Comptes de secteurs institutionnels',
  'FR-CNT-2010-OPERATIONS : Opérations sur biens et services',
  'FR-CNT-2010-PIB-EQB-RF : Équilibre du produit intérieur brut',
  'FR-CNT-2014-CB : Comptes des branches',
  'FR-CNT-2014-CSI : Comptes de secteurs institutionnels',
  'FR-CNT-2014-OPERATIONS : Opérations sur biens et services',
  'FR-CNT-2014-PIB-EQB-RF : Équilibre du produit intérieur brut',
  'FR-COEFF-EURO-FRANC : Coefficient de transformation de la monnaie',
  'FR-COM-EXT : Commerce extérieur de la France',
  'FR-COMPTES-ETAT : Budget de l’État',
  'FR-CONSO-MENAGES-2010 : Consommation des ménages en biens',
  'FR-CONSO-MENAGES-2014 : Consommation des ménages en biens',
  'FR-CONSTRUCTION-LOCAUX : Construction de locaux non résidentiels',
  'FR-CONSTRUCTION-LOGEMENTS : Construction de logements',
  'FR-CREATIONS-ENTREPRISES : Créations d’entreprises',
  'FR-DECES-MORTALITE : Décès et mortalité',
  'FR-DEFAILLANCES-ENTREPRISES : Défaillances d’entreprises',
  'FR-DEMANDES-EMPLOIS-NATIONALES : Demandeurs demploi inscrits à Pôle Emploi',
  'FR-DETTE-NEGOCIABLE-ETAT : Dette négociable de l’État',
  'FR-DETTE-TRIM-APU : Dette des administrations publiques au sens de Maastricht',
  'FR-DETTE-TRIM-APU-2014 : Dette des administrations publiques au sens de Maastricht',
  'FR-EMPLOI-BIT-TRIM : Emploi activité sous-emploi par secteur d’activité (sens BIT)',
  'FR-EMPLOI-SALARIE-TRIM-NATIONAL : Estimations demploi salarié par secteur dactivité',
  'FR-ENQ-CONJ-ACT-IND : Conjoncture dans l’industrie',
  'FR-ENQ-CONJ-ART-BAT : Conjoncture dans l’artisanat du bâtiment',
  'FR-ENQ-CONJ-COM-DET : Conjoncture dans le commerce de détail et le commerce et la réparation automobile',
  'FR-ENQ-CONJ-COM-GROS : Conjoncture dans le commerce de gros',
  'FR-ENQ-CONJ-IND-BAT : Conjoncture dans lindustrie du bâtiment',
  'FR-ENQ-CONJ-INV-IND : Investissements dans l’industrie',
  'FR-ENQ-CONJ-MENAGES : Conjoncture auprès des ménages',
  'FR-ENQ-CONJ-PROMO-IMMO : Conjoncture dans la promotion immobilière',
  'FR-ENQ-CONJ-SERV : Conjoncture dans les services',
  'FR-ENQ-CONJ-TP : Conjoncture dans les travaux publics',
  'FR-ENQ-CONJ-TRES-IND : Enquête de conjoncture de trésorerie dans lindustrie',
  'FR-ERI-ACTIVITE-PARTIELLE : TdB - ERI - Activité partielle dans les établissements',
  'FR-IC-PROD-CONS-2010 : Indices des coûts de production dans la construction',
  'FR-IC-PROD-CONS-2015 : Indices des coûts de production dans la construction',
  'FR-ICA-2005-COM-SERV : Indice de chiffre daffaires dans le commerce et les services',
  'FR-ICA-2005-EMAGSA : Indice de chiffre daffaire dans les grandes surfaces alimentaires',
  'FR-ICA-2005-IND-CONS : Indice de chiffre daffaire dans lindustrie et la construction',
  'FR-ICA-2010-COMMERCE : Volume des ventes dans le commerce',
  'FR-ICA-2010-EMAGSA : Indice de chiffre d’affaires dans les grandes surfaces alimentaires',
  'FR-ICA-2010-IND-CONS : Indice de chiffre d’affaires dans l’industrie et la construction',
  'FR-ICA-2010-SERVICE : Indice de production dans les services',
  'FR-ICA-2015-COMMERCE : Indices de chiffre daffaires dans le commerce',
  'FR-ICA-2015-EMAGSA : Indices de chiffre daffaires dans les grandes surfaces alimentaires',
  'FR-ICA-2015-IND-CONS : Indices de chiffre daffaires dans lindustrie et la construction',
  'FR-ICA-2015-SERVICES : Indices de chiffre daffaires dans les services',
  'FR-ICHT-2008 : Indice du coût horaire du travail',
  'FR-ICT-2012 : Indice du coût du travail',
  'FR-ICT-2016 : Indice du coût du travail',
  'FR-ILC-ILAT-ICC : Indices pour la révision d’un bail commercial ou professionnel',
  'FR-INDEX-BT-TP-DIV-ANCIENNES-BASES : Index bâtiment travaux publics et divers de la construction - Anciennes bases',
  'FR-INDEX-BT-TP-DIV-IM-2010 : Index bâtiment travaux publics et divers de la construction',
  'FR-INDICE-TRAITEMENT-FP : Indice de traitement brut dans la fonction publique de lÉtat',
  'FR-INDICES_LOYERS : Indices des loyers - Base 2019',
  'FR-IP-PROD-CONS-N-HAB-2010 : Indice des prix de production de la construction neuve à usage d’habitation',
  'FR-IP-PROD-CONS-N-HAB-2015 : Indice des prix de production de la construction neuve à usage dhabitation',
  'FR-IPAGRI : Indices des prix dans l’agriculture',
  'FR-IPAGRI-BASE-2015 : Indices des prix dans lagriculture',
  'FR-IPC-1970-1980 : Indices des prix à la consommation',
  'FR-IPC-1990 : Indices des prix à la consommation',
  'FR-IPC-1998 : Indices des prix à la consommation',
  'FR-IPC-2015 : Indices des prix à la consommation',
  'FR-IPC-PM-2015 : Prix moyens de vente de détail',
  'FR-IPCH-2005 : Indices des prix à la consommation harmonisés',
  'FR-IPCH-2015 : Indices des prix à la consommation harmonisés',
  'FR-IPEA-2010 : Indice des prix d’entretien-amélioration des logements',
  'FR-IPEA-2015 : Indice des prix dentretien-amélioration des bâtiments',
  'FR-IPGD-1998 : Indices des prix dans la grande distribution',
  'FR-IPGD-2015 : Indice des prix dans la grande distribution',
  'FR-IPI-1990 : IPI-1990',
  'FR-IPI-2005 : Indices de la production industrielle',
  'FR-IPI-2010 : Indices de la production industrielle',
  'FR-IPI-2015 : Indice de la production industrielle',
  'FR-IPLA-IPLNA-2010 : Indices des prix des logements neufs et indices Notaires-Insee des prix des logements anciens',
  'FR-IPLA-IPLNA-2015 : Indices des prix des logements neufs et Indices Notaires-Insee des prix des logements anciens',
  'FR-IPPI-2005 : Indices de prix de production et dimportation dans lindustrie',
  'FR-IPPI-2010 : Indices de prix de production et dimportation dans lindustrie',
  'FR-IPPI-2015 : Indices de prix de production et dimportation dans lindustrie',
  'FR-IPPMP : Indices de prix et cours des matières premières',
  'FR-IPPMP-NF : Indices de prix et cours des matières premières',
  'FR-IPPS-2010 : Indice des prix de production dans les services',
  'FR-IPPS-2015 : Indice des prix de production dans les services',
  'FR-IPS-2015-SERVICES : Indices de production dans les services',
  'FR-IRL : Indice pour la révision d’un loyer d’habitation',
  'FR-MARIAGES-NUPTIALITE-PACS-DIVORCES : Mariages nuptialité et Pactes civils de solidarité (Pacs)',
  'FR-NAISSANCES-FECONDITE : Naissances et fécondité',
  'FR-ODD-CONSOMMATION-PRODUCTION : Objectif de développement durable n°12 : Consommation et production responsable',
  'FR-ODD-EAU-PROPRE-ASSAINISSEMENT : Objectif de développement durable n°6 : Eau propre et assainissement',
  'FR-ODD-EDUCATION-QUALITE : Objectif de développement durable n°4 : Education de qualité',
  'FR-ODD-EGALITE-SEXES : Objectif de développement durable n°5 : Égalité entre les sexes',
  'FR-ODD-ENERGIE-PROPRE-COUT-ABORDABLE : Objectif de développement durable n°7 : Energie propre et dun coût abordable',
  'FR-ODD-ERADICATION-PAUVRETE : Objectif de développement durable n°1 : Éradication de la pauvreté',
  'FR-ODD-INFRASTRUCTURES-RESILIENTES-INNOVATION : Objectif de développement durable n°9 : Infrastructures résilientes et innovation',
  'FR-ODD-LUTTE-CHANGEMENT-CLIMATIQUE : Objectif de développement durable n°13 : Mesures relatives à la lutte contre le changement climatique',
  'FR-ODD-PAIX-JUSTICE-INSTITUTIONS-EFFICACES : Objectif de développement durable n°16 : Paix justice et institutions efficaces',
  'FR-ODD-PARTENARIATS-REALISATION : Objectif de développement durable n°17 : Partenariats pour la réalisation des objectifs',
  'FR-ODD-REDUCTION-INEGALITES : Objectif de développement durable n°10 : Réduction des inégalités',
  'FR-ODD-SANTE-BIEN-ETRE : Objectif de développement durable n°3 : Santé et bien-être',
  'FR-ODD-SECURITE-ALIMENTAIRE-AGRICULTURE-DURABLE : Objectif de développement durable n°2 : Sécurité alimentaire et agriculture durable',
  'FR-ODD-TRAVAIL-CROISSANCE : Objectif de développement durable n°8 : Travail décent et croissance durable',
  'FR-ODD-VIE-AQUATIQUE : Objectif de développement durable n°14 : Vie aquatique',
  'FR-ODD-VIE-TERRESTRE : Objectif de développement durable n°15 : Vie terrestre',
  'FR-ODD-VILLES-COMMUNAUTES-DURABLES : Objectif de développement durable n°11 : Villes et communautés durables',
  'FR-PARC-LOGEMENTS : Estimations annuelles du parc de logements (EAPL)',
  'FR-POPULATION-STRUCTURE : Population et structure de la population',
  'FR-REVALORISATION-PENSIONS : IPC utilisés par la calculette de revalorisation des pensions alimentaires',
  'FR-SALAIRES-ACEMO : Indices trimestriels de salaires dans le secteur privé',
  'FR-SALAIRES-ACEMO-2017 : Indices trimestriels de salaires dans le secteur privé',
  'FR-SALAIRES-ANNUELS : Salaires annuels',
  'FR-SERIES_BDM : Séries chronologiques de la BDM',
  'FR-SERIES_LOYERS : Variation des loyers',
  'FR-SMIC-COTISATIONS : Taux de cotisations et salaires minima garantis',
  'FR-TAUX-CHOMAGE : Taux de chômage localisé',
  'FR-TCRED-AGRICULTURE-CHEP-AGRI : TCRED - Cheptel présent dans les exploitations agricoles',
  'FR-TCRED-AGRICULTURE-EXP-AGRI : TCRED - Exploitations agricoles par superficie',
  'FR-TCRED-AGRICULTURE-OCC-SOL : TCRED - Occupation du sol',
  'FR-TCRED-AGRICULTURE-PROD-VEG : TCRED - Productions végétales',
  'FR-TCRED-CONDITIONSDEVIE-APE-PEC : TCRED - Affaires pénales et population écrouée',
  'FR-TCRED-CONDITIONSDEVIE-LICENCES-SPORTIVES : TCRED - Licences sportives de la fédération française',
  'FR-TCRED-CONDITIONSDEVIE-LOG-SOC : TCRED - Logements sociaux',
  'FR-TCRED-CONDITIONSDEVIE-NMU-MOH : TCRED - Nombre de musées de France et de monuments historiques',
  'FR-TCRED-CONDITIONSDEVIE-TXP-CDE : TCRED - Taux de participation au premier tour des différentes élections',
  'FR-TCRED-CONSTRUCTION-LOCAUX : TCRED - Construction de locaux',
  'FR-TCRED-ECONOMIE-DEP-REC : TCRED - Dépenses et recettes des régions',
  'FR-TCRED-ECONOMIE-PIB-REG : TCRED - Produits intérieurs bruts',
  'FR-TCRED-ECONOMIE-VAB-REG : TCRED - Valeurs ajoutées brutes',
  'FR-TCRED-EDUCATION-APP-JEN : TCRED - Apprentissage chez les jeunes',
  'FR-TCRED-EDUCATION-DIPLOMES-TECHNIQUES : TCRED - Réussite aux Brevet CAP BEP et BTS',
  'FR-TCRED-EDUCATION-EFF-PSS : TCRED - Effectifs scolaires du premier et second degré et du supérieur',
  'FR-TCRED-EDUCATION-ENS-PSD : TCRED - Enseignants du premier et du second degrés',
  'FR-TCRED-EDUCATION-ETAB-SCOL : TCRED - Établissements scolaires du premier et second degré',
  'FR-TCRED-EDUCATION-REU-BAC : TCRED - Réussite aux différents baccalauréats',
  'FR-TCRED-EMPLOI-31-DECEMBRE : TCRED - Emploi au 31 décembre',
  'FR-TCRED-EMPLOI-SALARIE-TRIM : Estimations demploi salarié par secteur dactivité',
  'FR-TCRED-ENTREPRISES-EFF-OPE : TCRED - Effectifs de recherche et développement des organismes publics et des entreprises',
  'FR-TCRED-ENTREPRISES-EMP-SAL-AN-TAILLE : TCRED - Effectif salarié par taille dentreprise',
  'FR-TCRED-ESTIMATIONS-POPULATION : TCRED - Estimations de population',
  'FR-TCRED-SALAIRES-REVENUS-MEN : TCRED - Revenus disponibles des ménages',
  'FR-TCRED-SALAIRES-REVENUS-RET-BEN : TCRED - Retraités et bénéficiaires du minimum vieillesse',
  'FR-TCRED-SALAIRES-REVENUS-REV-SAL-SEXE-CS : TCRED - Revenu salarial annuel moyen selon la catégorie socioprofessionnelle et le sexe',
  'FR-TCRED-SALAIRES-REVENUS-SAM-SEX-CSP : TCRED - Salaire annuel moyen net selon la catégorie socioprofessionnelle et le sexe',
  'FR-TCRED-SALAIRES-REVENUS-STRUCTURE-RDB : TCRED - Structure du revenu disponible',
  'FR-TCRED-SALAIRES-REVENUS-TAUX-PAUVRETE-AGE : TCRED - Taux de pauvreté selon lâge',
  'FR-TCRED-SALAIRES-REVENUS-TNB : TCRED - Nombre et taux dallocataires des différentes prestations sociales',
  'FR-TCRED-SANTE-ACCUEIL-PERS-AGEES : TCRED - Accueil des personnes âgées',
  'FR-TCRED-SANTE-CAUSES-DECES : TCRED - Causes de décès',
  'FR-TCRED-SANTE-EQUIP-HOP : TCRED - Équipements hospitaliers',
  'FR-TCRED-SANTE-EQUIP-SPE : TCRED - Ratio déquipement par spécialité',
  'FR-TCRED-SANTE-PERSONNELS : TCRED - Personnels de santé',
  'FR-TCRED-SERVICES-TOURISME-TRANSPORTS-IMM-VN : TCRED - Immatriculations de véhicules neufs',
  'FR-TCRED-SERVICES-TOURISME-TRANSPORTS-RR : TCRED - Réseau routier',
  'FR-TCRED-TRAVAIL-EMPLOI-EFF-FP-RATIO : TCRED - Effectifs de la fonction publique',
  'FR-TCRED-TRAVAIL-EMPLOI-EFF-FPE : TCRED - Effectifs de la fonction publique de lÉtat',
  'FR-TCRED-TRAVAIL-EMPLOI-EFF-FPH : TCRED - Effectifs de la fonction publique hospitalière',
  'FR-TCRED-TRAVAIL-EMPLOI-EFF-FPT : TCRED - Effectifs de la fonction publique territoriale',
  'FR-TCRED-TRAVAIL-EMPLOI-TCHOMA-SA : TCRED - Taux de chômage localisés par sexe et âge en moyenne annuelle',
  'FR-TOURISME-FRANCE-METHODE-REDRESSEMENT-2019 : Fréquentation touristique (nuitées arrivées)',
  'FR-TRANSPORTS : Transports de marchandises voyageurs et autres indicateurs - Immatriculations de véhicules',
  'FR-VOV-2015-COMMERCE : Volumes des ventes dans le commerce'
)

insee_perim = data.frame(insee_dt_id = insee_dt_id, insee_dt = insee_dt, stringsAsFactors = FALSE)
