
link_data = file.path(Sys.getenv("USERPROFILE"), "Desktop")

# périmètre du graphique
perimetre = "ZE"

# nom de la variable
folder_name = "covid_gdp_comparaison"

# nom du fichier
file_name = paste0("covid_pays_comparaison", ".jpg")

# titre du graphique
title = "PIB et mesures sanitaires"

link_image = file.path(link_data, file_name)

export_image(link_image = link_image,
           perim = perimetre,
           folder_name = folder_name,
           title = title)











