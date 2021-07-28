
link_data = "C:/Users/XLAPDO/Desktop/app/data/resultats"

# périmètre du graphique
perimetre = "FR"

# nom de la variable
folder_name = "CB_transaction_vente"

# nom du fichier
file_name = paste0(folder_name, ".png")

# titre du graphique
title = "Transactions par carte bancaire"

link_image = file.path(link_data, perimetre, folder_name, file_name)

export_minio_image(link_image = link_image,
           perim = perimetre,
           folder_name = folder_name,
           title = title)











