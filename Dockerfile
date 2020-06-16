# Dockerfile
# Utilisation de l'image shiny "perso"
FROM docker-registry.beta.innovation.insee.eu/xlapdo/image_dataviz_conj:latest

# Ajout de l'application
ADD ./*.R /srv/shiny-server/

# Fichier RMD
ADD ./cahier.Rmd /srv/shiny-server/cahier.Rmd

# Données
ADD ./data/* /srv/shiny-server/data/
