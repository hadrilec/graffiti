# Dockerfile
# Utilisation de l'image shiny "perso"
FROM docker-registry.beta.innovation.insee.eu/xlapdo/image_dataviz_conj:latest

# Ajout de l'application
ADD ./*.R /srv/shiny-server/

# Fichier RMD
ADD ./function/cahier.Rmd /srv/shiny-server/cahier.Rmd
ADD ./function/read_code.Rmd /srv/shiny-server/read_code.Rmd


# Données
ADD ./data/ /srv/shiny-server/data/
