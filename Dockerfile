# Dockerfile
# Utilisation de l'image shiny "perso"
FROM docker-registry.beta.innovation.insee.eu/xlapdo/image_dataviz_conj:latest

# Ajout de l'application
ADD ./*.R /srv/shiny-server/

# Fichier RMD
#ADD ./function/cahier.Rmd /srv/shiny-server/function/cahier.Rmd
#ADD ./function/read_code.Rmd /srv/shiny-server/function/read_code.Rmd


# duplicate app
ADD ./data/ /srv/shiny-server/data/
ADD ./code/ /srv/shiny-server/code/
ADD ./function/ /srv/shiny-server/function/
