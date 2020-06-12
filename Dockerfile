# Dockerfile
# Utilisation de l'image shiny "perso"
FROM docker-registry.beta.innovation.insee.eu/po511z/image_dataviz_conj:latest

# Ajout de l'application
ADD ./*.R /srv/shiny-server/

# Fichier MD
ADD ./cahier.md /srv/shiny-server/cahier.md

# Données
ADD ./data/* /srv/shiny-server/data/
