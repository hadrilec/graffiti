# Dockerfile
# Utilisation de l'image shiny "perso"
# FROM docker-registry.beta.innovation.insee.eu/conjoncture1/image-dataviz-conj:latest
FROM git-registry.lab.sspcloud.fr/hadrilec/graffiti-env:latest

# Ajout de l'application
ADD ./*.R /srv/shiny-server/
ADD ./*.sh /srv/shiny-server/

# Fichier RMD
#ADD ./function/cahier.Rmd /srv/shiny-server/function/cahier.Rmd
#ADD ./function/read_code.Rmd /srv/shiny-server/function/read_code.Rmd

# duplicate app
#ADD ./data/ /srv/shiny-server/data/
ADD ./code/ /srv/shiny-server/code/
ADD ./function/ /srv/shiny-server/function/
ADD ./www/ /srv/shiny-server/www/
#RUN chmod +x /function/data_update.R

#ADD ./data_update.sh /data_update.sh
#RUN chmod +x /data_update.sh

CMD ["/usr/bin/shiny-server.sh"]

#RUN R -e "install.packages(c('highcharter'), repos='https://cran.rstudio.com/', dependencies=TRUE)"
