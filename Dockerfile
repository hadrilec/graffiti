# Dockerfile
# Utilisation de l'image shiny "perso"
FROM docker-registry.beta.innovation.insee.eu/conjoncture1/image-dataviz-conj:latest

# Ajout de l'application
ADD ./*.R /srv/shiny-server/
ADD ./*.sh /srv/shiny-server/

# Fichier RMD
#ADD ./function/cahier.Rmd /srv/shiny-server/function/cahier.Rmd
#ADD ./function/read_code.Rmd /srv/shiny-server/function/read_code.Rmd

# duplicate app
ADD ./data/ /srv/shiny-server/data/
ADD ./code/ /srv/shiny-server/code/
ADD ./function/ /srv/shiny-server/function/

ADD ./entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]

CMD ["/usr/bin/shiny-server.sh"]

#RUN R -e "install.packages(c('highcharter'), repos='https://cran.rstudio.com/', dependencies=TRUE)"
