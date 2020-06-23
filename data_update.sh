#!/bin/bash

R_location = $(which R)
echo "folder : $R_location"

R CMD BATCH /srv/shiny-server/function/data_update.R