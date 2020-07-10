#!/bin/bash

#R_location = $(which R)
echo "folder : $PWD"
ls -ltr
R CMD BATCH ./function/data_update.R