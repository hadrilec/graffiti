#!/bin/bash

#R_location = $(which R)
echo "folder : $PWD"
ls -ltr
chmod +x /function/data_update.R
./function/data_update.R