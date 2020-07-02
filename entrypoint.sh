#!/bin/bash
echo " \
\n# Configure aws \
\nAWS_S3_ENDPOINT=${AWS_S3_ENDPOINT} \
\nAWS_ACCESS_KEY_ID = ${AWS_S3_ENDPOINT} \
\nAWS_SECRET_ACCESS_KEY = ${AWS_S3_ENDPOINT} \
\nAWS_DEFAULT_REGION = ${AWS_S3_ENDPOINT} 
" >> /usr/local/lib/R/etc/Renviron.site \
exec @$