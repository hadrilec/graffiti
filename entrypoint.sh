#!/bin/bash
echo " \
\n# Configure aws \
\nAWS_S3_ENDPOINT=${AWS_S3_ENDPOINT} \
...
" >> /usr/local/lib/R/etc/Renviron.site \
exec @$