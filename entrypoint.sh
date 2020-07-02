#!/bin/bash
echo -e " \
\n# Configure aws \
\nAWS_S3_ENDPOINT=$AWS_S3_ENDPOINT \
\nAWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID \
\nAWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY \
\nAWS_DEFAULT_REGION=$AWS_DEFAULT_REGION \
" >> /usr/local/lib/R/etc/Renviron.site 
exec "$@"