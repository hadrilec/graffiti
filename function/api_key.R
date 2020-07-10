# .libPaths("N:/GDCJ/N-GDCJ/Echanges.DCJ/DSC/R/R-3.6.1/library")

# BANQUE DE FRANCE
webstat_client_ID <- 'd85fb7da-a306-469f-9b60-2e35d251611b'

# EIA agence americaine energie
api_key_data_gov_us = "sudqWb4gHCNBIo00XXxrhkB1iSe6jRKod08vXyKc"
api_key_eia = "423b693a2da4d6e9f5b0a1957eff0ba8"
eia_set_key(api_key_eia)

# Federal Reserve API key
pkg_check = installed.packages()
if("fredr" %in% pkg_check[,1]){
  library(fredr)
  #fredr_set_key("1e1376b050a44076281adda2fe2e1a32")
}

# AQICN
api_token_aqicn = "d2cec6e4da536c80809e27cfc169962408f18642"

# QUANDL
Quandl.api_key('ZQv7EfJg6xnzC-by17Kn')
