
pkg = installed.packages()

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
library(shinyflags)
# library(shinycssloaders)
# library(rhandsontable)
library(aws.s3)

library(DT)

library(tidyverse)
library(lubridate)
library(zoo)

library(rmarkdown)
library(plotly)

# library(highcharter)
library(RColorBrewer)
library(ggthemes)

list_pkg = c("magick", "slickR", "rdbnomics", "jsonline", "highcharter", "tools", "rwebstat", "rhandsontable")

for(pkg_id in list_pkg){
  if(pkg_id %in% pkg[,1]){
    library(pkg_id)
  }
}
#library(fredr)
library(eia)
library(saqgetr)
library(eurostat)
# library(idbr)
library(Quandl)
library(rsdmx)
library(pdfetch)
library(RJSONIO)
library(xml2)
library(rvest)