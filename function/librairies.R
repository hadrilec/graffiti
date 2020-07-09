
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

if("magick" %in% pkg[,1]){
  library(magick)
}
if("slickR" %in% pkg[,1]){
  library(slickR)
}
if("highcharter" %in% pkg[,1]){
  library(highcharter)
}
if("tools" %in% pkg[,1]){
  library(tools)
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