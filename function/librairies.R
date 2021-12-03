
pkg = installed.packages()

library(shiny)
library(shinydashboard)
# devtools::install_version("shinydashboardPlus", version="0.7.5", repos = "http://cran.us.r-project.org")
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
# devtools::install_github('tutuchan/shinyflags')
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
if("rdbnomics" %in% pkg[,1]){
  library(rdbnomics)
}
if("jsonline" %in% pkg[,1]){
  library(jsonline)
}
if("rwebstat" %in% pkg[,1]){
  library(rwebstat)
}
# library(insee)
if("insee" %in% pkg[,1]){
  library(insee)
}
if("eia" %in% pkg[,1]){
  library(eia)
}
if("eurostat" %in% pkg[,1]){
  library(eurostat)
}
if("saqgetr" %in% pkg[,1]){
  library(saqgetr)
}
if("Quandl" %in% pkg[,1]){
  library(Quandl)
}
if("rsdmx" %in% pkg[,1]){
  library(rsdmx)
}
if("pdfetch" %in% pkg[,1]){
  library(pdfetch)
}
if("RJSONIO" %in% pkg[,1]){
  library(RJSONIO)
}
if("xml2" %in% pkg[,1]){
  library(xml2)
}
if("rvest" %in% pkg[,1]){
  library(rvest)
}
if("BARIS" %in% pkg[,1]){
  library(BARIS)
}

