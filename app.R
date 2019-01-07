################## Derry Leng Presents ##################
############### A Rob Sawyer Production #################
################## Shiny Croatia App #################### 
###### With Special Thanks To Pier Carlo Ferraris #######

# VERSION 3.0 NOTES:
# Added controller workload, track miles and fuel burn metrics
# Added functionality for displaying both current and PBN data side-by-side
# Added HTML and CSS features
# Added interactive data explorer for exploring data corresponding to each plot
# Added option to import database data when required (disabled in online distributions)
# Disabled conflict map 3D pending removal/rework
# Major UI rework (again)
# Return of sector specific plots in sector capacity KPA
# Reworked data.R to use offline csv files

# To install all required packages:
# install.packages(c("shiny","rstudioapi","RPostgreSQL","ggplot2","RColorBrewer",
#                    "tidyr","leaflet","rgeos","sp","plotly",
#                    "lubridate","plyr","DT","shinyjs","V8"),dependencies=T)

# Shiny App ---------------------------------------------------------------
library(shiny)
source("ui.R", local = T)
source("server.R", local = T)
shinyApp(ui=ui, server=server)