################## Derry Leng Presents ##################
############### A Rob Sawyer Production #################
################## Shiny Croatia App #################### 
###### With Special Thanks To Pier Carlo Ferraris #######

# VERSION 3.1 NOTES:
# Added sector polygon table (uses ~/3. Analysis & Results/Metrics/Croatia Polygons/Polygon conversion.R)
# Added support for both runway directions for current and PBN operations
# Corrected axis and title labels for Fuel Burn, Track Miles and Workload
# Joined FlightType and Routing columns from FB TM to Conflict table for use on conflict map labels
# Leaflet proxies: added new features for dynamic data displays on leaflet map
# Moved conflict map to server.R to allow for leaflet proxies*

# To install all required packages:
# install.packages(c("shiny","rstudioapi","RPostgreSQL","ggplot2","RColorBrewer",
#                    "tidyr","leaflet","rgeos","sp","plotly",
#                    "lubridate","plyr","DT","shinyjs","V8"),dependencies=T)

# To see currently installed list of packages:
# as.data.frame(installed.packages()[,c(1,3)])

# Shiny App ---------------------------------------------------------------
library(shiny)
source("ui.R", local = T)
source("server.R", local = T)
shinyApp(ui=ui, server=server)