################## Derry Leng Presents ##################
############### A Rob Sawyer Production #################
################## Shiny Croatia App #################### 
###### With Special Thanks To Pier Carlo Ferraris #######

# Shiny App ---------------------------------------------------------------

# VERSION 3.1 NOTES:
# Added filtering options to conflict map
# Added predicted sector capacity hover labels to Workload vs Entries plot
# Added separate arrival and departure Routing plots for Fuel Burn and Track Miles plots
# Added Sector Entries to Controller Workload plot
# Added Sector Entries vs Controller Workload plot
# Added sector polygon table (uses ~/3. Analysis & Results/Metrics/Croatia Polygons/Polygon conversion.R)
# Added severity colour indicator to Conflict Map
# Added support for runway scenarios for current and PBN operations
# Corrected axis and title labels for Fuel Burn, Track Miles and Workload
# Corrected Routing data for Fuel Burn and Track Miles plots
# Joined FlightType and Routing columns from Fuel Burn Track Miles to Conflict table for use in map labels
# Moved conflict map to server.R to enable leaflet proxies
# Moved import data function to separate R script
# Reworked imported data table manipulations for greater compatibility with plotting functions
# Reworked shiny reactive UI display using shinyjs to bypass leafletproxy problem on map unrendering

# To install all required packages:
# install.packages(c("shiny","rstudioapi","RPostgreSQL","ggplot2","RColorBrewer",
#                    "tidyr","leaflet","rgeos","sp","scales",
#                    "plotly","plyr","DT","shinyjs","V8"),dependencies=T)

# To see currently installed list of packages:
# as.data.frame(installed.packages()[,c(1,3)])

library(shiny)
source("ui.R", local = T)
source("server.R", local = T)
shinyApp(ui=ui, server=server)