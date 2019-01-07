################## Derry Leng Presents ##################
############### A Rob Sawyer Production #################
################## Shiny Croatia App #################### 
###### With Special Thanks To Pier Carlo Ferraris #######

# VERSION 2.0 --"Next Level"-- NOTES:
# Added airport locations
# Added 3D conflict map (3D!)
# Added total counts to sector capacity plots
# Changed SQL queries to remove redundant seconds from time variables
# Fixed data issues with aggregated hourly and rolling hourly throughput
# Fixed bad marker labels and label data in leaflet conflict map
# Full conversion of all ggplot/ggplotly to plot_ly
# Removed some now redundant airport/sector specific plots

library(shiny)
source("ui.R", local = T)
source("server.R", local = T)
shinyApp(ui=ui, server=server)