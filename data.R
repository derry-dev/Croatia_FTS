library(shiny)
library(plyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(leaflet)
library(leaflet.extras)
library(sp)
library(plotly)
library(DT)
library(shinyjs)

# To install all required packages:
# install.packages(c("shiny","rstudioapi","RPostgreSQL","ggplot2","RColorBrewer",
#                    "tidyr","leaflet","leaflet.extras","rgeos","sp","scales",
#                    "plotly","plyr","DT","shinyjs","V8"),dependencies=T)

# To see currently installed list of packages:
# as.data.frame(installed.packages()[,c(1,3)])

path <- paste0(here::here(), "/data/")

table.TotalThroughputs <- read.csv(paste0(path, "TotalThroughputs.csv"), stringsAsFactors = FALSE)
table.HourlyThroughputs <- read.csv(paste0(path, "HourlyThroughputs.csv"), stringsAsFactors = FALSE)
table.RollingHourlyThroughputs <- read.csv(paste0(path, "RollingHourlyThroughputs.csv"), stringsAsFactors = FALSE)
table.FuelBurnTrackMiles <- read.csv(paste0(path, "FuelBurnTrackMiles.csv"), stringsAsFactors = FALSE)
table.FuelBurn <- read.csv(paste0(path, "FuelBurn.csv"), stringsAsFactors = FALSE)
table.TrackMiles <- read.csv(paste0(path, "TrackMiles.csv"), stringsAsFactors = FALSE)
table.Conflicts <- read.csv(paste0(path, "Conflicts.csv"), stringsAsFactors = FALSE)
table.ConflictType <- read.csv(paste0(path, "ConflictType.csv"), stringsAsFactors = FALSE)
table.LateralConflictType <- read.csv(paste0(path, "LateralConflictType.csv"), stringsAsFactors = FALSE)
table.VerticalConflictType <- read.csv(paste0(path, "VerticalConflictType.csv"), stringsAsFactors = FALSE)
table.ConflictsFlightPlanPhase <- read.csv(paste0(path, "ConflictsFlightPlanPhase.csv"), stringsAsFactors = FALSE)
table.Severity <- read.csv(paste0(path, "Severity.csv"), stringsAsFactors = FALSE)
table.VerticalSeverity <- read.csv(paste0(path, "VerticalSeverity.csv"), stringsAsFactors = FALSE)
table.FlightType <- read.csv(paste0(path, "FlightType.csv"), stringsAsFactors = FALSE)
table.SectorOccupancy <- read.csv(paste0(path, "SectorOccupancy.csv"), stringsAsFactors = FALSE)
table.SectorEntry <- read.csv(paste0(path, "SectorEntry.csv"), stringsAsFactors = FALSE)
table.Workload <- read.csv(paste0(path, "Workload.csv"), stringsAsFactors = FALSE)
table.SectorPolygons <- read.csv(paste0(path, "SectorPolygons.csv"), stringsAsFactors = FALSE)
