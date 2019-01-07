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

table.TotalThroughputs <- read.csv("~/Croatia v3.3/data/TotalThroughputs.csv", stringsAsFactors = FALSE)
table.HourlyThroughputs <- read.csv("~/Croatia v3.3/data/HourlyThroughputs.csv", stringsAsFactors = FALSE)
table.RollingHourlyThroughputs <- read.csv("~/Croatia v3.3/data/RollingHourlyThroughputs.csv", stringsAsFactors = FALSE)
table.FuelBurnTrackMiles <- read.csv("~/Croatia v3.3/data/FuelBurnTrackMiles.csv", stringsAsFactors = FALSE)
table.FuelBurn <- read.csv("~/Croatia v3.3/data/FuelBurn.csv", stringsAsFactors = FALSE)
table.TrackMiles <- read.csv("~/Croatia v3.3/data/TrackMiles.csv", stringsAsFactors = FALSE)
table.Conflicts <- read.csv("~/Croatia v3.3/data/Conflicts.csv", stringsAsFactors = FALSE)
table.ConflictType <- read.csv("~/Croatia v3.3/data/ConflictType.csv", stringsAsFactors = FALSE)
table.LateralConflictType <- read.csv("~/Croatia v3.3/data/LateralConflictType.csv", stringsAsFactors = FALSE)
table.VerticalConflictType <- read.csv("~/Croatia v3.3/data/VerticalConflictType.csv", stringsAsFactors = FALSE)
table.ConflictsFlightPlanPhase <- read.csv("~/Croatia v3.3/data/ConflictsFlightPlanPhase.csv", stringsAsFactors = FALSE)
table.Severity <- read.csv("~/Croatia v3.3/data/Severity.csv", stringsAsFactors = FALSE)
table.VerticalSeverity <- read.csv("~/Croatia v3.3/data/VerticalSeverity.csv", stringsAsFactors = FALSE)
table.FlightType <- read.csv("~/Croatia v3.3/data/FlightType.csv", stringsAsFactors = FALSE)
table.SectorOccupancy <- read.csv("~/Croatia v3.3/data/SectorOccupancy.csv", stringsAsFactors = FALSE)
table.SectorEntry <- read.csv("~/Croatia v3.3/data/SectorEntry.csv", stringsAsFactors = FALSE)
table.Workload <- read.csv("~/Croatia v3.3/data/Workload.csv", stringsAsFactors = FALSE)
table.SectorPolygons <- read.csv("~/Croatia v3.3/data/SectorPolygons.csv", stringsAsFactors = FALSE)
