library(shiny)
library(plyr)
library(RPostgreSQL)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(leaflet)
library(sp)
library(plotly)
#library(lubridate)
library(DT)
library(shinyjs)
# setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path)))

# Change these lists if you want to include other airports/sectors
list.airports <- factor(c("All","LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"), ordered = TRUE) 
list.sectors <- factor(c("All","TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB"), ordered = TRUE)

table.SectorPolygons <- read.csv("~/Croatia v3.1/data/SectorPolygons.csv", stringsAsFactors = FALSE)
table.TotalThroughputs <- read.csv("~/Croatia v3.1/data/TotalThroughputs.csv", stringsAsFactors = FALSE)
table.HourlyThroughputs <- read.csv("~/Croatia v3.1/data/HourlyThroughputs.csv", stringsAsFactors = FALSE)
table.RollingHourlyThroughputs <- read.csv("~/Croatia v3.1/data/RollingHourlyThroughputs.csv", stringsAsFactors = FALSE)
table.FuelBurn <- read.csv("~/Croatia v3.1/data/FuelBurn.csv", stringsAsFactors = FALSE)
table.TrackMiles <- read.csv("~/Croatia v3.1/data/TrackMiles.csv", stringsAsFactors = FALSE)
table.Conflicts <- read.csv("~/Croatia v3.1/data/Conflicts.csv", stringsAsFactors = FALSE)
table.ConflictType <- read.csv("~/Croatia v3.1/data/ConflictType.csv", stringsAsFactors = FALSE)
table.LateralConflictType <- read.csv("~/Croatia v3.1/data/LateralConflictType.csv", stringsAsFactors = FALSE)
table.VerticalConflictType <- read.csv("~/Croatia v3.1/data/VerticalConflictType.csv", stringsAsFactors = FALSE)
table.ConflictsFlightPlanPhase <- read.csv("~/Croatia v3.1/data/ConflictsFlightPlanPhase.csv", stringsAsFactors = FALSE)
table.Severity <- read.csv("~/Croatia v3.1/data/Severity.csv", stringsAsFactors = FALSE)
table.VerticalSeverity <- read.csv("~/Croatia v3.1/data/VerticalSeverity.csv", stringsAsFactors = FALSE)
table.SectorOccupancy <- read.csv("~/Croatia v3.1/data/SectorOccupancy.csv", stringsAsFactors = FALSE)
table.SectorEntry <- read.csv("~/Croatia v3.1/data/SectorEntry.csv", stringsAsFactors = FALSE)
table.Workload <- read.csv("~/Croatia v3.1/data/Workload.csv", stringsAsFactors = FALSE)

dbimport <- function() {
# Connect to database -----------------------------------------------------
  con <- dbConnect(dbDriver("PostgreSQL"),dbname="airtopdb",host="192.168.1.157",port=5432,user="think",password="think")
  
# table.TotalThroughputs -------------------------------------------------------
  
  query.TotalThroughput <-
  'SELECT
  COUNT("Callsign") AS "Count", 
  "New DepartureOrArrivalAirport" AS "Airport",
  "AircraftState" AS "Category"
  FROM YOUR TABLE HERE
  WHERE "Time_day" = 2
  GROUP BY "New DepartureOrArrivalAirport", "AircraftState"
  ORDER BY "New DepartureOrArrivalAirport", "AircraftState"'
  
  table.current1.TotalThroughputs <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"',query.TotalThroughput))
  table.current2.TotalThroughputs <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"',query.TotalThroughput))
  table.PBN1.TotalThroughputs <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"',query.TotalThroughput))
  table.PBN2.TotalThroughputs <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"',query.TotalThroughput))
  
  t1 <- table.current1.TotalThroughputs
  t1$Scenario <- "Current Runway 1"
  t2 <- table.current2.TotalThroughputs
  t2$Scenario <- "Current Runway 2"
  t3 <- table.PBN1.TotalThroughputs
  t3$Scenario <- "PBN Runway 1"
  t4 <- table.PBN2.TotalThroughputs
  t4$Scenario <- "PBN Runway 2"
  table.TotalThroughputs <- rbind(t1,t2,t3,t4)
  
  table.TotalThroughputs <- table.TotalThroughputs[!(table.TotalThroughputs$Category == "Climbing"),]
  agg1 <- aggregate(data = table.TotalThroughputs,Count~Category+Scenario,"sum")
  agg1$Airport <- "All Airports"
  agg2 <- aggregate(data = subset(table.TotalThroughputs, Airport %in% list.airports),Count~Category+Scenario,"sum")
  agg2$Airport <- "All Major Airports"
  agg3 <- aggregate(data = table.TotalThroughputs,Count~Airport+Scenario,"sum")
  agg3$Category <- "Arrival and Departures"
  table.TotalThroughputs <- rbind(table.TotalThroughputs, agg1, agg2, agg3)
  table.TotalThroughputs$Category[table.TotalThroughputs$Category == "Descending"] <- "Arrivals"
  table.TotalThroughputs$Category[table.TotalThroughputs$Category == "GroundAccelerating"] <- "Departures"

# table.HourlyThroughputs -------------------------------------------------
  
  query.HourlyThroughputs <- 
  'SELECT
  EXTRACT(HOUR FROM "Time_time") AS "Hour",
  COUNT("Callsign") AS "Count", 
  "New DepartureOrArrivalAirport" AS "Airport",
  "AircraftState" AS "Category"
  FROM YOUR TABLE HERE
  WHERE "Time_day" = 2
  GROUP BY EXTRACT(HOUR FROM "Time_time"),"New DepartureOrArrivalAirport", "AircraftState"
  ORDER BY "New DepartureOrArrivalAirport", "AircraftState",EXTRACT(HOUR FROM "Time_time")'
  
  table.current1.HourlyThroughputs <- dbGetQuery(con,gsub("YOUR TABLE HERE", '"Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"',query.HourlyThroughputs))
  table.current2.HourlyThroughputs <- dbGetQuery(con,gsub("YOUR TABLE HERE", '"Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"',query.HourlyThroughputs))
  table.PBN1.HourlyThroughputs <- dbGetQuery(con,gsub("YOUR TABLE HERE", '"Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"',query.HourlyThroughputs))
  table.PBN2.HourlyThroughputs <- dbGetQuery(con,gsub("YOUR TABLE HERE", '"Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"',query.HourlyThroughputs))
  
  t1 <- table.current1.HourlyThroughputs
  t1$Scenario <- "Current Runway 1"
  t2 <- table.current2.HourlyThroughputs
  t2$Scenario <- "Current Runway 2"
  t3 <- table.PBN1.HourlyThroughputs
  t3$Scenario <- "PBN Runway 1"
  t4 <- table.PBN2.HourlyThroughputs
  t4$Scenario <- "PBN Runway 2"
  table.HourlyThroughputs <- rbind(t1,t2,t3,t4)
  
  table.HourlyThroughputs <- table.HourlyThroughputs[!(table.HourlyThroughputs$Category == "Climbing"),]
  agg1 <- aggregate(data = table.HourlyThroughputs,Count~Category+Scenario+Hour,"sum")
  agg1$Airport <- "All Airports"
  agg2 <- aggregate(data = subset(table.HourlyThroughputs, Airport %in% list.airports),Count~Category+Scenario+Hour,"sum")
  agg2$Airport <- "All Major Airports"
  agg3 <- aggregate(data = table.HourlyThroughputs,Count~Airport+Scenario+Hour,"sum")
  agg3$Category <- "Arrivals and Departures"
  table.HourlyThroughputs <- rbind(table.HourlyThroughputs, agg1, agg2, agg3)
  table.HourlyThroughputs$Category[table.HourlyThroughputs$Category == "Descending"] <- "Arrivals"
  table.HourlyThroughputs$Category[table.HourlyThroughputs$Category == "GroundAccelerating"] <- "Departures"

# table.RollingHourlyThroughputs ------------------------------------------
  
  query.RollingHourlyThroughputs <- 
  'SELECT
  to_char("Time_time", \'HH24:MI\') AS "Time",
  "AirportStatus" AS "Airport",
  "LiftOffCountInPeriod" AS "RollingLiftOffCount",
  "TouchDownCountInPeriod" AS "RollingTouchDownCount",
  "ThroughputCountInPeriod" AS "RollingThroughputCount"
  FROM YOUR TABLE HERE
  WHERE "Time_day" = 2
  ORDER BY "AirportStatus", "Time_time"'
  
  table.current1.RollingHourlyThroughputs <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS_RWYTHROUGHPUT"',query.RollingHourlyThroughputs))
  table.current2.RollingHourlyThroughputs <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS_RWYTHROUGHPUT"',query.RollingHourlyThroughputs))
  table.PBN1.RollingHourlyThroughputs <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS_RWYTHROUGHPUT"',query.RollingHourlyThroughputs))
  table.PBN2.RollingHourlyThroughputs <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS_RWYTHROUGHPUT"',query.RollingHourlyThroughputs))
  
  t1 <- table.current1.RollingHourlyThroughputs
  t1$Scenario <- "Current Runway 1"
  t2 <- table.current2.RollingHourlyThroughputs
  t2$Scenario <- "Current Runway 2"
  t3 <- table.PBN1.RollingHourlyThroughputs
  t3$Scenario <- "PBN Runway 1"
  t4 <- table.PBN2.RollingHourlyThroughputs
  t4$Scenario <- "PBN Runway 2"
  table.RollingHourlyThroughputs <- rbind(t1,t2,t3,t4)
  
  colnames(table.RollingHourlyThroughputs) <- c("Time","Airport","Departures","Arrivals","Arrivals and Departures","Scenario")
  table.RollingHourlyThroughputs <- gather(table.RollingHourlyThroughputs,"Category","Count",c(Departures,Arrivals,"Arrivals and Departures"))
  agg1 <- aggregate(data = table.RollingHourlyThroughputs, Count~Time+Category+Scenario, "sum")
  agg1$Airport <- "All Airports"
  agg2 <- aggregate(data = subset(table.RollingHourlyThroughputs, Airport %in% list.airports), Count~Time+Category+Scenario, "sum")
  agg2$Airport <- "All Major Airports"
  table.RollingHourlyThroughputs <- rbind(table.RollingHourlyThroughputs, agg1, agg2)
  table.RollingHourlyThroughputs <- table.RollingHourlyThroughputs[,c(1,5,2,4,3)]

# table.FuelBurn and table.TrackMiles -------------------------------------
  
  query.FuelBurnTrackMiles <-
  'SELECT
  "Callsign",
  "OriginInfo" AS "Origin",
  "DestinationInfo" AS "Destination",
  ROUND("TotalFuelBurned"::numeric, 5) AS "FuelBurn",
  ROUND("TotalDistanceFlown"::numeric/1852, 5) AS "TrackMiles",
  "AircraftType",
  CASE
  WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) NOT LIKE "DestinationInfo"||\'%\'
  THEN "OriginInfo"
  WHEN SPLIT_PART("Routing",\'_\',1) NOT LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
  THEN "DestinationInfo"
  WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
  THEN "OriginInfo"
  END AS "Airport",
  CASE
  WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) NOT LIKE "DestinationInfo"||\'%\'
  THEN REPLACE(SPLIT_PART("Routing",\'_\',2),\'.\',\' \')
  WHEN SPLIT_PART("Routing",\'_\',1) NOT LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
  THEN REPLACE(SPLIT_PART("Routing",\'_\',1),\'.\',\' \')
  WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
  THEN "DestinationInfo"
  END AS "Waypoint",
  CASE
  WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) NOT LIKE "DestinationInfo"||\'%\'
  THEN \'Departure\'
  WHEN SPLIT_PART("Routing",\'_\',1) NOT LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
  THEN \'Arrival\'
  WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
  THEN \'Domestic\'
  END AS "RoutingType"
  FROM YOUR TABLE HERE
  WHERE "Time_day" = 2 AND "FlightType" = \'AirCarrier\''
  
  table.current1.FuelBurnTrackMiles <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS.FB_TM"',query.FuelBurnTrackMiles))
  table.current2.FuelBurnTrackMiles <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS.FB_TM"',query.FuelBurnTrackMiles))
  table.PBN1.FuelBurnTrackMiles <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS.FB_TM"',query.FuelBurnTrackMiles))
  table.PBN2.FuelBurnTrackMiles <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS.FB_TM"',query.FuelBurnTrackMiles))
  
  t1 <- table.current1.FuelBurnTrackMiles
  t1$Scenario <- "Current Runway 1"
  t2 <- table.current2.FuelBurnTrackMiles
  t2$Scenario <- "Current Runway 2"
  t3 <- table.PBN1.FuelBurnTrackMiles
  t3$Scenario <- "PBN Runway 1"
  t4 <- table.PBN2.FuelBurnTrackMiles
  t4$Scenario <- "PBN Runway 2"
  table.FuelBurnTrackMiles <- rbind(t1,t2,t3,t4)
  
  temp <- table.FuelBurnTrackMiles[0,]
  for (i in 1:nrow(table.FuelBurnTrackMiles)) {
    if (table.FuelBurnTrackMiles[i,]$RoutingType == "Domestic") {
      t1 <- table.FuelBurnTrackMiles[i,]
      t1$RoutingType <- "Arrival"
      t1$Waypoint <- t1$Origin
      t2 <- table.FuelBurnTrackMiles[i,]
      t2$RoutingType <- "Departure"
      t2$Waypoint <- t2$Destination
      temp <- rbind(temp,t1,t2)
    }
  }
  table.FuelBurnTrackMiles <- rbind(subset(table.FuelBurnTrackMiles, !(RoutingType %in% "Domestic")), temp)
  
  table.FuelBurn <- aggregate(data=table.FuelBurnTrackMiles,FuelBurn~Airport+Scenario+RoutingType+Waypoint,"sum")
  temp1 <- aggregate(data = table.FuelBurn, FuelBurn~Airport+Scenario+Waypoint,"sum")
  temp1$RoutingType <- "Arrivals and Departures"
  temp2 <- aggregate(data = table.FuelBurn, FuelBurn~Airport+Scenario+RoutingType,"sum")
  temp2$Waypoint <- "All Routes"
  temp3 <- aggregate(data = table.FuelBurn, FuelBurn~Airport+Scenario,"sum")
  temp3$RoutingType <- "Arrivals and Departures"
  temp3$Waypoint <- "All Routes"
  temp4 <- aggregate(data = table.FuelBurn, FuelBurn~Scenario,"sum")
  temp4$RoutingType <- "Arrivals and Departures"
  temp4$Waypoint <- "All Routes"
  temp4$Airport <- "All Airports"
  table.FuelBurn <- rbind(table.FuelBurn,temp1,temp2,temp3,temp4)
  table.FuelBurn <- table.FuelBurn[,c(1,5,3,4,2)]
  
  table.TrackMiles <- aggregate(data=table.FuelBurnTrackMiles,TrackMiles~Airport+Scenario+RoutingType+Waypoint,"sum")
  temp1 <- aggregate(data = table.TrackMiles, TrackMiles~Airport+Scenario+Waypoint,"sum")
  temp1$RoutingType <- "Arrivals and Departures"
  temp2 <- aggregate(data = table.TrackMiles, TrackMiles~Airport+Scenario+RoutingType,"sum")
  temp2$Waypoint <- "All Routes"
  temp3 <- aggregate(data = table.TrackMiles, TrackMiles~Airport+Scenario,"sum")
  temp3$RoutingType <- "Arrivals and Departures"
  temp3$Waypoint <- "All Routes"
  temp4 <- aggregate(data = table.TrackMiles, TrackMiles~Scenario,"sum")
  temp4$RoutingType <- "Arrivals and Departures"
  temp4$Waypoint <- "All Routes"
  temp4$Airport <- "All Airports"
  table.TrackMiles <- rbind(table.TrackMiles,temp1,temp2,temp3,temp4)
  table.TrackMiles <- table.TrackMiles[,c(1,5,3,4,2)]
  
# table.Conflicts and whatnot ---------------------------------------------------------
  
  query.Conflicts <-
  'SELECT 
  "ID",
  "ATCSector" AS "Sector",
  "StartTime_time" AS "Start_Time",
  "ClosestApproachTime_time" AS "Closest_Time",
  "EndTime_time" AS "End_Time",
  "ConflictType",
  "Severity"::text,
  "VerticalSeverity"::text,
  "LateralConflictType",
  "VerticalConflictType",
  ROUND("VerticalSeparation_m"::numeric/0.3048, 5) AS "VerticalSeparation",
  ROUND("ReqVerticalSeparation_m"::numeric/0.3048, 5) AS "ReqVerticalSeparation",
  ROUND("LateralSeparation_m"::numeric/1852, 5) AS "LateralSeparation",
  ROUND("ReqLateralSeparation_m"::numeric/1852, 5) AS "ReqLateralSeparation",
  "Altitude_ft",
  "FlightPlanPhase1",
  "FlightPlanPhase2",
  t1."FlightType" AS "FlightType1",
  t2."FlightType" AS "FlightType2",
  t1."Routing" AS "Routing1",
  t2."Routing" AS "Routing2",
  SPLIT_PART("2DLocation",\' \',6)::numeric + SPLIT_PART("2DLocation",\' \',7)::numeric/60 + SPLIT_PART("2DLocation",\' \',8)::numeric/3600 AS "Longitude",
  SPLIT_PART("2DLocation",\' \',2)::numeric + SPLIT_PART("2DLocation",\' \',3)::numeric/60 + SPLIT_PART("2DLocation",\' \',4)::numeric/3600 AS "Latitude"
  FROM YOUR TABLE 1 HERE
  LEFT JOIN LATERAL (
  SELECT "Callsign", "FlightType", "Routing"
  FROM YOUR TABLE 2 HERE
  WHERE "Time_day" = 2
  AND "Callsign" = SPLIT_PART("ID",\'_\',2)
  ) AS t1 ON TRUE
  LEFT JOIN LATERAL (
  SELECT "Callsign", "FlightType", "Routing"
  FROM YOUR TABLE 2 HERE
  WHERE "Time_day" = 2
  AND "Callsign" = SPLIT_PART("ID",\'_\',3)
  ) AS t2 ON TRUE
  WHERE "StartTime_day" = 2'
  
  x1 <- query.Conflicts %<>% gsub("YOUR TABLE 1 HERE",'"Croatia"."Conflict"',.) %>% gsub("YOUR TABLE 2 HERE",'"Croatia"."RS.FB_TM"',.)
  table.current1.Conflicts <- dbGetQuery(con,x1)
  x2 <- query.Conflicts %<>% gsub("YOUR TABLE 1 HERE",'"Croatia"."Conflict"',.) %>% gsub("YOUR TABLE 2 HERE",'"Croatia"."RS.FB_TM"',.)
  table.current2.Conflicts <- dbGetQuery(con,x2)
  x3 <- query.Conflicts %<>% gsub("YOUR TABLE 1 HERE",'"Croatia"."Conflict"',.) %>% gsub("YOUR TABLE 2 HERE",'"Croatia"."RS.FB_TM"',.)
  table.PBN1.Conflicts <- dbGetQuery(con,x3)
  x4 <- query.Conflicts %<>% gsub("YOUR TABLE 1 HERE",'"Croatia"."Conflict"',.) %>% gsub("YOUR TABLE 2 HERE",'"Croatia"."RS.FB_TM"',.)
  table.PBN2.Conflicts <- dbGetQuery(con,x4)
  
  t1 <- table.current1.Conflicts
  t1$Scenario <- "Current Runway 1"
  t2 <- table.current2.Conflicts
  t2$Scenario <- "Current Runway 2"
  t3 <- table.PBN1.Conflicts
  t3$Scenario <- "PBN Runway 1"
  t4 <- table.PBN2.Conflicts
  t4$Scenario <- "PBN Runway 2"
  table.Conflicts <- rbind(t1,t2,t3,t4)
  
  table.ConflictType <- count(table.Conflicts, vars = c("Sector","ConflictType","Scenario"))
  temp1 <- aggregate(data = table.ConflictType,freq~ConflictType+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.ConflictType, Sector %in% list.sectors),freq~ConflictType+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.ConflictType <- rbind(table.ConflictType, temp1, temp2)
  table.ConflictType$ConflictType <- gsub("([a-z])([A-Z])", "\\1 \\2", table.ConflictType$ConflictType)
  names(table.ConflictType) <- c("Sector","ConflictType","Scenario","Count")
  table.ConflictType <- table.ConflictType[,c(4,1,2,3)]
  
  table.LateralConflictType <- count(table.Conflicts, vars = c("Sector","LateralConflictType","Scenario"))
  temp1 <- aggregate(data = table.LateralConflictType,freq~LateralConflictType+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.LateralConflictType, Sector %in% list.sectors),freq~LateralConflictType+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.LateralConflictType <- rbind(table.LateralConflictType, temp1, temp2)
  table.LateralConflictType$LateralConflictType <- gsub("([a-z])([A-Z])", "\\1 \\2", table.LateralConflictType$LateralConflictType)
  names(table.LateralConflictType) <- c("Sector","LateralConflictType","Scenario","Count")
  table.LateralConflictType <- table.LateralConflictType[,c(4,1,2,3)]
  
  table.VerticalConflictType <- count(table.Conflicts, vars = c("Sector","VerticalConflictType","Scenario"))
  temp1 <- aggregate(data = table.VerticalConflictType,freq~VerticalConflictType+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.VerticalConflictType, Sector %in% list.sectors),freq~VerticalConflictType+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.VerticalConflictType <- rbind(table.VerticalConflictType, temp1, temp2)
  table.VerticalConflictType$VerticalConflictType <- gsub("([a-z])([A-Z])", "\\1 \\2", table.VerticalConflictType$VerticalConflictType)
  names(table.VerticalConflictType) <- c("Sector","VerticalConflictType","Scenario","Count")
  table.VerticalConflictType <- table.VerticalConflictType[,c(4,1,2,3)]
  
  table.Severity <- count(table.Conflicts, vars = c("Sector","Severity","Scenario"))
  temp1 <- aggregate(data = table.Severity,freq~Severity+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.Severity, Sector %in% list.sectors),freq~Severity+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.Severity <- rbind(table.Severity, temp1, temp2)
  names(table.Severity) <- c("Sector","Severity","Scenario","Count")
  table.Severity <- table.Severity[,c(4,1,2,3)]
  
  table.VerticalSeverity <- count(table.Conflicts, vars = c("Sector","VerticalSeverity","Scenario"))
  temp1 <- aggregate(data = table.VerticalSeverity,freq~VerticalSeverity+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.VerticalSeverity, Sector %in% list.sectors),freq~VerticalSeverity+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.VerticalSeverity <- rbind(table.VerticalSeverity, temp1, temp2)
  names(table.VerticalSeverity) <- c("Sector","VerticalSeverity","Scenario","Count")
  table.VerticalSeverity <- table.VerticalSeverity[,c(4,1,2,3)]
  
# table.ConflictsFlightPlanPhase ------------------------------------------
  
  query.ConflictsFlightPlanPhase <-
  'SELECT
  "ATCSector" AS "Sector",
  CASE
  WHEN LOWER("FlightPlanPhase1") < LOWER("FlightPlanPhase2")
  THEN "FlightPlanPhase1" || \' \' || "FlightPlanPhase2"
  ELSE "FlightPlanPhase2" || \' \' || "FlightPlanPhase1"
  END AS "FlightPlanPhases",
  COUNT(*) AS "Count"
  FROM YOUR TABLE HERE
  GROUP BY "ATCSector",
  (CASE WHEN LOWER("FlightPlanPhase1") < LOWER("FlightPlanPhase2")
  THEN "FlightPlanPhase1" || \' \' || "FlightPlanPhase2"
  ELSE "FlightPlanPhase2" || \' \' || "FlightPlanPhase1" END)'
  
  table.current1.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."Conflict"',query.ConflictsFlightPlanPhase))
  table.current2.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."Conflict"',query.ConflictsFlightPlanPhase))
  table.PBN1.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."Conflict"',query.ConflictsFlightPlanPhase))
  table.PBN2.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."Conflict"',query.ConflictsFlightPlanPhase))
  
  t1 <- table.current1.ConflictsFlightPlanPhase
  t1$Scenario <- "Current Runway 1"
  t2 <- table.current2.ConflictsFlightPlanPhase
  t2$Scenario <- "Current Runway 2"
  t3 <- table.PBN1.ConflictsFlightPlanPhase
  t3$Scenario <- "PBN Runway 1"
  t4 <- table.PBN2.ConflictsFlightPlanPhase
  t4$Scenario <- "PBN Runway 2"
  table.ConflictsFlightPlanPhase <- rbind(t1,t2,t3,t4)
  
  agg1 <- aggregate(data = table.ConflictsFlightPlanPhase,Count~FlightPlanPhases+Scenario,"sum")
  agg1$Sector <- "All Sectors"
  agg2 <- aggregate(data = subset(table.ConflictsFlightPlanPhase, Sector %in% list.sectors),Count~FlightPlanPhases+Scenario,"sum")
  agg2$Sector <- "All TMA"
  table.ConflictsFlightPlanPhase <- rbind(table.ConflictsFlightPlanPhase, agg1, agg2)
  
  temp <- strsplit(table.ConflictsFlightPlanPhase$FlightPlanPhases, " ")
  for (i in 1:length(temp)) {
    if (unlist(temp[i])[1] == unlist(temp[i])[2]) {
      temp[i] <- paste("Both",gsub("([a-z])([A-Z])","\\1 \\2",unlist(temp[i])[1]))
    } else {
      temp[i] <- paste(gsub("([a-z])([A-Z])","\\1 \\2",unlist(temp[i])[1]),"and",gsub("([a-z])([A-Z])","\\1 \\2",unlist(temp[i])[2]))
    }
  }
  table.ConflictsFlightPlanPhase$FlightPlanPhases <- unlist(temp)
  table.ConflictsFlightPlanPhase <- table.ConflictsFlightPlanPhase[,c(1,3,2,4)]
  
# table.SectorOccupancy ---------------------------------------------------
  
  query.SectorOccupancy <-
  'SELECT
  to_char("Time_time", \'HH24:MI\') AS "Time",
  "ATCSector" AS "Sector",
  "AircraftLoad" AS "Count"
  FROM YOUR TABLE HERE
  WHERE "Time_day" = 2'
  
  table.current1.SectorOccupancy <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS_SECOCC"',query.SectorOccupancy))
  table.current2.SectorOccupancy <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS_SECOCC"',query.SectorOccupancy))
  table.PBN1.SectorOccupancy <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS_SECOCC"',query.SectorOccupancy))
  table.PBN2.SectorOccupancy <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS_SECOCC"',query.SectorOccupancy))
  
  t1 <- table.current1.SectorOccupancy
  t1$Scenario <- "Current Runway 1"
  t2 <- table.current2.SectorOccupancy
  t2$Scenario <- "Current Runway 2"
  t3 <- table.PBN1.SectorOccupancy
  t3$Scenario <- "PBN Runway 1"
  t4 <- table.PBN2.SectorOccupancy
  t4$Scenario <- "PBN Runway 2"
  table.SectorOccupancy <- rbind(t1,t2,t3,t4)
  
  agg1 <- aggregate(data = table.SectorOccupancy,Count~Time+Scenario,"sum")
  agg1$Sector <- "All Sectors"
  agg2 <- aggregate(data = subset(table.SectorOccupancy, Sector %in% list.sectors),Count~Time+Scenario,"sum")
  agg2$Sector <- "All TMA"
  table.SectorOccupancy <- rbind(table.SectorOccupancy,agg1,agg2)

# table.SectorEntry -------------------------------------------------------
 
  query.SectorEntry <-
  'SELECT
  to_char("Time_time", \'HH24:MI\') AS "Time", 
  "ATCSector" AS "Sector",
  "LastPeriodEntryCount" AS "Entries"
  FROM YOUR TABLE HERE
  WHERE "Time_day" = 2'
  
  table.current1.SectorEntry <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS_SECENTRY"',query.SectorEntry))
  table.current2.SectorEntry <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS_SECENTRY"',query.SectorEntry))
  table.PBN1.SectorEntry <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS_SECENTRY"',query.SectorEntry))
  table.PBN2.SectorEntry <- dbGetQuery(con,gsub("YOUR TABLE HERE",'"Croatia"."RS_SECENTRY"',query.SectorEntry))
  
  t1 <- table.current1.SectorEntry
  t1$Scenario <- "Current Runway 1"
  t2 <- table.current2.SectorEntry
  t2$Scenario <- "Current Runway 2"
  t3 <- table.PBN1.SectorEntry
  t3$Scenario <- "PBN Runway 1"
  t4 <- table.PBN2.SectorEntry
  t4$Scenario <- "PBN Runway 2"
  table.SectorEntry <- rbind(t1,t2,t3,t4)
  
  agg1 <- aggregate(data = table.SectorEntry,Entries~Time+Scenario,"sum")
  agg1$Sector <- "All Sectors"
  agg2 <- aggregate(data = subset(table.SectorEntry, Sector %in% list.sectors),Entries~Time+Scenario,"sum")
  agg2$Sector <- "All TMA"
  table.SectorEntry <- rbind(table.SectorEntry,agg1,agg2)

# table.Workload ---------------------------------------------------------
  
  query.Workload <-
  'DROP TABLE IF EXISTS "Croatia"."Workload";
  SELECT
    t1."Time" + (t2."Time Adjustment"||\'seconds\')::interval AS "Time",
    t2."Task",
    t1."Event Type",
    t1."Event Condition",
    t2."Task Duration",
    t2."Sector",
    t1."Callsign",
    t1."OriginInfo",
    t1."DestinationInfo",
    t1."FlightType"
    --t1."Routing"
  INTO "Croatia"."Workload"
  FROM (
    SELECT
      "Time_time" AS "Time",
      \'Sector Exit\' AS "Event Type",
      "New PreviousATCSector" AS "Event Condition",
      "Callsign",
      "OriginInfo",
      "DestinationInfo",
      "FlightType",
      "Routing"
    FROM TABLE1
    WHERE "Time_day" = 2
    UNION
    SELECT
      "Time_time" AS "Time",
      \'Sector Entry\' AS "Event Type",
      "New LastAirspaceEntered" AS "Event Condition",
      "Callsign",
      "OriginInfo",
      "DestinationInfo",
      "FlightType",
      "Routing"
    FROM TABLE2
    WHERE "Time_day" = 2
    UNION
    SELECT
      "Time_time" AS "Time",
      \'Lift Off\' AS "Event Type",
      "OriginInfo" AS "Event Condition",
      "Callsign",
      "OriginInfo",
      "DestinationInfo",
      "FlightType",
      "Routing"
    FROM TABLE3
    WHERE "Time_day" = 2
    UNION
    SELECT
      "Time_time" AS "Time",
      \'WP Passed\' AS "Event Type",
      "New LastWaypointPassed" AS "Event Condition",
      "Callsign",
      "OriginInfo",
      "DestinationInfo",
      "FlightType",
      "Routing"
    FROM TABLE4
    WHERE "Time_day" = 2
  ) AS t1
  LEFT JOIN LATERAL (
    SELECT "Task", "Event Type", "Event Condition", "Sector", "Task Duration", "Time Adjustment", "Origin", "Destination", "Flight Type"
    FROM "Croatia"."WORKLOAD_TASK_LIST"
    WHERE (
      "Event Type" = t1."Event Type"
      AND "Event Condition" = t1."Event Condition"
      AND ("Origin" = "OriginInfo" OR "Origin" = \'NA\')
      AND ("Destination" = "DestinationInfo" OR "Destination" = \'NA\')
      AND ("Flight Type" = "FlightType" OR "Flight Type" = \'NA\')
    )
  ) AS t2 ON TRUE
  WHERE t2."Task" IS NOT NULL
  ORDER BY "Time";

  DROP TABLE IF EXISTS "Croatia"."RollingHourlyWorkload_temp";
  SELECT DISTINCT
    t1."Time",
    t1."Sector",
    SUM("Task Duration") OVER (PARTITION BY t2."Time", t2."Sector" ORDER BY t2."Time") AS "Workload"
  INTO "Croatia"."RollingHourlyWorkload_temp"
  FROM (
    SELECT "Time", "ID" AS "Sector" FROM "Croatia"."time_template"
    CROSS JOIN (
      SELECT "ID" FROM "Croatia"."ATCSector"
    ) AS t
  ) AS t1
  LEFT JOIN (
    SELECT * FROM "Croatia"."Workload"
  ) AS t2 ON t2."Time" = t1."Time" AND t2."Sector" = t1."Sector"
  ORDER BY t1."Sector", t1."Time";
  
  DROP TABLE IF EXISTS "Croatia"."RollingHourlyWorkload";
  SELECT
    "Time",
    "Sector",
    CASE
      WHEN "Workload" IS NULL THEN 0
      ELSE "Workload"
    END AS "Workload",
    CASE
      WHEN SUM("Workload") OVER (PARTITION BY "Sector" ORDER BY "Time" ROWS BETWEEN 3599 PRECEDING AND CURRENT ROW) IS NULL THEN 0
      ELSE SUM("Workload") OVER (PARTITION BY "Sector" ORDER BY "Time" ROWS BETWEEN 3599 PRECEDING AND CURRENT ROW)
    END AS "HourlyWorkload"
  INTO "Croatia"."RollingHourlyWorkload"
  FROM "Croatia"."RollingHourlyWorkload_temp";
  
  DROP TABLE "Croatia"."RollingHourlyWorkload_temp";
  
  SELECT "Time", "Sector", ROUND("HourlyWorkload"/36) AS "PercentHourlyWorkload"
  FROM "Croatia"."RollingHourlyWorkload"
  WHERE EXTRACT(second from "Time") = 0
  AND EXTRACT(minute from "Time") IN (0,10,20,30,40,50)
  AND "Sector" LIKE \'TMA_%\'
  ORDER BY "Sector"'
  
  x1 <- query.Workload %<>%
    gsub("TABLE1",'"Croatia"."RS_ATC_SECTOR_EXIT_AND_CONTROLLED"',.) %>%
    gsub("TABLE2",'"Croatia"."RS_ATC_SECTOR_ENTRY_AND_CONTROLLED"',.) %>%
    gsub("TABLE3",'"Croatia"."RS_LIFT_OFF"',.) %>%
    gsub("TABLE4",'"Croatia"."RS_WP_PASSED"',.)
  table.current1.Workload <- dbGetQuery(con,x1)
  x2 <- query.Workload %<>%
    gsub("TABLE1",'"Croatia"."RS_ATC_SECTOR_EXIT_AND_CONTROLLED"',.) %>%
    gsub("TABLE2",'"Croatia"."RS_ATC_SECTOR_ENTRY_AND_CONTROLLED"',.) %>%
    gsub("TABLE3",'"Croatia"."RS_LIFT_OFF"',.) %>%
    gsub("TABLE4",'"Croatia"."RS_WP_PASSED"',.)
  table.current2.Workload <- dbGetQuery(con,x2)
  x3 <- query.Workload %<>%
    gsub("TABLE1",'"Croatia"."RS_ATC_SECTOR_EXIT_AND_CONTROLLED"',.) %>%
    gsub("TABLE2",'"Croatia"."RS_ATC_SECTOR_ENTRY_AND_CONTROLLED"',.) %>%
    gsub("TABLE3",'"Croatia"."RS_LIFT_OFF"',.) %>%
    gsub("TABLE4",'"Croatia"."RS_WP_PASSED"',.)
  table.PBN1.Workload <- dbGetQuery(con,x3)
  x4 <- query.Workload %<>%
    gsub("TABLE1",'"Croatia"."RS_ATC_SECTOR_EXIT_AND_CONTROLLED"',.) %>%
    gsub("TABLE2",'"Croatia"."RS_ATC_SECTOR_ENTRY_AND_CONTROLLED"',.) %>%
    gsub("TABLE3",'"Croatia"."RS_LIFT_OFF"',.) %>%
    gsub("TABLE4",'"Croatia"."RS_WP_PASSED"',.)
  table.PBN2.Workload <- dbGetQuery(con,x4)
  
  t1 <- table.current1.Workload
  t1$Scenario <- "Current Runway 1"
  t2 <- table.current2.Workload
  t2$Scenario <- "Current Runway 2"
  t3 <- table.PBN1.Workload
  t3$Scenario <- "PBN Runway 1"
  t4 <- table.PBN2.Workload
  t4$Scenario <- "PBN Runway 2"
  table.Workload <- rbind(t1,t2,t3,t4)
  
  agg1 <- aggregate(data = table.Workload,PercentHourlyWorkload~Time+Scenario,"sum")
  agg1$Sector <- "All Sectors"
  agg2 <- aggregate(data = subset(table.Workload, Sector %in% list.sectors),PercentHourlyWorkload~Time+Scenario,"sum")
  agg2$Sector <- "All TMA"
  table.Workload <- rbind(table.Workload,agg1,agg2)

# table.SectorPolygons ---------------------------------------------------------
  table.SectorPolygons <-
    dbGetQuery(con,'
               SELECT
               "Sector",
               --"Block",
               --"PolygonID",
               REPLACE(REPLACE(ST_AsText(ST_UNION("Polygon"::geometry)),\'POLYGON((\',\'\'),\'))\',\'\') AS "SectorPolygon"
               FROM (
               SELECT
               t1."Sector",
               t1."Block",
               t2."PolygonID",
               t3."Polygon"
               FROM (
               SELECT x."ID" AS "Sector",
               y."SectorBlockList" AS "Block"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',1) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',2) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',3) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',4) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',5) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',6) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',7) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',8) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',9) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',10) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',11) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',12) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',13) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',14) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',15) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',16) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',17) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',18) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',19) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',20) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',21) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',22) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',23) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',24) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',25) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',26) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',27) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',28) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',29) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',30) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',31) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',32) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',33) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',34) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',35) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',36) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',37) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',38) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',39) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',40) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',41) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',42) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',43) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',44) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',45) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',46) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',47) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',48) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',49) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',50) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',51) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',52) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',53) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',54) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',55) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',56) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',57) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',58) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',59) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',60) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',61) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',62) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',63) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',64) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',65) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',66) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',67) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',68) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',69) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               UNION
               SELECT x."ID",
               y."SectorBlockList"
               FROM "Croatia"."ATCSector" AS x
               LEFT JOIN LATERAL (
               SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',70) AS "SectorBlockList"
               FROM "Croatia"."ATCSector"
               WHERE x."ID" = "ID"
               ) AS y ON TRUE
               ) AS t1
               JOIN (
               SELECT "ID" AS "Block", "Polygon" AS "PolygonID"
               FROM "Croatia"."SectorBlock"
               ) AS t2 ON t1."Block" = t2."Block"
               JOIN (
               SELECT "PolygonID", "Polygon"
               FROM "Croatia"."Polygon2"
               ) AS t3 ON t3."PolygonID" = t2."PolygonID"
               WHERE t1."Block" NOT LIKE \'\'
               ORDER BY "Sector"
               ) AS t1
               GROUP BY "Sector"')
  
  table.SectorPolygons <- separate(data=table.SectorPolygons, col=SectorPolygon, into=paste("V", 1:120, sep=""), sep=",")
  table.SectorPolygons <- subset(gather(data=table.SectorPolygons, "V", "Position", -c(Sector)),is.na(Position)==FALSE)
  table.SectorPolygons <- separate(data=table.SectorPolygons, col=Position, into=c("Longitude","Latitude"), sep=" ")
  table.SectorPolygons$Longitude <- as.numeric(table.SectorPolygons$Longitude); table.SectorPolygons$Latitude <- as.numeric(table.SectorPolygons$Latitude)

# Disconnect and write to csv --------------------------------------------------------------
  dbDisconnect(con)
  write.csv(table.SectorPolygons, file="~/Croatia v3.1/data/SectorPolygons.csv", row.names=F)
  write.csv(table.TotalThroughputs, file="~/Croatia v3.1/data/TotalThroughputs.csv", row.names=F)
  write.csv(table.HourlyThroughputs, file="~/Croatia v3.1/data/HourlyThroughputs.csv", row.names=F)
  write.csv(table.RollingHourlyThroughputs, file="~/Croatia v3.1/data/RollingHourlyThroughputs.csv", row.names=F)
  write.csv(table.FuelBurn, file="~/Croatia v3.1/data/FuelBurn.csv", row.names=F)
  write.csv(table.TrackMiles, file="~/Croatia v3.1/data/TrackMiles.csv", row.names=F)
  write.csv(table.Conflicts, file="~/Croatia v3.1/data/Conflicts.csv", row.names=F)
  write.csv(table.ConflictType, file="~/Croatia v3.1/data/ConflictType.csv", row.names=F)
  write.csv(table.LateralConflictType, file="~/Croatia v3.1/data/LateralConflictType.csv", row.names=F)
  write.csv(table.VerticalConflictType, file="~/Croatia v3.1/data/VerticalConflictType.csv", row.names=F)
  write.csv(table.ConflictsFlightPlanPhase, file="~/Croatia v3.1/data/ConflictsFlightPlanPhase.csv", row.names=F)
  write.csv(table.Severity, file="~/Croatia v3.1/data/Severity.csv", row.names=F)
  write.csv(table.VerticalSeverity, file="~/Croatia v3.1/data/VerticalSeverity.csv", row.names=F)
  write.csv(table.SectorOccupancy, file="~/Croatia v3.1/data/SectorOccupancy.csv", row.names=F)
  write.csv(table.SectorEntry, file="~/Croatia v3.1/data/SectorEntry.csv", row.names=F)
  write.csv(table.Workload, file="~/Croatia v3.1/data/Workload.csv", row.names=F)
}
