################## Derry Leng Presents ##################
############### A Rob Sawyer Production #################
################## Shiny Croatia App #################### 
###### With Special Thanks To Pier Carlo Ferraris #######

# VERSION 1.8 --"It Just Works"-- NOTES:
# Added swanky new TMA sector polygons to conflict map
# Centralised airport/sector lists for ease of adding new airports/sectors to list
# Converted seperation distances in Conflict table to NM/ft from m
# Increased plot font size for better visibility
# Removed sector filtering from SQL scripts, filtering is now done entirely in plotting functions
# Removed view button, now plots render automatically on selection change
# Some adaptations to accomodate future PBN data

# Load Packages --------------------------------------------------
library(RPostgreSQL)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(shiny)
library(leaflet)
library(sp)

# Import from database --------------------------------------------------
# Get database connection
# con <- dbConnect(dbDriver("PostgreSQL"), dbname="airtopdb",host="192.168.1.137",port=5432,user="think",password="think")
con <- dbConnect(dbDriver("PostgreSQL"), dbname="airtopdb",host="localhost",port=5432,user="rob",password="rob")

# Current data
table.current.TotalThroughputs <- dbGetQuery(con,'
SELECT
  COUNT("Callsign") AS "Count", 
  "New DepartureOrArrivalAirport" AS "Airport",
  "AircraftState" AS "Category"
FROM "Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"
WHERE "Time_day" = 2
GROUP BY "New DepartureOrArrivalAirport", "AircraftState"
ORDER BY "New DepartureOrArrivalAirport", "AircraftState"')
table.current.HourlyThroughputs <- dbGetQuery(con,'
SELECT
  EXTRACT(HOUR FROM "Time_time") AS "Hour",
  COUNT("Callsign") AS "Count", 
  "New DepartureOrArrivalAirport" AS "Airport",
  "AircraftState" AS "Category"
FROM "Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"
WHERE "Time_day" = 2
GROUP BY EXTRACT(HOUR FROM "Time_time"),"New DepartureOrArrivalAirport", "AircraftState"
ORDER BY "New DepartureOrArrivalAirport", "AircraftState",EXTRACT(HOUR FROM "Time_time")')
table.current.RollingHourlyThroughputs <- dbGetQuery(con,'
SELECT
  "Time_time" AS "Time",
  "AirportStatus" AS "Airport",
  "LiftOffCountInPeriod" AS "RollingLiftOffCount",
  "TouchDownCountInPeriod" AS "RollingTouchDownCount",
  "ThroughputCountInPeriod" AS "RollingThroughputCount"
FROM "Croatia"."RS_RWYTHROUGHPUT"
WHERE "Time_day" = 2
ORDER BY "AirportStatus",  "Time_time"')
table.current.Conflicts <- dbGetQuery(con,'
SELECT 
	"ID",
	"ATCSector" AS "Sector",
	"StartTime_day" AS "Start_Day",
	"StartTime_time" AS "Start_Time",
	"ClosestApproachTime_day" AS "Closest_Day",
	"ClosestApproachTime_time" AS "Closest_Time",
	"EndTime_day" AS "End_Day",
	"EndTime_time" AS "End_Time",
	"ConflictType",
	"Severity",
	"VerticalSeverity",
	"LateralConflictType",
	"VerticalConflictType",
	ROUND("VerticalSeparation_m"::numeric/0.3048, 5) AS "VerticalSeparation",
	ROUND("ReqVerticalSeparation_m"::numeric/0.3048, 5) AS "ReqVerticalSeparation",
	ROUND("LateralSeparation_m"::numeric/1852, 5) AS "LateralSeparation",
	ROUND("ReqLateralSeparation_m"::numeric/1852, 5) AS "ReqLateralSeparation",
	"Altitude_ft",
  "FlightPlanPhase1",
  "FlightPlanPhase2",
	"2DLocation",
	SPLIT_PART("2DLocation",\' \',6)::numeric + SPLIT_PART("2DLocation",\' \',7)::numeric/60 + SPLIT_PART("2DLocation",\' \',8)::numeric/3600 AS "Longitude",
	SPLIT_PART("2DLocation",\' \',2)::numeric + SPLIT_PART("2DLocation",\' \',3)::numeric/60 + SPLIT_PART("2DLocation",\' \',4)::numeric/3600 AS "Latitude"
FROM "Croatia"."Conflict"
WHERE "StartTime_day" = 2')
table.current.ConflictsFlightPlanPhase <- dbGetQuery(con,'
SELECT
	"ATCSector" AS "Sector",
	CASE
		WHEN LOWER("FlightPlanPhase1") < LOWER("FlightPlanPhase2")
		THEN "FlightPlanPhase1" || \' \' || "FlightPlanPhase2"
		ELSE "FlightPlanPhase2" || \' \' || "FlightPlanPhase1"
	END AS "FlightPlanPhases",
	COUNT(*) AS "Count"
FROM "Croatia"."Conflict"
GROUP BY "ATCSector", (CASE WHEN LOWER("FlightPlanPhase1") < LOWER("FlightPlanPhase2") THEN "FlightPlanPhase1" || \' \' || "FlightPlanPhase2" ELSE "FlightPlanPhase2" || \' \' || "FlightPlanPhase1" END)')
table.current.SectorOccupancy <- dbGetQuery(con,'
SELECT
	"Time_time" AS "Time",
	"ATCSector" AS "Sector",
	"AircraftLoad" AS "Count"
FROM "Croatia"."RS_SECOCC"
WHERE "Time_day" = 2')
table.current.SectorEntry <- dbGetQuery(con,'
SELECT
  "Time_time" AS "Time", 
  "ATCSector" AS "Sector",
  "LastPeriodEntryCount" AS "Entries"
FROM "Croatia"."RS_SECENTRY"
WHERE "Time_day" = 2')

# PBN data
# table.PBN.TotalThroughputs <- dbGetQuery(con,'')
# table.PBN.HourlyThroughputs <- dbGetQuery(con,'')
# table.PBN.RollingHourlyThroughputs <- dbGetQuery(con,'')
# table.PBN.Conflicts <- dbGetQuery(con,'')
# table.PBN.ConflictsFlightPlanPhase <- dbGetQuery(con,'')
# table.PBN.SectorOccupancy <- dbGetQuery(con,'')
# table.PBN.SectorEntry <- dbGetQuery(con,'')

# Don't forget to disconnect from database when you're done!
dbDisconnect(con)

# Definitions --------------------------------------------------
# Change these lists if you want to include another airports/sectors
Airport.list <- factor(c("All","LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"), ordered = TRUE)

Sector.list <- factor(c("All","TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB"), ordered = TRUE)

# TMA Polygons (for leaflet mapping purposes)
polygon.DUBROVNIK <- Polygon(cbind(
  c(16.16194444444444,16.35388888888889,17.61638888888889,17.75861111111111,18.19444444444445,18.38111111111111,18.66361111111111,18.45027777777778,17.95305555555555,17.78277777777778,17.68944444444444,17.43083333333334,17.27722222222222,17.22777777777778,17.19944444444445,17.125,16.84694444444444,16.61666666666667,16.2763888888888900,15.93611111111111,16.16194444444444),
  c(42.43361111111111,42.34722222222222,41.75333333333333,41.91861111111111,41.93277777777777,41.90027777777777,41.94388888888889,42.63666666666666,42.89333333333333,42.98083333333334,43.02861111111111,43.18027777777778,43.35166666666667,43.43944444444444,43.49583333333334,43.51694444444444,43.30166666666666,43.12194444444445,42.8518055555555600,42.58166666666667,42.43361111111111)))
polygon.OSIJEK <- Polygon(cbind(
  c(17.865000000000002,17.981666666666666,18.099166666666665,18.21611111111111,18.329166666666666,18.413055555555555,18.519166666666667,18.611944444444447,18.636388888888888,18.772777777777776,18.849444444444444,19.011111111111113,19.012777777777778,19.39638888888889,19.395555555555553,19.0775,19.078055555555554,19.095555555555553,19.035833333333336,18.727777777777778,18.381666666666668,18.002777777777776,17.861666666666668,17.86416666666667,17.865000000000002),
  c(45.770833333333336,45.79194444444444,45.76694444444444,45.78055555555555,45.754444444444445,45.74583333333334,45.784166666666664,45.83638888888889,45.87861111111111,45.88805555555555,45.876666666666665,45.405833333333334,45.400555555555556,45.22416666666667,45.21222222222222,45.126666666666665,45.1225,45.00416666666667,44.90833333333333,45.003055555555555,45.10194444444445,45.06083333333333,45.04472222222222,45.740833333333335,45.770833333333336)))
polygon.PULA <- Polygon(cbind(
  c(12.99555555555556,13.32888888888889,13.49805555555555,13.75972222222222,14.24638888888889,14.39722222222222,14.65916666666667,14.74444444444444,14.98222222222222,15.15,14.94111111111111,14.8675,14.73916666666667,14.6025,14.44861111111111,14.22444444444444,14.19833333333333,14.13388888888889,13.97194444444445,13.77333333333333,13.66666666666667,13.58472222222222,13.53666666666667,13.38722222222222,12.99555555555556,12.99555555555556,12.99555555555556),
  c(45.16638888888889,44.53305555555556,44.38638888888889,44.15722222222222,44.34722222222222,44.40527777777778,44.53305555555556,44.595,44.76611111111111,44.88555555555556,45.48166666666666,45.46583333333334,45.52333333333333,45.49833333333333,45.46916666666667,45.50138888888888,45.47083333333334,45.47555555555557,45.44777777777777,45.46555555555556,45.44749999999999,45.47583333333333,45.49805555555556,45.55805555555556,45.29972222222222,45.2625,45.16638888888889)))
polygon.SPLIT <- Polygon(cbind(
  c(14.42472222222222,14.49555555555556,14.63027777777778,15.93611111111111,17.07527777777778,17.43083333333334,17.27722222222222,17.22777777777778,17.19944444444445,17.125,17.125,17.02083333333333,16.86416666666667,16.72944444444444,16.72944444444444,16.71611111111111,16.67111111111111,16.27444444444445,15.91388888888889,15.90277777777778,15.77583333333334,15.70638888888889,15.64027777777778,15.62027777777778,15.36416666666667,15.20388888888889,14.32888888888889,14.42472222222222),
  c(43.56416666666667,43.5,43.41777777777777,42.58166666666667,42.68,43.18027777777778,43.35166666666667,43.43944444444444,43.49583333333334,43.51694444444444,43.51694444444444,43.56861111111112,43.70972222222223,43.79222222222222,43.79222222222222,43.83611111111112,43.87611111111112,44.1675,44.13916666666667,44.10416666666667,44.14111111111111,44.12222222222222,44.09638888888889,44.11555555555556,44.09527777777778,44.0275,43.65055555555556,43.56416666666667)))
polygon.ZADAR <- Polygon(cbind(
  c(14.63583333333333,14.25916666666667,13.49805555555555,13.75972222222222,14.32888888888889,15.41861111111111,15.6425,15.77861111111111,15.90277777777778,15.91388888888889,15.36555555555556,15.15,14.63583333333333),
  c(44.82694444444445,44.68305555555556,44.38638888888889,44.15722222222222,43.65055555555556,43.62361111111111,43.61666666666667,43.75833333333333,44.10416666666667,44.13916666666667,44.89277777777778,44.88555555555556,44.82694444444445)))
polygon.ZAGREB <- Polygon(cbind(
  c(15.4275,15.35333333333333,15.35,15.34944444444444,15.29722222222222,15.29722222222222,15.09027777777778,15.18333333333334,15.75805555555555,15.76972222222222,15.77777777777778,15.77777777777778,16.90861111111111,16.95416666666667,17,17.05944444444445,17.16388888888889,17.38944444444444,17.29805555555556,17.28916666666667,17.18111111111111,17.05111111111111,16.96611111111111,16.83722222222222,16.72055555555555,16.58861111111111,16.5225,16.46972222222222,16.38138888888889,16.27305555555556,16.26166666666667,16.27416666666667,16.14694444444444,16.05333333333334,16.06694444444445,15.97611111111111,15.90194444444444,15.86666666666667,15.82611111111111,15.67361111111111,15.625,15.70305555555555,15.655,15.49111111111111,15.4275),
  c(45.79444444444443,45.77305555555555,45.66333333333332,45.63583333333333,45.57361111111111,45.57361111111111,45.51666666666667,45.42416666666667,45.07055555555556,45.07888888888888,45.17527777777778,45.17527777777778,44.56888888888889,44.69888888888889,44.83333333333334,44.96472222222223,45.15333333333333,45.93166666666667,45.99027777777778,46.02861111111112,46.12083333333334,46.20333333333333,46.23972222222222,46.37416666666667,46.39138888888889,46.44805555555556,46.47833333333334,46.50416666666667,46.5325,46.5175,46.45500000000001,46.37833333333333,46.40333333333332,46.38805555555555,46.34083333333334,46.30972222222222,46.28666666666666,46.27166666666666,46.26611111111112,46.22638888888889,46.17222222222222,45.97277777777778,45.84027777777778,45.8075,45.79444444444443)))

# Functions --------------------------------------------------
# Rounds up "x" to nearest 10s, for use in setting nice round y axis limits
roundUp <- function(x, limit=10000) {
  range <- seq(10,limit,10)
  if(x <= max(range)) { 
    i <- 1
    while(range[i] < x) {
      i <- i + 1
    }
    return(as.integer(range[i]))
  }
  else stop(paste("Error: Out of range, input value between 0 and", max(range), "or specify custom range for 'x' as RoundUp(x,range)"))
}

# Throughput plots
plotTotalThroughput <- function(airport="All"){
    if (airport == "All") {
      ggplot(subset(table.current.TotalThroughputs, Airport %in% Airport.list),
           aes(x=Airport,y=Count,group=Airport),
           colour=c("Arrivals","Departures")) + 
      geom_bar(stat="identity",
               position="stack",
               width=0.5,
               aes(fill=ordered(Category,levels=c("Descending","GroundAccelerating")))) + 
      labs(y="Count",
           title=paste("Total Throughputs")) +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand=c(0,0),
                         limit=c(0,roundUp(max(aggregate(data=table.current.TotalThroughputs,Count~Airport,"sum")$Count)))) +
      theme(legend.position="right",
            legend.background=element_blank(),
            panel.background=element_rect(fill = "grey97"),
            axis.line = element_line(colour = "black"),
            plot.title = element_text(size=20),
            axis.title = element_text(size=18),
            axis.text = element_text(size=14),
            legend.title = element_text(size=18),
            legend.text = element_text(size=14)) +
      scale_fill_manual(values=c("red4","green4"),
                        name="Category",
                        labels=c("Arrivals","Departures"))
    } else {
      ggplot(subset(table.current.TotalThroughputs, Airport %in% airport),
             aes(x=Category,y=Count),
             colour=c("Arrivals","Departures")) + 
        geom_bar(stat="identity",
                 position="dodge",
                 width=0.5,
                 aes(fill=ordered(Category,levels=c("Descending","GroundAccelerating")))) + 
        labs(y="Count",
             title=paste("Total Throughputs", airport)) +
        scale_x_discrete(expand=c(0,0),
                         labels=c("Arrivals","Departures")) +
        scale_y_continuous(expand=c(0,0),
                           limit=c(0,roundUp(max(aggregate(data=table.current.TotalThroughputs,Count~Airport,"sum")$Count)))) +
        theme(legend.position="right",
              legend.background=element_blank(),
              panel.background=element_rect(fill = "grey97"),
              axis.line = element_line(colour = "black"),
              plot.title = element_text(size=20),
              axis.title = element_text(size=18),
              axis.text = element_text(size=14),
              legend.title = element_text(size=18),
              legend.text = element_text(size=14)) +
        scale_fill_manual(values=c("red4","green4"),
                          name="Category",
                          labels=c("Arrivals","Departures"))
    }
}
plotHourlyThroughput <- function(airport="All"){
    ggplot(data = if (airport == "All") {
      subset(table.current.HourlyThroughputs, Airport %in% Airport.list)
    } else {
      subset(table.current.HourlyThroughputs, Airport %in% airport)
    },
         aes(x=Hour,y=Count,group=Hour),
         colour=c("Arrivals","Departures")) + 
    geom_bar(stat="identity",
             position="stack",
             width=0.5,
             aes(fill=ordered(Category,levels=c("Descending","GroundAccelerating")))) + 
    labs(y="Count",
         title=paste("Hourly Throughputs", airport)) +
    scale_x_continuous(expand=c(0,0),
                       breaks=seq(0,24,1),
                       limits=c(0,24)) +
    scale_y_continuous(expand=c(0,0),
                       limits=c(0,roundUp(if (airport == "All") {
                         max(aggregate(data=table.current.HourlyThroughputs,Count~Hour,"sum")$Count)
                       } else {
                         max(aggregate(data=table.current.HourlyThroughputs,Count~Hour+Airport,"sum")$Count)
                       }))) +
    theme(legend.position="right",
          legend.background=element_blank(),
          panel.background=element_rect(fill = "grey97"),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=20),
          axis.title = element_text(size=18),
          axis.text = element_text(size=14),
          legend.title = element_text(size=18),
          legend.text = element_text(size=14)) +
    scale_fill_manual(values=c("red4","green4"),
                      name="Category",
                      labels=c("Arrivals","Departures"))
}
plotRollingHourlyThroughput <- function(airport="All"){
  temp <- gather(table.current.RollingHourlyThroughputs,"count_type","count",c(RollingThroughputCount,RollingLiftOffCount,RollingTouchDownCount))
  ggplot(data = if (airport == "All") {
    subset(temp, Airport %in% Airport.list)
  } else {
    subset(temp, Airport %in% airport)
  },
  aes(x = as.POSIXct(strptime(Time,format="%H:%M:%S")),
             y = count,
             colour = factor(count_type, ordered = T, levels = c("RollingThroughputCount", "RollingLiftOffCount", "RollingTouchDownCount")))) + 
  geom_line(size=1.2) + 
  labs(x="Time", y="Count",
       title=paste("Rolling Hourly Throughputs", airport)) +
  scale_x_datetime(expand=c(0,0),
                   breaks=c("1 hour"),
                   minor_breaks = c("30 min"),
                   limits=as.POSIXct(strptime(c("00:00","24:00"),format="%H:%M")),
                   date_labels = format("%H:%M")) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,roundUp(if (airport == "All") {
                       max(temp$count)
                     } else {
                       max(table.current.RollingHourlyThroughputs$RollingThroughputCount)
                     }))) +
  theme(legend.position="right",
        legend.background=element_blank(),
        panel.background=element_rect(fill = "grey97"),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size=20),
        axis.title = element_text(size=18),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18),
        legend.text = element_text(size=14)) +
  scale_colour_manual(values = c("RollingThroughputCount" = "red4", "RollingLiftOffCount" = "green4", "RollingTouchDownCount" = "blue4"),
                      labels=c("Total","Arrivals","Departures"),
                      name="Category")
}

# Efficiency plots
# WORK IN PROGRESS

# Safety plots
plotConflictCount <- function(sector="All", group="Conflict Type"){
  if (sector == "All") {
    switch(group,
           "Conflict Type" = 
             ggplot(subset(table.current.Conflicts, Sector %in% Sector.list),
                    aes(x = Sector, group = ConflictType)) + 
             geom_bar(aes(fill = ConflictType),
                      position = "stack") +
             labs(x="Sector", y="Count",
                  title="Conflicts per Sector by Conflict Type") +
             theme(legend.position="right",
                   legend.background=element_blank(),
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=20),
                   axis.title = element_text(size=18),
                   axis.text = element_text(size=14),
                   legend.title = element_text(size=18),
                   legend.text = element_text(size=14)) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(table.current.Conflicts$Sector))),10),
                                limit=c(0,roundUp(max(table(table.current.Conflicts$Sector))))) +
             scale_fill_brewer(name = "Type", palette = "Set3"),
           "Conflict Type (Lateral)" = 
             ggplot(subset(table.current.Conflicts, Sector %in% Sector.list),
                    aes(x = Sector, group = LateralConflictType)) + 
             geom_bar(aes(fill = LateralConflictType),
                      position = "stack") +
             labs(x="Sector", y="Count",
                  title="Conflicts per Sector by Lateral Conflict Type") +
             theme(legend.position="right",
                   legend.background=element_blank(),
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=20),
                   axis.title = element_text(size=18),
                   axis.text = element_text(size=14),
                   legend.title = element_text(size=18),
                   legend.text = element_text(size=14)) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(table.current.Conflicts$Sector))),10),
                                limit=c(0,roundUp(max(table(table.current.Conflicts$Sector))))) +
             scale_fill_brewer(name = "Type", palette = "Set1"),
           "Conflict Type (Vertical)" = 
             ggplot(subset(table.current.Conflicts, Sector %in% Sector.list),
                    aes(x = Sector, group = VerticalConflictType)) + 
             geom_bar(aes(fill = VerticalConflictType),
                      position = "stack") +
             labs(x="Sector", y="Count",
                  title="Conflicts per Sector by Vertical Conflict Type") +
             theme(legend.position="right",
                   legend.background=element_blank(),
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=20),
                   axis.title = element_text(size=18),
                   axis.text = element_text(size=14),
                   legend.title = element_text(size=18),
                   legend.text = element_text(size=14)) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(table.current.Conflicts$Sector))),10),
                                limit=c(0,roundUp(max(table(table.current.Conflicts$Sector))))) +
             scale_fill_brewer(name = "Type", palette = "Set1"),
           "Flight Phase" = 
             ggplot(subset(table.current.ConflictsFlightPlanPhase, Sector %in% Sector.list),
                    aes(x = Sector, y = Count)) + 
             geom_bar(aes(fill = FlightPlanPhases),
                      stat = "identity",
                      position = "stack") +
             labs(x="Sector", y="Count",
                  title="Conflicts per Sector by Flight Plan Phase of Aircraft Pair") +
             theme(legend.position="right",
                   legend.background=element_blank(),
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=20),
                   axis.title = element_text(size=18),
                   axis.text = element_text(size=14),
                   legend.title = element_text(size=18),
                   legend.text = element_text(size=14)) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(aggregate(data=table.current.ConflictsFlightPlanPhase, Count~Sector, "sum")$Count)),10),
                                limit=c(0,roundUp(max(aggregate(data=table.current.ConflictsFlightPlanPhase, Count~Sector, "sum")$Count)))) +
             scale_fill_brewer(name = "Phase", palette = "Set1"),
           "Severity" = 
             ggplot(subset(table.current.Conflicts, Sector %in% Sector.list),
                    aes(x = Sector, group = factor(Severity))) + 
             geom_bar(aes(fill = factor(Severity)),
                      position = "stack") +
             labs(x="Sector", y="Count",
                  title="Conflicts per Sector by Severity") +
             theme(legend.position="right",
                   legend.background=element_blank(),
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=20),
                   axis.title = element_text(size=18),
                   axis.text = element_text(size=14),
                   legend.title = element_text(size=18),
                   legend.text = element_text(size=14)) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(table.current.Conflicts$Sector))),10),
                                limit=c(0,roundUp(max(table(table.current.Conflicts$Sector))))) +
             scale_fill_brewer(name = "Severity", palette = "YlOrRd"),
           "Severity (Vertical)" = 
             ggplot(subset(table.current.Conflicts, Sector %in% Sector.list),
                    aes(x = Sector, group = factor(VerticalSeverity))) + 
             geom_bar(aes(fill = factor(VerticalSeverity)),
                      position = "stack") +
             labs(x="Sector", y="Count",
                  title="Conflicts per Sector by Vertical Severity") +
             theme(legend.position="right",
                   legend.background=element_blank(),
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=20),
                   axis.title = element_text(size=18),
                   axis.text = element_text(size=14),
                   legend.title = element_text(size=18),
                   legend.text = element_text(size=14)) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(table.current.Conflicts$Sector))),10),
                                limit=c(0,roundUp(max(table(table.current.Conflicts$Sector))))) +
             scale_fill_brewer(name = "Severity", palette = "YlOrRd")
    )
  } else {
    switch(group,
           "Conflict Type" = 
             ggplot(subset(table.current.Conflicts, Sector %in% sector),
                    aes(x = ConflictType)) + 
             geom_bar(aes(fill = ConflictType),
                      position = "stack") +
             labs(x="Conflict Type", y="Count",
                  title="Conflicts per Sector by Conflict Type") +
             theme(legend.position="none",
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=20),
                   axis.title = element_text(size=18),
                   axis.text = element_text(size=14),
                   legend.title = element_text(size=18),
                   legend.text = element_text(size=14)) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(subset(table.current.Conflicts, Sector %in% sector)$ConflictType))),2),
                                limit=c(0,roundUp(max(table(subset(table.current.Conflicts, Sector %in% sector)$ConflictType))))) +
             scale_fill_brewer(palette = "Spectral"),
           "Conflict Type (Lateral)" = 
             ggplot(subset(table.current.Conflicts, Sector %in% sector),
                    aes(x = LateralConflictType)) + 
             geom_bar(aes(fill = LateralConflictType),
                      position = "stack") +
             labs(x="Conflict Type", y="Count",
                  title="Conflicts per Sector by Lateral Conflict Type") +
             theme(legend.position="none",
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=20),
                   axis.title = element_text(size=18),
                   axis.text = element_text(size=14),
                   legend.title = element_text(size=18),
                   legend.text = element_text(size=14)) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(subset(table.current.Conflicts, Sector %in% sector)$LateralConflictType))),2),
                                limit=c(0,roundUp(max(table(subset(table.current.Conflicts, Sector %in% sector)$LateralConflictType))))) +
             scale_fill_brewer(palette = "Set1"),
           "Conflict Type (Vertical)" = 
             ggplot(subset(table.current.Conflicts, Sector %in% sector),
                    aes(x = VerticalConflictType)) + 
             geom_bar(aes(fill = VerticalConflictType),
                      position = "stack") +
             labs(x="Conflict Type", y="Count",
                  title="Conflicts per Sector by Vertical Conflict Type") +
             theme(legend.position="none",
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=20),
                   axis.title = element_text(size=18),
                   axis.text = element_text(size=14),
                   legend.title = element_text(size=18),
                   legend.text = element_text(size=14)) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(subset(table.current.Conflicts, Sector %in% sector)$VerticalConflictType))),2),
                                limit=c(0,roundUp(max(table(subset(table.current.Conflicts, Sector %in% sector)$VerticalConflictType))))) +
             scale_fill_brewer(palette = "Set1"),
           "Flight Phase" = 
             ggplot(subset(table.current.ConflictsFlightPlanPhase, Sector %in% sector),
                    aes(x = FlightPlanPhases, y = Count)) + 
             geom_bar(aes(fill = FlightPlanPhases),
                      stat = "identity",
                      position = "stack") +
             labs(x="Flight Plan Phase", y="Count",
                  title=paste("Conflicts in sector", sector, "by Flight Plan Phase of Aircraft Pair")) +
             theme(legend.position="none",
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=20),
                   axis.title = element_text(size=18),
                   axis.text = element_text(size=14),
                   legend.title = element_text(size=18),
                   legend.text = element_text(size=14)) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(subset(table.current.ConflictsFlightPlanPhase, Sector %in% sector)$Count)),10),
                                limit=c(0,roundUp(max(subset(table.current.ConflictsFlightPlanPhase, Sector %in% sector)$Count)))) +
             scale_fill_brewer(palette = "Set1"),
           "Severity" = 
             ggplot(subset(table.current.Conflicts, Sector %in% sector),
                    aes(x = factor(Severity))) + 
             geom_bar(aes(fill = factor(Severity)),
                      position = "stack") +
             labs(x="Severity", y="Count",
                  title="Conflicts per Sector by Severity") +
             theme(legend.position="none",
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=20),
                   axis.title = element_text(size=18),
                   axis.text = element_text(size=14),
                   legend.title = element_text(size=18),
                   legend.text = element_text(size=14)) +
             scale_x_discrete(expand = c(0,0),
                              labels = c("0","1","2","3","4","5","6"),
                              limit = c("0","1","2","3","4","5","6")) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(subset(table.current.Conflicts, Sector %in% sector)$Severity))),5),
                                limit=c(0,roundUp(max(table(subset(table.current.Conflicts, Sector %in% sector)$Severity))))) +
             scale_fill_brewer(palette = "YlOrRd"),
           "Severity (Vertical)" = 
             ggplot(subset(table.current.Conflicts, Sector %in% sector),
                    aes(x = factor(VerticalSeverity))) + 
             geom_bar(aes(fill = factor(VerticalSeverity)),
                      position = "stack") +
             labs(x="Severity", y="Count",
                  title="Conflicts per Sector by Vertical Severity") +
             theme(legend.position="none",
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   plot.title = element_text(size=20),
                   axis.title = element_text(size=18),
                   axis.text = element_text(size=14),
                   legend.title = element_text(size=18),
                   legend.text = element_text(size=14)) +
             scale_x_discrete(expand = c(0,0),
                              labels = c("0","1","2","3","4","5","6"),
                              limit = c("0","1","2","3","4","5","6")) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(subset(table.current.Conflicts, Sector %in% sector)$VerticalSeverity))),5),
                                limit=c(0,roundUp(max(table(subset(table.current.Conflicts, Sector %in% sector)$VerticalSeverity))))) +
             scale_fill_brewer(palette = "YlOrRd")
    )
  }
}
plotConflictMap <- function(){
    Conflicts.labels <- sprintf(
    "ID: %s<br/>Sector: %s<br/>Time: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (NM): %s (Req. %s)<br/>Altitude (ft): %s",
    table.current.Conflicts$ID,table.current.Conflicts$Sector,table.current.Conflicts$Closest_Time,table.current.Conflicts$ConflictType,table.current.Conflicts$FlightPlanPhase1,table.current.Conflicts$FlightPlanPhase2,table.current.Conflicts$LateralSeparation,table.current.Conflicts$ReqLateralSeparation,table.current.Conflicts$VerticalSeparation,table.current.Conflicts$ReqVerticalSeparation,table.current.Conflicts$Altitude_ft) %>% lapply(htmltools::HTML)
    
  leaflet() %>% 
    setView(lng=16.8, lat=44.2, zoom=6) %>%
    addTiles(options = providerTileOptions(noWrap = TRUE), group="Default") %>%
    addProviderTiles(providers$CartoDB.Positron, group="Greyscale") %>% 
    addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
    addPolygons(data = polygon.DUBROVNIK,
                group="TMA Sectors",
                weight = 5,
                opacity = 0.5,
                color = "gray",
                dashArray = "18",
                label = "TMA Dubrovnik",
                labelOptions = labelOptions(noHide=T,
                                            textOnly = T,
                                            style = list("font-weight" = "bold"),
                                            opacity = 0.8,
                                            textsize = "12px",
                                            direction = "auto"),
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.35,
                                             bringToFront = F)) %>%
    addPolygons(data = polygon.SPLIT,
                group="TMA Sectors",
                weight = 5,
                opacity = 0.5,
                color = "gray",
                dashArray = "18",
                label = "TMA Split",
                labelOptions = labelOptions(noHide=T,
                                            textOnly = T,
                                            style = list("font-weight" = "bold"),
                                            opacity = 0.8,
                                            textsize = "12px",
                                            direction = "auto"),
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.35,
                                             bringToFront = F)) %>%
    addPolygons(data = polygon.ZADAR,
                group="TMA Sectors",
                weight = 5,
                opacity = 0.5,
                color = "gray",
                dashArray = "18",
                label = "TMA Zadar",
                labelOptions = labelOptions(noHide=T,
                                            textOnly = T,
                                            style = list("font-weight" = "bold"),
                                            opacity = 0.8,
                                            textsize = "12px",
                                            direction = "auto"),
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.35,
                                             bringToFront = F)) %>%
    addPolygons(data = polygon.PULA,
                group="TMA Sectors",
                weight = 5,
                opacity = 0.5,
                color = "gray",
                dashArray = "18",
                label = "TMA Pula",
                labelOptions = labelOptions(noHide=T,
                                            textOnly = T,
                                            style = list("font-weight" = "bold"),
                                            opacity = 0.8,
                                            textsize = "12px",
                                            direction = "auto"),
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.35,
                                             bringToFront = F)) %>%
    addPolygons(data = polygon.ZAGREB,
                group="TMA Sectors",
                weight = 5,
                opacity = 0.5,
                color = "gray",
                dashArray = "18",
                label = "TMA Zagreb",
                labelOptions = labelOptions(noHide=T,
                                            textOnly = T,
                                            style = list("font-weight" = "bold"),
                                            opacity = 0.8,
                                            textsize = "12px",
                                            direction = "auto"),
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.35,
                                             bringToFront = F)) %>%
    addPolygons(data = polygon.OSIJEK,
                group="TMA Sectors",
                weight = 5,
                opacity = 0.5,
                color = "gray",
                dashArray = "18",
                label = "TMA Osijek",
                labelOptions = labelOptions(noHide=T,
                                            textOnly = T,
                                            style = list("font-weight" = "bold"),
                                            opacity = 0.8,
                                            textsize = "12px",
                                            direction = "auto"),
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.35,
                                             bringToFront = F)) %>%
    addCircleMarkers(data=subset(table.current.Conflicts, !Sector %in% c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")),
                     lng=~Longitude ,
                     lat=~Latitude,
                     color="black",
                     fillColor="yellow",
                     label=Conflicts.labels,
                     labelOptions = labelOptions(textsize = "13px", direction="auto"),
                     radius=5, stroke = TRUE, fillOpacity = 0.8, group="Non-TMA Conflicts") %>%
    addCircleMarkers(data=subset(table.current.Conflicts, Sector %in% c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")),
                     lng=~Longitude ,
                     lat=~Latitude,
                     color="black",
                     fillColor="red",
                     label=Conflicts.labels,
                     labelOptions = labelOptions(textsize = "13px", direction="auto"),
                     radius=5, stroke = TRUE, fillOpacity = 0.8, group="TMA Conflicts") %>%
    addLayersControl(overlayGroups = c("TMA Sectors", "TMA Conflicts", "Non-TMA Conflicts"),
                     baseGroups = c("Default","Greyscale","Satellite"),
                     options = layersControlOptions(collapsed = FALSE))
}

# Sector Capacity plots
plotSectorOccupancy <- function(sector="All"){
  if (sector == "All") {
    ggplot(subset(table.current.SectorOccupancy, Sector %in% Sector.list),
           aes(x = as.POSIXct(strptime(Time,format="%H:%M:%S")),
               y = Count,
               colour = factor(Sector, ordered = T, levels = Sector.list))) +
      geom_line(size=1.2) +
      labs(x="Time", y="Count",
           title=paste("Number of aircraft in sector", sector)) +
      scale_x_datetime(expand=c(0,0),
                       breaks=c("1 hour"),
                       minor_breaks = c("30 min"),
                       limits=as.POSIXct(strptime(c("00:00","24:00"),format="%H:%M")),
                       date_labels = format("%H:%M")) +
      scale_y_continuous(expand=c(0,0),
                         breaks=seq(0,roundUp(max(table.current.SectorOccupancy$Count)),2),
                         limits=c(0,roundUp(max(table.current.SectorOccupancy$Count)))) +
      theme(legend.position="right",
            legend.background=element_blank(),
            panel.background=element_rect(fill = "grey97"),
            axis.line = element_line(colour = "black"),
            plot.title = element_text(size=20),
            axis.title = element_text(size=18),
            axis.text = element_text(size=14),
            legend.title = element_text(size=18),
            legend.text = element_text(size=14)) +
      scale_colour_manual(values = rainbow(6),
                          name="Sector")
  } else {
    ggplot(subset(table.current.SectorOccupancy, Sector %in% sector),
           aes(x = as.POSIXct(strptime(Time,format="%H:%M:%S")),
               y = Count,
               colour = factor(Sector, ordered = T, levels = Sector.list))) +
      geom_line(size=1.2) +
      labs(x="Time", y="Count",
           title=paste("Number of aircraft in sector", sector)) +
      scale_x_datetime(expand=c(0,0),
                       breaks=c("1 hour"),
                       minor_breaks = c("30 min"),
                       limits=as.POSIXct(strptime(c("00:00","24:00"),format="%H:%M")),
                       date_labels = format("%H:%M")) +
      scale_y_continuous(expand=c(0,0),
                         breaks=seq(0,roundUp(max(table.current.SectorOccupancy$Count)),2),
                         limits=c(0,roundUp(max(subset(table.current.SectorOccupancy, Sector %in% sector)$Count)))) +
      theme(legend.position="none",
            panel.background=element_rect(fill = "grey97"),
            axis.line = element_line(colour = "black"),
            plot.title = element_text(size=20),
            axis.title = element_text(size=18),
            axis.text = element_text(size=14),
            legend.title = element_text(size=18),
            legend.text = element_text(size=14)) +
      scale_colour_manual(values = "blue4",
                          name="")
  }
}
plotSectorEntry <- function(sector="All"){
  if (sector == "All") {
    ggplot(subset(table.current.SectorEntry, Sector %in% Sector.list),
           aes(x = as.POSIXct(strptime(Time,format="%H:%M:%S")),
               y = Entries,
               colour = factor(Sector, ordered = T, levels = Sector.list))) +
      geom_line(size=1.2) +
      labs(x="Time", y="Count",
           title=paste("Number of entries into sector", sector)) +
      scale_x_datetime(expand=c(0,0),
                       breaks=c("1 hour"),
                       minor_breaks = c("30 min"),
                       limits=as.POSIXct(strptime(c("00:00","24:00"),format="%H:%M")),
                       date_labels = format("%H:%M")) +
      scale_y_continuous(expand=c(0,0),
                         breaks=seq(0,roundUp(max(table.current.SectorEntry$Entries)),5),
                         limits=c(0,roundUp(max(table.current.SectorEntry$Entries)))) +
      theme(legend.position="right",
            legend.background=element_blank(),
            panel.background=element_rect(fill = "grey97"),
            axis.line = element_line(colour = "black"),
            plot.title = element_text(size=20),
            axis.title = element_text(size=18),
            axis.text = element_text(size=14),
            legend.title = element_text(size=18),
            legend.text = element_text(size=14)) +
      scale_colour_manual(values = rainbow(6),
                          name="Sector")
  } else {
    ggplot(subset(table.current.SectorEntry, Sector %in% sector),
           aes(x = as.POSIXct(strptime(Time,format="%H:%M:%S")),
               y = Entries,
               colour = factor(Sector, ordered = T, levels = Sector.list))) +
      geom_line(size=1.2) +
      labs(x="Time", y="Count",
           title=paste("Number of entries into sector", sector)) +
      scale_x_datetime(expand=c(0,0),
                       breaks=c("1 hour"),
                       minor_breaks = c("30 min"),
                       limits=as.POSIXct(strptime(c("00:00","24:00"),format="%H:%M")),
                       date_labels = format("%H:%M")) +
      scale_y_continuous(expand=c(0,0),
                         breaks=seq(0,roundUp(max(table.current.SectorEntry$Entries)),5),
                         limits=c(0,roundUp(max(subset(table.current.SectorEntry, Sector %in% sector)$Entries)))) +
      theme(legend.position="none",
            panel.background=element_rect(fill = "grey97"),
            axis.line = element_line(colour = "black"),
            plot.title = element_text(size=20),
            axis.title = element_text(size=18),
            axis.text = element_text(size=14),
            legend.title = element_text(size=18),
            legend.text = element_text(size=14)) +
      scale_colour_manual(values = "blue4",
                          name="")
  }
}

# Shiny App --------------------------------------------------
ui <- fluidPage(
  br(),
  div(style="width: auto;height: auto;position: relative;float: left;", tabsetPanel(id = "tabset",
              tabPanel("Throughput",
                       div(style="display: inline-block;vertical-align:top; width: 200px;",
                           selectInput("metric1",
                                       label = "Select metric:",
                                       choices = c("Total Throughputs","Hourly Throughputs", "Rolling Hourly Throughputs"))),
                       div(style="display: inline-block;vertical-align:top; width: 200px;",
                           selectInput("airport1",
                                       label = "Select airport:",
                                       choices = Airport.list))
              ),
              # tabPanel("Efficiency"),
              tabPanel("Safety",
                       div(style="display: inline-block;vertical-align:top; width: 200px;",
                           selectInput("metric3",
                                       label = "Select metric:",
                                       choices = c("Conflict Statistics", "Conflict Map"))),
                       div(style="display: inline-block;vertical-align:top; width: 200px;",
                           selectInput("sector3",
                                       label = "Select sector:",
                                       choices = Sector.list)),
                       div(style="display: inline-block;vertical-align:top; width: 200px;",
                           selectInput("group3",
                                       label = "Select grouping:",
                                       choices = c("Conflict Type","Conflict Type (Lateral)","Conflict Type (Vertical)","Flight Phase","Severity","Severity (Vertical)")))
              ),
              tabPanel("Capacity",
                       div(style="display: inline-block;vertical-align:top; width: 200px;",
                           selectInput("metric4",
                                       label = "Select metric:",
                                       choices = c("Sector Occupancy Count","Sector Entry Count"))),
                       div(style="display: inline-block;vertical-align:top; width: 200px;",
                           selectInput("sector4",
                                       label = "Select sector:",
                                       choices = Sector.list))
              )
              
  )),
  br(),
  uiOutput("display")
)

server <- function(input, output, session){
  
  observeEvent(input$tabset, {
    removeUI(
      selector = "display"
    )
  })
  
  output$display <- renderUI({
      if (input$tabset == "Safety" & input$metric3 == "Conflict Map") {
        tagList(
        leafletOutput("map", height="800px")
        #absolutePanel(top=20, left=70, textInput("search_map", "" , ""))
        )
      } else {
        plotOutput("plot", height="800px")
      }
  })
  
  observe({
    if (input$metric3 == "Conflict Statistics") {
      updateSelectInput(session,
                        "sector3",
                        label = "Select sector:",
                        choices = Sector.list)
      updateSelectInput(session,
                        "group3",
                        label = "Select grouping:",
                        choices = c("Conflict Type","Conflict Type (Lateral)","Conflict Type (Vertical)","Flight Phase","Severity","Severity (Vertical)"))
    } else if (input$metric3 == "Conflict Map") {
      updateSelectInput(session,
                        "sector3",
                        label = "Select sector:",
                        choices = c("None"))
      updateSelectInput(session,
                        "group3",
                        label = "Select grouping:",
                        choices = c("None"))
    }
  })
  
  output$map <- renderLeaflet({
    isolate({
      if (input$tabset == "Safety") {
        if (input$metric3 == "Conflict Map") {
          plotConflictMap()
        }
      }
    })
  })
  
  output$plot <- renderPlot({
      if (input$tabset == "Throughput") {
        if (input$metric1 == "Total Throughputs") {
          plotTotalThroughput(airport = input$airport1)
        } else if (input$metric1 == "Hourly Throughputs") {
          plotHourlyThroughput(airport = input$airport1)
        } else if (input$metric1 == "Rolling Hourly Throughputs") {
          plotRollingHourlyThroughput(airport = input$airport1)
        }
      } else if (input$tabset == "Efficiency") {
        
      } else if (input$tabset == "Safety") {
        if (input$metric3 == "Conflict Statistics") {
          plotConflictCount(sector = input$sector3, group = input$group3)
        }
      } else if (input$tabset == "Capacity") {
        if (input$metric4 == "Sector Occupancy Count") {
          plotSectorOccupancy(sector = input$sector4)
        } else if (input$metric4 == "Sector Entry Count") {
          plotSectorEntry(sector = input$sector4)
        }
      }
  })
}

# shinyApp(ui, server, options=c(host="0.0.0.0", port=8788)) # host="192.168.1.137", port=8788
shinyApp(ui, server)