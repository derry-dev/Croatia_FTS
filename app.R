################## Derry Leng Presents ##################
############### A Rob Sawyer Production #################
################## Shiny Croatia App #################### 
###### With Special Thanks To Pier Carlo Ferraris #######

# Load Packages --------------------------------------------------
library(RPostgreSQL)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(tidyr)
library(shiny)
library(leaflet)
library(sp)
#library(plotly)

# Import from database --------------------------------------------------
#con <- dbConnect(dbDriver("PostgreSQL"), dbname="airtopdb",host="192.168.1.137",port=5432,user="think",password="think")
con <- dbConnect(dbDriver("PostgreSQL"), dbname="airtopdb",host="localhost",port=5432,user="rob",password="rob")
TotalThroughputs <- dbGetQuery(con,'
                               SELECT
                               COUNT("Callsign") AS "Count", 
                               "New DepartureOrArrivalAirport" AS "Airport",
                               "AircraftState" AS "Category"
                               FROM "Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"
                               WHERE "Time_day" = 2
                               GROUP BY "New DepartureOrArrivalAirport", "AircraftState"
                               ORDER BY "New DepartureOrArrivalAirport", "AircraftState"')
HourlyThroughputs <- dbGetQuery(con,'
                                SELECT
                                EXTRACT(HOUR FROM "Time_time") AS "Hour",
                                COUNT("Callsign") AS "Count", 
                                "New DepartureOrArrivalAirport" AS "Airport",
                                "AircraftState" AS "Category"
                                FROM "Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"
                                WHERE "Time_day" = 2
                                GROUP BY EXTRACT(HOUR FROM "Time_time"),"New DepartureOrArrivalAirport", "AircraftState"
                                ORDER BY "New DepartureOrArrivalAirport", "AircraftState",EXTRACT(HOUR FROM "Time_time")')
RollingHourlyThroughputs <- dbGetQuery(con,'
                                       SELECT
                                       "Time_time" AS "Time",
                                       "AirportStatus" AS "Airport",
                                       "LiftOffCountInPeriod" AS "RollingLiftOffCount",
                                       "TouchDownCountInPeriod" AS "RollingTouchDownCount",
                                       "ThroughputCountInPeriod" AS "RollingThroughputCount"
                                       FROM "Croatia"."RS_RWYTHROUGHPUT"
                                       WHERE "Time_day" = 2
                                       ORDER BY "AirportStatus",  "Time_time"')
Conflicts <- dbGetQuery(con,'
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
                        "VerticalSeparation_m",
                        "ReqVerticalSeparation_m",
                        "LateralSeparation_m",
                        "ReqLateralSeparation_m",
                        "Altitude_ft",
                        "FlightPlanPhase1",
                        "FlightPlanPhase2",
                        "2DLocation",
                        SPLIT_PART("2DLocation",\' \',6)::numeric + SPLIT_PART("2DLocation",\' \',7)::numeric/60 + SPLIT_PART("2DLocation",\' \',8)::numeric/3600 AS "Longitude",
                        SPLIT_PART("2DLocation",\' \',2)::numeric + SPLIT_PART("2DLocation",\' \',3)::numeric/60 + SPLIT_PART("2DLocation",\' \',4)::numeric/3600 AS "Latitude"
                        FROM "Croatia"."Conflict"
                        WHERE "ATCSector" IN (\'TMA_DUBROVNIK\', \'TMA_OSIJEK\', \'TMA_PULA\', \'TMA_SPLIT\', \'TMA_ZADAR\', \'TMA_ZAGREB\')
                        AND "StartTime_day" = 2')
ConflictsFlightPlanPhase <- dbGetQuery(con,'
                                       SELECT
                                       "ATCSector" AS "Sector",
                                       CASE
                                       WHEN LOWER("FlightPlanPhase1") < LOWER("FlightPlanPhase2")
                                       THEN "FlightPlanPhase1" || \' \' || "FlightPlanPhase2"
                                       ELSE "FlightPlanPhase2" || \' \' || "FlightPlanPhase1"
                                       END AS "FlightPlanPhases",
                                       COUNT(*) AS "Count"
                                       FROM "Croatia"."Conflict"
                                       WHERE "ATCSector" IN (\'TMA_DUBROVNIK\', \'TMA_OSIJEK\', \'TMA_PULA\', \'TMA_SPLIT\', \'TMA_ZADAR\', \'TMA_ZAGREB\')
                                       GROUP BY "ATCSector", (CASE WHEN LOWER("FlightPlanPhase1") < LOWER("FlightPlanPhase2") THEN "FlightPlanPhase1" || \' \' || "FlightPlanPhase2" ELSE "FlightPlanPhase2" || \' \' || "FlightPlanPhase1" END)
                                       ORDER BY "ATCSector"')
SectorOccupancy <- dbGetQuery(con,'
                              SELECT
                              "Time_time" AS "Time",
                              "ATCSector" AS "Sector",
                              "AircraftLoad" AS "Count"
                              FROM "Croatia"."RS_SECOCC"
                              WHERE "Time_day" = 2
                              AND "ATCSector" IN (\'TMA_DUBROVNIK\', \'TMA_OSIJEK\', \'TMA_PULA\', \'TMA_SPLIT\', \'TMA_ZADAR\', \'TMA_ZAGREB\')
                              ORDER BY "ATCSector", "Time_time"')
SectorEntry <- dbGetQuery(con,'
                          SELECT
                          "Time_time" AS "Time", 
                          "ATCSector" AS "Sector",
                          "LastPeriodEntryCount" AS "Entries"
                          FROM "Croatia"."RS_SECENTRY"
                          WHERE "Time_day" = 2
                          AND "ATCSector" IN (\'TMA_DUBROVNIK\', \'TMA_OSIJEK\', \'TMA_PULA\', \'TMA_SPLIT\', \'TMA_ZADAR\', \'TMA_ZAGREB\')
                          ORDER BY "ATCSector", "Time_time"')
dbDisconnect(con)

# All the functions --------------------------------------------------
roundUp <- function(x, range=seq(10,10000,10)) { # Rounds up to nearest 10
  if(x <= max(range)) { 
    i <- 1
    while(range[i] < x) {
      i <- i + 1
    }
    return(as.integer(range[i]))
  }
  else stop(paste("Error: Out of range, input value between 0 and", max(range), "or specify custom range for 'x' as RoundUp(x,range)"))
}

plotTotalThroughput <- function(x="All"){
  if (x == "All") {
    ggplot(subset(TotalThroughputs, Airport %in% c("LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS")),
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
                         limit=c(0,roundUp(max(aggregate(data=TotalThroughputs,Count~Airport,"sum")$Count)))) +
      theme(legend.position="right",
            legend.background=element_blank(),
            panel.background=element_rect(fill = "grey97"),
            axis.line = element_line(colour = "black")) +
      scale_fill_manual(values=c("red4","green4"),
                        name="Category",
                        labels=c("Arrivals","Departures"))
  } else {
    ggplot(subset(TotalThroughputs, Airport %in% x),
           aes(x=Category,y=Count),
           colour=c("Arrivals","Departures")) + 
      geom_bar(stat="identity",
               position="dodge",
               width=0.5,
               aes(fill=ordered(Category,levels=c("Descending","GroundAccelerating")))) + 
      labs(y="Count",
           title=paste("Total Throughputs", x)) +
      scale_x_discrete(expand=c(0,0),
                       labels=c("Arrivals","Departures")) +
      scale_y_continuous(expand=c(0,0),
                         limit=c(0,roundUp(max(aggregate(data=TotalThroughputs,Count~Airport,"sum")$Count)))) +
      theme(legend.position="right",
            legend.background=element_blank(),
            panel.background=element_rect(fill = "grey97"),
            axis.line = element_line(colour = "black")) +
      scale_fill_manual(values=c("red4","green4"),
                        name="Category",
                        labels=c("Arrivals","Departures"))
  }
}

plotHourlyThroughput <- function(x="All"){
  ggplot(data = if (x == "All") {
    subset(HourlyThroughputs, Airport %in% c("LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"))
  } else {
    subset(HourlyThroughputs, Airport %in% x)
  },
  aes(x=Hour,y=Count,group=Hour),
  colour=c("Arrivals","Departures")) + 
    geom_bar(stat="identity",
             position="stack",
             width=0.5,
             aes(fill=ordered(Category,levels=c("Descending","GroundAccelerating")))) + 
    labs(y="Count",
         title=paste("Hourly Throughputs", x)) +
    scale_x_continuous(expand=c(0,0),
                       breaks=seq(0,24,1),
                       limits=c(0,24)) +
    scale_y_continuous(expand=c(0,0),
                       limits=c(0,roundUp(if (x == "All") {
                         max(aggregate(data=HourlyThroughputs,Count~Hour,"sum")$Count)
                       } else {
                         max(aggregate(data=HourlyThroughputs,Count~Hour+Airport,"sum")$Count)
                       }))) +
    theme(legend.position="right",
          legend.background=element_blank(),
          panel.background=element_rect(fill = "grey97"),
          axis.line = element_line(colour = "black")) +
    scale_fill_manual(values=c("red4","green4"),
                      name="Category",
                      labels=c("Arrivals","Departures"))
}

plotRollingHourlyThroughput <- function(x="All"){
  temp <- gather(RollingHourlyThroughputs,"count_type","count",c(RollingThroughputCount,RollingLiftOffCount,RollingTouchDownCount))
  ggplot(data = if (x == "All") {
    subset(temp, Airport %in% c("LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"))
  } else {
    subset(temp, Airport %in% x)
  },
  aes(x = as.POSIXct(strptime(Time,format="%H:%M:%S")),
      y = count,
      colour = factor(count_type, ordered = T, levels = c("RollingThroughputCount", "RollingLiftOffCount", "RollingTouchDownCount")))) + 
    geom_line(size=1.2) + 
    labs(x="Time", y="Count",
         title=paste("Rolling Hourly Throughputs", x)) +
    scale_x_datetime(expand=c(0,0),
                     breaks=c("1 hour"),
                     minor_breaks = c("30 min"),
                     limits=as.POSIXct(strptime(c("00:00","24:00"),format="%H:%M")),
                     date_labels = format("%H:%M")) +
    scale_y_continuous(expand=c(0,0),
                       limits=c(0,roundUp(if (x == "All") {
                         max(temp$count)
                       } else {
                         max(RollingHourlyThroughputs$RollingThroughputCount)
                       }))) +
    theme(legend.position="right",
          legend.background=element_blank(),
          panel.background=element_rect(fill = "grey97"),
          axis.line = element_line(colour = "black")) +
    scale_colour_manual(values = c("RollingThroughputCount" = "red4", "RollingLiftOffCount" = "green4", "RollingTouchDownCount" = "blue4"),
                        labels=c("Total","Arrivals","Departures"),
                        name="Category")
}

plotConflictCount <- function(sector="TMA_All", group="Conflict Type"){
  if (sector == "TMA_All") {
    switch(group,
           "Conflict Type" = 
             ggplot(Conflicts,
                    aes(x = Sector, group = ConflictType)) + 
             geom_bar(aes(fill = ConflictType),
                      position = "stack") +
             labs(x="TMA Sector", y="Count",
                  title="Conflicts per Sector by Conflict Type") +
             theme(legend.position="right",
                   legend.background=element_blank(),
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black")) +
             scale_x_discrete(expand = c(0,0),
                              labels = c("Dubrovnik","Osijek","Pula","Split","Zadar","Zagreb")) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(Conflicts$Sector))),10),
                                limit=c(0,roundUp(max(table(Conflicts$Sector))))) +
             scale_fill_brewer(name = "Type", palette = "Set3"),
           "Conflict Type (Lateral)" = 
             ggplot(Conflicts,
                    aes(x = Sector, group = LateralConflictType)) + 
             geom_bar(aes(fill = LateralConflictType),
                      position = "stack") +
             labs(x="TMA Sector", y="Count",
                  title="Conflicts per Sector by Lateral Conflict Type") +
             theme(legend.position="right",
                   legend.background=element_blank(),
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black")) +
             scale_x_discrete(expand = c(0,0),
                              labels = c("Dubrovnik","Osijek","Pula","Split","Zadar","Zagreb")) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(Conflicts$Sector))),10),
                                limit=c(0,roundUp(max(table(Conflicts$Sector))))) +
             scale_fill_brewer(name = "Type", palette = "Set1"),
           "Conflict Type (Vertical)" = 
             ggplot(Conflicts,
                    aes(x = Sector, group = VerticalConflictType)) + 
             geom_bar(aes(fill = VerticalConflictType),
                      position = "stack") +
             labs(x="TMA Sector", y="Count",
                  title="Conflicts per Sector by Vertical Conflict Type") +
             theme(legend.position="right",
                   legend.background=element_blank(),
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black")) +
             scale_x_discrete(expand = c(0,0),
                              labels = c("Dubrovnik","Osijek","Pula","Split","Zadar","Zagreb")) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(Conflicts$Sector))),10),
                                limit=c(0,roundUp(max(table(Conflicts$Sector))))) +
             scale_fill_brewer(name = "Type", palette = "Set1"),
           "Flight Phase" = 
             ggplot(ConflictsFlightPlanPhase,
                    aes(x = Sector, y = Count)) + 
             geom_bar(aes(fill = FlightPlanPhases),
                      stat = "identity",
                      position = "stack") +
             labs(x="TMA Sector", y="Count",
                  title="Conflicts per Sector by Flight Plan Phase of Aircraft Pair") +
             theme(legend.position="right",
                   legend.background=element_blank(),
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black")) +
             scale_x_discrete(expand = c(0,0),
                              labels = c("Dubrovnik","Osijek","Pula","Split","Zadar","Zagreb")) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(aggregate(data=ConflictsFlightPlanPhase, Count~Sector, "sum")$Count)),10),
                                limit=c(0,roundUp(max(aggregate(data=ConflictsFlightPlanPhase, Count~Sector, "sum")$Count)))) +
             scale_fill_brewer(name = "Phase", palette = "Set1"),
           "Severity" = 
             ggplot(Conflicts,
                    aes(x = Sector, group = factor(Severity))) + 
             geom_bar(aes(fill = factor(Severity)),
                      position = "stack") +
             labs(x="TMA Sector", y="Count",
                  title="Conflicts per Sector by Severity") +
             theme(legend.position="right",
                   legend.background=element_blank(),
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black")) +
             scale_x_discrete(expand = c(0,0),
                              labels = c("Dubrovnik","Osijek","Pula","Split","Zadar","Zagreb")) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(Conflicts$Sector))),10),
                                limit=c(0,roundUp(max(table(Conflicts$Sector))))) +
             scale_fill_brewer(name = "Severity", palette = "YlOrRd"),
           "Severity (Vertical)" = 
             ggplot(Conflicts,
                    aes(x = Sector, group = factor(VerticalSeverity))) + 
             geom_bar(aes(fill = factor(VerticalSeverity)),
                      position = "stack") +
             labs(x="TMA Sector", y="Count",
                  title="Conflicts per Sector by Vertical Severity") +
             theme(legend.position="right",
                   legend.background=element_blank(),
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black")) +
             scale_x_discrete(expand = c(0,0),
                              labels = c("Dubrovnik","Osijek","Pula","Split","Zadar","Zagreb")) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(Conflicts$Sector))),10),
                                limit=c(0,roundUp(max(table(Conflicts$Sector))))) +
             scale_fill_brewer(name = "Severity", palette = "YlOrRd")
    )
  } else {
    switch(group,
           "Conflict Type" = 
             ggplot(subset(Conflicts, Sector %in% sector),
                    aes(x = ConflictType)) + 
             geom_bar(aes(fill = ConflictType),
                      position = "stack") +
             labs(x="Conflict Type", y="Count",
                  title="Conflicts per Sector by Conflict Type") +
             theme(legend.position="none",
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black")) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(subset(Conflicts, Sector %in% sector)$ConflictType))),2),
                                limit=c(0,roundUp(max(table(subset(Conflicts, Sector %in% sector)$ConflictType))))) +
             scale_fill_brewer(palette = "Spectral"),
           "Conflict Type (Lateral)" = 
             ggplot(subset(Conflicts, Sector %in% sector),
                    aes(x = LateralConflictType)) + 
             geom_bar(aes(fill = LateralConflictType),
                      position = "stack") +
             labs(x="Conflict Type", y="Count",
                  title="Conflicts per Sector by Lateral Conflict Type") +
             theme(legend.position="none",
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black")) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(subset(Conflicts, Sector %in% sector)$LateralConflictType))),2),
                                limit=c(0,roundUp(max(table(subset(Conflicts, Sector %in% sector)$LateralConflictType))))) +
             scale_fill_brewer(palette = "Set1"),
           "Conflict Type (Vertical)" = 
             ggplot(subset(Conflicts, Sector %in% sector),
                    aes(x = VerticalConflictType)) + 
             geom_bar(aes(fill = VerticalConflictType),
                      position = "stack") +
             labs(x="Conflict Type", y="Count",
                  title="Conflicts per Sector by Vertical Conflict Type") +
             theme(legend.position="none",
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black")) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(subset(Conflicts, Sector %in% sector)$VerticalConflictType))),2),
                                limit=c(0,roundUp(max(table(subset(Conflicts, Sector %in% sector)$VerticalConflictType))))) +
             scale_fill_brewer(palette = "Set1"),
           "Flight Phase" = 
             ggplot(subset(ConflictsFlightPlanPhase, Sector %in% sector),
                    aes(x = FlightPlanPhases, y = Count)) + 
             geom_bar(aes(fill = FlightPlanPhases),
                      stat = "identity",
                      position = "stack") +
             labs(x="Flight Plan Phase", y="Count",
                  title=paste("Conflicts in sector", sector, "by Flight Plan Phase of Aircraft Pair")) +
             theme(legend.position="none",
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black")) +
             scale_x_discrete(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(subset(ConflictsFlightPlanPhase, Sector %in% sector)$Count)),10),
                                limit=c(0,roundUp(max(subset(ConflictsFlightPlanPhase, Sector %in% sector)$Count)))) +
             scale_fill_brewer(palette = "Set1"),
           "Severity" = 
             ggplot(subset(Conflicts, Sector %in% sector),
                    aes(x = factor(Severity))) + 
             geom_bar(aes(fill = factor(Severity)),
                      position = "stack") +
             labs(x="Severity", y="Count",
                  title="Conflicts per Sector by Severity") +
             theme(legend.position="none",
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black")) +
             scale_x_discrete(expand = c(0,0),
                              labels = c("0","1","2","3","4","5","6"),
                              limit = c("0","1","2","3","4","5","6")) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(subset(Conflicts, Sector %in% sector)$Severity))),5),
                                limit=c(0,roundUp(max(table(subset(Conflicts, Sector %in% sector)$Severity))))) +
             scale_fill_brewer(palette = "YlOrRd"),
           "Severity (Vertical)" = 
             ggplot(subset(Conflicts, Sector %in% sector),
                    aes(x = factor(VerticalSeverity))) + 
             geom_bar(aes(fill = factor(VerticalSeverity)),
                      position = "stack") +
             labs(x="Severity", y="Count",
                  title="Conflicts per Sector by Vertical Severity") +
             theme(legend.position="none",
                   panel.background=element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black")) +
             scale_x_discrete(expand = c(0,0),
                              labels = c("0","1","2","3","4","5","6"),
                              limit = c("0","1","2","3","4","5","6")) +
             scale_y_continuous(expand = c(0,0),
                                breaks=seq(0,roundUp(max(table(subset(Conflicts, Sector %in% sector)$VerticalSeverity))),5),
                                limit=c(0,roundUp(max(table(subset(Conflicts, Sector %in% sector)$VerticalSeverity))))) +
             scale_fill_brewer(palette = "YlOrRd")
    )
  }
}

plotConflictMap <- function(){
  labels <- sprintf(
    "ID: %s<br/>Sector: %s<br/>Time: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Lateral Sep. (NM): %s<br/>Vertical Sep. (NM): %s<br/>Altitude (ft): %s",
    Conflicts$ID,Conflicts$Sector,Conflicts$Closest_Time,Conflicts$ConflictType,Conflicts$FlightPlanPhase1,Conflicts$FlightPlanPhase2,Conflicts$LateralSeparation_m,Conflicts$VerticalSeparation_m,Conflicts$Altitude_ft
  ) %>% lapply(htmltools::HTML)
  
  
  #pal <- colorBin("YlOrRd", domain = Conflicts$Altitude_ft, bins =  c(0, 10, 20, 50, 100, 200, 500, 1000, Inf))
  leaflet() %>% 
    setView(lng=16.8, lat=44.2, zoom=6) %>%
    addTiles(options = providerTileOptions(noWrap = TRUE), group="Default") %>%
    addProviderTiles(providers$CartoDB.Positron, group="Greyscale") %>% 
    addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
    addCircleMarkers(data=Conflicts,
                     lng=~Longitude ,
                     lat=~Latitude,
                     color="black",
                     fillColor="red",
                     label=labels,
                     labelOptions = labelOptions(noHide = F, textOnly = F, textsize = "13px", direction="bottom"),
                     radius=5, stroke = TRUE, fillOpacity = 0.8, group="Conflicts") %>%
    # addPolygons(fillColor = ~pal(density),
    #             weight = 2,
    #             opacity = 1,
    #             color = "white",
    #             dashArray = "3",
    #             fillOpacity = 0.7,
    #             highlight = highlightOptions(weight = 5,
    #                                          color = "#666",
    #                                          dashArray = "",
    #                                          fillOpacity = 0.7,
    #                                          bringToFront = TRUE)) %>%
  addLayersControl(overlayGroups = c("Conflicts"),
                   baseGroups = c("Default","Greyscale","Satellite"),
                   options = layersControlOptions(collapsed = FALSE))
}
plotConflictMap()

plotSectorOccupancy <- function(sector="TMA_All"){
  if (sector == "TMA_All") {
    ggplot(subset(SectorOccupancy, Sector %in% c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")),
           aes(x = as.POSIXct(strptime(Time,format="%H:%M:%S")),
               y = Count,
               colour = factor(Sector, ordered = T, levels = c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")))) +
      geom_line(size=1.2) +
      labs(x="Time", y="Count",
           title=paste("Number of aircraft in sector", sector)) +
      scale_x_datetime(expand=c(0,0),
                       breaks=c("1 hour"),
                       minor_breaks = c("30 min"),
                       limits=as.POSIXct(strptime(c("00:00","24:00"),format="%H:%M")),
                       date_labels = format("%H:%M")) +
      scale_y_continuous(expand=c(0,0),
                         breaks=seq(0,roundUp(max(SectorOccupancy$Count)),2),
                         limits=c(0,roundUp(max(SectorOccupancy$Count)))) +
      theme(legend.position="right",
            legend.background=element_blank(),
            panel.background=element_rect(fill = "grey97"),
            axis.line = element_line(colour = "black")) +
      scale_colour_manual(values = rainbow(6),
                          labels = c("Dubrovnik","Osijek","Pula","Split","Zadar","Zagreb"),
                          name="TMA Sector")
  } else {
    ggplot(subset(SectorOccupancy, Sector %in% sector),
           aes(x = as.POSIXct(strptime(Time,format="%H:%M:%S")),
               y = Count,
               colour = factor(Sector, ordered = T, levels = c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")))) +
      geom_line(size=1.2) +
      labs(x="Time", y="Count",
           title=paste("Number of aircraft in sector", sector)) +
      scale_x_datetime(expand=c(0,0),
                       breaks=c("1 hour"),
                       minor_breaks = c("30 min"),
                       limits=as.POSIXct(strptime(c("00:00","24:00"),format="%H:%M")),
                       date_labels = format("%H:%M")) +
      scale_y_continuous(expand=c(0,0),
                         breaks=seq(0,roundUp(max(SectorOccupancy$Count)),2),
                         limits=c(0,roundUp(max(subset(SectorOccupancy, Sector %in% sector)$Count)))) +
      theme(legend.position="none",
            panel.background=element_rect(fill = "grey97"),
            axis.line = element_line(colour = "black")) +
      scale_colour_manual(values = "blue4",
                          name="")
  }
}

plotSectorEntry <- function(sector="TMA_All"){
  if (sector == "TMA_All") {
    ggplot(subset(SectorEntry, Sector %in% c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")),
           aes(x = as.POSIXct(strptime(Time,format="%H:%M:%S")),
               y = Entries,
               colour = factor(Sector, ordered = T, levels = c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")))) +
      geom_line(size=1.2) +
      labs(x="Time", y="Count",
           title=paste("Number of entries into sector", sector)) +
      scale_x_datetime(expand=c(0,0),
                       breaks=c("1 hour"),
                       minor_breaks = c("30 min"),
                       limits=as.POSIXct(strptime(c("00:00","24:00"),format="%H:%M")),
                       date_labels = format("%H:%M")) +
      scale_y_continuous(expand=c(0,0),
                         breaks=seq(0,roundUp(max(SectorEntry$Entries)),5),
                         limits=c(0,roundUp(max(SectorEntry$Entries)))) +
      theme(legend.position="right",
            legend.background=element_blank(),
            panel.background=element_rect(fill = "grey97"),
            axis.line = element_line(colour = "black")) +
      scale_colour_manual(values = rainbow(6),
                          labels = c("Dubrovnik","Osijek","Pula","Split","Zadar","Zagreb"),
                          name="TMA Sector")
  } else {
    ggplot(subset(SectorEntry, Sector %in% sector),
           aes(x = as.POSIXct(strptime(Time,format="%H:%M:%S")),
               y = Entries,
               colour = factor(Sector, ordered = T, levels = c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")))) +
      geom_line(size=1.2) +
      labs(x="Time", y="Count",
           title=paste("Number of entries into sector", sector)) +
      scale_x_datetime(expand=c(0,0),
                       breaks=c("1 hour"),
                       minor_breaks = c("30 min"),
                       limits=as.POSIXct(strptime(c("00:00","24:00"),format="%H:%M")),
                       date_labels = format("%H:%M")) +
      scale_y_continuous(expand=c(0,0),
                         breaks=seq(0,roundUp(max(SectorEntry$Entries)),5),
                         limits=c(0,roundUp(max(subset(SectorEntry, Sector %in% sector)$Entries)))) +
      theme(legend.position="none",
            panel.background=element_rect(fill = "grey97"),
            axis.line = element_line(colour = "black")) +
      scale_colour_manual(values = "blue4",
                          name="")
  }
}

# Shiny App --------------------------------------------------
ui <- fluidPage(
  br(),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabset",
                  tabPanel("Throughput",
                           br(),
                           selectInput("metric1",
                                       label = "Select which metric to view",
                                       choices = c("Total Throughputs","Hourly Throughputs", "Rolling Hourly Throughputs")
                           ),
                           selectInput("airport1",
                                       label = "Choose which airport to display",
                                       choices = c("All","LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS")
                           )
                  ),
                  # tabPanel("Efficiency",
                  #          br()
                  # ),
                  tabPanel("Safety",
                           br(),
                           selectInput("metric3",
                                       label = "Select which metric to view",
                                       choices = c("Conflict Statistics", "Conflict Map")
                           ),
                           selectInput("sector3",
                                       label = "Choose which TMA sector to display",
                                       choices = c("TMA_All","TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")
                           ),
                           selectInput("group3",
                                       label = "Choose grouping factor",
                                       choices = c("Conflict Type","Conflict Type (Lateral)","Conflict Type (Vertical)","Flight Phase","Severity","Severity (Vertical)")
                           )
                  ),
                  tabPanel("Capacity",
                           br(),
                           selectInput("metric4",
                                       label = "Select which metric to view",
                                       choices = c("Sector Occupancy Count","Sector Entry Count")
                           ),
                           selectInput("sector4",
                                       label = "Choose which TMA sector to display",
                                       choices = c("TMA_All","TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")
                           )
                  )
                  
      ),
      actionButton("view", "View"),
      width = 3
    ),
    mainPanel(
      uiOutput("display")
    )
  )
)

server <- function(input, output, session){
  v <- reactiveValues(doPlot = FALSE) # Variable to act in observed event
  
  observeEvent(input$view, {
    v$doPlot <- input$view # Create new display UI on view button press
  })
  
  observeEvent(input$tabset, {
    v$doPlot <- FALSE # Hide output display UI when switching to another tab
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
                        "group3",
                        label = "Choose grouping factor",
                        choices = c("Conflict Type","Conflict Type (Lateral)","Conflict Type (Vertical)","Flight Phase","Severity","Severity (Vertical)")
      )
      updateSelectInput(session,
                        "sector3",
                        label = "Choose which TMA sector to display",
                        choices = c("TMA_All","TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")
      )
    } else if (input$metric3 == "Conflict Map") {
      updateSelectInput(session,
                        "sector3",
                        label = "Choose map theme",
                        choices = c("None")
      )
      updateSelectInput(session,
                        "group3",
                        label = "Choose grouping factor",
                        choices = c("None")
      )
    }
  })
  
  output$map <- renderLeaflet({
    if (v$doPlot == FALSE) return()
    isolate({
      if (input$tabset == "Safety") {
        if (input$metric3 == "Conflict Map") {
          plotConflictMap()
        }
      }
    })
  })
  
  output$plot <- renderPlot({
    if (v$doPlot == FALSE) return()
    isolate({
      if (input$tabset == "Throughput") {
        if (input$metric1 == "Total Throughputs") {
          plotTotalThroughput(x = input$airport1)
        } else if (input$metric1 == "Hourly Throughputs") {
          plotHourlyThroughput(x = input$airport1)
        } else if (input$metric1 == "Rolling Hourly Throughputs") {
          plotRollingHourlyThroughput(x = input$airport1)
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
  })
}

#shinyApp(ui, server, options=c(host="0.0.0.0", port=8788)) # host="192.168.1.137", port=8788
shinyApp(ui, server)