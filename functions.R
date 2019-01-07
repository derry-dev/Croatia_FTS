source("data.R", local = T)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(leaflet)
library(sp)
library(plotly)
library(plyr)

# Change these lists if you want to include another airports/sectors
Airport.list <- factor(c("All","LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"), ordered = TRUE)
Sector.list <- factor(c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB"), ordered = TRUE)

plotlyTotalThroughput <- function(){
  dat <- subset(table.current.TotalThroughputs, Airport %in% Airport.list)
  dat$Category[dat$Category == "Descending"] <- "Arrivals"
  dat$Category[dat$Category == "GroundAccelerating"] <- "Departures"
  plot_ly(data = dat,
          x = ~Airport,
          y = ~Count,
          color = ~Category,
          colors = c("green4","blue4"),
          type = "bar") %>%
    layout(dragmode = "zoom",
           barmode = 'stack',
           title = "Total Throughputs",
           xaxis = list(tickmode = "auto"),
           yaxis = list(tickmode = "auto",
                        title = "Number of movements"),
           legend = list(x = 100, y = 0.5))
}

plotlyHourlyThroughput <- function(airport="All"){
  dat <- if (airport == "All") {
    aggregate(data=subset(table.current.HourlyThroughputs, Airport %in% Airport.list), Count~Category+Hour, "sum")
  } else {
    subset(table.current.HourlyThroughputs, Airport %in% airport)
  }
  dat$Category[dat$Category == "Descending"] <- "Arrivals"
  dat$Category[dat$Category == "GroundAccelerating"] <- "Departures"
  plot_ly(data = dat,
          x = ~Hour,
          y = ~Count,
          color = ~Category,
          colors = c("green4","blue4"),
          type = "bar") %>%
    layout(dragmode = "zoom",
           barmode = 'stack',
           title = paste("Hourly Throughputs", airport),
           xaxis = list(tickmode = "auto"),
           yaxis = list(tickmode = "auto",
                        title = "Number of movements"),
           legend = list(x = 100, y = 0.5))
}

plotlyRollingHourlyThroughput <- function(airport="All"){
  temp <- gather(table.current.RollingHourlyThroughputs,"Category","Count",c(RollingThroughputCount,RollingLiftOffCount,RollingTouchDownCount))
  dat <- if (airport == "All") {
    aggregate(data=subset(temp, Airport %in% Airport.list), Count~Time+Category, "sum")
  } else {
    subset(temp, Airport %in% airport)
  }
  dat$Category[dat$Category == "RollingTouchDownCount"] <- "Arrivals"
  dat$Category[dat$Category == "RollingLiftOffCount"] <- "Departures"
  dat$Category[dat$Category == "RollingThroughputCount"] <- "Total"
  plot_ly(data = dat,
          x = ~Time,
          y = ~Count,
          color = ~Category,
          colors = c("green4","blue4","red4"),
          type = "scatter",
          mode = "line") %>%
    layout(dragmode = "zoom",
           barmode = 'stack',
           title = paste("Rolling Hourly Throughputs", airport),
           xaxis = list(tickmode = "auto"),
           yaxis = list(tickmode = "auto",
                        title = "Number of movements"),
           legend = list(x = 100, y = 0.5))
}

plotlyConflictCount <- function(group="Conflict Type"){
  switch(group,
         "Conflict Type" = 
           plot_ly(data = count(subset(table.current.Conflicts, Sector %in% Sector.list), vars=c("Sector","ConflictType")),
                   x = ~Sector,
                   y = ~freq,
                   color = ~ConflictType,
                   colors = "Dark2",
                   type = "bar") %>%
           layout(dragmode = "zoom",
                  barmode = 'stack',
                  title = paste("Conflicts per Sector by", group),
                  xaxis = list(tickmode = "auto"),
                  yaxis = list(tickmode = "auto",
                               title = "Number of conflicts"),
                  legend = list(x = 100, y = 0.5)),
         "Conflict Type (Lateral)" = 
           plot_ly(data = count(subset(table.current.Conflicts, Sector %in% Sector.list), vars=c("Sector","LateralConflictType")),
                   x = ~Sector,
                   y = ~freq,
                   color = ~LateralConflictType,
                   colors = "Set1",
                   type = "bar") %>%
           layout(dragmode = "zoom",
                  barmode = 'stack',
                  title = paste("Conflicts per Sector by", group),
                  xaxis = list(tickmode = "auto"),
                  yaxis = list(tickmode = "auto",
                               title = "Number of conflicts"),
                  legend = list(x = 100, y = 0.5)),
         "Conflict Type (Vertical)" = 
           plot_ly(data = count(subset(table.current.Conflicts, Sector %in% Sector.list), vars=c("Sector","VerticalConflictType")),
                   x = ~Sector,
                   y = ~freq,
                   color = ~VerticalConflictType,
                   colors = "Set1",
                   type = "bar") %>%
           layout(dragmode = "zoom",
                  barmode = 'stack',
                  title = paste("Conflicts per Sector by", group),
                  xaxis = list(tickmode = "auto"),
                  yaxis = list(tickmode = "auto",
                               title = "Number of conflicts"),
                  legend = list(x = 100, y = 0.5)),
         "Flight Phase" = 
           plot_ly(data = subset(table.current.ConflictsFlightPlanPhase, Sector %in% Sector.list),
                   x = ~Sector,
                   y = ~Count,
                   color = ~FlightPlanPhases,
                   colors = "Set3",
                   type = "bar") %>%
           layout(dragmode = "zoom",
                  barmode = 'stack',
                  title = paste("Conflicts per Sector by", group),
                  xaxis = list(tickmode = "auto"),
                  yaxis = list(tickmode = "auto",
                               title = "Number of conflicts"),
                  legend = list(x = 100, y = 0.5)),
         "Severity" = 
           plot_ly(data = count(subset(table.current.Conflicts, Sector %in% Sector.list), vars=c("Sector","Severity")),
                   x = ~Sector,
                   y = ~freq,
                   text = ~paste("Count:",freq,"Severity:", Severity),
                   color = ~Severity,
                   colors = "YlOrRd",
                   hoverinfo = "text",
                   hoverlabel = "text",
                   type = "bar") %>%
           layout(dragmode = "zoom",
                  barmode = 'stack',
                  title = paste("Conflicts per Sector by", group),
                  xaxis = list(tickmode = "auto"),
                  yaxis = list(tickmode = "auto",
                               title = "Number of conflicts"),
                  legend = list(x = 100, y = 0.5)),
         "Severity (Vertical)" = 
           plot_ly(data = count(subset(table.current.Conflicts, Sector %in% Sector.list), vars=c("Sector","VerticalSeverity")),
                   x = ~Sector,
                   y = ~freq,
                   text = ~paste("Count:",freq,"Vertical Severity:",VerticalSeverity),
                   color = ~VerticalSeverity,
                   colors = "YlOrRd",
                   hoverinfo = "text",
                   hoverlabel = "text",
                   type = "bar") %>%
           layout(dragmode = "zoom",
                  barmode = 'stack',
                  title = paste("Conflicts per Sector by", group),
                  xaxis = list(tickmode = "auto"),
                  yaxis = list(tickmode = "auto",
                               title = "Number of conflicts"),
                  legend = list(x = 100, y = 0.5))
  )
}

plotConflictMap <- function(){
  # TMA Polygons
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
  # HTML enabled for multi-lined labelling of conflict markers
  TMA <- c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")
  table.current.Conflicts.NonTMA <- subset(table.current.Conflicts, !(Sector %in% TMA))
  table.current.Conflicts.TMA <- subset(table.current.Conflicts, Sector %in% TMA)
  Conflicts.labels.NonTMA <- sprintf(
    "ID: %s<br/>Sector: %s<br/>Time: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    table.current.Conflicts.NonTMA$ID,
    table.current.Conflicts.NonTMA$Sector,
    table.current.Conflicts.NonTMA$Closest_Time,
    table.current.Conflicts.NonTMA$ConflictType,
    table.current.Conflicts.NonTMA$FlightPlanPhase1,
    table.current.Conflicts.NonTMA$FlightPlanPhase2,
    table.current.Conflicts.NonTMA$LateralSeparation,
    table.current.Conflicts.NonTMA$ReqLateralSeparation,
    table.current.Conflicts.NonTMA$VerticalSeparation,
    table.current.Conflicts.NonTMA$ReqVerticalSeparation,
    table.current.Conflicts.NonTMA$Altitude_ft) %>% lapply(htmltools::HTML)
  Conflicts.labels.TMA <- sprintf(
    "ID: %s<br/>Sector: %s<br/>Time: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    table.current.Conflicts.TMA$ID,
    table.current.Conflicts.TMA$Sector,
    table.current.Conflicts.TMA$Closest_Time,
    table.current.Conflicts.TMA$ConflictType,
    table.current.Conflicts.TMA$FlightPlanPhase1,
    table.current.Conflicts.TMA$FlightPlanPhase2,
    table.current.Conflicts.TMA$LateralSeparation,
    table.current.Conflicts.TMA$ReqLateralSeparation,
    table.current.Conflicts.TMA$VerticalSeparation,
    table.current.Conflicts.TMA$ReqVerticalSeparation,
    table.current.Conflicts.TMA$Altitude_ft) %>% lapply(htmltools::HTML)
  airport.location <- data.frame(
    airport <- c("LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"),
    lng <- c(16.29800033569336,18.268199920654297,16.0687999725,13.922200202941895,15.346699714660645,14.3930997849,14.570300102233887,16.67970085144043,18.810199737548828),
    lat <- c(43.53889846801758,42.5614013671875,45.7429008484,44.89350128173828,44.108299255371094,44.5657997131,45.21689987182617,43.285701751708984,45.46269989013672)
  )
  names(airport.location) <- c("airport","lng","lat")
  # Leaflet map wrapper is first initialised by leaflet(), then map tiles, polygons, markers and controls are added
  leaflet() %>% 
    setView(lng=16.8, lat=44.2, zoom=7) %>%
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
                labelOptions = labelOptions(style = list("font-weight" = "bold"),
                                            opacity = 1,
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
                labelOptions = labelOptions(style = list("font-weight" = "bold"),
                                            opacity = 1,
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
                labelOptions = labelOptions(style = list("font-weight" = "bold"),
                                            opacity = 1,
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
                labelOptions = labelOptions(style = list("font-weight" = "bold"),
                                            opacity = 1,
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
                labelOptions = labelOptions(style = list("font-weight" = "bold"),
                                            opacity = 1,
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
                labelOptions = labelOptions(style = list("font-weight" = "bold"),
                                            opacity = 1,
                                            textsize = "12px",
                                            direction = "auto"),
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.35,
                                             bringToFront = F)) %>%
    addCircleMarkers(data = airport.location,
                     lng=~lng,
                     lat=~lat,
                     label=~airport,
                     labelOptions = labelOptions(noHide = T, textsize = "8px", direction="bottom", opacity = 0.8),
                     radius=0, stroke = F, fillOpacity = 0, group="Airports") %>%
    addCircleMarkers(data=table.current.Conflicts.NonTMA,
                     lng=~Longitude,
                     lat=~Latitude,
                     color="black",
                     fillColor="yellow",
                     label=Conflicts.labels.NonTMA,
                     labelOptions = labelOptions(textsize = "13px", direction="auto"),
                     radius=5, stroke = TRUE, fillOpacity = 0.8, group="Non-TMA Conflicts") %>%
    addCircleMarkers(data=table.current.Conflicts.TMA,
                     lng=~Longitude,
                     lat=~Latitude,
                     color="black",
                     fillColor="red",
                     label=Conflicts.labels.TMA,
                     labelOptions = labelOptions(textsize = "13px", direction="auto"),
                     radius=5, stroke = TRUE, fillOpacity = 0.8, group="TMA Conflicts") %>%
    addLegend("bottomright", colors=c("red","yellow"), labels=c("TMA Conflicts", "Non-TMA Conflicts"), title="Conflict Type") %>%
    addLayersControl(overlayGroups = c("Airports","TMA Sectors", "TMA Conflicts", "Non-TMA Conflicts"),
                     baseGroups = c("Default","Greyscale","Satellite"),
                     options = layersControlOptions(collapsed = TRUE))
}

plotlyConflict3D <- function(){
  plot_ly(data = table.current.Conflicts,
          x = ~Longitude,
          y = ~Latitude,
          z = ~Altitude_ft,
          color = ~Sector,
          colors = "Spectral",
          hovertext = ~paste(
            sprintf("ID: %s</br>Sector: %s</br>Closest Time: %s</br>Type: %s</br>FP Phase: %s %s</br>Lateral Sep. (NM): %s (Req. %s)</br>Vertical Sep. (ft): %s (Req. %s)",
                    ID,Sector,Closest_Time,ConflictType,FlightPlanPhase1,FlightPlanPhase2,LateralSeparation,ReqLateralSeparation,VerticalSeparation,ReqVerticalSeparation)
            %>% lapply(htmltools::HTML)),
          type = "scatter3d",
          mode = "markers") %>%
    add_trace(x = list(16.29800033569336,18.268199920654297,16.0687999725,13.922200202941895,15.346699714660645,14.3930997849,14.570300102233887,16.67970085144043,18.810199737548828),
              y = list(43.53889846801758,42.5614013671875,45.7429008484,44.89350128173828,44.108299255371094,44.5657997131,45.21689987182617,43.285701751708984,45.46269989013672),
              z = list(79,527,353,274,289,151,278,1776,290),
              text = list("LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"),
              type = "scatter3d",
              mode = "text",
              showlegend = F) %>%
    layout(
      scene = list(
        xaxis = list(title = "Longitude"),
        yaxis = list(title = "Latitude"),
        zaxis = list(title = "Altitude (ft)"),
        legend = list(x = 100, y = 0.5))
    )
}

plotlySectorOccupancy <- function(){
  dat <- subset(table.current.SectorOccupancy, Sector %in% Sector.list)
  temp <- aggregate(data=subset(table.current.SectorOccupancy, Sector %in% Sector.list),Count~Time,"sum")
  temp$Sector <- "All Sectors"
  dat <- rbind(dat,temp)
  plot_ly(data = dat,
          x = ~Time,
          y = ~Count,
          color = ~Sector,
          type = "scatter",
          mode = "line") %>%
    layout(dragmode = "zoom",
           title = paste("Sector occupancy count per minute"),
           xaxis = list(tickmode = "auto",
                        rangeslider = list(type = "time")),
           yaxis = list(tickmode = "auto",
                        title = "Number of aircraft"),
           legend = list(x = 100, y = 0.5))
}

plotlySectorEntry <- function(){
  dat <- subset(table.current.SectorEntry, Sector %in% Sector.list)
  temp <- aggregate(data=subset(table.current.SectorEntry, Sector %in% Sector.list),Entries~Time,"sum")
  temp$Sector <- "All Sectors"
  dat <- rbind(dat,temp)
  plot_ly(data = dat,
          x = ~Time,
          y = ~Entries,
          color = ~Sector,
          type = "scatter",
          mode = "lines") %>%
    layout(dragmode = "zoom",
           title = paste("Number of sector entries per 10 minute interval"),
           xaxis = list(tickmode = "auto",
                        rangeslider = list(type = "time")),
           yaxis = list(tickmode = "auto",
                        title = "Number of entries"),
           legend = list(x = 100, y = 0.5))
}