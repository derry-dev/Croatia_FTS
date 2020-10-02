source("global.R", local = T)

airport.location <- data.frame(
  airport <- c("LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"),
  lng <- c(16.29800033569336,18.268199920654297,16.0687999725,13.922200202941895,15.346699714660645,14.3930997849,14.570300102233887,16.67970085144043,18.810199737548828),
  lat <- c(43.53889846801758,42.5614013671875,45.7429008484,44.89350128173828,44.108299255371094,44.5657997131,45.21689987182617,43.285701751708984,45.46269989013672)
); names(airport.location) <- c("airport","lng","lat")

TMA <- c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")
NonTMA <- unlist(unique(subset(table.Conflicts,Sector %!in% TMA, select=Sector)),use.names=F)

# Selection choices
choices.throughputs <- c("Total Throughput","Hourly Throughput","Rolling Hourly Throughput")
choices.efficiency <- c("Track Miles","Fuel Burn")
choices.efficiencygrouping <- c("Airport (Combined)", "Airport (Arrivals Only)", "Airport (Departures Only)", "Routing (Combined)", "Routing (Arrivals Only)", "Routing (Departures Only)")
choices.safety <- c("Conflict Summary","Conflict Map", "Conflict Statistics")
choices.mapcolours <- c("Operation","Conflict Type","Lateral Conflict Type","Vertical Conflict Type","Flight Phase","Flight Type","Severity","Vertical Severity","Altitude","Lateral Separation","Vertical Separation")
choices.conflictsgrouping <- c("Conflict Type","Conflict Type (Lateral)","Conflict Type (Vertical)","Flight Phase","Severity","Severity (Vertical)")
choices.sectorcapacity <- c("Sector Entry Count","Sector Occupancy Count","Controller Workload","Workload vs Entries","Capacity Models")
choices.capacitymodels <- c("IFR Only", "IFR and VFR", "All (Separate)", "All (Combined)")

function(input, output, session){
  
  session$onSessionEnded(function() {
    stopApp()
  })

  session$allowReconnect(TRUE)
  
  output$metric <- renderUI({
    req(input$kpa)
    x <- input$kpa
    if (x == "Throughput") {
      div(class="dropdown_box",selectInput("metric","Metric",choices.throughputs))
    } else if (x == "Efficiency") {
      div(class="dropdown_box",selectInput("metric","Metric",choices.efficiency))
    } else if (x == "Safety") {
      div(class="dropdown_box",selectInput("metric","Metric",choices.safety,selected="Conflict Map"))
    } else if (x == "Sector Capacity") {
      div(class="dropdown_box",selectInput("metric","Metric",choices.sectorcapacity))
    }
  })
  
  output$options <- renderUI({
    req(input$metric)
    x <- input$metric
    if (x == "Total Throughput") {
      div(class="dropdown_box",selectInput("options","Airport",list.airports))
    } else if (x == "Hourly Throughput") {
      div(class="dropdown_box",selectInput("options","Airport",list.airports))
    } else if (x == "Rolling Hourly Throughput") {
      div(class="dropdown_box",selectInput("options","Airport",list.airports))
    } else if (x == "Track Miles") {
      tagList(
        div(class="dropdown_box",selectInput("options","Group by",choices.efficiencygrouping)),
        div(class="dropdown_box",selectInput("moreoptions","Airport",list.airports))
      )
    } else if (x == "Fuel Burn") {
      tagList(
        div(class="dropdown_box",selectInput("options","Group by",choices.efficiencygrouping)),
        div(class="dropdown_box",selectInput("moreoptions","Airport",list.airports))
      )
    } else if (x == "Conflict Summary") {
      div(class="dropdown_box",selectInput("options","Sector",list.sectors))
    } else if (x == "Conflict Map") {
      tagList(
        div(class="dropdown_box",selectInput("maptype","Map Type",c("Markers","Heatmap"))),
        div(class="dropdown_box",style="text-align:center;padding-top:10px;", checkboxGroupInput("sectors","Sectors",c("TMA","Non-TMA"),selected=c("TMA"),inline=T))
      )
    } else if (x == "Conflict Statistics") {
      div(class="dropdown_box",selectInput("options","Group by",choices.conflictsgrouping))
    } else if (x %in% c("Sector Occupancy Count","Sector Entry Count","Controller Workload","Workload vs Entries")) {
      div(class="dropdown_box",selectInput("options","Sector",list.sectors))
    } else if (x == "Capacity Models") {
      tagList(
        div(class="dropdown_box",selectInput("options","Sector",list.sectors[-c(1)])),
        div(class="dropdown_box",selectInput("moreoptions","Model",choices.capacitymodels))
      )
    }
  })
  
  output$operation <- renderUI({
    req(input$metric)
    if (input$metric != "Conflict Summary") {
      div(style="height:58px;",checkboxGroupInput("operation","Operation Scenario",c("Current","PBN"),selected=c("Current","PBN"),inline=T))
    }
  })
  
  output$run <- renderUI({
    req(input$metric)
    if (input$metric %!in% c("Track Miles","Fuel Burn","Conflict Map","Conflict Statistics","Workload vs Entries","Capacity Models")) {
      div(class="dropdown_box",selectInput("run", "Run", as.character(seq(1,5,1))))
    } else {
      div(class="dropdown_box",selectInput("run", "Run", as.character(c("All Runs",seq(1,5,1))), selected="1"))
    }
  })
  
  output$arrange <- renderUI({
    req(input$metric, input$operation)
    if ("Current" %in% input$operation & "PBN" %in% input$operation & input$metric %!in% c("Conflict Summary","Conflict Map","Capacity Models")) {
      div(style="height:78px;",radioButtons("arrange","Plot Arrangement",c("Vertical","Horizontal","Group"),selected="Vertical",inline=T))
    }
  })
  
  observeEvent({
    input$kpa
    input$metric
  },{
    if (input$metric == "Conflict Map") {
      showElement("mapdisplay")
      hideElement("plotdisplay")
      hideElement("tabledisplay")
    } else if (input$metric != "Conflict Map") {
      hideElement("mapdisplay")
      showElement("plotdisplay")
      hideElement("tabledisplay")
    }
  })
  
  observeEvent({
    input$kpa
    input$metric
    input$view
    },{
    if (input$metric == "Conflict Map") {
      showElement("mapdisplay")
      hideElement("plotdisplay")
      hideElement("tabledisplay")
    } else if (input$metric != "Conflict Map") {
      hideElement("mapdisplay")
      showElement("plotdisplay")
      hideElement("tabledisplay")
    }
  })
  
  observeEvent({
    input$kpa
    input$metric
    input$explore
  },{
    hideElement("mapdisplay")
    hideElement("plotdisplay")
    showElement("tabledisplay")
  })
  
  output$mapdisplay <- renderUI({
    tagList(
      div(id="wrap1",leafletOutput("map",height="100%")),
      absolutePanel(
        div(class="mapcontrols",
            div(id="mappolygons",checkboxInput("polygons","TMA Outlines",F)),
            div(id="mapairports",checkboxInput("airports","Airports",F)),
            uiOutput("mapcontrols1"),
            div(id="mapseverity1",
                sliderInput("severity","Severity",
                            min(table.Conflicts$Severity),max(table.Conflicts$Severity),
                            value=c(min(table.Conflicts$Severity),max(table.Conflicts$Severity)),step=1)),
            div(id="mapseverity2",
                sliderInput("verticalseverity","Vertical Severity",
                            min(table.Conflicts$VerticalSeverity),max(table.Conflicts$VerticalSeverity),
                            value=range(table.Conflicts$VerticalSeverity),step=1)),
            div(class="mapcontrolsleft",
                checkboxGroupInput("flightphase","Flight Phase",
                                   sort(unique(table.Conflicts$FlightPlanPhases)),
                                   selected=sort(unique(table.Conflicts$FlightPlanPhases)))),
            div(class="mapcontrolsright",
                div(style="height:100px", checkboxGroupInput("lateralconflicttype","Lateral Type",
                                   sort(unique(table.Conflicts$LateralConflictType)),
                                   selected=sort(unique(table.Conflicts$LateralConflictType)))),
                div(style="height:100px", checkboxGroupInput("verticalconflicttype","Vertical Type",
                                   sort(unique(table.Conflicts$VerticalConflictType)),
                                   selected=sort(unique(table.Conflicts$VerticalConflictType)))),
                checkboxGroupInput("flighttype","Flight Type",
                                   sort(unique(table.Conflicts$FlightTypes)),
                                   selected=sort(unique(table.Conflicts$FlightTypes))))
        ),
        width=300,
        right=10,
        top=10,
        fixed=T,
        draggable=F
      )
    )
  })
  
  output$mapcontrols1 <- renderUI({
    req(input$maptype)
    if (input$maptype != "Heatmap") {
      div(id="mapoutline",selectInput("colour","Colour Outline",choices.mapcolours,selected="Severity"))
    } else {
      div(style="height:5px")
    }
  })
  
  output$plotdisplay <- renderUI({
    div(id="wrap2",plotlyOutput("plot",height="100%"))
  })
  
  output$tabledisplay <- renderUI({
    div(id="wrap1",DT::dataTableOutput("table"))
  })
  
  output$table <- renderDataTable({
    x <- input$metric
    if (x == "Total Throughput") {
      data <- table.TotalThroughputs
      if (input$options != "All") {
        data <- subset(data, Airport %in% input$options)
      }
    } else if (x == "Hourly Throughput") {
      data <- table.HourlyThroughputs
      if (input$options != "All") {
        data <- subset(data, Airport %in% input$options)
      }
    } else if (x == "Rolling Hourly Throughput") {
      data <- table.RollingHourlyThroughputs
      if (input$options != "All") {
        data <- subset(data, Airport %in% input$options)
      }
    } else if (x == "Track Miles" | x== "Fuel Burn") {
      if (grepl("Routing",input$options)) {
        data <- table.FuelBurnTrackMiles
      } else {
        data <- merge(table.FuelBurn, table.TrackMiles)
      }
      if (grepl("Arrivals",input$options)) {
        data <- subset(data, RoutingType %in% "Arrival")
      } else if (grepl("Departures",input$options)) {
        data <- subset(data, RoutingType %in% "Departure")
      }
      if (input$moreoptions != "All") {
        data <- subset(data, Airport %in% input$moreoptions)
      }
    } else if (x == "Conflict Map") {
      if ("TMA" %in% input$sectors & "Non-TMA" %in% input$sectors) {
        data <- table.Conflicts
      } else if ("TMA" %in% input$sectors & "Non-TMA" %!in% input$sectors) {
        data <- subset(table.Conflicts, Sector %in% TMA)
      } else if ("TMA" %!in% input$sectors & "Non-TMA" %in% input$sectors) {
        data <- subset(table.Conflicts, Sector %in% NonTMA)
      } else if ("TMA" %!in% input$sectors & "Non-TMA" %!in% input$sectors) {
        data <- subset(table.Conflicts, Sector %!in% c(TMA,NonTMA))
      }
      data$FlightType1[data$FlightType1 == "AirCarrier"] <- "IFR"
      data$FlightType1[data$FlightType1 == "GeneralAviation"] <- "VFR"
      data$FlightType2[data$FlightType2 == "AirCarrier"] <- "IFR"
      data$FlightType2[data$FlightType2 == "GeneralAviation"] <- "VFR"
      data <- subset(data, select=-c(ConflictType,ReqLateralSeparation,ReqVerticalSeparation,FlightPlanPhases,FlightTypes))
    } else if (x == "Conflict Statistics") {
      if (input$options == "Conflict Type") {
        data <- table.ConflictType
      } else if (input$options == "Conflict Type (Lateral)") {
        data <- table.LateralConflictType
      } else if (input$options == "Conflict Type (Vertical)") {
        data <- table.VerticalConflictType
      } else if (input$options == "Flight Phase") {
        data <- table.ConflictsFlightPlanPhase
      } else if (input$options == "Severity") {
        data <- table.Severity
      } else if (input$options == "Severity (Vertical)") {
        data <- table.VerticalSeverity
      }
    } else if (x == "Sector Occupancy Count") {
      data <- table.SectorOccupancy
      if (input$options != "All") {
        data <- subset(data, Sector %in% input$options)
      }
    } else if (x == "Sector Entry Count") {
      data <- table.SectorEntry
      if (input$options != "All") {
        data <- subset(data, Sector %in% input$options)
      }
    } else if (x == "Controller Workload" | x == "Workload vs Entries" | x == "Capacity Models") {
      data <- table.Workload
      if (input$options != "All") {
        data <- subset(data, Sector %in% input$options)
      }
    }
    subset(data, Scenario %in% input$operation & Runway %in% input$runway & Run %in% input$run)
  }, options = list(pageLength = 20))
  
  output$plot <- renderPlotly({
    x <- input$metric
    if (x == "Total Throughput") {
      plotlyTotalThroughput(operation=c(input$operation), runway=input$runway, airport=input$options, arrange=input$arrange, run=input$run)
    } else if (x == "Hourly Throughput") {
      plotlyHourlyThroughput(operation=c(input$operation), runway=input$runway, airport=input$options, arrange=input$arrange, run=input$run)
    } else if (x == "Rolling Hourly Throughput") {
      plotlyRollingHourlyThroughput(operation=c(input$operation), runway=input$runway, airport=input$options, arrange=input$arrange, run=input$run)
    } else if (x == "Track Miles") {
      plotlyTrackMiles(operation=c(input$operation), runway=input$runway, group=input$options, airport=input$moreoptions, arrange=input$arrange, run=input$run)
    } else if (x == "Fuel Burn") {
      plotlyFuelBurn(operation=c(input$operation), runway=input$runway, group=input$options, airport=input$moreoptions, arrange=input$arrange, run=input$run)
    } else if (x == "Conflict Summary") {
      plotlyConflictSummary(runway=input$runway, sector=input$options, run=input$run)
    } else if (x == "Conflict Statistics") {
      plotlyConflictCount(operation=c(input$operation), runway=input$runway, group=input$options, arrange=input$arrange, run=input$run)
    } else if (x == "Sector Entry Count") {
      plotlySectorEntry(operation=c(input$operation), runway=input$runway, sector=input$options, arrange=input$arrange, run=input$run)
    } else if (x == "Sector Occupancy Count") {
      plotlySectorOccupancy(operation=c(input$operation), runway=input$runway, sector=input$options, arrange=input$arrange, run=input$run)
    } else if (x == "Controller Workload") {
      plotlyControllerWorkload(operation=c(input$operation), runway=input$runway, sector=input$options, arrange=input$arrange, run=input$run)
    } else if (x == "Workload vs Entries") {
      plotlyWorkloadEntries(operation=c(input$operation), runway=input$runway, sector=input$options, arrange=input$arrange, run=input$run)
    } else if (x == "Capacity Models") {
      plotlyCapacityModel(operation=input$operation, runway=input$runway, sector=input$options, model=input$moreoptions, run=input$run)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(options=leafletOptions(zoomControl=F)) %>% 
      setView(lng = 16.8, lat = 44.2, zoom = 7) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, options=providerTileOptions(noWrap=TRUE), group="Light") %>%
      addProviderTiles(providers$Esri.WorldImagery, options=providerTileOptions(noWrap=TRUE), group="Satellite") %>%
      addProviderTiles(providers$CartoDB.Positron, options=providerTileOptions(noWrap=TRUE), group="Grey") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, options=providerTileOptions(noWrap=TRUE), group="Dark") %>%
      addProviderTiles(providers$Esri.DeLorme, options=providerTileOptions(noWrap=TRUE), group="Topo") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, options=providerTileOptions(noWrap=TRUE), group="OSM") %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options=providerTileOptions(noWrap=TRUE), group="OSM B&W") %>%
      addLayersControl(baseGroups=c("Light","Satellite","Grey","Dark","Topo","OSM","OSM B&W"), options=layersControlOptions(collapsed=T))
  })
  
  observeEvent(input$polygons, {
    if (input$polygons == T) {
      leafletProxy("map") %>% clearGroup("TMA Sectors") %>%
        addPolygons(data = Polygon(subset(table.SectorPolygons,Sector %in% "TMA_DUBROVNIK", select=c(Longitude,Latitude))),
                    group="TMA Sectors", weight = 5, opacity = 0.5, fillOpacity = 0.1, color = "gray", dashArray = "18",
                    label = "TMA Dubrovnik", labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                    highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
        addPolygons(data = Polygon(subset(table.SectorPolygons,Sector %in% "TMA_SPLIT", select=c(Longitude,Latitude))),
                    group="TMA Sectors", weight = 5, opacity = 0.5, fillOpacity = 0.1, color = "gray", dashArray = "18",
                    label = "TMA Split", labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                    highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
        addPolygons(data = Polygon(subset(table.SectorPolygons,Sector %in% "TMA_ZADAR", select=c(Longitude,Latitude))),
                    group="TMA Sectors", weight = 5, opacity = 0.5, fillOpacity = 0.1, color = "gray", dashArray = "18",
                    label = "TMA Zadar", labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                    highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
        addPolygons(data = Polygon(subset(table.SectorPolygons,Sector %in% "TMA_PULA", select=c(Longitude,Latitude))),
                    group="TMA Sectors", weight = 5, opacity = 0.5, fillOpacity = 0.1, color = "gray", dashArray = "18",
                    label = "TMA Pula", labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                    highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
        addPolygons(data = Polygon(subset(table.SectorPolygons,Sector %in% "TMA_ZAGREB", select=c(Longitude,Latitude))),
                    group="TMA Sectors", weight = 5, opacity = 0.5, fillOpacity = 0.1, color = "gray", dashArray = "18",
                    label = "TMA Zagreb", labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                    highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
        addPolygons(data = Polygon(subset(table.SectorPolygons,Sector %in% "TMA_OSIJEK", select=c(Longitude,Latitude))),
                    group="TMA Sectors", weight = 5, opacity = 0.5, fillOpacity = 0.1, color = "gray", dashArray = "18",
                    label = "TMA Osijek", labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                    highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F))
    } else {
      leafletProxy("map") %>% clearGroup("TMA Sectors")
    }
  })
  
  observeEvent(input$airports, {
    if (input$airports == T) {
      leafletProxy("map") %>%
        addCircleMarkers(data = airport.location,
                   lng=~lng,
                   lat=~lat,
                   label=~airport,
                   labelOptions=labelOptions(noHide=T, textsize="8px", direction="top", opacity = 0.8),
                   radius=0, stroke=F, group="Airports")
    } else {
      leafletProxy("map") %>% clearGroup("Airports")
    }
  })
  
  observeEvent({
    input$operation
    input$runway
    input$run
    input$maptype
    input$sectors
    input$lateralconflicttype
    input$verticalconflicttype
    input$flightphase
    input$flighttype
    input$colour
    input$severity
    input$verticalseverity
  },{
    if ("TMA" %in% input$sectors & "Non-TMA" %in% input$sectors) {
      d <- table.Conflicts
    } else if ("TMA" %in% input$sectors & "Non-TMA" %!in% input$sectors) {
      d <- subset(table.Conflicts, Sector %in% TMA)
    } else if ("TMA" %!in% input$sectors & "Non-TMA" %in% input$sectors) {
      d <- subset(table.Conflicts, Sector %in% NonTMA)
    } else if ("TMA" %!in% input$sectors & "Non-TMA" %!in% input$sectors) {
      d <- subset(table.Conflicts, Sector %!in% TMA & Sector %!in% NonTMA)
    }
    if ("All Runs" %!in% input$run) {
      d <- subset(d, Run %in% input$run)
    }
    d <- subset(d,
                Scenario %in% input$operation
                & Runway %in% input$runway
                & LateralConflictType %in% as.character(c(input$lateralconflicttype))
                & VerticalConflictType %in% as.character(c(input$verticalconflicttype))
                & FlightPlanPhases %in% as.character(c(input$flightphase))
                & FlightTypes %in% as.character(c(input$flighttype))
                & Severity >= as.integer(input$severity[1]) & Severity <= as.integer(input$severity[2])
                & VerticalSeverity >= as.integer(input$verticalseverity[1]) & Severity <= as.integer(input$verticalseverity[2]))
    
    p <- leafletProxy("map", data=d) %>% clearGroup("Markers") %>% clearGroup("Heatmap") %>% removeControl("Legend") %>% removeControl("Legend2")
    
    req(input$maptype, input$colour)
    if (input$maptype == "Markers") {
      
      if (dim(d)[1] != 0 & dim(d)[2] != 0) {
        lab <- sprintf(
          "<b>ID</b>: %s<br/>
          <b>Scenario</b>: %s RD %s Run %s<br/>
          <b>Sector</b>: %s<br/>
          <b>Start Time</b>: %s<br/>
          <b>Closest Time</b>: %s<br/>
          <b>End Time</b>: %s<br/>
          <b>Conflict Type</b>: %s<br/>
          <b>Severity</b>: %s (Vertical %s)<br/>
          <b>Altitude (ft)</b>: %s<br/>
          <b>Lateral Sep. (NM</b>): %s (Req. %s)<br/>
          <b>Vertical Sep. (ft)</b>: %s (Req. %s)<br/>
          <b>FP Phases</b>: %s<br/>
          <b>Flight Types</b>: %s<br/>
          <b>Routings</b>:<br/>1. %s<br/>2. %s<br/>
          <b>Origin and Destination</b>:<br/>1. %s to %s<br/>2. %s to %s<br/>",
          d$ID,
          d$Scenario, d$Runway, d$Run,
          d$Sector,
          d$Start_Time, d$Closest_Time, d$End_Time,
          d$ConflictType,
          d$Severity, d$VerticalSeverity,
          d$Altitude_ft,
          d$LateralSeparation, d$ReqLateralSeparation,
          d$VerticalSeparation, d$ReqVerticalSeparation,
          ifelse(d$FlightPlanPhase1 == d$FlightPlanPhase2, paste("Both",d$FlightPlanPhase1), paste(d$FlightPlanPhase1,"and",d$FlightPlanPhase2)),
          ifelse(d$FlightType1 == d$FlightType2, paste("Both",gsub("AirCarrier","IFR",gsub("GeneralAviation","VFR",d$FlightType1))), paste(gsub("AirCarrier","IFR",gsub("GeneralAviation","VFR",d$FlightType1)),"and",gsub("AirCarrier","IFR",gsub("GeneralAviation","VFR",d$FlightType2)))),
          d$Routing1, d$Routing2,
          d$Origin1, d$Destination1, d$Origin2, d$Destination2) %>% lapply(htmltools::HTML)
      } else {
        lab <- NULL
      }
      
      if (input$colour == "Operation") {
        p %>% addCircleMarkers(lng=~Longitude, lat=~Latitude,
                               color=~ifelse(Scenario == "Current", "Blue", "Red"),
                               fillColor=~ifelse(Scenario == "Current", "Blue", "Red"),
                               label=lab, labelOptions=labelOptions(textsize="13px", direction="auto"),
                               clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=T,spiderfyOnMaxZoom=T,spiderfyDistanceMultiplier=1.5,maxClusterRadius=10),
                               weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Markers")
        
      } else if (input$colour == "Conflict Type") {
        pal1 <- colorFactor("Spectral", domain=d$ConflictType)
        p %>% addCircleMarkers(lng=~Longitude, lat=~Latitude,
                               color=~pal1(ConflictType),
                               fillColor=~ifelse(Scenario == "Current", "Blue", "Red"),
                               label=lab, labelOptions=labelOptions(textsize="13px", direction="auto"),
                               clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=T,spiderfyOnMaxZoom=T,spiderfyDistanceMultiplier=1.5,maxClusterRadius=10),
                               weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Markers") %>%
          addLegend("bottomleft", pal=pal1, values=~ConflictType, title="Conflict Type", opacity=0.95, layerId="Legend")
        
      } else if (input$colour == "Lateral Conflict Type") {
        pal1 <- colorFactor("Set1", domain=d$LateralConflictType)
        p %>% addCircleMarkers(lng=~Longitude, lat=~Latitude,
                               color=~pal1(LateralConflictType),
                               fillColor=~ifelse(Scenario == "Current", "Blue", "Red"),
                               label=lab, labelOptions=labelOptions(textsize="13px", direction="auto"),
                               clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=T,spiderfyOnMaxZoom=T,spiderfyDistanceMultiplier=1.5,maxClusterRadius=10),
                               weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Markers") %>%
          addLegend("bottomleft", pal=pal1, values=~LateralConflictType, title="Lateral Conflict Type", opacity=0.95, layerId="Legend")
        
      } else if (input$colour == "Vertical Conflict Type") {
        pal1 <- colorFactor("Set2", domain=d$VerticalConflictType)
        p %>% addCircleMarkers(lng=~Longitude, lat=~Latitude,
                               color=~pal1(VerticalConflictType),
                               fillColor=~ifelse(Scenario == "Current", "Blue", "Red"),
                               label=lab, labelOptions=labelOptions(textsize="13px", direction="auto"),
                               clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=T,spiderfyOnMaxZoom=T,spiderfyDistanceMultiplier=1.5,maxClusterRadius=10),
                               weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Markers") %>%
          addLegend("bottomleft", pal=pal1, values=~VerticalConflictType, title="Vertical Conflict Type", opacity=0.95, layerId="Legend")
        
      } else if (input$colour == "Flight Phase") {
        pal1 <- colorFactor(c("#FF0000", "#FF7D12", "#FFF717", "#BDFF14", "#74FF17", "#1BC221", "#2C9631", "#33FFC2", "#42DBD9", "#4EA3A6",
                              "#5CADFF", "#1C8EFF", "#121AFF", "#B0B4FF", "#9780FF", "#9F38FF", "#FFBDF5", "#FF4FF6", "#FF03FF", "#800077"),
                            domain=d$FlightPlanPhases)
        p %>% addCircleMarkers(lng=~Longitude, lat=~Latitude,
                               color=~pal1(FlightPlanPhases),
                               fillColor=~ifelse(Scenario == "Current", "Blue", "Red"),
                               label=lab, labelOptions=labelOptions(textsize="13px", direction="auto"),
                               clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=T,spiderfyOnMaxZoom=T,spiderfyDistanceMultiplier=1.5,maxClusterRadius=10),
                               weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Markers") %>%
          addLegend("bottomleft", pal=pal1, values=~FlightPlanPhases, title="Flight Plan Phases", opacity=0.95, layerId="Legend")
        
      } else if (input$colour == "Flight Type") {
        pal1 <- colorFactor(c("Red2","Grey20","Blue2","Yellow","Purple","Green4"), domain=d$FlightTypes)
        p %>% addCircleMarkers(lng=~Longitude, lat=~Latitude,
                               color=~pal1(FlightTypes),
                               fillColor=~ifelse(Scenario == "Current", "Blue", "Red"),
                               label=lab, labelOptions=labelOptions(textsize="13px", direction="auto"),
                               clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=T,spiderfyOnMaxZoom=T,spiderfyDistanceMultiplier=1.5,maxClusterRadius=10),
                               weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Markers") %>%
          addLegend("bottomleft", pal=pal1, values=~FlightTypes, title="Flight Types", opacity=0.95, layerId="Legend")
        
      } else if (input$colour == "Severity") {
        pal1 <- colorFactor("YlOrRd", domain=seq(0,6,1))
        p %>% addCircleMarkers(lng=~Longitude, lat=~Latitude,
                               color=~pal1(Severity),
                               fillColor=~ifelse(Scenario == "Current", "Blue", "Red"),
                               label=lab, labelOptions=labelOptions(textsize="13px", direction="auto"),
                               clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=T,spiderfyOnMaxZoom=T,spiderfyDistanceMultiplier=1.5,maxClusterRadius=10),
                               weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Markers") %>%
          addLegend("bottomleft", pal=pal1, values=~Severity, title="Severity", opacity=0.95, layerId="Legend")
        
      } else if (input$colour == "Vertical Severity") {
        pal1 <- colorFactor("YlOrRd", domain=seq(0,6,1))
        p %>% addCircleMarkers(lng=~Longitude, lat=~Latitude,
                               color=~pal1(VerticalSeverity),
                               fillColor=~ifelse(Scenario == "Current", "Blue", "Red"),
                               label=lab, labelOptions=labelOptions(textsize="13px", direction="auto"),
                               clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=T,spiderfyOnMaxZoom=T,spiderfyDistanceMultiplier=1.5,maxClusterRadius=10),
                               weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Markers") %>%
          addLegend("bottomleft", pal=pal1, values=~VerticalSeverity, title="Vertical Severity", opacity=0.95, layerId="Legend")
        
      } else if (input$colour == "Altitude") {
        pal1 <- colorNumeric("YlGnBu", domain=d$Altitude_ft)
        p %>% addCircleMarkers(lng=~Longitude, lat=~Latitude,
                               color=~pal1(Altitude_ft),
                               fillColor=~ifelse(Scenario == "Current", "Blue", "Red"),
                               label=lab, labelOptions=labelOptions(textsize="13px", direction="auto"),
                               clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=T,spiderfyOnMaxZoom=T,spiderfyDistanceMultiplier=1.5,maxClusterRadius=10),
                               weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Markers") %>%
          addLegend("bottomleft", pal=pal1, values=~Altitude_ft, title="Altitude (ft)", opacity=0.95, layerId="Legend")
        
      } else if (input$colour == "Lateral Separation") {
        pal1 <- colorNumeric("RdYlGn", domain=seq(0,10,0.1))
        p %>% addCircleMarkers(lng=~Longitude, lat=~Latitude,
                               color=~pal1(LateralSeparation),
                               fillColor=~ifelse(Scenario == "Current", "Blue", "Red"),
                               label=lab, labelOptions=labelOptions(textsize="13px", direction="auto"),
                               clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=T,spiderfyOnMaxZoom=T,spiderfyDistanceMultiplier=1.5,maxClusterRadius=10),
                               weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Markers") %>%
          addLegend("bottomleft", pal=pal1, values=~LateralSeparation, title="Lat. Sep. (NM)", opacity=0.95, layerId="Legend")
        
      } else if (input$colour == "Vertical Separation") {
        pal1 <- colorNumeric("RdYlGn", domain=seq(0,1000,1))
        p %>% addCircleMarkers(lng=~Longitude, lat=~Latitude,
                               color=~pal1(VerticalSeparation),
                               fillColor=~ifelse(Scenario == "Current", "Blue", "Red"),
                               label=lab, labelOptions=labelOptions(textsize="13px", direction="auto"),
                               clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=T,spiderfyOnMaxZoom=T,spiderfyDistanceMultiplier=1.5,maxClusterRadius=10),
                               weight=5, radius=5, stroke=T, opacity=0.85, fillOpacity=0.85, group="Markers") %>%
          addLegend("bottomleft", pal=pal1, values=~VerticalSeparation, title="Vert. Sep. (ft)", opacity=0.95, layerId="Legend")
        
      }
      
      p %>% addLegend("bottomleft", colors=c("Blue","Red"), labels=c("Current","PBN"), title="Operation", opacity=0.95, layerId="Legend2")
      
    } else if (input$maptype == "Heatmap") {
      
      p %>% addHeatmap(lng=~Longitude, lat=~Latitude, max=2, radius=15, group="Heatmap")
      
    }
    
  })
}