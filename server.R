source("global.R", local = T)

airport.location <- data.frame(
  airport <- c("LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"),
  lng <- c(16.29800033569336,18.268199920654297,16.0687999725,13.922200202941895,15.346699714660645,14.3930997849,14.570300102233887,16.67970085144043,18.810199737548828),
  lat <- c(43.53889846801758,42.5614013671875,45.7429008484,44.89350128173828,44.108299255371094,44.5657997131,45.21689987182617,43.285701751708984,45.46269989013672)
); names(airport.location) <- c("airport","lng","lat")

# Selection choices
choices.throughputs <- c("Total Throughput","Hourly Throughput","Rolling Hourly Throughput")
choices.efficiency <- c("Track Miles","Fuel Burn")
choices.efficiencygrouping <- c("Airport (Combined)", "Airport (Arrivals Only)", "Airport (Departures Only)", "Routing (Combined)", "Routing (Arrivals Only)", "Routing (Departures Only)")
choices.safety <- c("Conflict Map", "Conflict Statistics")
choices.mapcolours <- c("Operation","Conflict Type","Lateral Conflict Type","Vertical Conflict Type","Flight Phase","Flight Type","Severity","Vertical Severity","Altitude","Lateral Separation","Vertical Separation")
choices.conflictsgrouping <- c("Conflict Type","Conflict Type (Lateral)","Conflict Type (Vertical)","Flight Phase","Severity","Severity (Vertical)")
choices.sectorcapacity <- c("Sector Entry Count","Sector Occupancy Count","Controller Workload","Workload vs Entries")

function(input, output, session){
  
  session$allowReconnect(TRUE)
  
  output$metric <- renderUI({
    x <- input$kpa
    if (x == "Throughput") {
      selectInput("metric","Metric",choices.throughputs)
    } else if (x == "Efficiency") {
      selectInput("metric","Metric",choices.efficiency)
    } else if (x == "Safety") {
      selectInput("metric","Metric",choices.safety)
    } else if (x == "Sector Capacity") {
      selectInput("metric","Metric",choices.sectorcapacity)
    }
  })
  
  output$options <- renderUI({
    x <- input$metric
    d <- subset(table.Conflicts, Scenario %in% input$operation & Runway %in% input$runway)
    if (x == "Total Throughput") {
      selectInput("options","Airport",list.airports)
    } else if (x == "Hourly Throughput") {
      selectInput("options","Airport",list.airports)
    } else if (x == "Rolling Hourly Throughput") {
      selectInput("options","Airport",list.airports)
    } else if (x == "Track Miles") {
      tagList(
        selectInput("options","Group by",choices.efficiencygrouping),
        selectInput("moreoptions","Airport",list.airports)
      )
    } else if (x == "Fuel Burn") {
      tagList(
        selectInput("options","Group by",choices.efficiencygrouping),
        selectInput("moreoptions","Airport",list.airports)
      )
    } else if (x == "Conflict Map") {
      div(style="text-align:center;",checkboxGroupInput("sectors","Sectors",c("TMA","Non-TMA"),selected=c("TMA"),inline=T))
    } else if (x == "Conflict Statistics") {
      selectInput("options","Group by",choices.conflictsgrouping)
    } else if (x == "Sector Occupancy Count") {
      selectInput("options","Sector",list.sectors)
    } else if (x == "Sector Entry Count") {
      selectInput("options","Sector",list.sectors)
    } else if (x == "Controller Workload") {
      selectInput("options","Sector",list.sectors)
    } else if (x == "Workload vs Entries") {
      selectInput("options","Sector",list.sectors)
    }
  })
  
  output$arrange <- renderUI({
    if ("Current" %in% input$operation & "PBN" %in% input$operation & input$metric != "Conflict Map") {
      radioButtons("arrange","Plot Arrangement",c("Vertical","Horizontal","Group"),selected="Vertical",inline=T)
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
            div(id="mapcontrols1",
                checkboxGroupInput("lateralconflicttype","Lateral Type",
                                   sort(unique(table.Conflicts$LateralConflictType)),
                                   selected=sort(unique(table.Conflicts$LateralConflictType))),
                checkboxGroupInput("verticalconflicttype","Vertical Type",
                                   sort(unique(table.Conflicts$VerticalConflictType)),
                                   selected=sort(unique(table.Conflicts$VerticalConflictType))),
                checkboxGroupInput("flighttype","Flight Type",
                                   sort(unique(table.Conflicts$FlightTypes)),
                                   selected=sort(unique(table.Conflicts$FlightTypes)))),
            div(id="mapcontrols2",
                checkboxGroupInput("flightphase","Flight Phase",
                                   sort(unique(table.Conflicts$FlightPlanPhases)),
                                   selected=sort(unique(table.Conflicts$FlightPlanPhases)))),
            div(id="mapcontrols3",selectInput("colour","Colour Outline",choices.mapcolours,selected="Severity")),
            div(id="mapcontrols4",
                sliderInput("severity","Severity",
                            min(table.Conflicts$Severity),max(table.Conflicts$Severity),
                            value=range(table.Conflicts$Severity),step=1)),
            div(id="mapcontrols5",
                sliderInput("verticalseverity","Vertical Severity",
                            min(table.Conflicts$VerticalSeverity),max(table.Conflicts$VerticalSeverity),
                            value=range(table.Conflicts$VerticalSeverity),step=1)),
            div(id="mapcontrols6",checkboxInput("polygons","TMA Outlines",T)),
            div(id="mapcontrols7",checkboxInput("airports","Airports",T))
        ),
        width=300,
        right=10,
        top=10,
        fixed=T,
        draggable=F
      )
    )
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
      data <- table.Conflicts
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
    } else if (x == "Controller Workload" | x == "Workload vs Entries") {
      data <- table.Workload
      if (input$options != "All") {
        data <- subset(data, Sector %in% input$options)
      }
    }
    subset(data, Scenario %in% input$operation & Runway %in% input$runway)
  }, options = list(pageLength = 20))
  
  output$plot <- renderPlotly({
    x <- input$metric
    if (x == "Total Throughput") {
      plotlyTotalThroughput(operation=c(input$operation), runway=input$runway, airport=input$options, arrange=input$arrange)
    } else if (x == "Hourly Throughput") {
      plotlyHourlyThroughput(operation=c(input$operation), runway=input$runway, airport=input$options, arrange=input$arrange)
    } else if (x == "Rolling Hourly Throughput") {
      plotlyRollingHourlyThroughput(operation=c(input$operation), runway=input$runway, airport=input$options, arrange=input$arrange)
    } else if (x == "Track Miles") {
      plotlyTrackMiles(operation=c(input$operation), runway=input$runway, group=input$options, airport=input$moreoptions, arrange=input$arrange)
    } else if (x == "Fuel Burn") {
      plotlyFuelBurn(operation=c(input$operation), runway=input$runway, group=input$options, airport=input$moreoptions, arrange=input$arrange)
    } else if (x == "Conflict Statistics") {
      plotlyConflictCount(operation=c(input$operation), runway=input$runway, group=input$options, arrange=input$arrange)
    } else if (x == "Sector Entry Count") {
      plotlySectorEntry(operation=c(input$operation), runway=input$runway, sector=input$options, arrange=input$arrange)
    } else if (x == "Sector Occupancy Count") {
      plotlySectorOccupancy(operation=c(input$operation), runway=input$runway, sector=input$options, arrange=input$arrange)
    } else if (x == "Controller Workload") {
      plotlyControllerWorkload(operation=c(input$operation), runway=input$runway, sector=input$options, arrange=input$arrange)
    } else if (x == "Workload vs Entries") {
      plotlyWorkloadEntries(operation=c(input$operation), runway=input$runway, sector=input$options, arrange=input$arrange)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = F)) %>% 
      setView(lng = 16.8, lat = 44.2, zoom = 7) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group="OSM.Mapnik") %>%
      addProviderTiles(providers$CartoDB.Positron, group="CartoDB.Positron") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group="CartoDB.DarkMatter") %>%
      addProviderTiles(providers$Stamen.Toner, group="Stamen.Toner") %>% 
      addProviderTiles(providers$Stamen.TonerLite, group="Stamen.TonerLite") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group="Esri.WorldImagery") %>%
      addLayersControl(baseGroups = c("OSM.Mapnik","CartoDB.Positron","CartoDB.DarkMatter","Stamen.Toner","Stamen.TonerLite","Esri.WorldImagery"),
                       options = layersControlOptions(collapsed = T))
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
    input$sectors
    input$lateralconflicttype
    input$verticalconflicttype
    input$flightphase
    input$flighttype
    input$colour
    input$severity
    input$verticalseverity
  },{
    TMA <- c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")
    NonTMA <- unlist(unique(subset(table.Conflicts,Sector %!in% TMA, select=Sector)),use.names=F)
    if ("TMA" %in% input$sectors & "Non-TMA" %in% input$sectors) {
      d <- table.Conflicts
    } else if ("TMA" %in% input$sectors & "Non-TMA" %!in% input$sectors) {
      d <- subset(table.Conflicts, Sector %in% TMA)
    } else if ("TMA" %!in% input$sectors & "Non-TMA" %in% input$sectors) {
      d <- subset(table.Conflicts, Sector %in% NonTMA)
    } else if ("TMA" %!in% input$sectors & "Non-TMA" %!in% input$sectors) {
      d <- subset(table.Conflicts, Sector %!in% TMA & Sector %!in% NonTMA)
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
    
    if (dim(d)[1] != 0 & dim(d)[2] != 0) {
      lab <- sprintf(
        "ID: %s<br/>Sector: %s<br/>Time Closest: %s<br/>Conflict Type: %s<br/>Severity: %s<br/>Vertical Severity: %s<br/>FP Phases: %s<br/>
        Flight Types: %s<br/>Routings: %s<br/>Altitude (ft): %s<br/>Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)",
        d$ID, d$Sector, d$Closest_Time, d$ConflictType, d$Severity, d$VerticalSeverity,
        d$FlightPlanPhases, d$FlightTypes, d$Routings, d$Altitude_ft,
        d$LateralSeparation, d$ReqLateralSeparation, d$VerticalSeparation, d$ReqVerticalSeparation) %>% lapply(htmltools::HTML)
    } else {
      lab <- NULL
    }
    
    p <- leafletProxy("map", data=d) %>% clearGroup("Conflicts") %>% removeControl("Legend")
    
    pal2 <- colorFactor(c("Red","Blue"), domain=d$Scenario)
    
    if (input$colour == "Operation") {
      p %>% addCircleMarkers(lng=~Longitude,
                             lat=~Latitude,
                             color=~pal2(Scenario),
                             fillColor=~pal2(Scenario),
                             label=lab,
                             labelOptions=labelOptions(textsize="13px", direction="auto"),
                             clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=F,spiderfyOnMaxZoom=F,maxClusterRadius=10),
                             radius = 5, stroke = TRUE, fillOpacity = 0.85, group="Conflicts") %>%
        addLegend("bottomleft", pal=pal2, values=~Scenario, title="Operation", opacity=0.95, layerId="Legend")
    } else if (input$colour == "Conflict Type") {
      pal1 <- colorFactor("Spectral", domain=d$ConflictType)
      p %>% addCircleMarkers(lng=~Longitude,
                             lat=~Latitude,
                             color=~pal1(ConflictType),
                             fillColor=~pal2(Scenario),
                             label=lab,
                             labelOptions=labelOptions(textsize="13px", direction="auto"),
                             clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=F,spiderfyOnMaxZoom=F,maxClusterRadius=10),
                             radius = 5, stroke = TRUE, fillOpacity = 0.85, group="Conflicts") %>%
        addLegend("bottomleft", pal=pal1, values=~ConflictType, title="Conflict Type", opacity=0.95, layerId="Legend")
      
    } else if (input$colour == "Lateral Conflict Type") {
      pal1 <- colorFactor("Set1", domain=d$LateralConflictType)
      p %>% addCircleMarkers(lng=~Longitude,
                             lat=~Latitude,
                             color=~pal1(LateralConflictType),
                             fillColor=~pal2(Scenario),
                             label=lab,
                             labelOptions=labelOptions(textsize="13px", direction="auto"),
                             clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=F,spiderfyOnMaxZoom=F,maxClusterRadius=10),
                             radius = 5, stroke = TRUE, fillOpacity = 0.85, group="Conflicts") %>%
        addLegend("bottomleft", pal=pal1, values=~LateralConflictType, title="Lateral Conflict Type", opacity=0.95, layerId="Legend")
      
    } else if (input$colour == "Vertical Conflict Type") {
      pal1 <- colorFactor("Set2", domain=d$VerticalConflictType)
      p %>% addCircleMarkers(lng=~Longitude,
                             lat=~Latitude,
                             color=~pal1(VerticalConflictType),
                             fillColor=~pal2(Scenario),
                             label=lab,
                             labelOptions=labelOptions(textsize="13px", direction="auto"),
                             clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=F,spiderfyOnMaxZoom=F,maxClusterRadius=10),
                             radius = 5, stroke = TRUE, fillOpacity = 0.85, group="Conflicts") %>%
        addLegend("bottomleft", pal=pal1, values=~VerticalConflictType, title="Vertical Conflict Type", opacity=0.95, layerId="Legend")
      
    } else if (input$colour == "Flight Phase") {
      pal1 <- colorFactor(c("#FF0000", "#FF7D12", "#FFF717", "#BDFF14", "#74FF17", "#1BC221", "#2C9631", "#33FFC2", "#42DBD9", "#4EA3A6",
                               "#5CADFF", "#1C8EFF", "#121AFF", "#B0B4FF", "#9780FF", "#9F38FF", "#FFBDF5", "#FF4FF6", "#FF03FF", "#800077"),
                             domain=d$FlightPlanPhases)
      p %>% addCircleMarkers(lng=~Longitude,
                             lat=~Latitude,
                             color=~pal1(FlightPlanPhases),
                             fillColor=~pal2(Scenario),
                             label=lab,
                             labelOptions=labelOptions(textsize="13px", direction="auto"),
                             clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=F,spiderfyOnMaxZoom=F,maxClusterRadius=10),
                             radius = 5, stroke = TRUE, fillOpacity = 0.85, group="Conflicts") %>%
        addLegend("bottomleft", pal=pal1, values=~FlightPlanPhases, title="Flight Plan Phases", opacity=0.95, layerId="Legend")
      
    } else if (input$colour == "Flight Type") {
      pal1 <- colorFactor(c("Red2","Grey20","Blue2","Yellow","Purple","Green4"), domain=d$FlightTypes)
      p %>% addCircleMarkers(lng=~Longitude,
                             lat=~Latitude,
                             color=~pal1(FlightTypes),
                             fillColor=~pal2(Scenario),
                             label=lab,
                             labelOptions=labelOptions(textsize="13px", direction="auto"),
                             clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=F,spiderfyOnMaxZoom=F,maxClusterRadius=10),
                             radius = 5, stroke = TRUE, fillOpacity = 0.85, group="Conflicts") %>%
        addLegend("bottomleft", pal=pal1, values=~FlightTypes, title="Flight Types", opacity=0.95, layerId="Legend")
      
    } else if (input$colour == "Severity") {
      pal1 <- colorFactor("YlOrRd", domain=seq(0,6,1))
      p %>% addCircleMarkers(lng=~Longitude,
                             lat=~Latitude,
                             color=~pal1(Severity),
                             fillColor=~pal2(Scenario),
                             label=lab,
                             labelOptions=labelOptions(textsize="13px", direction="auto"),
                             clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=F,spiderfyOnMaxZoom=F,maxClusterRadius=10),
                             radius = 5, stroke = TRUE, fillOpacity = 0.85, group="Conflicts") %>%
        addLegend("bottomleft", pal=pal1, values=~Severity, title="Severity", opacity=0.95, layerId="Legend")
      
    } else if (input$colour == "Vertical Severity") {
      pal1 <- colorFactor("YlOrRd", domain=seq(0,6,1))
      p %>% addCircleMarkers(lng=~Longitude,
                             lat=~Latitude,
                             color=~pal1(VerticalSeverity),
                             fillColor=~pal2(Scenario),
                             label=lab,
                             labelOptions=labelOptions(textsize="13px", direction="auto"),
                             clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=F,spiderfyOnMaxZoom=F,maxClusterRadius=10),
                             radius = 5, stroke = TRUE, fillOpacity = 0.85, group="Conflicts") %>%
        addLegend("bottomleft", pal=pal1, values=~VerticalSeverity, title="Vertical Severity", opacity=0.95, layerId="Legend")
      
    } else if (input$colour == "Altitude") {
      pal1 <- colorNumeric("YlGnBu", domain=d$Altitude_ft)
      p %>% addCircleMarkers(lng=~Longitude,
                             lat=~Latitude,
                             color=~pal1(Altitude_ft),
                             fillColor=~pal2(Scenario),
                             label=lab,
                             labelOptions=labelOptions(textsize="13px", direction="auto"),
                             clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=F,spiderfyOnMaxZoom=F,maxClusterRadius=10),
                             radius = 5, stroke = TRUE, fillOpacity = 0.85, group="Conflicts") %>%
        addLegend("bottomleft", pal=pal1, values=~Altitude_ft, title="Altitude (ft)", opacity=0.95, layerId="Legend")
      
    } else if (input$colour == "Lateral Separation") {
      pal1 <- colorNumeric("RdYlGn", domain=seq(0,10,0.1))
      p %>% addCircleMarkers(lng=~Longitude,
                             lat=~Latitude,
                             color=~pal1(LateralSeparation),
                             fillColor=~pal2(Scenario),
                             label=lab,
                             labelOptions=labelOptions(textsize="13px", direction="auto"),
                             clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=F,spiderfyOnMaxZoom=F,maxClusterRadius=10),
                             radius = 5, stroke = TRUE, fillOpacity = 0.85, group="Conflicts") %>%
        addLegend("bottomleft", pal=pal1, values=~LateralSeparation, title="Lateral Separation (NM)", opacity=0.95, layerId="Legend")
      
    } else if (input$colour == "Vertical Separation") {
      pal1 <- colorNumeric("RdYlGn", domain=seq(0,1000,1))
      p %>% addCircleMarkers(lng=~Longitude,
                             lat=~Latitude,
                             color=~pal1(VerticalSeparation),
                             fillColor=~pal2(Scenario),
                             label=lab,
                             labelOptions=labelOptions(textsize="13px", direction="auto"),
                             clusterOptions = markerClusterOptions(showCoverageOnHover=T,zoomToBoundsOnClick=F,spiderfyOnMaxZoom=F,maxClusterRadius=10),
                             radius = 5, stroke = TRUE, fillOpacity = 0.85, group="Conflicts") %>%
        addLegend("bottomleft", pal=pal1, values=~VerticalSeparation, title="Vertical Separation (ft)", opacity=0.95, layerId="Legend")
      
    }
  })
}