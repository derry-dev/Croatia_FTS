source("global.R", local = T)

function(input, output, session){
  
  session$allowReconnect(TRUE)
  options(shiny.launch.browser=FALSE)
  
  v <- reactiveValues(doDisplay = TRUE, doTable = FALSE)
  
  observeEvent(input$view, {
    v$doDisplay <- TRUE
    v$doTable <- FALSE
  })
  
  observeEvent(input$explore, {
    v$doDisplay <- FALSE
    v$doTable <- TRUE
  })
  
  observeEvent(input$import, {
    dbimport()
    v$doDisplay <- FALSE
    v$doTable <- FALSE
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
    shinyjs::reset("plot","map")
  })
  
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
    if (x == "Total Throughput") {
      selectInput("options","Airport",list.airports)
    } else if (x == "Hourly Throughput") {
      selectInput("options","Airport",list.airports)
    } else if (x == "Rolling Hourly Throughput") {
      selectInput("options","Airport",list.airports)
    } else if (x == "Track Miles") {
      tagList(
        selectInput("options","Grouping",choices.efficiencygrouping),
        selectInput("moreoptions","Airport",list.airports)
      )
    } else if (x == "Fuel Burn") {
      tagList(
        selectInput("options","Grouping",choices.efficiencygrouping),
        selectInput("moreoptions","Airport",list.airports)
      )
    } else if (x == "Conflict Map") {
      tagList(
        selectInput("options","Grouping",choices.conflictsgrouping),
        selectInput("moreoptions","Sector",list.sectors)
      )
    } else if (x == "Conflict Statistics") {
      selectInput("options","Grouping",choices.conflictsgrouping)
    } else if (x == "Sector Occupancy Count") {
      selectInput("options","Sector",list.sectors)
    } else if (x == "Sector Entry Count") {
      selectInput("options","Sector",list.sectors)
    } else if (x == "Controller Workload") {
      selectInput("options","Sector",list.sectors)
    }
  })
  
  output$moreoptions <- renderUI({
    x <- input$metric
    y <- input$data
    if (length(y) > 1) {
      if (x == "Total Throughput") {
        radioButtons("arrange","Plot arrangement:",c("Vertical","Horizontal","Group","Stack"),inline=T)
      } else if (x == "Hourly Throughput") {
        radioButtons("arrange","Plot arrangement:",c("Vertical","Horizontal","Group","Stack"),inline=T)
      } else if (x == "Rolling Hourly Throughput") {
        radioButtons("arrange","Plot arrangement:",c("Vertical","Horizontal","Group"),inline=T)
      } else if (x == "Track Miles") {
        radioButtons("arrange","Plot arrangement:",c("Vertical","Horizontal","Group"),inline=T)
      } else if (x == "Fuel Burn") {
        radioButtons("arrange","Plot arrangement:",c("Vertical","Horizontal","Group"),inline=T)
      } else if (x == "Conflict Map") {
        
      } else if (x == "Conflict Statistics") {
        radioButtons("arrange","Plot arrangement:",c("Vertical","Horizontal","Group","Stack"),inline=T)
      } else if (x == "Sector Occupancy Count") {
        radioButtons("arrange","Plot arrangement:",c("Vertical","Horizontal","Group"),inline=T)
      } else if (x == "Sector Entry Count") {
        radioButtons("arrange","Plot arrangement:",c("Vertical","Horizontal","Group"),inline=T)
      } else if (x == "Controller Workload") {
        radioButtons("arrange","Plot arrangement:",c("Vertical","Horizontal","Group"),inline=T)
      }
    }
  })
  
  output$table <- renderDataTable({
    x <- input$metric
    if (x == "Total Throughput") {
      data <- table.TotalThroughputs
      if (input$options != "All") {
        data <- subset(data, Airport %in% input$options)
      }
      subset(data, Scenario %in% input$data)
    } else if (x == "Hourly Throughput") {
      data <- table.HourlyThroughputs
      if (input$options != "All") {
        data <- subset(data, Airport %in% input$options)
      }
      subset(data, Scenario %in% input$data)
    } else if (x == "Rolling Hourly Throughput") {
      data <- table.RollingHourlyThroughputs
      if (input$options != "All") {
        data <- subset(data, Airport %in% input$options)
      }
      subset(data, Scenario %in% input$data)
    } else if (x == "Track Miles") {
      data <- table.TrackMiles
    } else if (x == "Fuel Burn") {
      data <- table.FuelBurn
    } else if (x == "Conflict Map") {
      data <- table.Conflicts
      subset(data, Scenario %in% input$data)
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
      subset(data, Scenario %in% input$data)
    } else if (x == "Sector Occupancy Count") {
      data <- table.SectorOccupancy
      if (input$options != "All") {
        data <- subset(data, Sector %in% input$options)
      }
      subset(data, Scenario %in% input$data)
    } else if (x == "Sector Entry Count") {
      data <- table.SectorEntry
      if (input$options != "All") {
        data <- subset(data, Sector %in% input$options)
      }
      subset(data, Scenario %in% input$data)
    } else if (x == "Controller Workload") {
      data <- table.Workload
      if (input$options != "All") {
        data <- subset(data, Sector %in% input$options)
      }
      subset(data, Scenario %in% input$data)
    }
  }, options = list(pageLength = 20))
  
  output$display <- renderUI({
    if (v$doDisplay == TRUE) {
        if (input$metric == "Conflict Map") {
          div(id="wrap100",leafletOutput("map",height="100%"))
        } else {
          div(id="wrap96",plotlyOutput("plot",height="100%"))
        }
    }
    else if (v$doTable == TRUE) {
      isolate({
        div(id="wrap96",DT::dataTableOutput("table"))
      })  
    } else {
      return()
    }
  })
  
  output$plot <- renderPlotly({
    x <- input$metric
    if (x == "Total Throughput") {
      plotlyTotalThroughput(data = c(input$data), airport = input$options, arrange = input$arrange)
    } else if (x == "Hourly Throughput") {
      plotlyHourlyThroughput(data = c(input$data), airport = input$options, arrange = input$arrange)
    } else if (x == "Rolling Hourly Throughput") {
      plotlyRollingHourlyThroughput(data = c(input$data), airport = input$options, arrange = input$arrange)
    } else if (x == "Track Miles") {
      plotlyTrackMiles(data = c(input$data), group = input$options, airport = input$moreoptions, arrange = input$arrange)
    } else if (x == "Fuel Burn") {
      plotlyFuelBurn(data = c(input$data), group = input$options, airport = input$moreoptions, arrange = input$arrange)
    } else if (x == "Conflict Statistics") {
      plotlyConflictCount(data = c(input$data), group = input$options, arrange = input$arrange)
    } else if (x == "Sector Entry Count") {
      plotlySectorEntry(data = c(input$data), sector = input$options, arrange = input$arrange)
    } else if (x == "Sector Occupancy Count") {
      plotlySectorOccupancy(data = c(input$data), sector = input$options, arrange = input$arrange)
    } else if (x == "Controller Workload") {
      plotlyControllerWorkload(data = c(input$data), sector = input$options, arrange = input$arrange)
    }
  })
  
  output$map <- renderLeaflet({
    
    airport.location <- data.frame(
      airport <- c("LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"),
      lng <- c(16.29800033569336,18.268199920654297,16.0687999725,13.922200202941895,15.346699714660645,14.3930997849,14.570300102233887,16.67970085144043,18.810199737548828),
      lat <- c(43.53889846801758,42.5614013671875,45.7429008484,44.89350128173828,44.108299255371094,44.5657997131,45.21689987182617,43.285701751708984,45.46269989013672)
    ); names(airport.location) <- c("airport","lng","lat")
    
    leaflet() %>% 
      setView(lng = 16.8, lat = 44.2, zoom = 7) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group="OSM.Mapnik") %>%
      addProviderTiles(providers$CartoDB.Positron, group="CartoDB.Positron") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group="CartoDB.DarkMatter") %>%
      addProviderTiles(providers$Stamen.Toner, group="Stamen.Toner") %>% 
      addProviderTiles(providers$Stamen.TonerLite, group="Stamen.TonerLite") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group="Esri.WorldImagery") %>%
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
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
      addCircleMarkers(data = airport.location,
                       lng=~lng,
                       lat=~lat,
                       label=~airport,
                       labelOptions=labelOptions(noHide=T, textsize="8px", direction="bottom", opacity = 0.8),
                       radius = 0, stroke = F, fillOpacity = 0, group="Airports") %>%
      addLayersControl(baseGroups = c("OSM.Mapnik","CartoDB.Positron","CartoDB.DarkMatter","Stamen.Toner","Stamen.TonerLite","Esri.WorldImagery"),
                       overlayGroups = c("TMA Sectors", "Airports"),
                       options = layersControlOptions(collapsed = T))
    
  })
  
  observeEvent(list(input$data,input$options,input$moreoptions),{
    
    proxy <- leafletProxy("map", session)
    d <- subset(table.Conflicts, Scenario %in% c(input$data) & Sector %in% input$moreoptions)
    
    labels<- sprintf(
      "ID: %s<br/>Sector: %s<br/>Time Closest: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Flight Types: %s %s<br/>Routings: %s %s<br/>
      Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
      d$ID,d$Sector,d$Closest_Time,d$ConflictType,d$FlightPlanPhase1,d$FlightPlanPhase2,d$FlightType1,d$FlightType2,d$Routing1,d$Routing2,
      d$LateralSeparation,d$ReqLateralSeparation,d$VerticalSeparation,d$ReqVerticalSeparation,d$Altitude_ft) %>% lapply(htmltools::HTML)
    
    palette.factor.current <- brewer.pal(n = 8, name = "Spectral")
    palette.factor.PBN <- rainbow(12)[1:8]
    palette.numeric.current <- brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)]
    palette.numeric.PBN <- brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)]
    
    # if (input$options == "Conflict Type") {
    #   proxy %>%
    #     addCircleMarkers(data = d,
    #                      lng=~Longitude,
    #                      lat=~Latitude,
    #                      color=palette.factor.current,
    #                      fillColor=~ConflictType,
    #                      label=labels,
    #                      labelOptions=labelOptions(textsize="13px", direction="auto"),
    #                      radius = 5, stroke = TRUE, fillOpacity = 0.8, group="Conflicts")
    #   } else if (input$options == "Conflict Type (Lateral)") {
    # 
    #   } else if (input$options == "Conflict Type (Vertical)") {
    # 
    #   } else if (input$options == "Flight Phase") {
    # 
    #   } else if (input$options == "Severity") {
    # 
    #   } else if (input$options == "Severity (Vertical)") {
    # 
    # }
    
    # scenario <- input$data
    # group <- input$options
    # 
    # 
    # 

    # 
    # 
    # sector <- input$moreoptions
    # 
    #  
    # 
    # TMA <- c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")
    # t1 <- subset(table.Conflicts, Scenario %in% "Current" & !(Sector %in% TMA))
    # t2 <- subset(table.Conflicts, Scenario %in% "Current" & Sector %in% TMA)
    # t3 <- subset(table.Conflicts, Scenario %in% "PBN" & !(Sector %in% TMA))
    # t4 <- subset(table.Conflicts, Scenario %in% "PBN" & Sector %in% TMA)
    # 
    # labels.current.NonTMA <- sprintf(
    #   "ID: %s<br/>Sector: %s<br/>Time Closest: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Flight Types: %s %s<br/>Routings: %s %s<br/>
    #   Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    #   t1$ID,t1$Sector,t1$Closest_Time,t1$ConflictType,t1$FlightPlanPhase1,t1$FlightPlanPhase2,t1$FlightType1,t1$FlightType2,t1$Routing1,t1$Routing2,
    #   t1$LateralSeparation,t1$ReqLateralSeparation,t1$VerticalSeparation,t1$ReqVerticalSeparation,t1$Altitude_ft) %>% lapply(htmltools::HTML)
    # 
    # labels.current.TMA <- sprintf(
    #   "ID: %s<br/>Sector: %s<br/>Time Closest: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Flight Types: %s %s<br/>Routings: %s %s<br/>
    #   Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    #   t2$ID,t2$Sector,t2$Closest_Time,t2$ConflictType,t2$FlightPlanPhase1,t2$FlightPlanPhase2,t2$FlightType1,t2$FlightType2,t2$Routing1,t2$Routing2,
    #   t2$LateralSeparation,t2$ReqLateralSeparation,t2$VerticalSeparation,t2$ReqVerticalSeparation,t2$Altitude_ft) %>% lapply(htmltools::HTML)
    # 
    # labels.PBN.NonTMA <- sprintf(
    #   "ID: %s<br/>Sector: %s<br/>Time Closest: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Flight Types: %s %s<br/>Routings: %s %s<br/>
    #   Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    #   t3$ID,t3$Sector,t3$Closest_Time,t3$ConflictType,t3$FlightPlanPhase1,t3$FlightPlanPhase2,t3$FlightType1,t3$FlightType2,t3$Routing1,t3$Routing2,
    #   t3$LateralSeparation,t3$ReqLateralSeparation,t3$VerticalSeparation,t3$ReqVerticalSeparation,t3$Altitude_ft) %>% lapply(htmltools::HTML)
    # 
    # labels.PBN.TMA <- sprintf(
    #   "ID: %s<br/>Sector: %s<br/>Time Closest: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Flight Types: %s %s<br/>Routings: %s %s<br/>
    #   Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    #   t4$ID,t4$Sector,t4$Closest_Time,t4$ConflictType,t4$FlightPlanPhase1,t4$FlightPlanPhase2,t4$FlightType1,t4$FlightType2,t4$Routing1, t4$Routing2,
    #   t4$LateralSeparation,t4$ReqLateralSeparation,t4$VerticalSeparation,t4$ReqVerticalSeparation,t4$Altitude_ft) %>% lapply(htmltools::HTML)
    # 
    # if ("Current" %in% scenario & "PBN" %in% scenario) {
    #   proxy %>%
    #     addCircleMarkers(data = d,
    #                      lng=~Longitude,
    #                      lat=~Latitude,
    #                      color="orange",
    #                      fillColor="orange",
    #                      label=labels.current.NonTMA,
    #                      labelOptions=labelOptions(textsize="13px", direction="auto"),
    #                      radius = 5, stroke = TRUE, fillOpacity = 0.8, group="Non-TMA Conflicts") %>%
    #     addCircleMarkers(data = d,
    #                      lng=~Longitude,
    #                      lat=~Latitude,
    #                      color="red",
    #                      fillColor="red",
    #                      label=labels.current.NonTMA,
    #                      labelOptions=labelOptions(textsize="13px", direction="auto"),
    #                      radius = 5, stroke = TRUE, fillOpacity = 0.8, group="TMA Conflicts")
    # }

  })
  
}