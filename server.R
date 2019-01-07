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
    table.SectorPolygons <- read.csv("~/Croatia v3.0/data/SectorPolygons.csv", stringsAsFactors = FALSE)
    table.TotalThroughputs <- read.csv("~/Croatia v3.0/data/TotalThroughputs.csv", stringsAsFactors = FALSE)
    table.HourlyThroughputs <- read.csv("~/Croatia v3.0/data/HourlyThroughputs.csv", stringsAsFactors = FALSE)
    table.RollingHourlyThroughputs <- read.csv("~/Croatia v3.0/data/RollingHourlyThroughputs.csv", stringsAsFactors = FALSE)
    table.FuelBurn <- read.csv("~/Croatia v3.0/data/FuelBurn.csv", stringsAsFactors = FALSE)
    table.TrackMiles <- read.csv("~/Croatia v3.0/data/TrackMiles.csv", stringsAsFactors = FALSE)
    table.Conflicts <- read.csv("~/Croatia v3.0/data/Conflicts.csv", stringsAsFactors = FALSE)
    table.ConflictType <- read.csv("~/Croatia v3.0/data/ConflictType.csv", stringsAsFactors = FALSE)
    table.LateralConflictType <- read.csv("~/Croatia v3.0/data/LateralConflictType.csv", stringsAsFactors = FALSE)
    table.VerticalConflictType <- read.csv("~/Croatia v3.0/data/VerticalConflictType.csv", stringsAsFactors = FALSE)
    table.ConflictsFlightPlanPhase <- read.csv("~/Croatia v3.0/data/ConflictsFlightPlanPhase.csv", stringsAsFactors = FALSE)
    table.Severity <- read.csv("~/Croatia v3.0/data/Severity.csv", stringsAsFactors = FALSE)
    table.VerticalSeverity <- read.csv("~/Croatia v3.0/data/VerticalSeverity.csv", stringsAsFactors = FALSE)
    table.SectorOccupancy <- read.csv("~/Croatia v3.0/data/SectorOccupancy.csv", stringsAsFactors = FALSE)
    table.SectorEntry <- read.csv("~/Croatia v3.0/data/SectorEntry.csv", stringsAsFactors = FALSE)
    table.Workload <- read.csv("~/Croatia v3.0/data/Workload.csv", stringsAsFactors = FALSE)
    shinyjs::reset("plot","map")
  })
  
  output$metric <- renderUI({
    x <- input$kpa
    if (x == "Throughput") {
      selectInput("metric","Select metric:",choices.throughputs)
    } else if (x == "Efficiency") {
      selectInput("metric","Select metric:",choices.efficiency)
    } else if (x == "Safety") {
      selectInput("metric","Select metric:",choices.safety)
    } else if (x == "Sector Capacity") {
      selectInput("metric","Select metric:",choices.sectorcapacity)
    }
  })
  
  output$options <- renderUI({
    x <- input$metric
    if (x == "Total Throughput") {
      selectInput("options","Select airport:",list.airports)
    } else if (x == "Hourly Throughput") {
      selectInput("options","Select airport:",list.airports)
    } else if (x == "Rolling Hourly Throughput") {
      selectInput("options","Select airport:",list.airports)
    } else if (x == "Track Miles") {
      selectInput("options","Select grouping:",choices.efficiencygrouping)
    } else if (x == "Fuel Burn") {
      selectInput("options","Select grouping:",choices.efficiencygrouping)
    } else if (x == "Conflict Statistics") {
      selectInput("options","Select grouping:",choices.conflictsgrouping)
    } else if (x == "Sector Occupancy Count") {
      selectInput("options","Select sector:",list.sectors)
    } else if (x == "Sector Entry Count") {
      selectInput("options","Select sector:",list.sectors)
    } else if (x == "Controller Workload") {
      selectInput("options","Select sector:",list.sectors)
    }
  })
  
  output$moreoptions <- renderUI({
    x <- input$metric
    if (x == "Track Miles") {
      selectInput("moreoptions","Select airport:",list.airports)
    } else if (x == "Fuel Burn") {
      selectInput("moreoptions","Select airport:",list.airports)
    } 
  })
  
  output$display <- renderUI({
    if (v$doDisplay == TRUE) {
        if (input$metric == "Conflict Map") {
          div(id="wrap1",leafletOutput("map",height="100%"))
        } else {
          div(id="wrap2",plotlyOutput("plot",height="100%"))
        }
    }
    else if (v$doTable == TRUE) {
      isolate({
        div(id="wrap1",DT::dataTableOutput("table"))
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
    } else if (x == "Conflict Map 3D") {
      plotlyConflict3D(data = c(input$data))
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
    if (input$metric == "Conflict Map") {
      plotConflictMap()
    }
  })
  
  output$data <- renderUI({
    if (input$metric != "Conflict Map") {
      checkboxGroupInput("data","Scenario data to display:",c("Current","PBN"),selected="Current",inline=T)
    }
  })
  
  output$arrange <- renderUI({
    if ("Current" %in% input$data & "PBN" %in% input$data & input$metric %in% c("Total Throughput","Hourly Throughput","Conflict Statistics")) {
      radioButtons("arrange","Plot arrangement:",c("Vertical","Horizontal","Group","Stack"),inline=T)
    } else if ("Current" %in% input$data & "PBN" %in% input$data & input$metric %in% c("Rolling Hourly Throughput","Track Miles","Fuel Burn","Sector Entry Count","Sector Occupancy Count","Controller Workload")) {
      radioButtons("arrange","Plot arrangement:",c("Vertical","Horizontal","Group"),inline=T)
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
  
}