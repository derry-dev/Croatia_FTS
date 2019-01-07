source("functions.R", local = T)
function(input, output, session){
  
  output$options <- renderUI({
    if (input$kpa == "Throughput") {
      tagList(
        div(style="display:inline-block;vertical-align:top; width:200px;",
            selectInput("metric1",
                        label = "Select metric:",
                        choices = c("Total Throughputs","Hourly Throughputs", "Rolling Hourly Throughputs"))),
        div(style="display:inline-block; vertical-align:top; width:auto;",
            uiOutput("options1")),
        div(style="vertical-align:top; position:absolute; top:0; left:0; right:0; padding-top:100px; padding-left:10px; padding-right:10px; padding-bottom:10px; width:100%; height:100%;",
            plotlyOutput("plot", height="100%"))
      )
    } else if (input$kpa == "Safety") {
      tagList(
        div(style="display:inline-block;vertical-align:top; width:200px;",
            selectInput("metric3",
                        label = "Select metric:",
                        choices = c("Conflict Map", "Conflict Map 3D","Conflict Statistics"))),
        div(style="display:inline-block; vertical-align:top; width:auto;",
            uiOutput("options3"))
      )
    } else if (input$kpa == "Sector Capacity") {
      tagList(
        div(style="display:inline-block;vertical-align:top; width:200px;",
            selectInput("metric4",
                        label = "Select metric:",
                        choices = c("Sector Entry Count","Sector Occupancy Count"))),
        div(style="vertical-align:top; position:absolute; top:0; left:0; right:0; padding-top:100px; padding-left:10px; padding-right:10px; padding-bottom:10px; width:100%; height:100%;",
            plotlyOutput("plot", height="100%"))
      )
    }
  })
  
  output$options1 <- renderUI({
    if (input$metric1 == "Total Throughputs") {
      isolate({})
    } else if (input$metric1 != "Total Throughputs") {
      div(style="display:inline-block;vertical-align:top; width:200px;",
          selectInput("airport1",
                      label = "Select airport:",
                      choices = Airport.list))
    }
  })
  
  output$options3 <- renderUI({
    if (input$metric3 == "Conflict Statistics") {
      tagList(
        div(style="display:inline-block;vertical-align:top; width:200px;",
            selectInput("group3",
                        label = "Select grouping:",
                        choices = c("Conflict Type","Conflict Type (Lateral)","Conflict Type (Vertical)","Flight Phase","Severity","Severity (Vertical)"))),
        div(style="vertical-align:top; position:absolute; top:0; left:0; right:0; padding-top:100px; padding-left:10px; padding-right:10px; padding-bottom:10px; width:100%; height:100%;",
            plotlyOutput("plot", height="100%"))
      )
    } else if (input$metric3 == "Conflict Map") {
      div(style="vertical-align:top; position:absolute; top:0; left:0; right:0; padding-top:100px; padding-left:10px; padding-right:10px; padding-bottom:10px; width:100%; height:100%;",
          leafletOutput("map", height="100%"))
    } else if (input$metric3 == "Conflict Map 3D") {
      div(style="vertical-align:top; position:absolute; top:0; left:0; right:0; padding-top:100px; padding-left:10px; padding-right:10px; padding-bottom:10px; width:100%; height:100%;",
          plotlyOutput("plot", height="100%"))
    }
  })
  
  output$map <- renderLeaflet({
    if (input$kpa == "Safety") {
      if (input$metric3 == "Conflict Map") {
        plotConflictMap()
      }
    }
  })
  
  output$plot <- renderPlotly({
    if (input$kpa == "Throughput") {
      if (input$metric1 == "Total Throughputs") {
        plotlyTotalThroughput()
      } else if (input$metric1 == "Hourly Throughputs") {
        plotlyHourlyThroughput(airport = input$airport1)
      } else if (input$metric1 == "Rolling Hourly Throughputs") {
        plotlyRollingHourlyThroughput(airport = input$airport1)
      }
    } else if (input$kpa == "Efficiency") {
      
    } else if (input$kpa == "Safety") {
      if (input$metric3 == "Conflict Statistics") {
        plotlyConflictCount(group = input$group3)
      } else if (input$metric3 == "Conflict Map 3D") {
        plotlyConflict3D()
      }
    } else if (input$kpa == "Sector Capacity") {
      if (input$metric4 == "Sector Occupancy Count") {
        plotlySectorOccupancy()
      } else if (input$metric4 == "Sector Entry Count") {
        plotlySectorEntry()
      }
    }
  })
}