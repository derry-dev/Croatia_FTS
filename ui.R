fillPage(
  
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js")
  ),
  
  tags$body(
    #div(class="header"),
    div(class="content",
        uiOutput("mapdisplay"),
        hidden(
          uiOutput("plotdisplay"),
          uiOutput("tabledisplay")
        )
    ),
    #div(class="footer"),
    absolutePanel(
      class="controlpanel",
      div(class="dropdown_box",
          selectInput("kpa","KPA",c("Throughput","Efficiency","Safety","Sector Capacity"),selected="Safety"),
          uiOutput("metric"),
          uiOutput("options")
          #selectInput("run", "Run", as.character(seq(1,10,1)))
      ),
      uiOutput("operation"),
      radioButtons("runway","Runway Scenario",c("1","2"),inline=T),
      uiOutput("arrange"),
      actionButton("view", "View Plot"),
      actionButton("explore", "View Data"),
      draggable=T,
      fixed=T
    )
  )
  
)