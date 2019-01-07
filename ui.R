source("global.R", local = T)

fillPage(
  
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    #tags$script(src = "script.js")
  ),
  tags$body(
    div(class="header"),
    
    div(class="content",
      uiOutput("display")
    ),
    
    div(class="footer"),
    
    absolutePanel(
      class = "controls",
      div(class="dropdown_box",selectInput("kpa","KPA",choices.kpa,selected="Safety")),
      div(class="dropdown_box",uiOutput("metric")),
      div(class="dropdown_box",uiOutput("options")),
      div(class="check_box",checkboxGroupInput("data","Scenario data to display:",c(choices.operations),selected="Current Runway 1",inline=T)),
      div(class="check_box",uiOutput("moreoptions")),
      div(class="centerwrapper",
          div(class="button",actionButton("view", "View Plot")),
          div(class="button",actionButton("explore", "View Data"))
      ),
      #div(id="import_new_data",actionButton("import", "Import new data")),
      draggable = T,
      fixed = T
    )
  )
  
)