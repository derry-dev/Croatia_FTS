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
      div(class="dropdown_box",selectInput("kpa","Select KPA:",choices.kpa)),
      div(class="dropdown_box",uiOutput("metric")),
      div(class="dropdown_box",uiOutput("options")),
      div(class="dropdown_box",uiOutput("moreoptions")),
      div(class="check_box",uiOutput("data")),
      div(class="check_box",uiOutput("arrange")),
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