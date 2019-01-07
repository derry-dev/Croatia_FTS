source("functions.R", local = T)
fluidPage(
  br(),
  div(style="display:inline-block; vertical-align:top; position:relative; top:5; width:200px;",
      selectInput("kpa",
                  label = "Select KPA:",
                  choices = c("Throughput", "Safety", "Sector Capacity"))),
  div(style="display:inline-block; vertical-align:top; width:auto;",
      uiOutput("options")),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
)