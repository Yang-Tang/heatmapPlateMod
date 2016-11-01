# A demo shiny app of heatmapPlateMod

source('modules/hmplateMod.R')
library(shiny)

server <- function(input, output) {

  microplate <- reactive({
      expand.grid(Row = LETTERS[1:8], Col = as.character(1:12),
                  stringsAsFactors = F) %>%
      unite(Well, Row, Col, sep = '') %>%
      mutate(Value = runif(n())) %>%
      sample_frac(0.9)
    })

  selected <- callModule(hmplate, id = 'test', data = microplate)

  output$out <- renderPrint({
    selected()
  })

}

ui <- fluidPage(

  hmplateUI('test'),
  verbatimTextOutput('out')

)

shinyApp(ui, server)
