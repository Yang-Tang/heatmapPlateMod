# shiny module of heatmap plate
# This module is used in a shiny app for plotting data as a heatmap in microplate layout
# Usage:
# 1. source this file in app.R or server.R
# 2. add hmplateUI('MODULE_ID') in ui
# 3. add callModule(hmplate, id = 'MODULE_ID', data, nrow, ncol)
#    in server, where:
#    data: a df with at least Well column and Value column
#    nrow, ncol: microplate type (eg: nrow = 8, ncol = 12 for 96well plate)


library(magrittr)
library(dplyr)
library(tidyr)
library(highcharter)


hmplate_formatData <- function(df, nrow, ncol) {

  stopifnot('Well' %in% names(df))
  stopifnot('Value' %in% names(df))

  if(!'Name' %in% names(df)) df %<>% mutate(Name = round(Value, 2))

  row_names <- LETTERS[1:nrow]
  col_names <- as.character(1:ncol)
  full_plate <- expand.grid(Row = row_names, Col = col_names) %>%
    unite(Well, Row, Col, sep = '', remove = F)

  full_plate %>%
    left_join(df, by = 'Well') %>%
    rename(value = Value, name = Name) %>%
    mutate(Row = factor(Row, levels = row_names),
           Col = factor(Col, levels = col_names),
           x = Col %>% as.integer() %>% subtract(1),
           y = Row %>% as.integer() %>% subtract(1)) %>%
    select(x, y, value, name, Well, Row, Col)

}

hmplate_buildHeatmap <- function(data, id) {

  tooltip_format <-"function(){return '<span style=\"color: ' +
    this.color + '\">\u25CF</span> ' +
    this.Well + ': <b>' + this.name + '</b><br/>';}"

  highchart() %>%
    hc_xAxis(categories = data$Col %>% levels(),
             title = list(), opposite = T, tickLength = 0) %>%
    hc_yAxis(categories = data$Row %>% levels(),
             title = list(), reversed = T) %>%
    hc_add_series(data = list_parse(data),
                  type = 'heatmap',
                  dataLabels = list(enabled = T, format = '{point.name}',
                                    color = 'black'),
                  states = list(
                    hover = list(color = "#023858"),
                    select = list(color = "yellow")
                  ),
                  tooltip = list(headerFormat = '', pointFormatter = JS(tooltip_format)),
                  events = list(
                    click = JS(sprintf("
                    function(event){
                      var chart=$('#%s').highcharts();
                      if (event.ctrlKey) {
                        event.point.select(null,true);
                      } else if (event.shiftKey){
                        var selectedPoints = chart.getSelectedPoints();
                        var lastPoint = selectedPoints[selectedPoints.length - 1];
                        var x_min = Math.min(lastPoint.x, event.point.x);
                        var x_max = Math.max(lastPoint.x, event.point.x);
                        var y_min = Math.min(lastPoint.y, event.point.y);
                        var y_max = Math.max(lastPoint.y, event.point.y);
                        var points = chart.series[0].data;
                        $.each(points, function (i,p) {
                          if (p.x >= x_min && p.x <= x_max && p.y >= y_min && p.y <= y_max) {
                            p.select(true, true);
                          }
                        });
                      } else {
                        event.point.select(null,false);
                      }
                      var selectedPoints = chart.getSelectedPoints();
                      var selectedWells = selectedPoints.map(function(a) {return a.Well;});
                      Shiny.onInputChange('%s' + '_selected', selectedWells);
                    }
                  ", id, id))
                  )) %>%
    hc_colorAxis(minColor = '#ece7f2', maxColor = '#045a8d')

}

hmplateUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(sprintf("
      Shiny.addCustomMessageHandler('hmplate_clearSelectedCallbackHandler',
        function(dummy){Shiny.onInputChange('%s' + '_selected', null);}
      )
    ", ns('hmplate'))),

    highchartOutput(ns('hmplate'))
  )
}

hmplate <- function(input, output, session,
                    data, nrow = 8, ncol = 12) {

  output$hmplate <- renderHighchart({
    validate(need(data(), ''))

    data() %>%
      hmplate_formatData(nrow, ncol) %>%
      hmplate_buildHeatmap(session$ns('hmplate'))
  })

  observeEvent(data(), {
    session$sendCustomMessage(type = "hmplate_clearSelectedCallbackHandler", '')
  })

  selected_data <- reactive({

    if(is.null(input$hmplate_selected)) return(NULL)

    isolate({

      out <- data() %>%
        filter(Well %in% input$hmplate_selected)

      if(nrow(out) == 0) return(NULL)

      return(out)
    })
  })

  return(selected_data)

}
