# Module UI

#' @title   mod_perform_checks_ui and mod_perform_checks_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_perform_checks
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_perform_checks_ui <- function(id) {
  ns <- NS(id)
  tagList(column(12,
                 div(
                   h1("Perform Cleaning"),
          
                   column(
                     12,
                     fluidRow(column(4,
                                     h4("Records Submitted", class = "control-header"),
                                     div(class = "control-header stats-green", textOutput(ns("total")))),
                              column(4,
                                     h4("Data Checks Performed", class = "control-header"),
                                     div(class = "control-header stats-green", textOutput(ns("checks")))),
                              column(4,
                                     h4("Records After Filtering", class = "control-header"),
                                     div(class = "control-header stats-green", textOutput(ns("filtered"))))),
                     
                     fluidRow(
                       column(
                         12,
                         actionButton(ns("perform"), "Perform Data Checks", width = "14%"),
                         actionButton("a", "Select All Passed Records", width = "14%"),
                         actionButton("b", "Select All Failed Records", width = "14%"),
                         actionButton("c", "Select All Missing Records", width = "14%"),
                         actionButton("d", "Clear Selections", width = "14%"),
                         actionButton("e", "Accept Selections", width = "14%"),
                         actionButton("f", "Download Final Data", width = "14%")
                       )
                       
                     ),
                     
                     br(),
                     
                     column(
                       12,
                       DT::dataTableOutput(ns("summaryTable"))
                     )
                   )
                   
                 )))
}

# Module Server

#' @rdname mod_perform_checks
#' @export
#' @keywords internal

mod_perform_checks_server <- function(input, output, session) {
  ns <- session$ns
  
  summary <- data.frame()
  
  observeEvent(input$perform, {
    checks <-
      bdchecks::perform_dc(
        bdchecks::data_bats,
        c(
          "elevationOutOfRange",
          "eventDateInFuture",
          "monthInvalid",
          "occurrenceIdNotGuid",
          "dateIdentifiedInFuture",
          "dataGeneralised",
          "countryMismatch",
          "dateNull",
          "coordinatesZero"
        )
      )
    
    summary <<-
      bdchecks::summary_dc(checks, fancy = FALSE, filtering_dt = TRUE)
  })
  
  output$summaryTable <- DT::renderDataTable(
    DT::datatable({
      input$perform
      summary
    },
    rownames = FALSE,
    options = list(pageLength = 100,
                   columnDefs = list(
                     list(className = "no_select",
                          targets = 0:1)
                   )),
    selection = list(target = "cell"),
    callback = DT::JS(
      "table.on('click', 'td.no_select', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if ($(td.node()).hasClass('selected')) {
      $(td.node()).removeClass('selected');
    }});"
    ))
  )
  
  output$filtered <-
    renderText({
      69
    })
  
  output$total <-
    renderText({
      128
    })
  
  output$checks <-
    renderText({
      14
    })
}

## To be copied in the UI
# mod_perform_checks_ui("perform_checks_ui_1")

## To be copied in the server
# callModule(mod_perform_checks_server, "perform_checks_ui_1")
