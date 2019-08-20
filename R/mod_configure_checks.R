# Module UI

#' @title   mod_configure_checks_ui and mod_configure_checks_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_configure_checks
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_configure_checks_ui <- function(id) {
  ns <- NS(id)
  
  darwinCoreClass <- get_dc_groups("DarwinCoreClass")
  dimension <- get_dc_groups("Dimension")
  warning <- get_dc_groups("Warning")
  output <- get_dc_groups("Output")
  severity <- get_dc_groups("Severity")
  
  
  
  components <- list()
  
  for (check in bdchecks::data.checks@dc_body) {
    components[[length(components) + 1]] <-
      div(
        class = "element-item checksListContent",
        "darwinCoreClass" = darwinCoreClass[check@name, ]$group,
        "dimension" = dimension[check@name, ]$group,
        "warning" = warning[check@name, ]$group,
        "output" = output[check@name, ]$group,
        "severity" = severity[check@name, ]$group,
        
        HTML(
          paste(
            "<input type=checkbox name=typeInput value=",
            check@name,
            ">"
          )
        ),
        
        h4(check@name),
        
        conditionalPanel(
          "input['bdChecksConfigure-showDetailed'] == false",
          div(
            fluidRow(
              div(class = "checksListTopic col-sm-4", p("Description: ")),
              div(class = "checksListTitle col-sm-8",
                  p(check@description$Main))
            ),
            
            fluidRow(
              div(class = "checksListTopic col-sm-4", p("Sample Passing Data: ")),
              div(class = "checksListTitle col-sm-8",
                  p(check@description$Example$Pass))
            ),
            
            fluidRow(
              div(class = "checksListTopic col-sm-4", p("Sample Failing Data: ")),
              div(class = "checksListTitle col-sm-8",
                  p(check@description$Example$Fail))
            ),
            
            fluidRow(
              div(class = "checksListTopic col-sm-4", p("Category of Check: ")),
              div(class = "checksListTitle col-sm-8",
                  p(check@description$DarwinCoreClass))
            ),
            
            fluidRow(
              div(class = "checksListTopic col-sm-4", p("DWC Field Targetted: ")),
              div(class = "checksListTitle col-sm-8",
                  p(check@input$Target))
            ),
            
            fluidRow(
              div(class = "checksListTopic col-sm-4", p("Sorting Flags: ")),
              div(class = "checksListTitle col-sm-8",
                  p(check@flags))
            )
          )
        )
        
        
      )
    
  }
  
  tagList(column(
    12,
    
    fluidRow(column(
      5,
      div(
        p("Sort Checks By: "),
        class = "btn-group btn-group-justified",
        "role" = "group",
        actionButton(
          "a",
          class = "button is-checked ",
          label = "Darwin Core Class",
          "data-sort-value" = "darwinCoreClass"
        ),
        actionButton(
          "b",
          class = "button",
          label = "Dimension",
          "data-sort-value" = "dimension"
        ),
        actionButton(
          "c",
          class = "button",
          label = "Warning Type",
          "data-sort-value" = "warning"
        ),
        actionButton(
          "d",
          class = "button",
          label = "Output Type",
          "data-sort-value" = "output"
        ),
        actionButton(
          "e",
          class = "button",
          label = "Severity",
          "data-sort-value" = "severity"
        )
      )
    ),
    column(
      6,
      column(
        3,
        p("Show Stacked View: "),
        checkboxInput(ns("showDetailed"), label = "", value = FALSE)
      )
      ,
      column(
        2,
        p("Select All: "),
        checkboxInput(ns("showDetailed"), label = "", value = FALSE)
      )
      ,
      column(
        2,
        p("Select None: "),
        checkboxInput(ns("showDetailed"), label = "", value = FALSE)
      ),
      column(
        4,
        p("Next:"),
        actionButton("configureToPerform", "Perform Cleaning")
      )
      
      
      
    )),
    
    div(class = "grid", components)
    
    # actionButton("a", class = "button is-checked", label = "Darwin Core Class", "data-filter" = ".DarwinCoreClass .Event"),
    # actionButton("b", class = "button", label = "Dimension", "data-filter" = ".Dimension .Location"),
    # actionButton("c", class = "button", label = "Warning Type", "data-filter" = ".Warning .Record"),
    # actionButton("d", class = "button", label = "Output Type", "data-filter" = ".Output .Record_level_Terms"),
    # actionButton("e", class = "button", label = "Severity", "data-filter" = ".Severity .Taxon_Occurrence")),
    
    
    
  ))
}

# Module Server

#' @rdname mod_configure_checks
#' @export
#' @import shinyjs
#' @keywords internal
mod_configure_checks_server <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$showDetailed, {
    shinyjs::runjs("$grid.isotope( 'reloadItems' ).isotope();")
    
  })
  
}

## To be copied in the UI
# mod_configure_checks_ui("configure_checks_ui_1")

## To be copied in the server
# callModule(mod_configure_checks_server, "configure_checks_ui_1")
