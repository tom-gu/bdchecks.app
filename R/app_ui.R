#' @import shiny shinydashboard bdutilities.app golem
app_ui <- function() {
  tagList(
    dashboardPage(
      dashboardHeader(title = "bdchecks"),
      
      dashboardSidebar(
        sidebarMenu(
          id = "sideBar",
          menuItem(
            "Add Data",
            tabName = "add",
            icon = icon("plus-circle")
          ),
          menuItem(
            "Select Data Checks",
            tabName = "configure",
            icon = icon("wrench")
          ),
          menuItem(
            "Perform Data Checks",
            tabName = "clean",
            icon = icon("broom")
          ),
          menuItem("Cite Us",
                   tabName = "cite",
                   icon = icon("copyright"))
        )
      ),
      
      dashboardBody(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        
        tabItems(
          tabItem("add",
                  fluidRow(
                    div(bdutilities.app::mod_add_data_ui("bdFileInput")),
                    
                    bdutilities.app::mod_darwinize_ui("darwinize"),
                    
                    column(12,
                           div(
                             id = "dataToDictionaryDiv",
                             tags$br(),
                             actionButton("dataToDictionary", "Next: Configure Cleaning")
                           ))
                  )),
          
          tabItem("configure",
                  fluidRow(column(
                    12,
                    h1("Select Checks to Perform"),
                    br(),
                    div(mod_configure_checks_ui("bdChecksConfigure")),
                    
                    div(
                      id = "configureToPerformDiv",
                      tags$br(),
                      actionButton("configureToPerform", "Next: Perform Cleaning")
                    )
                  ))),
          
          tabItem("clean",
                  fluidRow(div(
                    mod_perform_checks_ui("bdChecksPerform")
                  ))),
          
          tabItem("cite",
                  fluidRow(
                    div(
                      bdutilities.app::mod_citation_ui("bdcite", "bdchecks.app")
                    )
                  ))
        )
      )
    ),
    tags$script(src = "www/isotope.pkgd.min.js"),
    tags$script(src = "www/script.js")
  )
}

#' @import shiny shinyjs
golem_add_external_resources <- function() {
  addResourcePath('www', system.file('app/www', package = 'bdchecks.app'))
  
  tags$head(
    golem::activate_js(),
    golem::favicon(ico = "www/bdchecks_favicon_4.png"),
    useShinyjs(),
    
    tags$link(rel = "stylesheet", type = "text/css", href = "www/input.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/shuffle.css")
  )
}
