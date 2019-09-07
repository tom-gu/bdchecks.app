#' @import shiny shinydashboard bddwc.app golem
app_ui <- function() {
  dbHeader <- dashboardHeader()
  dbHeader$children[[2]]$children <- tags$img(src='www/bdchecks-logo.png',height='50',width='50')
  
  tagList(
    dashboardPage(
      dbHeader,
    
    dashboardSidebar(sidebarMenu(
      id = "sideBar",
      menuItem(
        "Add Data",
        tabName = "add",
        icon = icon("plus-circle")
      ),
      menuItem(
        "Configure Cleaning",
        tabName = "configure",
        icon = icon("wrench")
      ),
      menuItem(
        "Perform Cleaning",
        tabName = "clean",
        icon = icon("broom")
      ),
      menuItem(
        "Cite Us",
        tabName = "cite",
        icon = icon("copyright")
      )
    )),
    
    dashboardBody(
      # Leave this function for adding external resources
      golem_add_external_resources(),
      
      tabItems(
        # ------------- Add Data Module -------------------
        tabItem("add",
                fluidRow(
                  div(bddwc.app::mod_add_data_ui("bdFileInput")),
                  
                  column(12,
                         div(
                           id = "dataToDictionaryDiv",
                           tags$br(),
                           actionButton("dataToDictionary", "Next: Configure Cleaning")
                         ))
                )),
        
        tabItem("configure",
                fluidRow(
                  column(12,
                         h1("Select Checks to Perform"),
                         br(),
                         div(mod_configure_checks_ui("bdChecksConfigure")),
                         
                         div(
                           id = "configureToPerformDiv",
                           tags$br(),
                           actionButton("configureToPerform", "Next: Perform Cleaning")
                         )
                        )
                  )),
        
        tabItem("clean",
                fluidRow(
                  div(mod_perform_checks_ui("bdChecksPerform"))
                )),
        
        tabItem("cite",
                fluidRow(
                  div(bddwc.app:: mod_citation_ui("bdcite"))
                ))
      )
    )
  ),
  tags$script(src = "www/isotope.pkgd.min.js"),
  tags$script(src="www/script.js"))
}

#' @import shiny shinyjs
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'bdchecks.app')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    useShinyjs(),
    
    tags$link(rel = "stylesheet", type = "text/css", href = "www/input.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/shuffle.css")
  )
}
