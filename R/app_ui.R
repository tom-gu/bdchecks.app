#' @import shiny shinydashboard mod_add_data_ui
app_ui <- function() {
  tagList(
    dashboardPage(
    dashboardHeader(title = "bdchecks"),
    
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
      )
      # menuItem("Darwinize",
      #          tabName = "darwinize",
      #          icon = icon("blocks"))
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
                  div(mod_configure_checks_ui("bdChecksConfigure"))
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
