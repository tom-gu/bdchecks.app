#' @import shiny bdutilities.app shinyjs
app_server <- function(input, output,session) {
    
    #------------- Data --------------
    data_store <-
        shiny::reactiveValues(
            data_user = data.frame(),
            data_checks = character()
        )
    
    shuffled <- FALSE
    #------------- Data --------------
    
    
    #------------- Modules --------------
    data_store$data_user <-
        callModule(
            bdutilities.app::mod_add_data_server,
            id = "bdFileInput"
        )
    
    data_store$data_checks <- callModule(
        mod_configure_checks_server,
        id = "bdChecksConfigure"
    )
    
    callModule(
        mod_perform_checks_server,
        id = "bdChecksPerform",
        data_store$data_user,
        data_store$data_checks
    )
    
    callModule(
        bdutilities.app::mod_citation_server,
        id = "bdcite",
        package = "bdchecks.app"
    )
    
    #------------- Modules --------------
    
    
    #------------- Events --------------
    observeEvent(input$dataToDictionary, {
        dat <- data_store$data_user
        
        if (length(dat()) == 0) {
            showNotification("Please add data",
                             duration = 6)
        } else {
            updateTabItems(session, "sideBar", "configure")
        }
    })
    
    observeEvent(input$configureToPerform, {
        
        
        updateTabItems(session, "sideBar", "clean")
    })
    
    observeEvent(input$sideBar, {
        if(input$sideBar == "configure" && !shuffled){
            shuffled <<- TRUE
            shinyjs::runjs("$grid.isotope('shuffle');")
        }
        
    })
    
    
    #------------- Events --------------
}
