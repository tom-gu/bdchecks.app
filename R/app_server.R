#' @import shiny bdutilities.app shinyjs bdutilities
#' @export
app_server <- function(input, output,session) {
    
    session$onSessionEnded(function() {
        stopApp()
    })
    
    #------------- Data --------------
    data_store <-
        shiny::reactiveValues(
            data_user = data.frame(),
            darwinized_data = data.frame(),
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
    
    data_store$darwinized_data <-
        callModule(bdutilities.app::mod_darwinize_server,
                   "darwinize",
                   dat = data_store$data_user)
    
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
        idata <- bdutilities::return_core(data_store$data_user)
        dData <- bdutilities::return_core(data_store$darwinized_data)
        
        if (length(dData) > 0) {
            data_store$data_user <<- dData
            data_store$darwinized_data <<- data.frame()
        } else {
            data_store$data_user <<- idata
        }
        
        if (length(data_store$data_user) == 0) {
            showNotification("Please add data",
                             duration = 6)
        } else {
            updateTabItems(session, "sideBar", "configure")
        }
    })
    
    observeEvent(input$configureToPerform, {
        dat <- data_store$data_checks
        
        if (length(dat()) == 0) {
            showNotification("Please add checks",
                             duration = 6)
        } else {
            updateTabItems(session, "sideBar", "clean")
        }
    })
    
    observeEvent(input$sideBar, {
        if(input$sideBar == "configure" && !shuffled){
            shuffled <<- TRUE
            shinyjs::runjs("$grid.isotope('shuffle');")
        }
    })
    
    
    #------------- Events --------------
}
