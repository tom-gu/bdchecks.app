#' @import shiny bddwc.app
app_server <- function(input, output,session) {
    
    #------------- Data --------------
    data_store <-
        shiny::reactiveValues(
            data_user = data.frame()
        )
    #------------- Data --------------
    
    
    #------------- Modules --------------
    data_store$data_user <-
        callModule(
            bddwc.app::mod_add_data_server,
            id = "bdFileInput"
        )
    
    callModule(
        mod_configure_checks_server,
        id = "bdChecksConfigure"
    )
    
    callModule(
        mod_perform_checks_server,
        id = "bdChecksPerform"
    )
    
    callModule(
        bddwc.app::mod_citation_server,
        id = "bdcite"
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
    
    observeEvent(input$sideBar, {
        if(input$sideBar == "configure"){
            shinyjs::runjs("$grid.isotope('shuffle');")
        }
       
        
        
    })
    
    
    #------------- Events --------------
}
