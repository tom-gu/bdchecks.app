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
    
    #------------- Events --------------
}
