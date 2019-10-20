#' @export 
#' @import bdchecks
get_dc_groups <- function(group){
    dc_group <- lapply(bdchecks:::data.checks@dc_body, function(x) {
        if (group == "DarwinCoreClass") {
            data.frame(
                DC = x@name,
                group = gsub(" ", "_", x@information$darwin_core_class),
                groupName = gsub("_", " ", x@information$darwin_core_class)
            )
        } else {
            data.frame(
                DC = x@name,
                group = gsub(" ", "_", x@information[[group]]),
                groupName = gsub("_", " ", x@information[[group]])
            )
        }
    })
    do.call(rbind, dc_group)
}


#' @export 
#' @import bdchecks
get_dc_ids <- function(){
    return(names(bdchecks::data.checks@dc_body))
}