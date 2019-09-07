#' @export 
#' @import bdchecks
get_dc_groups <- function(group){
    dc_group <- lapply(bdchecks:::data.checks@dc_body, function(x) {
        if (group == "DarwinCoreClass") {
            data.frame(
                DC = x@name,
                group = gsub(" ", "_", x@description$DarwinCoreClass),
                groupName = gsub("_", " ", x@description$DarwinCoreClass)
            )
        } else {
            data.frame(
                DC = x@name,
                group = gsub(" ", "_", x@flags[[group]]),
                groupName = gsub("_", " ", x@flags[[group]])
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