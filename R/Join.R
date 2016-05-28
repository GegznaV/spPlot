# Join - merge containers ----------------------------------------------------

#' [!] Merge objects to `knitr_container`
#'
#' \code{Join} converts objects to \code{knitr_container} objects
#' (method "as is") and \bold{merges} them to one container.
#' @param ... objects to be merged.
#' @param list objects to be merged provided as a list.
#' @return knitr_container
#' @export
#'
#' @examples
#'
#' # Find examples in link `knitr_container-class`
#'
#' @author Vilmantas Gegzna
#' @family \code{knitr_container} functions
#'
Join <- function(..., list = NULL) {

    if (is.null(list)) list <- list(...)

    # Convert non `knitr_containers` to `knitr_containers`
    list <- lapply(list, function(obj)
        if (!inherits(obj, "knitr_container")){
            obj <- as.knitr_container(obj)
        } else {
            obj
        }
    )

    # Merge and return the result
    if (length(list) == 1) {
        return(list)
    } else if  (length(list) > 1) {
        return(as.knitr_container(unlist(list,recursive = FALSE)))
    }
}