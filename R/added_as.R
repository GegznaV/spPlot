# INTERNAL FUNCTION

# added_as ----------------------------------------------------------------

# @author Vilmantas Gegzna
# @family \code{knitr_container} functions

# Function either returns value of  `attributes(obj)$added_as` (if TYPE is not provided)
# OR
# returns updated `obj` with addedd attribute if both  conditions are fulfilled:
# `TYPE` is provided and attribute is `NULL`. otherwise returns `obj` intact.
#
# obj - object to be updated
# TYPE - attribute to  be added as `$added_as` if `$added_as` is not already present.
#
added_as <- function(obj, TYPE = NULL){
    contained_TYPE <- attributes(obj)$added_as

    if (is.null(TYPE))  {
        return(contained_TYPE)

    } else if (is.null(contained_TYPE))   {
        attributes(obj)$added_as <- TYPE
    }
    return(obj)
}






#' setClass("knitr_container")
#'
#' #' @name Merge `knitr_container` objects
#' #'
#' #' @rdname Merge `knitr_container` objects
#' #' @aliases c,knitr_container-method c
#' #' @export
#' setMethod("c",
#'           signature("knitr_container"),
#'           function(...) {
#'               as.knitr_container(c(...))
#'           }
#' )
#'
#' #' @rdname Merge `knitr_container` objects
#' #' @aliases +,knitr_container,knitr_container-method
#' #' @export
#' setMethod("+",
#'           signature(e1 = "knitr_container", e2 = "knitr_container"),
#'           function(e1, e2) {
#'               as.knitr_container(c(e1, e2))
#' })



#  ------------------------------------------------------------------------

#
#         if(inherits(x,"character")){
#             # noquote critical here  also turn off auto.asis very important
#             noquote(paste0(x, collapse="\n")) %>% cat(.)
#
#         } else  if (inherits(x,"htmlwidget")) {
#             # print the html piece of the htmlwidgets
#             htmltools::renderTags(x)$html  %>% cat(.)
#         } else {
#             cat("\n");print(x);cat("\n")
#         }
