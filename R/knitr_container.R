# @param knitr.auto.asis Value for \pkg{pander} parameter \code{knitr.auto.asis}.

#' @name knitr_container
#' @title Create, test and convert to `knitr_container` object
#'
#' @description
#' \code{knitr_container()} - creates a new empty \code{knitr_container} object.
#'
#' \code{is.knitr_container()} - tests if class of an object is object
#'                             \code{knitr_container()}.
#'
#' \code{as.knitr_container()} - converts an object to \code{knitr_container}.
#'  More specifically:\cr
#' for list - only the class attribute is added,\cr
#' for other objects - firstly an object is inctluded into list by
#' \code{list(object)} and then class attribute is added;\cr
#' for \code{knitr_container} objects - they are returned as-is.
#'
#'
#'
#' @export
#'
#' @examples
#'
#' # Find examples in link `knitr_container-class`
#'
#' @author Vilmantas Gegzna
#' @family \code{knitr_container} functions
knitr_container <- function(
    # results = 'asis'
    # , knitr.auto.asis = FALSE
){
    # knitr::opts_chunk$set(results = results)

    container <- list()
    # container <- htmltools::tagList()
    class(container) <- c("knitr_container", setdiff(class(container), "knitr_container"))
    return(container)
}

# is.knitr_container --------------------------------------------------------------------

#' @rdname knitr_container
#' @param obj object to be tested or converted to \code{knitr_container} object.
#'
#' @export
is.knitr_container <- function(obj){
    inherits(obj, "knitr_container")
}

# as.knitr_container --------------------------------------------------------------------
#
#' @rdname knitr_container
#' @param ... arguments to be passed to function \code{knitr_container}.
#' @export
#
as.knitr_container <- function(obj = NULL, ...){
    if (is.null(obj)) obj <- knitr_container(...)

    # If NOT a container and NOT a list
    if (!inherits(obj, c("knitr_container","list"))){
        obj <- added_as(obj, "As is")
        obj <- list(obj)
    }

    # Add class attribute for list
    if (inherits(obj, c("list"))){
        class(obj)  <-  c("knitr_container", setdiff(class(obj), "knitr_container"))
    }

    if (inherits(obj,"knitr_container")) {
        return(obj)
    } else {
        stop(paste("Object of class ",
                   paste(class(obj), collapse = ", "),
                   "can not be converted to class 'knitr_container'."))
    }
}