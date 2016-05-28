# print ------------------------------------------------------------------------

#' @name print
#' @title Print and summary methods for \code{knitr_container}
#' @description Print summary of \code{knitr_container} object.
#'
#' @template container
#' @param len length of text (number of characters) to be shown in summary.
#' Default is 25.
#' @inheritParams utils::object.size
#'
#' @export
#' @method print knitr_container
#'
#' @examples
#'
#' # Find examples in link `knitr_container-class`
#'
#' @author Vilmantas Gegzna
#' @family \code{knitr_container} functions
#'
print.knitr_container <- function(container, len = 25, units = "Kb"){
    summary(container, len = len, units = units)
}
