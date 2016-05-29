#' @title Add object as a heading/section name to \code{knitr_container}
#'
#' @description
#' \code{add_as_section()} converts \code{obj} to \code{\link[base]{character}},
#'  formats it as a \bold{heading of a section} and includes it in the
#'  \code{container}. Function \code{print_objects} will print the object as text.
#'
#' @template container
#' @template obj
#' @param level The level of heading/section to be added. Default is 1
#'        (top level section).
#'
#'
#' @details
#'   All the remaining functions listed here are wrappers of
#'   \code{add_as_section}  with different value of parameter
#'   \code{level} passed to \code{add_as_section}:\cr
#'
#'  In \code{add_as_heading1()} the default value of  \code{level = 1}.\cr
#'
#'  In \code{add_as_subsection()} the default value of  \code{level = 2}.\cr
#'  In \code{add_as_heading2()}  the default value of  \code{level = 2}.\cr
#'
#'  In \code{add_as_heading3()} the default value of  \code{level = 3}.\cr
#'
#'  In \code{add_as_heading4()} the default value of  \code{level = 4}.\cr
#'
#'
#' @rdname add_as_section
#' @export
#'
#' @examples
#' # Find more examples in link `knitr_container-class`
#'
#'
#' # Initialize container
#' container <- knitr_container()
#'
#' # Fill the container
#'
#' container %<>% add_as_section(container, obj = "Heading 1")
#' container %<>% add_as_subsection(container, obj = "Heading 2")
#'
#' container %<>% add_as_header1(container, obj = "Heading 1")
#' container %<>% add_as_header2(container, obj = "Heading 2")
#' container %<>% add_as_header3(container, obj = "Heading 3")
#' container %<>% add_as_header4(container, obj = "Heading 4")
#'
#' container %<>% add_as_section(container, obj = "Heading 5", level = 5)
#' container %<>% add_as_section(container, obj = "Heading 6", level = 6)
#'
#' # Extract and print the contents of the container
#' print_objects(container)
#'
#' @author Vilmantas Gegzna
#' @family \code{knitr_container} functions
add_as_section <- function(container = NULL, obj = "", level = 1){
    container <- as.knitr_container(container)

    obj <- sprintf("\n\n%s %s\n\n  ",
                   paste0(rep("#",level), collapse = ""), #line adds required number of symbols (#)
                   as.character(obj))
    obj <- added_as(obj, "Section")
    container <- add_as_is(container, obj)
    return(container)
}
#' @rdname add_as_section
#' @export
add_as_subsection <- function(container = NULL, obj = ""){
    add_as_section(container = container, obj = obj, level = 2)
}
#' @rdname add_as_section
#' @export
add_as_heading1 <- function(container = NULL, obj = ""){
    add_as_section(container = container, obj = obj, level = 1)
}
#' @rdname add_as_section
#' @export
add_as_heading2 <- function(container = NULL, obj = ""){
    add_as_section(container = container, obj = obj, level = 2)
}
#' @rdname add_as_section
#' @export
add_as_heading3 <- function(container = NULL, obj = ""){
    add_as_section(container = container, obj = obj, level = 3)
}
#' @rdname add_as_section
#' @export
add_as_heading4 <- function(container = NULL, obj = ""){
    add_as_section(container = container, obj = obj, level = 4)
}

#  ------------------------------------------------------------------------
