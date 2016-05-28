# print_objects ----------------------------------------------------------------
# @param results Value for \code{knitr} chunk parameter \code{results}.
#  Default option is to set \code{results} to \code{asis}.


#' [!] Print objects from \code{knitr_container} in R Markdown / \code{knitr} file
#'
#'  Function takes every object from the \code{container}, prints it
#'  and attaches \code{html} dependencies to show objects
#'  (such as plotly htmlwidgets) correctly.
#'
#' @template container
#' @export
#'
#' @examples
#'
#' # Find examples in link `knitr_container-class`
#'
#' @author Vilmantas Gegzna
#' @family \code{knitr_container} functions
#'
print_objects <- function(container
                          # , results = "asis"
                          ){
    # knitr::opts_chunk$set(results = results)

    if (length(container)==0) {
        stop("knitr_container is empty!!!")
        # cat("*** Empty container ***")
        # return(NULL)
    }

    # now cat/print all of the content
    for(j in 1:length(container)){
        x <- container[[j]]

        #  we know only character and htmlwidget in this case
        #   if more need to handle appropriately

        if (inherits(x,"character") & added_as(x)!= "As is"){
            # noquote critical here  also turn off auto.asis very important
            noquote(paste0(x, collapse="\n")) %>% cat
            # %>% knitr::asis_output(.)
            #

        } else  if (inherits(x,"htmlwidget")) {
            # print the html piece of the htmlwidgets
            htmltools::renderTags(x)$html %>% cat
            # %>% knitr::asis_output(.)
        } else {
            cat("\n")
            print(x)
            cat("\n")
        }

    }
    # Attach the Dependencies
    # since they do not get included with renderTags(...)$html

    # List dependencies
    widget_obj <- Filter(function(x){inherits(x,'htmlwidget')},container)
    list_dependencies <- function(hw){htmltools::renderTags(hw)$dependencies}
    deps <- lapply(widget_obj, list_dependencies)

    # Attach dependencies
    htmltools::attachDependencies(
        htmltools::tagList(),
        unlist(deps,recursive=FALSE))


}
