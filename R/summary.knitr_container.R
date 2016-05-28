
# Summary --------------------------------------------------------------------

#' @name summary
#' @title Print and summary methods for \code{knitr_container}
#' @description Print summary of \code{knitr_container} object.
#'
#' @template container
#' @param len length of text (number of characters) to be shown in summary.
#' Default is 30.
#' @inheritParams utils::object.size
#'
#' @export
#' @method summary knitr_container
#'
#' @examples
#'
#' # Find examples in link `knitr_container-class`
#'
#' @author Vilmantas Gegzna
#' @family \code{knitr_container} functions
summary.knitr_container <- function(container,len = 30, units = "Kb"){


    if (length(container)==0) {
        cat("*** Empty container ***")

    } else {
        bru(n = 90)
        cat(paste("*** knitr container *** \n\nContains",  length(container),
                  "object(s):\n\n"))

        data.frame(
            "Added as" = sapply(container, function(x){added_as(x) %if.NULL% " "}),
            "Text"     = sapply(container, function(x){
                if (added_as(x)  %in% c("Section","Text")){
                    x <- gsub("[\r\n]", "", x)
                    n <- nchar(x)
                    if (n > len) substr(x , 1, len-3)  %++% "..."
                    else substr(x , 1, len)
                } else {
                    " "
                }}),
            "Size"     = sapply(container, function(x){format(object.size(x), units = "Kb")}),
            "Classes"  = sapply(container, function(x){paste(class(x), collapse=", ")})
        )  %>% print(right = FALSE, quote = FALSE)

        bru(n = 90)
    }
}