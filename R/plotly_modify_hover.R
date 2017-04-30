#' [!] Modify hover in a \code{plotly} object
#'
#' Function corrects text in a hover of  a \code{plotly} object.
#'
#' @template obj-plotly
#' @param new_text Either \code{NULL}, if the text should not be updated, or
#'        \bold{named list} (or object convertible to named list) with
#'        with replaced and replacement text in such form: \cr
#'       \code{list("OLD_text" = "REPLACEMENT_text",
#'        "OLD_text2" = "REPLACEMENT_text2",)}.\cr
#'         If not \code{NULL}, and not explicitly modified the default
#'         values are:\cr
#'       \code{new_text <- list(
#'       ".wavelength" = "Wavelength",
#'       "spc" = "Y axis value",
#'       ".rownames" = "Row",
#'       ".aggregate" = "Group")}. \cr
#'
#' @param hovermode (enumerated: \code{"closest"} | \code{FALSE})\cr
#'  Source:\url{https://plot.ly/r/reference/#layout-scene-hovermode}.\cr
#'  If \code{NULL} (default) value of this option will not be changed.
#'
#' @param hoverinfo [NOT IMPLEMENTED YET. Option has no effect.] (flaglist string)
#'  Any combination of "x", "y", "z", "text", "name" joined with a "+" OR "all" or "none". \cr
#' Examples: "x", "y", "x+y", "x+y+z", "all".
#' Determines which trace information appear on hover.\cr
#'  Source: \url{https://plot.ly/r/reference/#box-hoverinfo}.\cr
#'  If \code{NULL} (default) value of this option will not be changed.
#'
#' @param hoverformat (string)  [NOT IMPLEMENTED YET. Option has no effect.]
#' Sets the hover text formatting rule for data values on this axis, using
#' the python/d3 number formatting language. See
#' \url{https://github.com/mbostock/d3/wiki/Formatting#numbers} or
#' \url{https://docs.python.org/release/3.1.3/library/string.html#formatspec}
#'  for more info.\cr
#'  Source: \url{https://plot.ly/r/reference/#layout-yaxis-hoverformat}.\cr
#'  If \code{NULL} (default) value of this option will not be changed.
#'
#' @template plotly-updated
#' @export
#'
#' @examples
#' library(spPlot)
#' library(spHelper)
#' library(hyperSpec)
#' library(plotly)
#'
#' # Create `plotly` object:
#' obj <- qplot_spStat(chondro,"clusters",mean)  %>%
#'        label_expr2text() %>%
#'        ggplotly()
#'
#'
#' # Then compare hover in this plot:
#' plotly_modify_hover(obj)
#'
#' # With the hover in the original plot:
#' print(obj)
#'
#' @seealso Online reference
#' \url{https://plot.ly/r/reference/#scatterternary-hoverinfo}; \cr
#' Functions \link[plotly]{layout}(), \link[plotly]{plot_ly}(),
#'
#' @family \pkg{spPlot} functions for \pkg{plotly}
#'
#' @author Vilmantas Gegzna
#'
plotly_modify_hover <- function(obj, new_text = list(),
                                hovermode = NULL,
                                hoverinfo = NULL,
                                hoverformat = NULL)

                                {
    # Convert plotly object to editable object
    obj <- plotly::plotly_build(obj)

    # Length of data list
    LEN <- length(obj$x$data)

    # Remove repeated text in hower and update contents.
    if (!is.null(new_text)){
        if (!is.list(new_text)) new_text <- as.list(new_text)

        # Apply the function
        for (i in 1:LEN) {
            obj$x$data[[i]]$text %<>% update_text(replace_with = new_text)
        }
    }
    return(obj)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(hovermode)){obj$x$layout$hovermode <- hovermode}

    # obj$x$data[[1]]$hoverinfo
    # [1] "text"

    # obj$x$layout$xaxis$hoverformat
    # [1] ".2f"
}



#  ------------------------------------------------------------------------

# obj$data[[1]]$hoverinfo
# [1] "text"
# obj$layout$xaxis$hoverformat
# [1] ".2f"
# obj$layout$hovermode
# [1] "closest")

#  ------------------------------------------------------------------------
# text <- obj$data[[1]]$text
update_text <- function(in_text, replace_with = list()){

    # Choose default values or selected values to update
    replace_with <- modifyList(list(
               ".wavelength"     = "Wavelength",
                    "spc"        = "Y axis value",
                    ".rownames"  = "Row",
                    ".aggregate" = "Group"),
               replace_with)

    # Convert to vector
    replace_with %<>% simplify2array

    # Extract initial and replacement strings
    FROM <- names(replace_with)
    TO   <- replace_with

    # Remove duplicated entries
    # txt <- gsub("(.*)(<br>.+)(<br>)?.*(?:\\2)","\\1\\2", in_text)
    txt <- gsub("(.*)(<br>.+)(<br>).*(?:\\2)","\\1\\2", in_text)

    # Add \\ to treat special symbols as symbols
    FROM <- gsub("([\\^\\$\\{\\}\\[\\]\\(\\)\\.\\*\\+\\?\\<\\>\\&\\|\\\\])",
               "\\\\\\\\\\1", FROM, perl=TRUE)
    # Create pattern
    FROM_as_patern  <- paste0("(<br>)?(",FROM,")(:)")

    # Replace
    eval_(paste0("txt <- gsub('",FROM_as_patern,"','\\\\1",TO,"\\\\3', txt)"))

    # Return results
    return(txt)
}
