#' Tidy up `plotly` plot
#'
#' Tidy up \code{plotly} object, created from \code{ggplot} object.
#'
#' @details This function is a wrapper of following functions with default
#'  parameters applied in sequence as follows:
#' \code{\link{label_expr2text}},
#' \code{\link{plotly_modify_legend}},
#' \code{\link{plotly_modify_hover}},
#' \code{\link{plotly_annotation_rm}}.
#'
#' @template obj-plotly
#' @param ... arguments to be passed to \code{\link{plotly_annotation_rm}}.
#'
#' @template plotly-updated
#'
#' @export
#'
#' @seealso  \link[plotly]{layout}(),
#'           \link[plotly]{plot_ly}(),
#'           \link[plotly]{subplot}().
#'
#'
#' @author Vilmantas Gegzna
#'
#' @family \pkg{spPlot} functions for \pkg{plotly}
#'
plotly_tidy <- function (obj, ...) {
    obj  %>%
        label_expr2text      %>%
        plotly_modify_legend %>%
        plotly_modify_hover  %>%
        plotly_annotation_rm(...)
}
