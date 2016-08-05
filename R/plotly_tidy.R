#' Tidy up `plotly` plot
#'
#' Functions \code{plotly_tidy} and \code{plotly_tidy0} tidy* up \code{plotly}
#' object, usually created using function \code{ggplotly}.\cr
#' Function \code{ggplotly_tidy0} converts \code{ggplot} object to a tity*
#' \code{plotly} plot.\cr
#' These are the convenience functions.\cr
#' * - see section "Details"
#'
#' @details Function \code{plotly_tidy} is a wrapper of following functions
#' with default parameters applied in sequence as follows:\cr
#' \code{\link{label_expr2text}},\cr
#' \code{\link{plotly_modify_legend}},\cr
#' \code{\link{plotly_modify_hover}}\cr\cr
#'
#' \code{plotly_tidy0} additionaly applies \code{\link{plotly_annotation_rm}(...)}
#' after the sequence described above.\cr\cr
#'
#' \code{ggplotly_tidy} additionaly applies \code{\link{ggplotly}(...)}
#' before the sequence described above but does not remove annotations.
#'
#' @template obj-plotly
#' @param ... in \code{plotly_tidy} arguments to be passed to
#'               \code{\link{plotly_annotation_rm}}.\cr
#'          in \code{ggplotly_tidy} arguments to be passed to
#'               \code{\link{plotly_annotation_rm}}.\cr
#'
#' @template plotly-updated
#'
#' @export
#'
#' @seealso  \link[plotly]{ggplotly}(),
#'           \link[plotly]{layout}(),
#'           \link[plotly]{plot_ly}(),
#'           \link[plotly]{subplot}(),
#'           \link[ggplot2]{ggplot}().
#'
#'
#' @author Vilmantas Gegzna
#'
#' @family \pkg{spPlot} functions for \pkg{plotly}
#'
plotly_tidy <- function (obj) {
    obj  %>%
        label_expr2text      %>%
        plotly_modify_legend %>%
        plotly_modify_hover
}

#' @rdname plotly_tidy
#' @export
plotly_tidy0 <- function (obj, ...) {
    obj  %>%
        label_expr2text      %>%
        plotly_modify_legend %>%
        plotly_modify_hover  %>%
        plotly_annotation_rm(...)
}

#' @rdname plotly_tidy
#' @export
ggplotly_tidy <- function (obj, ...) {
    obj  %>%
        ggplotly(...)        %>%
        label_expr2text      %>%
        plotly_modify_legend %>%
        plotly_modify_hover
}
