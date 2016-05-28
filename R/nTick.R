#' @name nTick
#' @aliases nTick_x
#' @aliases nTick_y
#'
#' @title [+] Control number of ticks in \code{ggplot2} plots with continuous scale
#'
#' @description Convenience function to control number of ticks in \code{ggplot2}
#'  plots with continuous scale.
#'
#'
#' @param n An Integer giving the desired number of intervals. Non-integer
#'          values are rounded down.
#' @param min.n nonnegative integer giving the minimal number of intervals.
#'        If \code{min.n == 0}, \code{pretty(.)} may return a single value.
#' @param ... Further parameters to be passed to appropriate function
#'   which is either \code{scale_x_continuous} for \code{nTick_x} or
#'   \code{scale_y_continuous} for \code{nTick_y}.
#'
#' @param pretty.args Further parameters to be passed to function \code{pretty}.
#'
#'
#' @export
#' @seealso Pretty breakpoints: \code{\link[base]{pretty}}.
#' @examples
#'
#' library(spPlot)
#' library(ggplot2)
#'
#' # Make a plot but do not print
#' p <- qplot_spStat(Spectra2, "gr", mean, All.linetype = "solid") + facet_grid(.~gr)
#'
#' #Print the plot
#' p
#'
#' # Correct number of ticks:
#' p + nTick_x(2)
#' p + nTick_x(2) + nTick_y(8)
#'

#' @author Vilmantas Gegzna
#' @family \pkg{spPlot} functions for \pkg{ggplot2}
#'

nTick_x <- function(n = 2, min.n = 2, ..., pretty.args = list()){
    pretty.args = modifyList(list(n = n, min.n = min.n), pretty.args)
    scale_x_continuous(breaks = do.call(number_ticks, pretty.args),
                       ...)
}

#' @rdname nTick
#' @export
nTick_y <- function(n = 2, min.n = 2, ..., pretty.args = list()){
    pretty.args = modifyList(list(n = n, min.n = min.n), pretty.args)
    scale_y_continuous(breaks = do.call(number_ticks, pretty.args),
                       ...)
}


#  ------------------------------------------------------------------------
#  [Internal function]
#  Function to to calculate position of ticks
number_ticks <- function(n, min.n = 2, ...) {
    function(limits) pretty(limits, n, min.n = min.n,...)
}
