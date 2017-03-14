
#' [!] Convenience functions to get and set the axis limits in ggplot2 object
#'
#' @param obj Eithar a ggplot object (for getting the limits) or a numeric
#'           vector with limits (for setting the limits).
#'            If values of limits: either vector of \bold{2 values} (min and max)
#'            for axis indicated in \code{which} or vector of \bold{4 values}
#'           (x min, x max, y min, y max) to be passed to function
#'           \code{\link[ggplot2]{coord_cartesian}}.
#'           \emph{NOTE}, that this function \code{ggLims} behaves differently
#'           than \code{\link[ggplot2]{lims}}.
#'
#' @param which (A string) either "x", "y" (default) or "xy" which
#'        indicates axis if inerest.\cr
#'        In case of "set_ggLims" following inputs are also possible if a list
#'        should be returned: "xy_" or "xy_list".
#'
#' @param ... For generic use.
#'
#' @return One of the following depending on inputs: \cr
#' a) vector of ranges of \emph{one} axis as
#' \code{(min, max)}, or \emph{both} axes as
#' \code{(x_min, x_max, y_min, y_max)},
#' b) list of limits for both axes as
#'  \code{list(x = (x_min, x_max), y = (y_min, y_max)}\cr
#' c) updated ggplot object
#' @export
#' @details
#' \code{get_ggLims(obj, "y")} is a wrapper for \cr
#' \code{ggplot_build(obj)$panel$ranges[[1]]$y.range} \cr\cr
#'
#' \code{set_ggLims(value, "y")} is a wrapper for \cr
#' \code{coord_cartesian(ylim = value)}
#'
#' @examples
#'
#' library(ggplot2)
#' library(spPlot)
#'
#' obj <- qplot(mpg, wt, data = mtcars, geom = c("line","point"))
#'
#' # Get y axis limits ==========================
#' get_ggLims(obj)
#' ggLims(obj)
#' ggLims(obj, "y")
#'
#' ggLims(obj, "x")
#' ggLims(obj, "xy")  # return as a vector
#' ggLims(obj, "xy_") # return as a list
#'
#'
#' \donttest{
#' \dontrun{
#'
#' # Set y  .axis limits ==========================
#'
#' # Original plot:
#' obj
#'
#' # Settim limmits this way:
#' obj + ggLims(c(0, 5))
#'
#' # is the same as doing it this way:
#' obj + ggLims(c(0, 5),"y")
#'
#' # and this way:
#' obj + coord_cartesian(ylim = c(0, 5))
#'
#' # BUT differs from this way, where some lines are trimmed:
#' obj + ylim(c(0, 5))
#'
#' }}
#'
#' @author Vilmantas Gegzna
#'
#' @seealso \code{\link[ggplot2]{lims}}
#'
#' @family \pkg{spPlot} functions for spectroscopy and \pkg{hyperSpec}
#' @family \pkg{spPlot} functions for \pkg{ggplot2}

ggLims <- function(obj, which = "y", ...){
    UseMethod("ggLims")
}

#' @rdname ggLims
#' @export
#' @method ggLims gg
ggLims.gg <- function(obj, which = "y", ...){
    get_ggLims(obj, which)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname ggLims
#' @export
#' @method ggLims numeric
ggLims.numeric <- function(obj, which = "y", ...){
    set_ggLims(obj, which)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname ggLims
#' @export
get_ggLims <- function(obj, which = "y", ...) {

    if (packageVersion("ggplot2") <= "2.1.0") {
        x = ggplot_build(obj)$panel$ranges[[1]]$y.range
        y = ggplot_build(obj)$panel$ranges[[1]]$x.range
    } else if (packageVersion("ggplot2") <= "2.2.1") {
        x = ggplot_build(obj)$layout$panel_ranges[[1]]$x.range
        y = ggplot_build(obj)$layout$panel_ranges[[1]]$y.range
    } else {
        # Atnaujinti pagal naujesnę ggplot versiją
        x = ggplot_build(obj)$layout$panel_ranges[[1]]$x.range
        y = ggplot_build(obj)$layout$panel_ranges[[1]]$y.range
    }


    switch(tolower(which),
       "y"  = y,
       "x"  = x,
       "xy" = c(x,y),
       "xy_"     = list(x = x, y = y),
       "xy_list" = list(x = x, y = y),
       stop(sprintf("which = %s is not supported.", which))
    )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname ggLims
#'
#' @inheritParams ggplot2::coord_cartesian
#' @export
set_ggLims <- function(obj, which = "y", ..., expand = TRUE) {
    if (!length(obj) %in% c(2, 4))
        stop("`obj` must contain either 2 or 4 values.")

    if (length(obj) == 4) which = "xy"

    switch(tolower(which),
           "y" = coord_cartesian(ylim = obj,
                                 expand = expand),
           # scale_y_continuous(limits = obj),

           "x" = coord_cartesian(xlim = obj,
                                 expand = expand),
           # scale_x_continuous(limits = obj),

           "xy" = coord_cartesian(xlim = obj[1:2],
                                  ylim = obj[3:4],
                                  expand = expand),
           stop(sprintf("which = %s is not supported.", which))
    )
}

