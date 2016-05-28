
#' [!] Convenience functions to get and set the axis limits in ggplot2 object
#'
#' @param obj A ggplot object.
#' @param which (A string) either "x", "y" (default) or "xy" which
#'        indicates axis if inerest.
#' @param ... For generic use.
#'
#' @return One of the following depending on inputs: \cr
#' a) vector of ranges of \emph{one} axis as
#' \code{(min, max)},
#' b) list of limits for both axes as
#'  \code{list(x = (x min, x max), y = (y min, y max)}\cr
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
#' obj <- qplot(mpg, wt, data = mtcars, geom = c("line","point"))
#'
#' # Get y axis limits ==========================
#' get_ggLims(obj)
#' ggLims(obj)
#' ggLims(obj, "y")
#'
#' ggLims(obj, "x")
#' ggLims(obj, "xy")
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
ggLims.gg <- function(obj, which = "y",...){
    get_ggLims(obj, which)
}

#' @rdname ggLims
#' @export
#' @method ggLims numeric
ggLims.numeric <- function(value, which = "y",...){
    set_ggLims(value, which)
}


#' @rdname ggLims
#' @export
get_ggLims <- function(obj, which = "y",...) {
    x = ggplot_build(obj)$panel$ranges[[1]]$y.range
    y = ggplot_build(obj)$panel$ranges[[1]]$x.range
    switch(tolower(which),
       "y" = x,
       "x" = y,
       "xy"= list(x = x, y = y),
       stop(sprintf("which = %s is not supported.", which))
    )
}

#' @rdname ggLims
#' @param value Values of limits: either vector of \bold{2 values} (min and max)
#' for axis indicated in \code{which} or vector of \bold{4 values}
#' (x min, x max, y min, y max) to be passed to function
#' \code{\link[ggplot2]{coord_cartesian}}. \emph{NOTE}, that this function behaves differently than \code{\link[ggplot2]{lims}}.
#' @export
set_ggLims <- function(value, which = "y",...) {
    if (length(value)  %!in% c(2,4)) stop("`value` must contain either 2 or 4 values.")
    if (length(value)==4) which == "xy"
    switch(tolower(which),
           "y" = coord_cartesian(ylim = value), # scale_y_continuous(limits = value),
           "x" = coord_cartesian(xlim = value), #scale_x_continuous(limits = value),
           "xy" = coord_cartesian(xlim = value[1:2], ylim = value[3:4]),
           stop(sprintf("which = %s is not supported.", which))
    )
}

