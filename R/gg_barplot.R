#' A barplot with counts above bars.
#'
#' A simple `ggplot2` barplot with counts above bars.
#'
#' @param data A data frame.
#' @param x (string) A name of factor variable in the data frame.
#' @param alpha (number from 0 to 1) Level of transperency.
#'  Default is 1 (not transperent).
#' @param color (string) Edge color of bars.
#' @param drop_NA (logical) Should NA values be dropped?
#'  Default is \code{FALSE}.
#' @param ... (not used yet)
#'
#' @export
#' @examples
#' #' library(spPlot)
#'
#' #`data` is a data frame:
#' gg_barplot("cyl", mtcars)
#'
#'
#' # data` is a `hyperSpec` object:
#' gg_barplot("class", Spectra2)
#'
gg_barplot <-
    function(x,
             data,
             alpha = 1,
             color = "black",
             drop_NA = FALSE,
             ...) {
        UseMethod("gg_barplot", data)
    }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname gg_barplot
#' @export
#' @method gg_barplot hyperSpec
gg_barplot.hyperSpec <-
    function(x,
             data,
             alpha = 1,
             color = "black",
             drop_NA = FALSE,
             ...)
    {
        gg_barplot.default(x, data$..,  alpha, ...) +
            labs(x = labels(data, x))
    }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname gg_barplot
#' @export
#' @method gg_barplot default
gg_barplot.default <- function(x, data, alpha = 1, color = "black", drop_NA = FALSE, ...) {

    # Convert to factor
    data[[x]]  %<>%  as.factor()

    # Make a plot
    gg <- ggplot(data, aes_string(x)) +
        geom_bar(
            stat = "count",
            aes_string(fill = x),
            color = color,
            alpha = alpha
        ) +
        geom_text(stat = 'count',
                  aes(label = ..count..),
                  vjust = -.3) +
        scale_x_discrete(drop = drop_NA) +
        scale_fill_discrete(drop = drop_NA)  # + x30()

    # Correct the limits
    # LIMS <- c(ggLims(gg, "x"), 0, ggLims(gg)[2])
    LIMS <- c(0, ggLims(gg)[2])
    gg <- gg + ggLims(LIMS)

    # Return the result
    return(gg)
}