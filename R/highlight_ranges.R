#' Highlight wavelength ranges in a ggplot2 plot of hyperSpec object
#'
#' @param zones formula or list of formulas, that indicate which  zones of
#' wavelength axis should be highlighted e.g., 460~800.
#' @param fill vector of rectangle fill colors to be used for each zone;
#' @param alpha (number from 0 to 1) Level of transperency.
#'  Default is 1 (not transperent).
#'
#' @details
#' \code{highlight_ranges()} Can be added to ggplot object using "+" operator.
#'
#' @export
#'
#' @examples
#' gg <- ggplot(Spectra2) + geom_line()
#'
#' gg + highlight_ranges(400~500)
#'
#' zones <- c(400~460, 545~670)
#' gg + highlight_ranges(zones)

highlight_ranges <- function(zones, fill = RColorBrewer::brewer.pal(9, "Set1")[1:length(zones)], alpha = 0.2 )

{
    zones <- c(zones)
    eval_("list(" %++%
              paste0("spZone(", zones,
                     ", fill = '", fill, "'",
                     ", alpha = ", alpha, ")",
                     collapse = ", ") %++%
              ")" )
}