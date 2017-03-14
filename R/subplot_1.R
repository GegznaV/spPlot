# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#' Wrapper functions for plotly::subplot
#'
#' \code{subplot_1} puts figures in 1 row\cr
#' \code{subplot_2} puts figures in 2 rows\cr
#'
#' @param title (string) the main title of the plot.
#' @param ... inputs to be passed to \code{\link[plotly]{subplot}}.
#' @inheritParams plotly::subplot
#'
#' @details subplot
#' @return \code{plotly} object
#' @export
#'
#' @examples
#'
#' # ----- no examples yet -----
#'
subplot_1 <- function(..., title = "Spectra and their derivatives",
                      titleX = T, titleY = T, shareY = T,
                      nrows = 1) {
    plotly::subplot(...,
                    titleX = titleX,
                    titleY = titleY,
                    shareY = shareY,
                    nrows  = nrows) %>%
        plotly_tidy0(title = title)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# subplot_2: 2 rows
#' @rdname subplot_1
#' @export
subplot_2 <- function(..., title = "Spectra and their derivatives",
                      titleX = T, titleY = T, shareX = T, shareY = T,
                      nrows = 2 ){
    plotly::subplot(...,
                    titleX = titleX,
                    titleY = titleY,
                    shareX = shareX,
                    shareY = shareY,
                    nrows  = nrows) %>%
        plotly_tidy0(title = title)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~