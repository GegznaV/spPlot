#' [!.] Add color palette to `ggplot`
#'
#' @details
#' \code{gg_palette} adds color palette to \code{ggplot} object. It wraps two
#' functions \code{\link[ggplot2]{scale_color_manual}} and
#' \code{\link[ggplot2]{scale_fill_manual}}.
#'
#' \code{palette2vec} converts dataframe with variables \code{$colors}
#' and \code{$labels} to a named vector, were \code{$labels} are the names and
#' \code{$colors} are the values.The data frame is usually generated with function
#' \code{\link{hyGet_palette0}}.
#'

#' @template obj-hy
#' @param palette Color palette. Eithar a named or unnamed vector with colors,
#'  or data frame with variables \code{$colors} and \code{labels} (data frame is
#'  usually generated with function \code{\link{hyGet_palette0}}).
#' @param name the main title of the legend.
#' @param ... further parameters to be passed to
#' \code{\link[ggplot2]{scale_color_manual}} and
#' \code{\link[ggplot2]{scale_fill_manual}}
#'
#'
#' @template ggplot-updated
#' @export
#'
#' @author Vilmantas Gegzna
#'
#' @family \pkg{spPlot} functions for spectroscopy and \pkg{hyperSpec}
#' @family \pkg{spPlot} functions for \pkg{ggplot2}
#'
#' @examples
#' ## NO EXAMPLES YET ##
#'
gg_palette <- function(obj, palette = NULL, name = NULL,...){

    if (is.null(palette) | identical(palette,TRUE)) {
        palette <- hyGet_palette0(obj)
        name <- attributes(palette)$var_label
    }

    # match.call(expand.dots = FALSE)$`...`

    # as.list()
    if (is.data.frame(palette))
        palette <- palette2vec(palette)

    if (length(palette) > 0) {

        p <- list(scale_color_manual(name = name, values = palette, ...),
                   scale_fill_manual(name = name, values = palette, ...))
        return(p)
    } else {
        return(NULL)
    }

}

# @param p \code{ggplot} object.
#
# gg_palette <- function(p, obj, palette = NULL, name = NULL,...){
#
#     if (is.null(palette) | identical(palette,TRUE)) {
#         palette <- hyGet_palette0(obj)
#         name <- attributes(palette)$var_label
#     }
#
#     # match.call(expand.dots = FALSE)$`...`
#
#     # as.list()
#     if (is.data.frame(palette))
#         palette <- palette2vec(palette)
#
#     if (length(palette) > 0) {
#         p<- p +
#         scale_color_manual(name = name, values = palette, ...) +
#         scale_fill_manual( name = name, values = palette, ...)
#     }
#     return(p)
# }


