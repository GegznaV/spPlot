#' @export
#' @rdname gg_palette
palette2vec <- function(palette) {
    if (is.data.frame(palette))
        palette <- setNames(palette$colors, palette$labels)

    return(palette)
}
