#' [!] \code{ggplot} method for class \code{hyperSpec}
#'
#'
#' Method to create a new \code{ggplot} object for class \code{hyperSpec}.
#' More details in \code{\link[ggplot2]{ggplot}}.
#'
#' @param data A \code{hyperSpec} object
#'
#' @param wl.range wavelength ranges to plot. The ranges will be plotted
#'  as separate columns of facets. Examples in \code{\link[hyperSpec]{wl2i}}
#'  show how to select wavelength ranges of \code{hyperSpec} object \cr
#'   If \code{wl.range} is not \code{NULL}
#'  function \code{\link{facet_wl}} may be used to control facetting
#'  parameters. Otherwise standard \pkg{ggplot2}  functions, such as
#'  \code{\link[ggplot2]{facet_grid}} or
#'  \code{\link[ggplot2]{facet_wrap}} should be used if facetting is needed.
#'
#' @param ... further arguments to be passed to function
#'            \code{\link[ggplot2]{ggplot}}.
#'
#' @param palette [Expreimental!!!] Color palete for \code{color} and \code{fill}.
#'
#' @param format A string with format to be passed to \code{\link[base]{sprintf}}.
#' This string is used to format text in labels of wavelength intervals, when
#' \code{wl.range} is specified. Default format is \code{'\%g'}.
#'
#' @param environment Not used yet.
#'
#'
#' @inheritParams ggplot2::ggplot
#'
#'
#' @template ggplot
#' @method ggplot hyperSpec
#' @export
#'
#' @param mapping Default list of aesthetic mappings to be used for plotting.
#' Default values are \code{aes_string(x = ".wavelength",
#' y = "spc", group = ".rownames")}. Provided pameters update and modify this
#' list. Other not specified parameters must be suppled in each layer added
#' to the plot.
#'
#' @seealso \code{\link[hyperSpec]{qplotspc}} in package \pkg{hyperSpec}.\cr
#' \code{\link[ggplot2]{ggplot}} in package \pkg{ggplot2}.
#'
#' @family \pkg{spPlot} functions for spectroscopy and \pkg{hyperSpec}
#' @family \pkg{spPlot} functions for \pkg{ggplot2}
#'
#' @importFrom ggplot2 ggplot
#' @examples
#' library(spPlot)
#' library(spHelper)
#' data(Spectra2)
#'
#' ggplot(Spectra2) + geom_line()
#' ggplot(Spectra2, aes(color=class)) + geom_line()
#' ggplot(Spectra2) + geom_line(color = "red", alpha = 0.1) + theme_bw()
#'
#'
#' ggplot(Spectra2, wl.range = c(min ~ 320, 500~600, 700 ~ max)) +
#'      geom_line(aes(color = gr))
#'
#' Sp2 <- hyAdd_color(Spectra2, "gr", c("green","green3","skyblue"))
#' ggplot(Sp2, palette = TRUE) + geom_line()
#'
#' ggplot(Sp2, aes(color = gr), palette = c("blue","gold4","tan")) +
#'     geom_line()
#'
#' ggplot(Sp2) + geom_line(aes(color = I(.color)))
#'
#'
#' library(ggspectra)
#' ggplot(Sp2[1]) + stat_color(geom = "bar")+ scale_color_identity()


ggplot.hyperSpec <- function(data, mapping = aes(),
                              ...,
                              wl.range = NULL,
                              palette = NULL,
                              format = "%g",
                              environment)
{
    chk.hy(data)
    validObject(data)

    force(palette)
    force(format)

    mapping <- modifyList(aes_string(x = ".wavelength",
                                     y = "spc",
                                     group = ".rownames"),
                          mapping)

    if (identical(palette,TRUE)){
        palette <- hyGet_palette0(data)
        if (!is.null(palette)) {
            col_VAR   <- attributes(palette)$var_name
            col_LABEL <- attributes(palette)$var_label
            palette %<>% palette2vec

            mapping <- modifyList(mapping,
                                  aes_string(color = col_VAR)
            )
            # Reset value - the line WILL BE CORRECTED in the future
            palette <- TRUE
        }
    }

    # If `wl.range` is provided a subset of object is taken
    if (!is.null(wl.range))   {
        wl.range <- c(wl.range) # Convert to list
        obj_list <- lapply(wl.range,
                           extract_wl_ranges,
                           obj = data,
                           format = format)
        # Transform to data frame
        DF <- lapply(obj_list, as_longDF_and_rm_NA) %>% Reduce(rbind, .)

    } else {
        # if `wl.range` is missing whole dataset is used

        # Transform to data frame
        DF <- as_longDF_and_rm_NA(data)
    }

    # Make ggplot
    p <- ggplot2::ggplot(DF, mapping = mapping)
    # p <- ggplot2::ggplot(DF, mapping = mapping, ...)

    # Modify x and y axis labels in ggplot
    p <- p +
          xlab(hyperSpec::labels(data, ".wavelength")) +
          ylab(hyperSpec::labels(data, "spc"))

    # Use facetting if two or more wl.ranges are selected
    if (!is.null(wl.range) & length(wl.range) > 1) {
        p <- p + facet_wl()
    }

    # Add color palette

    # if (identical(palette,TRUE)) palette <- hyGet_palette0(data)
    if (length(palette) > 0 & !identical(palette,FALSE)) {

        p <- p + gg_palette(data, palette)

        # p <- p + scale_color_manual(values = palette) +
        #           scale_fill_manual(values = palette)
    }

    return(p)
}


# Internal function definitions

# obj - hyperSpec object
as_longDF_and_rm_NA <- function(obj) {
    # Convert to long data frame format:
    df <- hyperSpec::as.long.df(obj, rownames = TRUE, na.rm = FALSE)
    # Remove NA values:
    df <- df[!is.na(df$spc), , drop = FALSE]
    return(df)
}


# Function subsets dataframe and adds variable `x$.wl.range` which indicates
# the name of spectral range.
#
# obj - hyperSpec object
# wl.range - wavelength range as formula
extract_wl_ranges <- function(obj, wl.range, format = "%g") {

    wl.range <- c(wl.range)

    if (length(wl.range) != 1) {
        stop("`wl.range` must contain only one range."  %.+.%
             "Now the ranges are:\n "  %.+.%
             paste(wl.range, collapse = ", "))
    }
    # extract range
    x <- (obj[, , wl.range])
    # Make label for range
    range_label <- wl(x) %>% range %>% sprintf(format,.) %>%
        paste(collapse = "-")
    # Create variable `.wl.range` which wil be used for facetting
    x$.wl.range  <- as.factor(range_label)
    return(x)

}





    # # Chech if number of spectra is not too big
    # if (nrow(obj) > spc.nmax) {
    #     warning("Number of spectra exceeds spc.nmax. Only the first ",
    #             spc.nmax, " are plotted.")
    #     obj <- obj[seq_len(spc.nmax)]
    # }



    # if (map.lineonly)
    #     # p <- ggplot(df) + geom_line(mapping = mapping, ...)
    #     p <- ggplot(df) + geom_line(mapping = mapping, ...)
    #
    # else {
    #     p <- ggplot(df, mapping = mapping) + geom_line(...)
    #     }
# @param map Logical. If \code{TRUE} - mapping is provided to function
#       \code{ggplot}. If \code{FALSE} - parameter \code{mapping} is ignored.
