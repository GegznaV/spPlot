#' [!] Convert 'plotmath' expresions in labels to text
#'
#' @param obj Object of approptiate class.
#' @param FUN Function that translates expressions in labels to text or
#' other kind of expressions to be applied, e.g., simsalapar::expr2latex,
#' simsalapar::escapeLatex, expr2latex_.
#' @param ... For generic use.
#'
#' @return Updated object with expresions in its labels converted to strings.
#' @export
#'
#' @author Vilmantas Gegzna
#'
#' @seealso \code{\link{expr2latex_}}
#' @family \pkg{spPlot} utilities
#' @family \pkg{spPlot} functions for \pkg{plotly}
#' @family \pkg{spPlot} functions for spectroscopy and \pkg{hyperSpec}
#'
#' @examples
#'
#' library(spPlot)
#' library(hyperSpec)
#' library(plotly)
#'
#' # ===== for hyperSpec object ===============================================
#'
#' labels(chondro)
#' labels(label_expr2text(chondro))
#'
#' # without `label_expr2text` labels with expressions are distorted"
#' chondro[1:10,,] %>% qplotspc %>% ggplotly
#'
#' # If function `label_expr2text` is applied:
#' chondro[1:10,,] %>% label_expr2text  %>% qplotspc %>% ggplotly
#'
#'
#' # ===== for ggplot2 object =================================================
#'
#' chondro[1:10,,]  %>% qplotspc %>% label_expr2text %>% ggplotly
#'
#'
#' # ===== for plotly object ==================================================
#'
#' chondro[1:10,,]  %>% qplotspc %>%  ggplotly %>% label_expr2text
#' chondro[1:10,,]  %>% qplotspc %>%  ggplotly %>% plotly_build %>% label_expr2text
#'
#' # ==========================================================================
#' @importFrom simsalapar expr2latex

label_expr2text <- function(obj, FUN = expr2latex_, ...){
    UseMethod("label_expr2text")
}

#' @rdname label_expr2text
#' @export
#' @method label_expr2text ggplot
#'
label_expr2text.ggplot <- function(obj, FUN = expr2latex_, ...){
    obj$labels <- lapply(obj$labels, FUN = FUN)
    return(obj)
}

#' @rdname label_expr2text
#' @export
#' @method label_expr2text hyperSpec
#'
label_expr2text.hyperSpec <- function(obj, FUN = expr2latex_, ...) {
    # require(hyperSpec)
    validObject(obj)
    labels(obj) <- lapply(obj@label, FUN = FUN)
    return(obj)
}

#' @rdname label_expr2text
#' @export
#' @method label_expr2text plotly_hash
#'
label_expr2text.plotly_hash <- function(obj, FUN = expr2latex_, ...){
    .Deprecated("label_expr2text.plotly")
    obj <- plotly::plotly_build(obj)  %>%  label_expr2text(., FUN, ...)
    return(obj)
}

#' @rdname label_expr2text
#' @export
#' @method label_expr2text plotly
#'

label_expr2text.plotly <- function(obj, FUN = expr2latex_, ...){

    # Update the main titles:
    obj$x$layout$title %<>% FUN

    # Update titles of all axes:
    ind <- grep("axis", names(obj$x$layout), ignore.case = TRUE)
    for (i in ind) {
        if (is.null(obj[["x"]][["layout"]][[i]][["title"]])) {next}
        obj$x$layout[[i]]$title   %<>% FUN
        }

    # obj$x$layout$xaxis$title   %<>% FUN
    # obj$x$layout$yaxis$title   %<>% FUN
    # obj$x$layout$zaxis$title   %<>% FUN

    return(obj)
}


# label_expr2text.plotly_built <- function(obj, FUN = expr2latex_, ...){

