#' [!] Manage annotations in a \code{plotly} object
#'
#' Manage annotations of \code{plotly} object.
#'
#' @template obj-plotly
#' @param which Numeric vector with numbers of annotations to remove.
#'
#' @return
#' The functions described here do the following actions to \code{plotly}
#'  object:\cr\cr
#' \code{plotly_annotation_rm}() removes annotations and returns
#' updated \code{plotly} object;\cr\cr
#' \code{plotly_annotation_n}() returns the number of annotations in the object;\cr\cr
#' \code{plotly_annotation_list}() returns a list of annotation parameters;\cr\cr
#' \code{plotly_annotation_text}() returns a list with text of each annotation.
#'
#' @export
#'
#' @examples
#' library(spPlot)
#' library(plotly)
#'
#' # Create plotly object 'obj':
#' iris$Species2 <- sample(iris$Species)
#' p <- ggplot(iris, aes(x = Sepal.Length,
#'                          y = Sepal.Width))
#' obj <- subplot(p + geom_line(aes(color = Species)),
#'                p + geom_point(aes(shape = Species2)))
#'
#' # Print the plot:
#' obj
#'
#' # Get parameters of annotations:
#' plotly_annotation_list(obj)
#' plotly_annotation_n(obj)
#' plotly_annotation_text(obj)
#'
#'
#' # The same plot with all annotations removed:
#' plotly_annotation_rm(obj)
#'
#' @author Vilmantas Gegzna
#'
#' @family \pkg{spPlot} functions for \pkg{plotly}


plotly_annotation_rm <- function(obj, which = NULL){
    obj <- plotly::plotly_build(obj)
    if(is.null(which))  obj$x$layout$annotations <- list()
    else obj$x$layout$annotations[which] <- NULL
    return(obj)
}

#' @export
#' @rdname plotly_annotation_rm
plotly_annotation_list <- function(obj, which = NULL){
    obj <- plotly::plotly_build(obj)
    if(is.null(which))  ann <- obj$x$layout$annotations
    else                ann <- obj$x$layout$annotations[which]
    return(ann)
}

#' @export
#' @rdname plotly_annotation_rm
plotly_annotation_n <- function(obj){
    obj <- plotly::plotly_build(obj)
    return(length(obj$x$layout$annotations))
}
#' @export
#' @rdname plotly_annotation_rm
plotly_annotation_text <- function(obj){
    obj <- plotly::plotly_build(obj)
    lapply(obj$x$layout$annotations, function(x) x$text)
}
# obj$layout$showlegend
