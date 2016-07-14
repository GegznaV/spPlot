#'
#' [!] Modify legend of a \code{plotly} object
#'
#' Function allows modifying various legend options of object (plot) created
#' with package \pkg{plotly}.
#'
#' @details
#' If option \code{rm_repText} is \code{TRUE} (default), function corrects text
#' in legend of object  created with function \code{\link[plotly]{ggplotly}} so
#' that only relevant information is displayed.
#' For example, if the text in a label is \code{"(matrix,matrix,matrix)"}, it is
#' converted to shorter string \code{"matrix"}.
#'
#' @template obj-plotly
#'
#' @param showlegend logical, which determins if will be shown.
#'        If \code{NULL} (default) value of this option will not be changed.
#'
#' @param unique.legend logical. If \code{TRUE} (default), only one legend per
#' legend group is shown. Result is visible if function
#' \code{\link[plotly]{subplot}} was applied.
#'
#' @param traceorder Any combination of "reversed", "grouped" joined with a "+" OR "normal".
#' examples: "reversed", "grouped", "reversed+grouped", "normal"
#' Determines the order at which the legend items are displayed. If "normal",
#'  the items are displayed top-to-bottom in the same order as the input data.
#'  If "reversed", the items are displayed in the opposite order as "normal".
#'  If "grouped", the items are displayed in groups (when a trace `legendgroup`
#'  is provided). if "grouped+reversed", the items are displayed in the
#'  opposite order as "grouped". Described in
#'  \href{https://plot.ly/r/reference/#layout-legend}{R plotly reference}.
#'
#' @param bgcolor Legend's background color. If \code{NULL} (default) value of this option will not be changed.
#' @param bordercolor Legend's border color. If \code{NULL} (default) value of this option will not be changed.
#' @param borderwidth Legend's border width. If \code{NULL} (default) value of this option will not be changed.
#' @param fontcolor Legend's font color.     If \code{NULL} (default) value of this option will not be changed.
#' @param fontfamily Legend's font family.   If \code{NULL} (default) value of this option will not be changed.
#' @param fontsize   Legend's font size.     If \code{NULL} (default) value of this option will not be changed.
#' @param x Legend's x position.             If \code{NULL} (default) value of this option will not be changed.
#' @param y Legend's y position.             If \code{NULL} (default) value of this option will not be changed.
#'
#' @param ... Further parameters to methods.
#'
#' @param tracegroupgap Described in \href{https://plot.ly/r/reference/#layout-legend}{R plotly reference}.
#' @param orientation "v","h". If \code{NULL} (default) value of this option will not be changed.
#' Described in \href{https://plot.ly/r/reference/#layout-legend}{R plotly reference}.
#'
#' @param yanchor Described in \href{https://plot.ly/r/reference/#layout-legend}{R plotly reference}.
#' @param xanchor Described in \href{https://plot.ly/r/reference/#layout-legend}{R plotly reference}.
#' @param rm_repText Logical. If \code{TRUE} (default), repeated text in
#' legend's label is removed.
#'
#' @template plotly-updated
#' @export
#'
#' @examples
#' library(spPlot)
#' library(spHelper)
#' library(hyperSpec)
#' library(plotly)
#'
#' # Create `plotly` object:
#' obj <- qplot_spStat(chondro,"clusters",mean)  %>% ggplotly
#'
#' # Then compare this plot:
#' plotly_modify_legend(obj)
#'
#' # With the original one:
#' print(obj)
#'
#' # More options
#' plotly_modify_legend(obj, traceorder = "reversed")
#' plotly_modify_legend(obj, showlegend = FALSE)
#'
#'
#'
#' @seealso Online reference \url{https://plot.ly/r/reference/#layout-legend};\cr
#' Functions \link[plotly]{layout}(), \link[plotly]{plot_ly}(),
#' @family \pkg{spPlot} functions for \pkg{plotly}
#'
#' @author Vilmantas Gegzna
#'
plotly_modify_legend <- function(obj,
                                 showlegend      = NULL,
                                 traceorder      = NULL,
                                 bgcolor         = NULL,
                                 bordercolor     = NULL,
                                 borderwidth     = NULL,
                                 fontcolor       = NULL,
                                 fontfamily      = NULL,
                                 fontsize        = NULL,
                                 x               = NULL,
                                 y               = NULL,
                                 yanchor         = NULL,
                                 xanchor         = NULL,
                                 tracegroupgap   = NULL,
                                 orientation     = NULL,
                                 rm_repText      = TRUE,
                                 unique.legend   = TRUE,
                                 ...){

    # Convert plotly object to editable object
    obj <- plotly::plotly_build(obj)

    # Corrections
    if (!is.null(showlegend))   {obj$x$layout$showlegend           <- as.logical(showlegend)}
    if (!is.null(orientation))  {obj$x$layout$legend$orientation   <- match.arg(orientation, c("v", "h"))}
    if (!is.null(traceorder))   {obj$x$layout$legend$traceorder    <- match.arg(traceorder, c( "normal", "reversed", "grouped", "reversed+grouped","grouped+reversed"))}
    if (!is.null(bgcolor))      {obj$x$layout$legend$bgcolor       <- bgcolor}
    if (!is.null(bordercolor))  {obj$x$layout$legend$bordercolor   <- bordercolor}
    if (!is.null(borderwidth))  {obj$x$layout$legend$borderwidth   <- borderwidth}
    if (!is.null(fontcolor))    {obj$x$layout$legend$font$color    <- fontcolor}
    if (!is.null(fontfamily))   {obj$x$layout$legend$font$family   <- fontfamily}
    if (!is.null(fontsize))     {obj$x$layout$legend$font$size     <- fontsize}
    if (!is.null(tracegroupgap)){obj$x$layout$legend$tracegroupgap <- as.numeric(tracegroupgap)}

    if (!is.null(x))            {obj$x$layout$legend$x             <- x}
    if (!is.null(y))            {obj$x$layout$legend$y             <- y}
    if (!is.null(xanchor))      {obj$x$layout$legend$xanchor       <- match.arg(xanchor, c("auto", "left", "center", "right"))}
    if (!is.null(yanchor))      {obj$x$layout$legend$yanchor       <- match.arg(yanchor, c("auto", "top" , "middle", "bottom"))}

    # Length of list "data"
    LEN <- length(obj$x$data)

    # Remove repeated text in legend labels and legend group names
    if (rm_repText == TRUE){
        # Function to correct text in labels: extract relevant nonrepeated information
        rm_repeated <- function(TXT) gsub("\\((.+?)(?:,\\1)+\\)", "\\1", TXT, perl = T)

        # Apply the function
        for (i in 1:LEN) {
            obj$x$data[[i]]$name        %<>% rm_repeated
            obj$x$data[[i]]$legendgroup %<>% rm_repeated
        }

    }

    # # Recover missing label
    # if (recover.legend == TRUE){
    #     recover_missing <- function(FROM, TO){
    #         if (length(FROM) == 0 |                 # if emply or
    #             grepl("^\\s*$",FROM, perl = TRUE))  # if white space only
    #             return(TO)
    #         else
    #             return(FROM)
    #     }
    #
    # # Apply the function
    #     for (i in 1:LEN) {
    #         obj$x$data[[i]]$name <- recover_missing(obj$x$data[[i]]$name,
    #                                               obj$x$data[[i]]$legendgroup)
    #     }
    # }

    # Show only one legend per legend group -----------------------------
    if (unique.legend == TRUE & obj$x$layout$showlegend == TRUE){
        if_len0 <- function(x,y) if(length(x)!=0) x else y

        # Extract variables "showlegend" and "legendgroup"
        DF <- sapply(obj$x$data, function(x) {
            list(showlegend  =         x$showlegend,
                 legendgroup = if_len0(x$legendgroup, NA))
        }) %>% t %>% as.data.frame()

        # Find unique and not NA values and show only their legends
        is_unique_gr <- function(x) (!duplicated(x) & !is.na(x))
        DF$showlegend <- is_unique_gr(DF$legendgroup)

        for (i in 1:LEN) {
            obj$x$data[[i]]$showlegend <- DF$showlegend[i]
        }

    }

    return(obj)
}
