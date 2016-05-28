#' [!.] Translate `plotmath` expressions to LaTeX and remove leading and tailing quotes
#'
#' \code{\link[simsalapar]{expr2latex}} translates a "R graphics annotation"
#' expression to the corresponding LaTeX one.\cr
#' \code{expr2latex_} additionally removes leading and tailing  double quotes (").
#'
#' @inheritParams simsalapar::expr2latex
#'
#' @return A \link[base]{character} string with the LaTeX expression
#' corresponding to "R graphics annotation" expression \code{expr}.
#' @export
#'
#' @author Vilmantas Gegzna
#'
#' @seealso \code{\link[simsalapar]{expr2latex}} - translate 'plotmath'
#' expressions to LaTeX.\cr
#' \code{\link{label_expr2text}} remove expresions from labels of \code{ggplot2},
#' \code{hyperSpec} and \code{plotly} objects.
#'
#' @family \pkg{spPlot} utilities
#'
#' @examples
#'
#' library(spPlot)
#' library(simsalapar)
#'
#' expr2latex_(quote(N[sim]))
#'
#'
#' labels(chondro)$.wavelength
#'   ## expression(Delta * tilde(nu)/cm^-1)
#'
#' labels(chondro)$.wavelength  %>% expr2latex
#'   ## [1] "Delta * tilde(nu)/cm^-1"
#'
#' labels(chondro)$.wavelength  %>% expr2latex_
#'   ## [1] "Delta * tilde(nu)/cm^-1"
#'
#'
#'
#' labels(chondro)$spc
#'   ## expression("I / a.u.")
#'
#' labels(chondro)$spc  %>% expr2latex
#' ## [1] "\"I / a.u.\""
#'
#' labels(chondro)$spc  %>% expr2latex_
#' ## [1] "I / a.u."
expr2latex_ <- function(expr){
    simsalapar::expr2latex(expr) %>% gsub("(^\")|(\"$)","", .)
}
# unquote()
