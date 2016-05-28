## ----options1, echo = FALSE, message = FALSE, warning = FALSE------------
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "#>",
                      fig.align = 'center')
optDEF <- knitr::opts_chunk$get()

## ----Load main package, message = FALSE, warning = FALSE-----------------
library(spHelper)
library(spPlot)

## ------------------------------------------------------------------------
plotly_obj <- plot_ly(economics, x = date, y = uempmed, type = "scatter",
             showlegend = FALSE)

ggplot_obj <- qplot(mpg, wt, data = mtcars, colour = cyl)

## ------------------------------------------------------------------------
obj <- knitr_container()
class(obj)
obj

## ------------------------------------------------------------------------
obj <- add_as_section(obj, "Plots")

obj <- add_as_section(obj, "Add `plotly`", level = 2)
obj <- add_as_plotly(obj, plotly_obj)

obj <- add_as_section(obj, "Add `ggplot` as `plotly`", level = 2)
obj <- add_as_plotly(obj, ggplot_obj)

obj <- add_as_section(obj, "Add `ggplot`", level = 2)
obj <- add_as_is(obj, ggplot_obj)

obj <- add_as_section(obj, "Attention: Not Plotted", level = 1)
obj <- add_as_is(obj, plotly_obj)
obj <- add_as_text(obj, paste("As you noticed, the last",
      "`plotly` object was not plotted as it was added with ",
      "`add_as_is()` and not with `add_as_plotly()`"))


obj <- add_as_section(obj, "Pander and text", level = 1)

obj <- add_as_section(obj, "As pander", level = 2)
obj <- add_as_pander(obj, summary(mtcars))

obj <- add_as_section(obj, "As text", level = 2)
obj <- add_as_text(obj, summary(mtcars))

obj <- add_as_section(obj, "As is", level = 2)
obj <- add_as_is(obj, summary(mtcars))


## ------------------------------------------------------------------------
print(obj)

is.knitr_container(obj) 
is.knitr_container(ggplot_obj)

as.knitr_container(ggplot_obj) 

class(obj)

## ------------------------------------------------------------------------
Join(obj, obj)   %>% length()

## ------------------------------------------------------------------------
Join(obj, ggplot_obj)   %>% length()

## ------------------------------------------------------------------------
Join(ggplot_obj, plotly_obj)

## ---- results = 'asis'---------------------------------------------------
print_objects(obj)

## ----session info--------------------------------------------------------
devtools::session_info()

