
#' Knit reveal.js presentation from Knit button in RStudio
#'
#' This function calls [rmarkdown::render()] from the RStudio
#' Knit button.  To use this, add the following to the top-level
#' YAML in a `.Rmd` document:
#'
#' ```
#' knit: revealjmg::knit_revealjs
#' ````
#'
#' See <https://pkg.yihui.org/rmarkdown-cookbook/custom-knit> for more
#' details.
#'
#' @param input Input file (`.Rmd`)
#' @param output_format output format. See [rmarkdown::render()] for
#'   details.
#' @param ... Other arguments passed to [rmarkdown::render()].
#'
#' @return See [rmarkdown::render()]
#' @seealso [rmarkdown::render()]
#' @export
#'
knit_revealjs <- function(input,
                          output_format = "revealjmg::revealjs_presentation",
                          ...) {
  rmarkdown::render(
    input,
    output_format = output_format,
    ...
  )
}
