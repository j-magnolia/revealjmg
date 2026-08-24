globalVariables(c(".", "extension", "value"))

#' Convert to a reveal.js presentation
#'
#' Format for converting from R Markdown to a reveal.js presentation.
#'
#' @inheritParams rmarkdown::beamer_presentation
#' @inheritParams rmarkdown::pdf_document
#' @inheritParams rmarkdown::html_document
#'
#' @param center \code{TRUE} to vertically center content on slides
#' @param controls \code{TRUE} to show navigation controls on slides
#' @param width \code{NULL} to override default width (pixels)
#' @param height \code{NULL} to override default height (pixels)
#' @param margin \code{NULL} to override default margin around the slides.
#' @param slide_level Level of heading to denote individual slides. If
#'   \code{slide_level} is 2 (the default), a two-dimensional layout will be
#'   produced, with level 1 headers building horizontally and level 2 headers
#'   building vertically. It is not recommended that you use deeper nesting of
#'   section levels with reveal.js.
#' @param smart Use smartypants transformations for special characters and
#'   punctuation.
#' @param theme Visual theme ("simple", "sky", "beige", "moon", "night",
#'   "solarized", "league", "serif", "blood", "dracula",
#'   "black", "black-contrast", "white", or "white-contrast").
#' @param custom_theme Custom theme, not included in reveal.js distribution
#' @param custom_theme_dark Does the custom theme use a dark-mode?
#' @param transition Slide transition ("default", "none", "fade", "slide",
#'   "convex", "concave" or "zoom")
#' @param custom_transition Custom slide transition, not included in reveal.js
#'   distribuion.
#' @param background_transition Slide background-transition ("default", "none",
#'   "fade", "slide", "convex", "concave" or "zoom")
#' @param custom_background_transition Custom background-transition, not
#'   included in reveal.js distribuion.
#' @param reveal_options Additional options to specify for reveal.js (see
#'   \href{https://github.com/hakimel/reveal.js#configuration}{https://github.com/hakimel/reveal.js#configuration}
#'   for details).
#' @param reveal_plugins Reveal plugins to include. Available plugins include
#'   "notes", "search", "zoom", "chalkboard", and "menu". Note that
#'   \code{self_contained} must be set to \code{FALSE} in order to use Reveal
#'   plugins.
#' @param reveal_version Version of reveal.js to use.
#' @param reveal_location Location to search for reveal.js (Expects to find
#' reveal.js distribution at
#' \code{file.path(reveal_location, paste0('revealjs-', reveal_version))}
#' @param template Pandoc template to use for rendering. Pass "default" to use
#'   the rmarkdown package default template; pass \code{NULL} to use pandoc's
#'   built-in template; pass a path to use a custom template that you've
#'   created. Note that if you don't use the "default" template then some
#'   features of \code{revealjs_presentation} won't be available (see the
#'   Templates section below for more details).
#' @param custom_asset_path Path to custom theme css.
#' @param resource_location Optional custom path to reveal.js templates and skeletons
#' @param tex_extensions LaTeX extensions for MathJax
#' @param tex_defs LaTeX macro definitions for MathJax
#' @param md_extensions Pandoc markdown extensions
#' @param mathjax_scale Scale (in percent) for MathJax. Default = 100
#' @param extra_dependencies Additional function arguments to pass to the base R
#'   Markdown HTML output formatter [rmarkdown::html_document_base()].
#' @param custom_plugins Add custom plugins to the list of supported plugins.
#' @param no_postprocess Omit the post-processing step.
#' @param ... Extra arguments, passed to the child presentation
#'   generators.
#'
#' @return R Markdown output format to pass to [rmarkdown::render()]
#'
#' @details
#'
#' In reveal.js presentations you can use level 1 or level 2 headers for slides.
#' If you use a mix of level 1 and level 2 headers then a two-dimensional layout
#' will be produced, with level 1 headers building horizontally and level 2
#' headers building vertically.
#'
#' For additional documentation on using revealjs presentations see
#' \href{https://github.com/j-magnolia/revealjmg}{https://github.com/j-magnolia/revealjmg}.
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#' library(revealjmg)
#'
#' # simple invocation
#' render("pres.Rmd", revealjs_presentation())
#'
#' # specify an option for incremental rendering
#' render("pres.Rmd", revealjs_presentation(incremental = TRUE))
#' }
#'
#'
#' @export
revealjs_presentation <- function(incremental = FALSE,
                                  center = FALSE,
                                  width = NULL,
                                  height = NULL,
                                  margin = NULL,
                                  slide_level = 2,
                                  fig_width = 8,
                                  fig_height = 6,
                                  fig_retina = if (!fig_caption) 2,
                                  fig_caption = FALSE,
                                  self_contained = TRUE,
                                  smart = TRUE,
                                  theme = "simple",
                                  custom_theme = NULL,
                                  custom_theme_dark = FALSE,
                                  custom_asset_path = NULL,
                                  transition = "default",
                                  custom_transition = NULL,
                                  background_transition = "default",
                                  custom_background_transition = NULL,
                                  reveal_options = NULL,
                                  reveal_plugins = NULL,
                                  reveal_version = "6.0.1",
                                  reveal_location = "default",
                                  resource_location = "default",
                                  controls = FALSE,
                                  highlight = "default",
                                  mathjax = "default",
                                  mathjax_scale = NULL,
                                  tex_extensions = NULL,
                                  tex_defs = NULL,
                                  template = "default",
                                  css = NULL,
                                  includes = NULL,
                                  md_extensions = NULL,
                                  keep_md = FALSE,
                                  lib_dir = NULL,
                                  pandoc_args = NULL,
                                  extra_dependencies = NULL,
                                  custom_plugins = NULL,
                                  no_postprocess = FALSE,
                                  ...) {

  args <- c(as.list(environment()), list(...))

  # Reveal version: layout of files changed a lot between versions
  # 4 and 6.

  message("revealjmg::revealjs_presentation: reveal version ",
          reveal_version)

  if (reveal_version == "default") {
    reveal_version <- "6.0.1"
  }

  reveal_new_version <- semver::parse_version(reveal_version) >=
    semver::parse_version("6.0.0")

  if (reveal_new_version) {
    doc_fn <- revealjs_6_presentation
  } else {
    doc_fn <- revealjs_3_presentation
  }

  message("revealjmg::revealjs_presentation: args = [",
          purrr::imap(args,
                      ~stringr::str_c(.y, ": ",
                                      ifelse(is.null(.x), "NULL", .x))
                      ) |> stringr::str_c(collapse = ",\n  "),
          "  ]"
          )
  rlang::exec(doc_fn, !!!args)
}
