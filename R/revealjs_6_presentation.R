globalVariables(c(".", "extension", "value"))


#' Append variable to pandoc variable list
#'
#' Appends a new value to a pandoc variable. Discards duplicate
#' values and sorts the results. Handles the case where there is
#' no previous value for that key.
#'
#' @param pvars A named list with keys and values. Values may be vectors
#'   with length > 1.
#' @param key The key (character).
#' @param value The value (any type).
#'
#' @return An appended list of keys and values.
#' @examples
#' \dontrun{
#' pandoc_vars <- list()
#' pandoc_vars <- append_pandoc_var("width", 1920)
#' }
#'
append_pandoc_var <- function(pvars, key, value) {
  jsbool <- function(value) ifelse(value, "true", "false")

  if(is.logical(value)) {
    value <- jsbool(value)
  }
  pvars[[key]] <- c(pvars[[key]], value) |> purrr::discard(is.null) |>
    unique() |> sort()
  pvars
}

#' Convert to a reveal.js presentation
#'
#' Format for converting from R Markdown to a reveal.js presentation.
#'
#' @inheritParams rmarkdown::beamer_presentation
#' @inheritParams rmarkdown::pdf_document
#' @inheritParams rmarkdown::html_document
#' @inheritParams revealjs_presentation
#'
#' @param mathjax_version MathJax version (2, 3, or 4)
#' @param mathjax_font MathJax alternate font.
#' @param ... Extra options (none at the moment)
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
revealjs_6_presentation <- function(incremental = FALSE,
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
                                    mathjax_version = 4,
                                    mathjax_font = NULL,
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

  message("revealjmg::revealjs_6_presentation: version: ",
          reveal_version)

  # Reveal version: layout of files changed a lot between versions
  # 4 and 6.

  if (stringr::str_to_lower(reveal_location) != "default") {
    loc <- reveal_location
  } else {
    ver <- reveal_version
    if (ver == "default") {
      ver <- "6.0.1"
    }
    loc <- system.file(stringr::str_c("reveal.js-", ver),
                       package = "revealjmg")
  }

  reveal_package <- try(
    jsonlite::read_json(file.path(loc, "package.json"))
  )
  if (inherits(reveal_package, "try-error")) {
    reveal_package = NULL
  }

  if (stringr::str_to_lower(reveal_version) == "default" &&
      ! is.null(reveal_package)) {
    reveal_version <- reveal_package$version
  }

  if (! is.null(reveal_package)) {
    reveal_versions <- c(reveal_version, reveal_package$version)
  } else {
    reveal_versions <- reveal_version
  }

  reveal_new_version <- semver::parse_version(reveal_version) >=
    semver::parse_version("6.0.0")

  if (all(reveal_new_version) != any(reveal_new_version)) {
    stop("Error: inconsistent reveal versions: ", reveal_versions[1],
         " and ", reveal_versions[2])
  }

  reveal_new_version = all(reveal_new_version)

  if (! reveal_new_version) {
    stop("Cannot build a revealjs_6 presentation for reveal ", reveal_version)
  } else {
    resource_loc <- "revealjs-6"
  }

  # function to lookup reveal resource
  reveal_resources <- function() {
    if(identical(resource_location, "default")) {
      resloc <- system.file(
        file.path("rmarkdown", resource_loc,
                  "templates/revealjs_presentation/resources"),
        package = "revealjmg"
      )
    } else {
      resloc <- resource_location
    }
    message("Resource location = ", resloc)
    resloc
  }

  # base pandoc options for all reveal.js output
  pandoc_vars <- list()
  args <- c()


  # template path and assets
  default_template <- file.path(reveal_resources(), 'default.html')
  if (identical(template, "default")) {
    message("Using default template")
    t <- default_template
  } else {
    if(file.exists(template)) {
      t <- template
      message("Found local template ", t)
    } else {
      t <-  file.path(reveal_resources(), template)
      if (! file.exists(t)) {
        t <- file.path(reveal_resources(), 'templates', template)
      }
      if (file.exists(t)) {
        message("Found template in resource directory: ", t)
      } else {
        message("Can't find template, ", t)
        t <- default_template
      }
    }
    message("Using template ", t)
    args <- c(args, "--template", pandoc_path_arg(t))
  }

  # incremental
  if (incremental)
    args <- c(args, "--incremental")

  # centering
  pandoc_vars <- append_pandoc_var(pandoc_vars, "center", center)

  # controls
pandoc_vars <- append_pandoc_var(pandoc_vars, "controls", controls)

  # width and height
  if (! is.null(width))
    pandoc_vars <- append_pandoc_var(pandoc_vars, "width", width)
  if (! is.null(height))
    pandoc_vars <- append_pandoc_var(pandoc_vars, "height", height)
  if (! is.null(margin))
    pandoc_vars <- append_pandoc_var(pandoc_vars, "margin", margin)

  # slide level
  args <- c(args, "--slide-level", as.character(slide_level))

  # theme
  theme <- match.arg(theme, revealjs_6_themes())
  theme_dark <- FALSE
  if (identical(theme, "custom")) {
    if (is.null(custom_theme))
    {
      stop("Missing custom_theme in YAML header")
    } else {
      theme <- NULL
      theme_dark <- custom_theme_dark
    }
  } else {
    if (identical(theme, "default"))
      theme <- "simple"
    else if (identical(theme, "dark"))
      theme <- "black"
    if (theme %in% c("black", "blood", "moon", "night"))
      theme_dark <- TRUE
  }
  if (theme_dark) {
    pandoc_vars <- c(pandoc_vars, list("theme-dark" = 'true'))
  }
  if (is.null(theme)) {
    pandoc_vars <- c(pandoc_vars, list('local-theme' = custom_theme))
  } else {
    pandoc_vars <- append_pandoc_var(pandoc_vars, "theme", theme)
  }


  # transition
  transition <- match.arg(transition, revealjs_6_transitions())
  if (identical(transition, "custom")) {
    if (is.null(custom_transition)) {
      stop("Missing custom_transition in YAML header")
    }
    else {
      transition <- custom_transition
    }
  }
  pandoc_vars <- append_pandoc_var(pandoc_vars, "transition", transition)

  # background_transition
  background_transition <- match.arg(background_transition, revealjs_6_transitions())
  if (identical(background_transition, 'custom')) {
    if (is.null(custom_background_transition)) {
      stop("Missing custom_background_transition in YAML header")
    } else {
      background_transition <- custom_background_transition
    }
  }
  pandoc_vars <- append_pandoc_var(pandoc_vars, "backgroundTransition", background_transition)

  # use history
  pandoc_vars <- append_pandoc_var(pandoc_vars, "history", "true")

  # use hash
  pandoc_vars <- append_pandoc_var(pandoc_vars, "hash", "true")

  # mathjax-version
  if (! is.null(mathjax_version)) {
    message("Mathjax version = ", mathjax_version)
    mjv <- as.integer(floor(as.numeric(mathjax_version)))
    if (mjv == 4L) {
      pandoc_vars <- pandoc_vars |>
        append_pandoc_var("mathjax-version", mjv) |>
        append_pandoc_var("mathjax4", "true")
      if (! is.null(mathjax_font) && mathjax_font != "default") {
        pandoc_vars <- append_pandoc_var(pandoc_vars, "mathjax-font",
                                         mathjax_font)
        }
    } else if (mjv == 3L) {
      pandoc_vars <- pandoc_vars |>
        append_pandoc_var("mathjax-version", mjv) |>
        append_pandoc_var("mathjax3", "true")
    }
  }

  # mathjax-scale
  if (! is.null(mathjax_scale)) {
    pandoc_vars <- append_pandoc_var(pandoc_vars, "mathjax-scale",
                                     mathjax_scale)
  }

  # additional reveal options
  if (is.list(reveal_options)) {
    add_reveal_option <- function(option, value) {
      pandoc_vars <<- append_pandoc_var(pandoc_vars, option, value)
    }

    default_options <- revealjs_6_defaults() |>
      purrr::compact() |> purrr::discard(\(x) all(is.na(x)))

    message("  original reveal_options = [\n",
            stringr::str_c("    ",
                  purrr::imap(reveal_options,
                              \(val, key)
                              stringr::str_c(key, ": ",
                                    stringr::str_c(val, collapse = ", "))),
                  collapse = ",\n"),
            "\n  ]")

    for (option in names(default_options)) {
      if (! option %in% names(reveal_options)) {
        reveal_options[option] <- default_options[option]
      }
    }

    message("  updated reveal_options = [\n",
            stringr::str_c("    ", purrr::imap(reveal_options,
                               \(val, key)
                               stringr::str_c(key, ": ",
                                     stringr::str_c(val, collapse = ", "))
                               ),
                  collapse = ",\n"),
            "\n  ]")


    for (option in names(reveal_options)) {
      # special handling for nested options
      if (option %in% c("chalkboard", "menu")) {
        nested_options <- reveal_options[[option]]
        for (nested_option in names(nested_options)) {
          add_reveal_option(paste0(option, "-", nested_option),
                            nested_options[[nested_option]])
        }
      }
      # standard top-level options
      else {
        for(o in reveal_options[[option]]) {
          add_reveal_option(option, o)
        }
      }
    }
  }

  # reveal plugins
  if (is.character(reveal_plugins)) {
    message("plugins = [", stringr::str_c(reveal_plugins, collapse = ", "), "]")
    # validate that we need to use self_contained for plugins
    if (self_contained)
      stop("Using reveal_plugins requires self_contained: false")

    # validate specified plugins are supported
    supported_plugins <- c("notes", "search", "zoom", "chalkboard", "menu")
    if (!is.null(custom_plugins)) {
      supported_plugins <- c(supported_plugins, custom_plugins)
    }
    invalid_plugins <- setdiff(reveal_plugins, supported_plugins)
    if (length(invalid_plugins) > 0)
      stop("The following plugin(s) are not supported: ",
           paste(invalid_plugins, collapse = ", "), call. = FALSE)

    # add plugins
    sapply(reveal_plugins, function(plugin) {
      pandoc_vars <<- append_pandoc_var(pandoc_vars,
                                        paste0("plugin-", plugin), "1")
      # if (plugin %in% c("chalkboard", "menu")) {
      #   extra_dependencies <<- append(extra_dependencies,
      #                                 list(rmarkdown::html_dependency_font_awesome()))
      # }
    })
  }

  # TeX extensions for MathJax
  if (! is.null(tex_extensions)) {
    pandoc_vars <- append_pandoc_var(pandoc_vars, 'mathjax-packages',
                                     tex_extensions)
  }

  # TeX macro definitions for MathJax
  if (! is.null(tex_defs)) {
    pandoc_vars <- append_pandoc_var(
      pandoc_vars, 'tex-macros',
      purrr::map_chr(\(x) stringr::str_c(x$name, ': "', x$def, '"') |>
                stringr::str_replace_all(stringr::fixed('\\'), '\\\\'))
    )
  }

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # additional css
  for (css_file in css)
    args <- c(args, "--css", pandoc_path_arg(css_file))


  markdown_extensions <- tibble::tibble(
    extension = c("implicit_figures", "smart", "markdown_in_html_blocks"),
    value = c(fig_caption, smart, TRUE)
  )

  # message("Base extensions = [", str_c(markdown_extensions, collapse = ", "), "]")

  if(! is.null(md_extensions)) {
    user_md_extensions = stringr::str_extract_all(md_extensions, "([+-])([A-Za-z0-9_]+)") %>%
      simplify() %>% tibble(extension = .) %>%
      mutate(value = stringr::str_detect(extension, '^\\+'), extension = stringr::str_sub(extension, 2))

    # message("User extensions = [", str_c(md_extensions, collapse = ", "), "]")
    # message("Processed User extensions = [", str_c(user_md_extensions, collapse = ", "), "]")

    markdown_extensions <- markdown_extensions %>%
      filter(! extension %in% user_md_extensions$extension) %>%
      bind_rows(user_md_extensions)
  }

  markdown_extensions <- markdown_extensions %>%
    transmute(string = stringr::str_c(ifelse(value, "+", "-"), extension)) %>%
    simplify() %>% stringr::str_c(collapse = "")

  # message("Merged extensions = [", str_c(markdown_extensions, collapse = ", "), "]")

  # pre-processor for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  pre_processor_6 <- function(metadata, input_file, runtime, knit_meta,
                              files_dir, output_dir) {

    message("Starting revealjs 6 preprocessor...")
    # we don't work with runtime shiny
    if (identical(runtime, "shiny")) {
      stop("revealjs_presentation is not compatible with runtime 'shiny'",
           call. = FALSE)
    }

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <- files_dir

    # extra args
    args <- c()

    # reveal.js
    reveal_home <- paste0("reveal.js-", reveal_version)
    if (identical(reveal_location, "default")) {
      revealjs_path <- system.file(reveal_home, package = "revealjmg")
      if (identical(revealjs_path, '')) {
        message('Empty revealjs_path')
        revealjs_path <- file.path(lib_dir, reveal_home)
      }
    } else {
      revealjs_path <- file.path(reveal_location, reveal_home)
    }
    if (reveal_new_version) {
      revealjs_path <- file.path(revealjs_path, "dist")
    }
    if (is.null(custom_asset_path) || identical(custom_asset_path, "default")) {
      custom_asset_path <-  revealjs_path
    }
    if (!self_contained || identical(.Platform$OS.type, "windows")) {
      message("rendering: self_contained = ", self_contained,
              ", OS = ", .Platform$OS.type)
      message("revealjs_path = ", revealjs_path,
              ",\n  custom_asset_path = ", custom_asset_path,
              ",\n  lib_dir = ", lib_dir,
              ",\n  current directory = ", getwd(),
              ",\n  output_dir = ",
              output_dir)
      old_rjs_path <- revealjs_path
      revealjs_path <- relative_to(
        output_dir, render_supporting_files(revealjs_path, lib_dir))
      if (custom_asset_path == old_rjs_path) {
        custom_asset_path <- revealjs_path
      } else {
      custom_asset_path <- relative_to(
        output_dir,
        render_supporting_files(custom_asset_path, lib_dir))
      }
      message("revealjs_path = ", revealjs_path,
              ",\n  custom_asset_path = ", custom_asset_path,
              ",\n  current directory = ", getwd(),
              ",\n  output_dir = ",
              output_dir)
    }else  {
      revealjs_path <- pandoc_path_arg(revealjs_path)
      custom_asset_path <- pandoc_path_arg(custom_asset_path)
    }
    message("setting revealjs-url to ", revealjs_path,
            " in pre-processor")
    pandoc_vars <- append_pandoc_var(pandoc_vars, "revealjs-url",
                                     revealjs_path)
    if (! is.null(custom_asset_path) && ! is.na(custom_asset_path)) {
      message("setting local-asset-url to ", custom_asset_path,
              " in pre-processor")
      pandoc_vars <- append_pandoc_var(pandoc_vars,
                                       "local-asset-url",
                                       custom_asset_path)
    }

    message("pandoc variables = [\n",
            stringr::str_c("    ", names(pandoc_vars), " = ",
                  purrr::map_chr(pandoc_vars, \(x) stringr::str_c(x, collapse = ", ")),
                  collapse = ",\n"),
            "  ]")

    args <- c(args,
              purrr::imap(
                pandoc_vars,
                \(values, name) purrr::map(
                  values,
                  \(val) pandoc_variable_arg(name, val)
                  )
                ) |> unlist())

    # highlight
    message("setting highlight args in pre-processor")
    args <- c(args, pandoc_highlight_args(highlight,
                                          default = "pygments"))

    message("Done preprocessing: args = [",
            ~stringr::str_c(args, collapse = "\n  "),
            "\n  ]")

    message("Done preprocessing. Returning")
    # return additional args
    args
  }

  if (no_postprocess) {
    postprocessor = NULL
  } else {
    postprocessor = revealjmg_postprocessor
  }

  message("Built arguments: building output format for revealjs 6")
  message("  pandoc args = [",
          stringr::str_c("    ", args, collapse = "\n  "),
          "\n  ]")
  # return format
  of <- output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md),
    pandoc = pandoc_options(to = "revealjs",
                            from = rmarkdown_format(markdown_extensions),
                            args = args),
    keep_md = keep_md,
    clean_supporting = self_contained,
    pre_processor = pre_processor_6,
    post_processor = postprocessor,
    base_format = html_document_base(smart = FALSE, lib_dir = lib_dir,
                                     self_contained = self_contained,
                                     mathjax = mathjax,
                                     pandoc_args = pandoc_args,
                                     extra_dependencies = extra_dependencies,
                                     ...))

  message("Done generating output format.")

  invisible(of)
}


revealjs_6_themes <- function() {
  c("default",
    "beige",
    "black",
    "black-contrast",
    "blood",
    "dracula",
    "league",
    "moon",
    "night",
    "serif",
    "simple",
    "sky",
    "solarized",
    "solarized-jmg",
    "white",
    "white-contrast",
    "custom")
}


revealjs_6_transitions <- function() {
  c(
    "default",
    "none",
    "fade",
    "slide",
    "convex",
    "concave",
    "zoom",
    "custom"
  )
}

revealjs_6_defaults <- function() {
  list(
    'abstract' = NA_character_,
    'author' = NA_character_,
    'author-meta' = NA_character_,
    'autoPlayMedia' = FALSE,
    'autoSlide' = FALSE,
    'autoSlideMethod' = NA_character_,
    'autoSlideStoppable' = NA_character_,
    'background-image' = NA_character_,
    'backgroundTransition' = NA_character_,
    'backgroundcolor' = NA_character_,
    'center' = NA_character_,
    'class_date' = NA_character_,
    'class_no' = NA_character_,
    'controls' = NA_character_,
    'controlsBackArrows' = NA_character_,
    'controlsLayout' = NA_character_,
    'controlsTutorial' = NA_character_,
    'course' = NA_character_,
    'course_name' = NA_character_,
    'csl-css' = NA_character_,
    'css' = NA_character_,
    'date' = NA_character_,
    'date-meta' = NA_character_,
    'defaultTiming' = NA_character_,
    'dir' = NA_character_,
    'disableLayout' = NA_character_,
    'display' = NA_character_,
    'displaymath-css' = NA_character_,
    'document-css' = NA_character_,
    'embedded' = NA_character_,
    'fontcolor' = NA_character_,
    'fontsize' = NA_character_,
    'fragmentInURL' = NA_character_,
    'fragments' = NA_character_,
    'hash' = NA_character_,
    'hashOneBasedIndex' = NA_character_,
    'header-includes' = NA_character_,
    'height' = '1080',
    'help' = 'true',
    'hideCursorTime' = NA_character_,
    'hideInactiveCursor' = NA_character_,
    'highlight-js' = NA_character_,
    'highlighting-css' = NA_character_,
    'highlightjs-theme' = NA_character_,
    'history' = NA_character_,
    'idprefix' = NA_character_,
    'include-after' = NA_character_,
    'include-before' = NA_character_,
    'institute' = NA_character_,
    'keyboard' = NA_character_,
    'keywords' = NA_character_,
    'lang' = NA_character_,
    'linestretch' = NA_character_,
    'linkcolor' = NA_character_,
    'local-asset-url' = NA_character_,
    'local-theme' = NA_character_,
    'loop' = NA_character_,
    'mainfont' = NA_character_,
    'margin-bottom' = NA_character_,
    'margin-left' = NA_character_,
    'margin-right' = NA_character_,
    'margin-top' = NA_character_,
    'math' = NA_character_,
    'mathjax' = '4',
    'mathjax-font' = NA_character_,
    'mathjax-packages' = c('mhchem', 'ams'),
    'mathjax-url' = NA_character_,
    'maxwidth' = NA_character_,
    'mobileViewDistance' = NA_character_,
    'monobackgroundcolor' = NA_character_,
    'monofont' = NA_character_,
    'mouseWheel' = NA_character_,
    'navigationMode' = NA_character_,
    'overview' = NA_character_,
    'pagetitle' = NA_character_,
    'parallaxBackgroundHorizontal' = NA_character_,
    'parallaxBackgroundImage' = NA_character_,
    'parallaxBackgroundSize' = NA_character_,
    'parallaxBackgroundVertical' = NA_character_,
    'pause' = NA_character_,
    'pdfSeparateFragments' = FALSE,
    'preloadIframes' = NA_character_,
    'previewLinks' = NA_character_,
    'progress' = NA_character_,
    'quotes' = NA_character_,
    'respondToHashChanges' = NA_character_,
    'revealjs-custom-plugin-url' = NA_character_,
    'revealjs-url' = NA_character_,
    'rtl' = NA_character_,
    'scrollActivationWidth' = NA_character_,
    'scrollLayout' = NA_character_,
    'scrollProgress' = NA_character_,
    'scrollProgressAuto' = NA_character_,
    'scrollSnap' = NA_character_,
    'semester' = NA_character_,
    'sep' = NA_character_,
    'showNotes' = NA_character_,
    'showSlideNumber' = NA_character_,
    'shuffle' = NA_character_,
    'slideNumber' = NA_character_,
    'subtitle' = NA_character_,
    'table-caption-below' = NA_character_,
    'table-of-contents' = NA_character_,
    'tex-macros' = NA_character_,
    'theme' = NA_character_,
    'theme-dark' = NA_character_,
    'title' = NA_character_,
    'title-prefix' = NA_character_,
    'title-slide-attributes' = NA_character_,
    'toc' = NA_character_,
    'toc-title' = NA_character_,
    'touch' = NA_character_,
    'transition' = NA_character_,
    'transitionSpeed' = NA_character_,
    'view' = NA_character_,
    'viewDistance' = NA_character_,
    'width' = '1920',
    'year' = NA_character_
  )
}
