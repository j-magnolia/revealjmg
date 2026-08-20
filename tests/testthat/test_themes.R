
context("Themes")

test_theme <- function(theme, rjs_ver = 3) {
  new_themes <- c("black-contrast", "dracula", "white-contrast")
  if (theme %in% new_themes) {
    return(NULL)
  }
  if(identical(theme, "custom"))
    return(NULL)
  test_that(stringr::str_c(theme, "theme, version", rjs_ver,
                           sep = " "), {
    # don't run on cran because pandoc is required
    skip_on_cran()

    # work in a temp directory
    tmpdir <- tempdir(check = TRUE)
    tstdir <- tempfile("revealjmg-check", tmpdir)
    dir.create(tstdir)

    # create a draft of a presentation
    testdoc <- file.path(tstdir, "testdoc.Rmd")
    rmd_file <- rmarkdown::draft(
      testdoc,
      system.file("rmarkdown", stringr::str_c("revealjs-", rjs_ver),
                  "templates", "revealjs_presentation",
                  package = "revealjmg"),
      create_dir = FALSE,
      edit = FALSE
      )

    # render it with the specified theme

    capture.output({
      expect_true(rmarkdown::pandoc_available())
      expect_true(dir.exists(tstdir))
      expect_true(file.exists(rmd_file))
      output_file <- "testdoc.html"
      output_format <- revealjs_presentation(theme = theme)
      rmarkdown::render(rmd_file,
                        output_format = output_format,
                        output_file = output_file)
      expect_true(file.exists(file.path(dirname(rmd_file),
                                        output_file)))
    })
  })
}

# test all themes
if (rmarkdown::pandoc_available()) {
  sapply(c(3, 6), function(v) {
    sapply(revealjmg:::revealjs_themes(), test_theme, rjs_ver = v)
  })
}
