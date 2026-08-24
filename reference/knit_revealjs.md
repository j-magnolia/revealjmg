# Knit reveal.js presentation from Knit button in RStudio

This function calls
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
from the RStudio Knit button. To use this, add the following to the
top-level YAML in a `.Rmd` document:

## Usage

``` r
knit_revealjs(input, output_format = "revealjmg::revealjs_presentation", ...)
```

## Arguments

- input:

  Input file (`.Rmd`)

- output_format:

  output format. See
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
  for details.

- ...:

  Other arguments passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

## Value

See rmarkdown::render

## Details

    knit: revealjmg::knit_revealjs

See <https://pkg.yihui.org/rmarkdown-cookbook/custom-knit> for more
details.

## See also

[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
