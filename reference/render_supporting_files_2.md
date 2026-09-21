# Replacement for [`rmarkdown::render_supporting_files()`](https://pkgs.rstudio.com/rmarkdown/reference/render_supporting_files.html)

The original
[`rmarkdown::render_supporting_files()`](https://pkgs.rstudio.com/rmarkdown/reference/render_supporting_files.html)
only copies files if the destination directory does not exist. This
replacement walks the source and destination trees and copies all
missing files as well as files that have been modified in the source
tree.

## Usage

``` r
render_supporting_files_2(from, files_dir)
```

## Arguments

- from:

  Source path.

- files_dir:

  Destination path.

## Details

This version omits the optional `rename_to` argument from the original.

## Examples

``` r
if (FALSE) { # \dontrun{
render_supporting_files_2("lecture_lib/library/reveal.js-6.0.0/dist",
                          "lecture_lib/library/dist")
} # }
```
