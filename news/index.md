# Changelog

## revealjmg 2.1.2

- Replace the original
  [`rmarkdown::render_supporting_files()`](https://pkgs.rstudio.com/rmarkdown/reference/render_supporting_files.html)
  to a new
  [`render_supporting_files_2()`](https://j-magnolia.github.io/revealjmg/reference/render_supporting_files_2.md),
  which copies missing and modified files from the source directory to
  the destination. This helps keep the destination tree updated when I
  rebuild things like `reveal.js` themes and re-render documents.

## revealjmg 2.1.1

- Updated to handle putting date into the title slide and metadata.

## revealjmg 2.1.0

- Major internal refactoring to do better at merging user options and
  default options for reveal.js configuration variables.
- Avoid creating revealjs_path ending in “dist/dist”.

## revealjmg 2.0.0

- Updated to use reveal.js version 6.0
- Change package name to `revealjmg`.
