# Check whether file needs to be updated

Heuristics for quickly checking whether `from` should be copied to `to`
to synchronise files.

## Usage

``` r
files_differ(from, to)
```

## Arguments

- from:

  Source file.

- to:

  Destination file.

## Value

logical: Should `from` be copied over `to`?

## Examples

``` r
if (FALSE) { # \dontrun{
files_differ("src/foo.css", "dest/foo.css")
} # }
```
