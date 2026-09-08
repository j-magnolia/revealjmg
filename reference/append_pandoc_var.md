# Append variable to pandoc variable list

Appends a new value to a pandoc variable. Discards duplicate values and
sorts the results. Handles the case where there is no previous value for
that key.

## Usage

``` r
append_pandoc_var(pvars, key, value)
```

## Arguments

- pvars:

  A named list with keys and values. Values may be vectors with length
  \> 1.

- key:

  The key (character).

- value:

  The value (any type).

## Value

An appended list of keys and values.

## Examples

``` r
if (FALSE) { # \dontrun{
pandoc_vars <- list()
pandoc_vars <- append_pandoc_var("width", 1920)
} # }
```
