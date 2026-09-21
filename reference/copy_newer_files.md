# Synchronize directories by copying newer files to destination

Go through directory trees `src_path` and `dest_path`, and copy newer
files from `src_path` to `dest_path`. Don't just rely on file
modification times, but use other heuristics to check whether the files
differ.

## Usage

``` r
copy_newer_files(src_path, dest_path)
```

## Arguments

- src_path:

  DESCRIPTION.

- dest_path:

  DESCRIPTION.

## Examples

``` r
if (FALSE) { # \dontrun{
copy_newer_files("src", "dest")
} # }
```
