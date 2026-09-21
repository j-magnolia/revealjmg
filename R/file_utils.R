
#' Check whether file needs to be updated
#'
#' Heuristics for quickly checking whether `from` should be copied to
#' `to` to synchronise files.
#'
#' @param from Source file.
#' @param to Destination file.
#'
#' @return logical: Should `from` be copied over `to`?
#' @examples
#' \dontrun{
#' files_differ("src/foo.css", "dest/foo.css")
#' }
files_differ <- function(from, to) {
  if (! file.exists(from)) {
    stop("Source file ", from, " does not exist.")
  }
  if (! file.exists(to)) return(TRUE)
  if (file.info(to)$isdir && ! file.info(from)$isdir) {
    to <- file.path(to |> stringr::str_replace("/+$", ""),
                    basename(from))
  }
  if (! file.exists(to)) return(TRUE)
  if (file.mtime(from) > file.mtime(to)) return(TRUE)
  if (file.size(from) != file.size(to)) return(TRUE)
  fcrc <- digest::digest(from, "sha1", file = TRUE)
  tcrc <- digest::digest(to, "sha1", file = TRUE)
  return (fcrc != tcrc)
}


#' Synchronize directories by copying newer files to destination
#'
#' Go through directory trees `src_path` and `dest_path`, and copy
#' newer files from `src_path` to `dest_path`. Don't just rely on
#' file modification times, but use other heuristics to check whether
#' the files differ.
#'
#' @param src_path DESCRIPTION.
#' @param dest_path DESCRIPTION.
#'
#' @return NULL
#' @examples
#' \dontrun{
#' copy_newer_files("src", "dest")
#' }
copy_newer_files <- function(src_path, dest_path) {
  # message("copy_newer_files: src = ", src_path, ", dest = ", dest_path)
  if (! dir.exists(dest_path)) {
    dir.create(dest_path)
  }
  src_files <- list.files(src_path)
  for (s in src_files) {
    src <- file.path(src_path, s)
    dest <- file.path(dest_path, s)
    if (! file.info(src)$isdir && (! file.exists(dest) ||
                                   files_differ(src, dest))) {
      # message("  copying ", src, " to ", dest)
      file.copy(from = src, to = dest, recursive = FALSE,
                overwrite = TRUE)
    }
    src_dirs <- list.dirs(src_path, full.names = FALSE,
                          recursive = FALSE) |>
      purrr::discard(~.x == ".git")
    for (sd in src_dirs) {
      copy_newer_files(file.path(src_path, sd),
                       file.path(dest_path, sd))
    }
  }
  NULL
}


#' Replacement for [rmarkdown::render_supporting_files()]
#'
#' The original [rmarkdown::render_supporting_files()] only copies
#' files if the destination directory does not exist. This replacement
#' walks the source and destination trees and copies all missing files
#' as well as files that have been modified in the source tree.
#'
#' This version omits the optional `rename_to` argument from the
#' original.
#'
#' @param from Source path.
#' @param files_dir Destination path.
#'
#' @return NULL
#' @examples
#' \dontrun{
#' render_supporting_files_2("lecture_lib/library/reveal.js-6.0.0/dist",
#'                           "lecture_lib/library/dist")
#' }
render_supporting_files_2 <- function(from, files_dir) {
  message("render_supporting_files_2: from = ", from, ", files_dir = ",
          files_dir)
  files_dir <- stringr::str_replace(files_dir, "/+$", "")
  from <- stringr::str_replace(from, "/+$", "")
  if (!dir.exists(files_dir)) {
    dir.create(files_dir)
  }
  target_dir <- file.path(files_dir, basename(from))
  if (! dir.exists(target_dir)) {
    dir.create(target_dir)
    file.copy(from = from, to = files_dir, recursive = TRUE,
              copy.mode = FALSE)
  } else {
    copy_newer_files(from, target_dir)
  }
  target_dir
}
