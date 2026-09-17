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
  if (file.mtime(from) < file.mtime(to)) return(TRUE)
  if (file.size(from) != file.size(to)) return(TRUE)
  fcrc <- digest::digest(from, "sha1", file = TRUE)
  tcrc <- digest::digest(to, "sha1", file = TRUE)
  return (fcrc != tcrc)
}

copy_newer_files <- function(src_path, dest_path) {
  src_files <- list.files(src_path)
  for (s in src_files) {
    src <- file.path(src_path, s)
    dest <- file.path(dest_path, s)
    if (files_differ(src, dest)) {
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
}

render_supporting_files_2 <- function(from, files_dir) {
  files_dir <- stringr::str_replace(files_dir, "/+$", "")
  from <- stringr::str_replace(from, "/+$", "")
  if (!dir_exists(files_dir)) {
    dir.create(files_dir)
  }
  target_dir <- file.path(files_dir, basename(from))
  if (! dir.exists(target_dir)) {
    file.copy(from = from, to = files_dir, recursive = TRUE,
              copy.mode = FALSE)
  } else {
    copy_newer_files(from, target_dir)
  }
  target_dir
}
