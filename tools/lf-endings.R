# ------------------------------------------------------------------------------
# Force LF (Unix) line endings across the package sources.
#
# Why: `R CMD check --as-cran` reports
#     "Found the following sources/headers with CR or CRLF line endings"
# and CRAN maintainers routinely ask for LF-only sources. On Windows, Git and
# RStudio happily write CRLF, so this has to be enforced in three places:
#
#   1. .gitattributes  -> `* text=auto eol=lf` (already shipped in this repo);
#      it fixes what Git checks OUT and commits IN, for every collaborator.
#   2. This script     -> it fixes the files already on disk.
#   3. RStudio         -> Tools > Global Options > Code > Saving >
#                         "Line ending conversion: Posix (LF)".
#
# Usage:
#     source("tools/lf-endings.R")
#     lf_endings()              # dry run: only reports what would change
#     lf_endings(dry_run = FALSE)
#
# This file is listed in .Rbuildignore and is not part of the installed package.
# ------------------------------------------------------------------------------

lf_endings <- function(path = ".",
                       extensions = c("R", "r", "Rmd", "rmd", "Rd", "md",
                                      "csv", "txt", "yaml", "yml", "Rproj"),
                       extra_files = c("DESCRIPTION", "NAMESPACE", "LICENSE",
                                       "CITATION", ".Rbuildignore", ".gitignore",
                                       ".gitattributes"),
                       exclude = c("[.]Rproj[.]user", "[.]git/", "[.]Rcheck",
                                   "^renv/", "packrat"),
                       dry_run = TRUE) {
  pattern <- paste0("[.](", paste(extensions, collapse = "|"), ")$")

  all_files <- list.files(path, recursive = TRUE, all.files = TRUE,
                          full.names = TRUE)
  files <- all_files[grepl(pattern, all_files) |
                       basename(all_files) %in% extra_files]
  files <- unique(normalizePath(files, winslash = "/", mustWork = FALSE))

  for (rx in exclude) {
    files <- files[!grepl(rx, files)]
  }

  changed <- character()

  for (f in files) {
    size <- file.info(f)$size
    if (is.na(size) || size == 0) next

    raw <- readBin(f, what = "raw", n = size)
    if (!any(raw == as.raw(13L))) next          # no CR at all: nothing to do

    # Drop every CR immediately followed by LF (CRLF -> LF), then turn any
    # remaining lone CR (classic Mac line ending) into LF.
    is_cr <- raw == as.raw(13L)
    next_is_lf <- c(raw[-1] == as.raw(10L), FALSE)
    out <- raw[!(is_cr & next_is_lf)]
    out[out == as.raw(13L)] <- as.raw(10L)

    changed <- c(changed, f)
    if (!dry_run) {
      writeBin(out, f)
    }
  }

  root <- normalizePath(path, winslash = "/", mustWork = FALSE)

  if (length(changed) == 0) {
    message("All files already use LF line endings.")
  } else {
    message(if (dry_run) "Would convert " else "Converted ",
            length(changed), " file(s) to LF:")
    message(paste0("  ", sub(paste0("^", root, "/"), "", changed),
                   collapse = "\n"))
  }

  invisible(changed)
}
