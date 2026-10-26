#' Null-coalesce helper
#'
#' Broader than `rlang::`%||%`` / `base::`%||%``: empty and scalar `NA`
#' also fall through.
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

#' Snapshot the global RNG state
#'
#' Returns a function that puts `.Random.seed` back as it was, removing it
#' if it did not exist. Use as `restore <- seed_restorer(); on.exit(restore())`.
#' @noRd
seed_restorer <- function() {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv())
  function() {
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }
}

#' Root that designs, previews, and the index are read from
#'
#' `options(DesignLibrary.root = ...)` or `DESIGNLIBRARY_ROOT` when set,
#' otherwise the loaded package (the source tree under `load_all()`, whose
#' `system.file()` shim points there). The working directory is never
#' consulted, so a cwd inside an old checkout cannot mix one tree's designs
#' with another tree's previews and index. Maintainer writers pin the root
#' they write to for the rest of their call; see `source_write_paths()`.
#' @noRd
library_root <- function() {
  if (nzchar(root_override())) return(find_package_root())
  path <- system.file(package = "DesignLibrary")
  if (!nzchar(path)) {
    stop(
      "DesignLibrary is not installed. Set options(DesignLibrary.root = ...) ",
      "to your package source.",
      call. = FALSE
    )
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Package designs directory
#' @noRd
designs_dir <- function() {
  path <- package_write_paths(library_root())$designs
  if (!dir.exists(path)) {
    stop("No designs directory at ", path, ".", call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Package previews directory (read)
#' @noRd
previews_dir <- function() {
  normalizePath(package_write_paths(library_root())$previews, winslash = "/", mustWork = FALSE)
}

#' Package library-index directory (read)
#' @noRd
library_index_dir <- function() {
  normalizePath(package_write_paths(library_root())$index, winslash = "/", mustWork = FALSE)
}

#' Root set by option or environment variable, or `""`
#' @noRd
root_override <- function() {
  opt <- getOption("DesignLibrary.root", NULL)
  if (is.null(opt) || !nzchar(as.character(opt)[[1]])) {
    opt <- Sys.getenv("DESIGNLIBRARY_ROOT", unset = Sys.getenv("RESEARCHDESIGNS_ROOT", unset = ""))
  }
  as.character(opt)[[1]]
}

#' Is this path a DesignLibrary DESCRIPTION root?
#' @noRd
is_designlibrary_root <- function(path) {
  desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc)) return(FALSE)
  pkg <- tryCatch(
    unname(read.dcf(desc, fields = "Package")[1, 1]),
    error = function(e) NA_character_
  )
  identical(as.character(pkg)[[1]], "DesignLibrary")
}

#' Walk parents looking for package root
#' @noRd
walk_for_package_root <- function(start) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  if (!nzchar(current) || current == ".") return(NULL)
  for (i in seq_len(24)) {
    if (is_designlibrary_root(current)) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  NULL
}

#' Writable package root (for maintainer refresh)
#'
#' Lookup order:
#' 1. `options(DesignLibrary.root = ...)` or env `DESIGNLIBRARY_ROOT`
#'    (`RESEARCHDESIGNS_ROOT` is still read as a fallback)
#' 2. Walk up from `start` (default `getwd()`)
#' 3. Installed / `load_all` package path
#'
#' @param start Directory to start walking from.
#' @return Normalized path to the package root.
#' @noRd
find_package_root <- function(start = getwd()) {
  opt <- root_override()
  if (nzchar(opt)) {
    opt <- normalizePath(opt, winslash = "/", mustWork = FALSE)
    if (is_designlibrary_root(opt)) return(opt)
    stop(
      "DesignLibrary.root / DESIGNLIBRARY_ROOT is set to '", opt,
      "' but that folder has no DesignLibrary DESCRIPTION.",
      call. = FALSE
    )
  }

  hit <- walk_for_package_root(start)
  if (!is.null(hit)) return(hit)

  pkg_path <- tryCatch(
    system.file(package = "DesignLibrary"),
    error = function(e) ""
  )
  if (nzchar(pkg_path)) {
    hit <- walk_for_package_root(pkg_path)
    if (!is.null(hit)) return(hit)
    if (is_designlibrary_root(pkg_path)) {
      return(normalizePath(pkg_path, winslash = "/", mustWork = FALSE))
    }
  }

  stop(
    "Could not find DesignLibrary package root (DESCRIPTION).\n",
    "Do one of:\n",
    "  setwd(\"C:/path/to/DesignLibrary\")\n",
    "  options(DesignLibrary.root = \"C:/path/to/DesignLibrary\")\n",
    "  Sys.setenv(DESIGNLIBRARY_ROOT = \"C:/path/to/DesignLibrary\")\n",
    "then re-run refresh_library().",
    call. = FALSE
  )
}

#' Paths for writing designs / previews / index
#'
#' Source trees use `inst/...`; installed packages use top-level `designs/` etc.
#' @noRd
package_write_paths <- function(root = find_package_root()) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (dir.exists(file.path(root, "inst"))) {
    list(
      root = root,
      source_tree = TRUE,
      designs = file.path(root, "inst", "designs"),
      previews = file.path(root, "inst", "previews"),
      # Not "index": on Windows that collides with the package INDEX file
      index = file.path(root, "inst", "library_index")
    )
  } else {
    list(
      root = root,
      source_tree = FALSE,
      designs = file.path(root, "designs"),
      previews = file.path(root, "previews"),
      index = file.path(root, "library_index")
    )
  }
}

#' Write paths for maintainer tools, refusing an installed package
#'
#' Maintainer tools write previews, the index, and reports. They must never
#' write into a library under `.libPaths()`, so a root without `inst/` stops.
#' The root found here is also set as `DesignLibrary.root` until `envir`
#' exits, so the designs a tool reads come from the tree it writes to.
#' @noRd
source_write_paths <- function(envir = parent.frame()) {
  paths <- package_write_paths()
  if (!isTRUE(paths$source_tree)) {
    stop(
      "Maintainer tools write only into a DesignLibrary source tree, and '",
      paths$root, "' is an installed package. Work from inside the source ",
      "tree or set options(DesignLibrary.root = \"path/to/DesignLibrary\").",
      call. = FALSE
    )
  }
  old <- options(DesignLibrary.root = paths$root)
  do.call(on.exit, list(substitute(options(old), list(old = old)), add = TRUE), envir = envir)
  paths
}

#' Humanize an id into a short label
#' @noRd
humanize_id <- function(id) {
  id <- as.character(id[[1]] %||% id)
  id <- gsub("[_.]+", " ", id)
  paste0(toupper(substring(id, 1L, 1L)), substring(id, 2L))
}

#' Ensure character vector from yaml scalar/list
#' @noRd
as_chr <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) return(character(0))
  as.character(unlist(x, use.names = FALSE))
}

#' Attach packages quietly; return missing names
#' @noRd
ensure_packages_attached <- function(pkgs) {
  missing <- character(0)
  for (p in unique(as_chr(pkgs))) {
    if (!nzchar(p)) next
    ok <- suppressPackageStartupMessages(requireNamespace(p, quietly = TRUE))
    if (!ok) {
      missing <- c(missing, p)
      next
    }
    suppressPackageStartupMessages(suppressWarnings(
      library(p, character.only = TRUE, quietly = TRUE)
    ))
  }
  missing
}

#' Core stack always available when loading designs
#' @noRd
core_packages <- function() {
  # library(DesignLibrary) attaches all four through DeclareDesign's Depends,
  # so attaching them here matters only when DesignLibrary is used through
  # `::` without being attached: the design files call them by name.
  c("fabricatr", "DeclareDesign", "estimatr", "randomizr")
}
