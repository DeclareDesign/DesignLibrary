#' Null-coalesce helper
#'
#' Broader than `rlang::`%||%`` / `base::`%||%``: empty and scalar `NA`
#' also fall through. The rlang import covers R < 4.4 when design files
#' or rewrite helpers look up `%||%`.
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

#' Package designs directory
#'
#' Prefers the source-tree `inst/designs` (or installed `designs/`) from
#' `find_package_root()` so maintainer refresh / `list_designs()` see local
#' edits. Falls back to `system.file()` for a normal installed package.
#' @noRd
designs_dir <- function() {
  root <- tryCatch(find_package_root(), error = function(e) NULL)
  if (!is.null(root)) {
    candidate <- package_write_paths(root)$designs
    if (dir.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }
  }
  path <- system.file("designs", package = "ResearchDesigns")
  if (nzchar(path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  stop(
    "No designs directory found. Set options(ResearchDesigns.root = ...) ",
    "to your package source, or reinstall ResearchDesigns.",
    call. = FALSE
  )
}

#' Package previews directory (read)
#' @noRd
previews_dir <- function() {
  root <- tryCatch(find_package_root(), error = function(e) NULL)
  if (!is.null(root)) {
    candidate <- package_write_paths(root)$previews
    if (dir.exists(candidate) || dir.exists(dirname(candidate))) {
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }
  }
  path <- system.file("previews", package = "ResearchDesigns")
  if (nzchar(path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  # Allow creating under source tree
  if (!is.null(root)) {
    return(normalizePath(package_write_paths(root)$previews, winslash = "/", mustWork = FALSE))
  }
  stop("No previews directory found.", call. = FALSE)
}

#' Package library-index directory (read)
#' @noRd
library_index_dir <- function() {
  root <- tryCatch(find_package_root(), error = function(e) NULL)
  if (!is.null(root)) {
    candidate <- package_write_paths(root)$index
    if (dir.exists(candidate) || dir.exists(dirname(candidate))) {
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }
  }
  path <- system.file("library_index", package = "ResearchDesigns")
  if (nzchar(path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  if (!is.null(root)) {
    return(normalizePath(package_write_paths(root)$index, winslash = "/", mustWork = FALSE))
  }
  stop("No library index directory found.", call. = FALSE)
}

#' Is this path a ResearchDesigns DESCRIPTION root?
#' @noRd
is_researchdesigns_root <- function(path) {
  desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc)) return(FALSE)
  pkg <- tryCatch(
    unname(read.dcf(desc, fields = "Package")[1, 1]),
    error = function(e) NA_character_
  )
  identical(as.character(pkg)[[1]], "ResearchDesigns")
}

#' Walk parents looking for package root
#' @noRd
walk_for_package_root <- function(start) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  if (!nzchar(current) || current == ".") return(NULL)
  for (i in seq_len(24)) {
    if (is_researchdesigns_root(current)) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  NULL
}

#' Writable package root (for maintainer refresh)
#'
#' Lookup order:
#' 1. `options(ResearchDesigns.root = ...)` or env `RESEARCHDESIGNS_ROOT`
#' 2. Walk up from `start` (default `getwd()`)
#' 3. Installed / `load_all` package path
#'
#' @param start Directory to start walking from.
#' @return Normalized path to the package root.
#' @noRd
find_package_root <- function(start = getwd()) {
  opt <- getOption("ResearchDesigns.root", NULL)
  if (is.null(opt) || !nzchar(as.character(opt)[[1]])) {
    opt <- Sys.getenv("RESEARCHDESIGNS_ROOT", unset = "")
  }
  if (nzchar(as.character(opt)[[1]])) {
    opt <- normalizePath(as.character(opt)[[1]], winslash = "/", mustWork = FALSE)
    if (is_researchdesigns_root(opt)) return(opt)
    stop(
      "ResearchDesigns.root / RESEARCHDESIGNS_ROOT is set to '", opt,
      "' but that folder has no ResearchDesigns DESCRIPTION.",
      call. = FALSE
    )
  }

  hit <- walk_for_package_root(start)
  if (!is.null(hit)) return(hit)

  pkg_path <- tryCatch(
    system.file(package = "ResearchDesigns"),
    error = function(e) ""
  )
  if (nzchar(pkg_path)) {
    hit <- walk_for_package_root(pkg_path)
    if (!is.null(hit)) return(hit)
    if (is_researchdesigns_root(pkg_path)) {
      return(normalizePath(pkg_path, winslash = "/", mustWork = FALSE))
    }
  }

  stop(
    "Could not find ResearchDesigns package root (DESCRIPTION).\n",
    "Do one of:\n",
    "  setwd(\"C:/path/to/ResearchDesigns\")\n",
    "  options(ResearchDesigns.root = \"C:/path/to/ResearchDesigns\")\n",
    "  Sys.setenv(RESEARCHDESIGNS_ROOT = \"C:/path/to/ResearchDesigns\")\n",
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
  # fabricatr before DeclareDesign, which imports it. Attach order used to be
  # load-bearing here, because the rewrite carried *Zero names and the two
  # copies of estimatr shadowed each other for the 13 names they shared. Each
  # package has one name again, so nothing shadows anything.
  c("fabricatr", "DeclareDesign", "estimatr", "randomizr")
}
