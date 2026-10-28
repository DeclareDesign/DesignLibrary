#' Path to the bundled Shiny app
#' @noRd
shiny_app_dir <- function(package = "DesignLibrary") {
  path <- system.file("shiny", package = package)
  if (!nzchar(path) || !dir.exists(path)) {
    root <- tryCatch(find_package_root(), error = function(e) NULL)
    if (!is.null(root)) path <- file.path(root, "inst", "shiny")
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Resolve deploy destination without nesting into itself
#' @noRd
resolve_shiny_dest <- function(dest = getwd()) {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (missing(dest) || is.null(dest)) return(cwd)
  dest <- as.character(dest[[1]] %||% dest)
  if (!nzchar(dest) || dest %in% c(".", "./")) return(cwd)

  dest_norm <- gsub("\\\\", "/", dest, fixed = TRUE)
  if (grepl("^/", dest_norm) || grepl(":", dest_norm, fixed = TRUE)) {
    if (!dir.exists(dest_norm)) {
      dir.create(dest_norm, recursive = TRUE, showWarnings = FALSE)
    }
    return(normalizePath(dest_norm, winslash = "/", mustWork = TRUE))
  }

  out <- file.path(cwd, dest_norm)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  normalizePath(out, winslash = "/", mustWork = TRUE)
}

#' Launch the DesignLibrary Shiny browser
#'
#' @param ... Passed to [shiny::runApp()].
#' @return The value of `shiny::runApp()` (called for side effects).
#' @examples
#' if (interactive()) {
#'   designs_shiny_app()
#' }
#' @export
designs_shiny_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package 'shiny' is required for designs_shiny_app(). ",
      "Run install_library_dependencies() first.",
      call. = FALSE
    )
  }
  app_dir <- shiny_app_dir()
  if (!file.exists(file.path(app_dir, "app.R"))) {
    stop("Bundled Shiny app not found under inst/shiny/.", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}

#' Copy the bundled Shiny app to a standalone folder
#'
#' Materializes `app.R` (and `www/` if present) so a Shiny Server / Posit
#' Connect host can point at a folder. The folder relies on the installed
#' `DesignLibrary` package for designs and helpers.
#'
#' Typical server workflow:
#' ```r
#' install.packages("DesignLibrary")
#' DesignLibrary::install_library_dependencies()
#' DesignLibrary::copy_library_shiny("/srv/shiny-server/designlibrary")
#' ```
#'
#' Existing `local.R` in `dest` is never overwritten.
#'
#' @param dest Directory to write (created if needed). Default: working directory.
#' @param overwrite If `TRUE`, replace `app.R` / `www` files (not `local.R`).
#' @param package Package that ships the app; default `"DesignLibrary"`.
#' @return Invisibly, the normalized destination path.
#' @export
copy_library_shiny <- function(
  dest = getwd(),
  overwrite = TRUE,
  package = "DesignLibrary"
) {
  src <- shiny_app_dir(package)
  if (!nzchar(src) || !dir.exists(src)) {
    stop(
      "No Shiny app found in ", package,
      ". Reinstall DesignLibrary.",
      call. = FALSE
    )
  }

  dest <- resolve_shiny_dest(dest)

  failed <- character(0)
  copy_one <- function(from, to) {
    if (!overwrite && file.exists(to)) return(invisible(FALSE))
    parent <- dirname(to)
    if (!dir.exists(parent)) dir.create(parent, recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(from, to, overwrite = overwrite)
    if (!ok) failed <<- c(failed, to)
    invisible(ok)
  }

  app_r <- file.path(src, "app.R")
  if (!file.exists(app_r)) {
    stop("Missing app.R in package Shiny directory.", call. = FALSE)
  }
  copy_one(app_r, file.path(dest, "app.R"))

  www_src <- file.path(src, "www")
  if (dir.exists(www_src)) {
    for (rel in list.files(www_src, recursive = TRUE)) {
      copy_one(file.path(www_src, rel), file.path(dest, "www", rel))
    }
  }

  example <- file.path(src, "local.R.example")
  if (file.exists(example)) {
    copy_one(example, file.path(dest, "local.R.example"))
  }

  # Deploy stamp (always overwritten); local.R may override options later
  ver <- tryCatch(
    as.character(utils::packageVersion(package)),
    error = function(e) "unknown"
  )
  stamp <- c(
    paste0("# Written by DesignLibrary::copy_library_shiny() at ",
           format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("options(designlibrary.deploy_pkg_version = ",
           encodeString(ver, quote = "\""), ")")
  )
  writeLines(stamp, file.path(dest, "deploy-options.R"), useBytes = TRUE)

  # Designs attach their packages at run time, which a manifest scanner
  # (Posit Connect) cannot see, so name them where it looks.
  pkgs <- sort(unique(c(package, "shiny", "bslib", "DT", "ggplot2", design_declared_packages())))
  writeLines(c(
    "# Written by DesignLibrary::copy_library_shiny() so that deployment",
    "# tools list every package a design needs. The app never runs this.",
    "if (FALSE) {",
    paste0("  library(", pkgs, ")"),
    "}"
  ), file.path(dest, "deps.R"))

  if (length(failed)) {
    stop("Could not copy: ", paste(failed, collapse = ", "), call. = FALSE)
  }

  message(
    "Shiny app written to ", dest, " (DesignLibrary ", ver, "). ",
    "Point Shiny Server at this folder, then restart workers if needed."
  )
  invisible(dest)
}
