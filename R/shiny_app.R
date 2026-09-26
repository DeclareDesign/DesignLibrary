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

  # If cwd already ends with dest, stay put (server re-deploy pattern)
  suffix <- sub("^/+", "", sub("/+$", "", dest_norm))
  if (nzchar(suffix) && endsWith(cwd, suffix)) return(cwd)

  out <- file.path(cwd, dest_norm)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  normalizePath(out, winslash = "/", mustWork = TRUE)
}

#' Launch the DesignLibrary Shiny browser
#'
#' @param ... Passed to [shiny::runApp()].
#' @return The value of `shiny::runApp()` (called for side effects).
#' @export
run_shiny <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package 'shiny' is required for run_shiny(). ",
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
#' remotes::install_github("DeclareDesign/DesignLibrary@RDrewrite")
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
      ". Reinstall with remotes::install_github(...) ?",
      call. = FALSE
    )
  }

  dest <- resolve_shiny_dest(dest)

  copy_one <- function(from, to) {
    if (!file.exists(from)) return(invisible(FALSE))
    if (!overwrite && file.exists(to)) return(invisible(FALSE))
    parent <- dirname(to)
    if (!dir.exists(parent)) dir.create(parent, recursive = TRUE, showWarnings = FALSE)
    file.copy(from, to, overwrite = overwrite)
    invisible(TRUE)
  }

  app_r <- file.path(src, "app.R")
  if (!file.exists(app_r)) {
    stop("Missing app.R in package Shiny directory.", call. = FALSE)
  }
  copy_one(app_r, file.path(dest, "app.R"))

  www_src <- file.path(src, "www")
  if (dir.exists(www_src)) {
    www_files <- list.files(www_src, recursive = TRUE, full.names = TRUE)
    www_root <- normalizePath(www_src, winslash = "/", mustWork = FALSE)
    for (f in www_files) {
      if (dir.exists(f)) next
      rel <- sub(paste0("^", gsub("\\\\", "/", www_root), "/?"), "", gsub("\\\\", "/", f))
      if (!nzchar(rel)) next
      copy_one(f, file.path(dest, "www", rel))
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
           encodeString(ver, quote = "\""), ")"),
    paste0("options(designlibrary.deploy_dir = ",
           encodeString(dest, quote = "\""), ")")
  )
  writeLines(stamp, file.path(dest, "deploy-options.R"), useBytes = TRUE)

  message(
    "Shiny app written to ", dest, " (DesignLibrary ", ver, "). ",
    "Point Shiny Server at this folder, then restart workers if needed."
  )
  invisible(dest)
}
