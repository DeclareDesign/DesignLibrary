#' GitHub sources for Imports not on CRAN
#' @noRd
github_package_sources <- function() {
  # fabricatr before DeclareDesign (DD imports fabricatr). These build under the
  # released names, so installing them REPLACES the CRAN versions rather than
  # sitting beside them.
  c(
    fabricatr = "DeclareDesign/fabricatr@rewrite",
    estimatr = "DeclareDesign/estimatr@rewrite",
    DeclareDesign = "DeclareDesign/DeclareDesign@rewrite",
    # network_experiment needs this and it has never been on CRAN
    interference = "szonszein/interference"
  )
}

#' Packages listed in DESCRIPTION Imports / Suggests
#' @noRd
description_packages <- function(
  fields = c("Imports", "Suggests"),
  package = "DesignLibrary"
) {
  path <- system.file("DESCRIPTION", package = package)
  if (!nzchar(path) || !file.exists(path)) {
    root <- tryCatch(find_package_root(), error = function(e) NULL)
    if (!is.null(root)) path <- file.path(root, "DESCRIPTION")
  }
  if (!file.exists(path)) {
    stop("Could not find DESCRIPTION for ", package, call. = FALSE)
  }
  dcf <- read.dcf(path)
  out <- character(0)
  for (field in fields) {
    if (!field %in% colnames(dcf)) next
    raw <- dcf[1, field]
    if (is.na(raw) || !nzchar(raw)) next
    bits <- strsplit(raw, ",")[[1]]
    bits <- gsub("\\s*\\([^)]*\\)", "", bits)
    bits <- trimws(bits)
    bits <- bits[nzchar(bits) & bits != "R"]
    out <- c(out, bits)
  }
  unique(out)
}

#' Packages declared in design YAML `packages:` fields
#' @noRd
design_declared_packages <- function() {
  files <- tryCatch(design_files(), error = function(e) character(0))
  if (!length(files)) return(character(0))
  pkgs <- unlist(lapply(files, function(path) {
    m <- tryCatch(parse_design_file(path)$meta, error = function(e) NULL)
    if (is.null(m)) return(character(0))
    if (!isTRUE(m$functional %||% TRUE)) return(character(0))
    p <- m$packages %||% character(0)
    as.character(p[nzchar(as.character(p))])
  }), use.names = FALSE)
  unique(pkgs)
}

#' Install DesignLibrary system dependencies
#'
#' Installs package Imports (and, by default, Suggests needed for the Shiny
#' browser), plus any extra packages declared in design YAML `packages:` fields.
#' GitHub-only stack packages (`DeclareDesign`, `fabricatr`, `estimatr`)
#' come from the rewrite branches via `remotes::install_github()`. They
#' are never installed from CRAN.
#'
#' Typical server workflow:
#' ```r
#' remotes::install_github("DeclareDesign/DesignLibrary@RDrewrite")
#' DesignLibrary::install_library_dependencies()
#' DesignLibrary::copy_library_shiny("/path/to/shiny-app")
#' ```
#'
#' @param include_shiny If `TRUE` (default), also install Shiny Suggests
#'   (`shiny`, `bslib`, `htmltools`, `markdown`).
#' @param include_suggests If `TRUE`, install all DESCRIPTION Suggests.
#'   Ignored when `include_shiny` already covers the Shiny set unless you
#'   want test/doc packages too.
#' @param ask Passed through to install helpers when supported.
#' @param upgrade Passed to `remotes::install_github()` / `install.packages()`.
#' @param verbose Print progress messages.
#' @return Invisibly, a list with `installed`, `already_ok`, and `failed`.
#' @export
install_library_dependencies <- function(
  include_shiny = TRUE,
  include_suggests = FALSE,
  ask = FALSE,
  upgrade = "never",
  verbose = TRUE
) {
  fields <- "Imports"
  if (isTRUE(include_suggests)) fields <- c(fields, "Suggests")
  pkgs <- description_packages(fields = fields)

  shiny_pkgs <- c("shiny", "bslib", "htmltools", "markdown")
  if (isTRUE(include_shiny)) {
    pkgs <- unique(c(pkgs, shiny_pkgs))
  }

  # Design YAML may declare extras beyond DESCRIPTION
  pkgs <- unique(c(pkgs, design_declared_packages()))

  # Never try to install ourselves this way
  pkgs <- setdiff(pkgs, "DesignLibrary")

  # fabricatr before DeclareDesign (DD imports fabricatr); others independent
  prefer <- c("fabricatr", "estimatr", "DeclareDesign", "randomizr")
  pkgs <- unique(c(intersect(prefer, pkgs), setdiff(pkgs, prefer)))

  gh <- github_package_sources()
  already_ok <- character(0)
  installed <- character(0)
  failed <- character(0)

  say <- function(...) if (isTRUE(verbose)) message(...)

  github_stack_stale <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) return(FALSE)
    if (identical(pkg, "DeclareDesign")) {
      return(!("declare_parameters" %in% getNamespaceExports("DeclareDesign")))
    }
    if (identical(pkg, "estimatr")) {
      return(utils::packageVersion("estimatr") < "2.0.0")
    }
    if (identical(pkg, "fabricatr")) {
      return(utils::packageVersion("fabricatr") < "2.0.0")
    }
    FALSE
  }

  for (pkg in pkgs) {
    present <- requireNamespace(pkg, quietly = TRUE)
    if (present && !github_stack_stale(pkg)) {
      already_ok <- c(already_ok, pkg)
      next
    }
    if (present && github_stack_stale(pkg)) {
      say(pkg, " is a CRAN build; replacing it with the GitHub rewrite.")
    }

    ok <- FALSE
    if (pkg %in% names(gh)) {
      if (!requireNamespace("remotes", quietly = TRUE)) {
        say("Installing remotes to fetch ", pkg, " from GitHub...")
        utils::install.packages("remotes", quiet = !verbose)
      }
      if (!requireNamespace("remotes", quietly = TRUE)) {
        failed <- c(failed, pkg)
        say("Failed: need remotes to install ", pkg, " (", gh[[pkg]], ")")
        next
      }
      say("Installing ", pkg, " from GitHub (", gh[[pkg]], ")...")
      ok <- tryCatch({
        remotes::install_github(
          gh[[pkg]],
          upgrade = upgrade,
          quiet = !verbose,
          ask = ask
        )
        requireNamespace(pkg, quietly = TRUE)
      }, error = function(e) {
        say("Failed ", pkg, ": ", conditionMessage(e))
        FALSE
      })
    } else if (pkg %in% c("DeclareDesign", "fabricatr", "estimatr")) {
      failed <- c(failed, pkg)
      say(
        "Refusing CRAN for ", pkg, "; install the GitHub rewrite ",
        "(DeclareDesign/fabricatr@rewrite, DeclareDesign/estimatr@rewrite, ",
        "DeclareDesign/DeclareDesign@rewrite)."
      )
      next
    } else {
      say("Installing ", pkg, " from CRAN...")
      ok <- tryCatch({
        utils::install.packages(
          pkg,
          quiet = !verbose,
          ask = ask
        )
        requireNamespace(pkg, quietly = TRUE)
      }, error = function(e) {
        say("Failed ", pkg, ": ", conditionMessage(e))
        FALSE
      })
    }

    if (isTRUE(ok)) {
      installed <- c(installed, pkg)
    } else {
      failed <- c(failed, pkg)
    }
  }

  if (length(failed)) {
    warning(
      "Could not install: ", paste(failed, collapse = ", "),
      call. = FALSE
    )
  } else {
    say("Library dependencies OK (", length(already_ok), " present, ",
        length(installed), " newly installed).")
  }

  invisible(list(
    installed = installed,
    already_ok = already_ok,
    failed = failed
  ))
}
