#' Contributor checklist for a design
#'
#' Used by [audit_designs()] and documented for authors.
#'
#' @return Character vector of checklist items.
#' @export
contributor_checklist <- function() {
  c(
    "File lives in inst/designs/ and is self-contained (no source() of other designs).",
    "Filename matches the substantive id (e.g. two_arm_simple.R).",
    "YAML frontmatter is optional. If present, may set id, alias (book ref), label, category, keywords, packages, diagnosands, include_in_shiny, functional, book_link, params; object: only if the design is not named `design`.",
    "No YAML is fine: id = filename stem, label = humanized id, category = Other, object = design, include_in_shiny = TRUE, functional = TRUE.",
    "Set functional: false to park a design (e.g. unavailable dependencies). Skipped by audit, smoke tests, and dependency install; forces include_in_shiny: false.",
    "The design object is the source of truth for editable parameters: only names assigned before `design <-` (e.g. N <- 1000; declare_model(N = N, ...)) count. Literals inside declare_* (e.g. declare_model(N = 1000)) are not parameters.",
    "Scalar parameters (N <- 100) can be swept with redesign(N = c(50, 100)). Vector parameters (probs <- c(.1, .2, .3)) are replaced as a whole; the browser edits them as a comma-separated list. Data frames and matrices are package parameters (make_design(..., data = ...)), not Shiny controls — prefer `data <- example_pop` over naming the example as the knob.",
    "YAML params map names to tip strings; always quote keys (e.g. \"N\": \"Sample size\", \"b\": \"Effect size\"); names must match those redesignable parameters (no extras). Design steps (model_*, inquiry_*, etc.) are not params.",
    "Optional diagnosands: preferred display diagnosands (e.g. diagnosands: rmse, bias or [rmse, bias]); prefix with - to exclude (rmse, -bias, power). Shiny Diagnosis and Redesign use these defaults.",
    "Extra packages listed under packages: and available to install.",
    "Design evaluates under DeclareDesign; redesign() works for documented parameters. A design that loads but does not run fails the audit.",
    "Run refresh_library() from the package source tree after adding or editing designs (or set options(ResearchDesigns.root = \"...\"))."
  )
}

#' Empty design-index data frame (YAML metadata columns)
#' @noRd
empty_design_index <- function() {
  data.frame(
    id = character(0),
    alias = character(0),
    label = character(0),
    category = character(0),
    keywords = character(0),
    packages = character(0),
    params = character(0),
    description = character(0),
    include_in_shiny = logical(0),
    functional = logical(0),
    file = character(0),
    stringsAsFactors = FALSE
  )
}

#' Comma-separated param names for the library TOC (no design eval)
#'
#' YAML `params:` keys plus top-level assignments before `design <-`,
#' excluding `declare_*` design pieces. Enough for listing; [get_args()]
#' remains the source of truth for redesignable names.
#' @noRd
index_params_string <- function(parsed) {
  yaml_nms <- names(parsed$meta$params %||% list())
  yaml_nms <- as.character(yaml_nms)
  yaml_nms <- yaml_nms[nzchar(yaml_nms) & !is.na(yaml_nms)]
  pre <- tryCatch(
    extract_pre_design_objects(parsed$code %||% ""),
    error = function(e) NULL
  )
  knobs <- if (!is.null(pre) && nrow(pre) && "name" %in% names(pre)) {
    nms <- as.character(pre$name)
    typ <- if ("type" %in% names(pre)) as.character(pre$type) else rep("", length(nms))
    unique(nms[!typ %in% "design_piece" & nzchar(nms) & !is.na(nms)])
  } else {
    character(0)
  }
  nms <- unique(c(knobs, yaml_nms))
  if (!length(nms)) "" else paste(nms, collapse = ", ")
}

#' Fill NA `params` from YAML + pre-design names (no design eval)
#' @noRd
fill_missing_index_params <- function(df, files) {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  if (!"params" %in% names(df)) {
    df$params <- rep(NA_character_, nrow(df))
  }
  need <- is.na(df$params)
  if (!any(need)) return(df)
  names(files) <- basename(files)
  for (i in which(need)) {
    f <- unname(files[as.character(df$file[[i]])])
    if (!length(f) || is.na(f) || !nzchar(f) || !file.exists(f)) next
    parsed <- tryCatch(parse_design_file(f[[1]]), error = function(e) NULL)
    if (is.null(parsed)) next
    df$params[[i]] <- index_params_string(parsed)
  }
  df$params[is.na(df$params)] <- ""
  df
}

#' Overlay audit-discovered param names onto an index
#' @noRd
overlay_audit_params <- function(index, audit) {
  if (!is.data.frame(index) || !nrow(index)) return(index)
  if (is.null(audit) || is.null(audit$results) || !length(audit$results)) {
    return(index)
  }
  if (!"params" %in% names(index)) {
    index$params <- rep("", nrow(index))
  }
  for (r in audit$results) {
    p <- r$params
    if (is.null(p) || !length(p)) next
    p <- as.character(p)
    p <- p[nzchar(p) & !is.na(p)]
    if (!length(p)) next
    hit <- which(as.character(index$id) == as.character(r$id[[1]]))
    if (!length(hit)) next
    index$params[[hit[[1]]]] <- paste(p, collapse = ", ")
  }
  index
}

#' One index row from a design file (YAML + cheap param names; no design eval)
#' @noRd
index_row_from_path <- function(path) {
  parsed <- parse_design_file(path)
  m <- parsed$meta
  pkgs <- m$packages %||% character(0)
  pkgs <- pkgs[nzchar(as.character(pkgs))]
  data.frame(
    id = m$id,
    alias = if (is.null(m$alias) || (length(m$alias) == 1L && is.na(m$alias))) {
      NA_character_
    } else {
      as.character(m$alias)[[1]]
    },
    label = as.character(m$label)[[1]],
    category = as.character(m$category %||% "Other")[[1]],
    keywords = paste(m$keywords %||% character(0), collapse = ", "),
    packages = if (!length(pkgs)) "" else paste(pkgs, collapse = ", "),
    params = index_params_string(parsed),
    description = if (is.null(m$description) || (length(m$description) == 1L && is.na(m$description))) {
      NA_character_
    } else {
      as.character(m$description)[[1]]
    },
    include_in_shiny = isTRUE(m$include_in_shiny),
    functional = isTRUE(m$functional),
    file = basename(path),
    stringsAsFactors = FALSE
  )
}

#' YAML scan of design files (shared by make_index and overlay)
#' @noRd
design_index_from_files <- function(files) {
  if (!length(files)) return(empty_design_index())
  rows <- lapply(files, index_row_from_path)
  normalize_index_df(do.call(rbind, rows))
}

#' Coerce a data frame to public make_index() columns and types
#' @noRd
normalize_index_df <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) {
    return(empty_design_index())
  }
  needed <- names(empty_design_index())
  for (nm in needed) {
    if (!nm %in% names(df)) {
      df[[nm]] <- if (nm %in% c("include_in_shiny", "functional")) {
        rep(TRUE, nrow(df))
      } else {
        rep(NA_character_, nrow(df))
      }
    }
  }
  chr_cols <- c(
    "id", "alias", "label", "category", "keywords", "packages", "params",
    "description", "file"
  )
  for (nm in chr_cols) {
    df[[nm]] <- as.character(df[[nm]])
  }
  df$alias[!nzchar(df$alias %||% "")] <- NA_character_
  df$description[!nzchar(df$description %||% "")] <- NA_character_
  df$include_in_shiny <- vapply(
    df$include_in_shiny,
    function(x) yaml_is_true(x, default = FALSE),
    logical(1)
  )
  df$functional <- vapply(
    df$functional,
    function(x) yaml_is_true(x, default = FALSE),
    logical(1)
  )
  df <- df[needed]
  rownames(df) <- NULL
  df
}

#' Paths to baked index artifacts
#' @noRd
baked_index_paths <- function() {
  dir <- tryCatch(library_index_dir(), error = function(e) "")
  if (!nzchar(dir)) {
    return(list(rds = "", csv = ""))
  }
  list(
    rds = file.path(dir, "designs_index.rds"),
    csv = file.path(dir, "designs_index.csv")
  )
}

#' Read baked library index (CSV first, then RDS). NULL if missing.
#'
#' CSV is the git artifact and is safer on Dropbox than a half-written RDS.
#' @noRd
read_baked_index <- function() {
  paths <- baked_index_paths()
  df <- NULL
  artifact_mtime <- NULL
  source_mtimes <- NULL

  take <- function(obj, mtime) {
    if (!is.data.frame(obj) || !"file" %in% names(obj)) {
      return(FALSE)
    }
    df <<- obj
    artifact_mtime <<- mtime
    source_mtimes <<- attr(obj, "source_mtimes")
    if (is.null(source_mtimes) && "file_mtime" %in% names(obj) && "file" %in% names(obj)) {
      source_mtimes <<- stats::setNames(as.numeric(obj$file_mtime), as.character(obj$file))
    }
    TRUE
  }

  if (nzchar(paths$csv) && file.exists(paths$csv)) {
    csv_df <- tryCatch(
      utils::read.csv(paths$csv, stringsAsFactors = FALSE, encoding = "UTF-8"),
      error = function(e) NULL
    )
    take(csv_df, file.info(paths$csv)$mtime)
  }
  if (is.null(df) && nzchar(paths$rds) && file.exists(paths$rds)) {
    rds_df <- tryCatch(readRDS(paths$rds), error = function(e) NULL)
    take(rds_df, file.info(paths$rds)$mtime)
  }
  if (is.null(df)) return(NULL)
  list(
    df = normalize_index_df(df),
    artifact_mtime = artifact_mtime,
    source_mtimes = source_mtimes
  )
}

#' Baked index plus YAML overlay for extra / missing / newer files
#'
#' Returns NULL when there is no usable artifact (caller full-scans).
#' @noRd
index_from_baked_overlay <- function(files) {
  baked <- read_baked_index()
  if (is.null(baked)) return(NULL)

  live <- basename(files)
  names(files) <- live
  baked_files <- as.character(baked$df$file)

  extra <- setdiff(live, baked_files)
  gone <- setdiff(baked_files, live)
  both <- intersect(live, baked_files)

  live_mtime <- stats::setNames(as.numeric(file.info(unname(files))$mtime), live)
  stored <- baked$source_mtimes
  if (!is.null(stored) && length(stored)) {
    stored_hit <- as.numeric(stored[both])
    newer <- both[which(is.na(stored_hit) | (!is.na(live_mtime[both]) & live_mtime[both] > stored_hit))]
  } else if (!is.null(baked$artifact_mtime) && length(both)) {
    artifact <- as.numeric(baked$artifact_mtime)
    newer <- both[which(!is.na(live_mtime[both]) & live_mtime[both] > artifact)]
  } else {
    newer <- character(0)
  }

  if (!length(extra) && !length(gone) && !length(newer)) {
    return(baked$df)
  }

  keep <- setdiff(both, newer)
  kept <- baked$df[baked$df$file %in% keep, , drop = FALSE]
  to_parse <- unname(files[c(extra, newer)])
  parsed <- design_index_from_files(to_parse)
  if (!nrow(kept)) return(parsed)
  if (!nrow(parsed)) return(normalize_index_df(kept))
  normalize_index_df(rbind(kept, parsed))
}

#' Build an in-memory index of all designs
#'
#' Shared by [list_designs()] (metadata-only default) and maintainer tools.
#' When `use_cache = TRUE`, reads the baked `inst/library_index` artifact if
#' live `.R` filenames match, and YAML-parses only extra, missing, or newer
#' files so a local `inst/designs/my_design.R` appears without refresh.
#'
#' @param use_cache If `TRUE` (default), use the baked index with a live-file
#'   overlay. If `FALSE`, parse YAML from every design file (used by
#'   [refresh_library()]).
#' @return A data frame (same columns as [list_designs()], plus description).
#'   `params` is a comma-separated name string from the baked index, or from
#'   YAML `params:` keys plus pre-design assignment names for overlay files.
#' @export
make_index <- function(use_cache = TRUE) {
  files <- design_files()
  if (!length(files)) return(empty_design_index())
  out <- NULL
  if (isTRUE(use_cache)) {
    out <- tryCatch(index_from_baked_overlay(files), error = function(e) NULL)
  }
  if (is.null(out)) {
    out <- design_index_from_files(files)
  }
  out <- fill_missing_index_params(out, files)
  out <- out[order(as.character(out$file), as.character(out$id)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Classify an audit error message into a coarse issue type
#' @noRd
classify_audit_issue <- function(msg) {
  if (is.null(msg) || length(msg) == 0L || is.na(msg) || !nzchar(msg)) {
    return(NA_character_)
  }
  msg <- as.character(msg)[[1]]
  if (grepl("needs packages not installed", msg, fixed = TRUE)) return("missing_packages")
  if (grepl("YAML params not in design", msg, fixed = TRUE)) return("yaml_extra_params")
  if (grepl("zero-length variable name", msg, fixed = TRUE)) return("param_discovery")
  if (grepl("not found after sourcing|Object '.*' not found", msg)) return("missing_object")
  if (grepl("object '.*' not found", msg, ignore.case = TRUE)) return("load_error")
  if (grepl("there is no package called", msg, fixed = TRUE)) return("missing_packages")
  "other"
}

#' Audit one or more designs
#'
#' Checks that each file loads, exposes a design object, and that any YAML
#' `params:` names are a subset of the design's modifiable parameters. Also
#' records pre-design objects that are used by the design but missing from the
#' redesignable parameter list (see [param_coverage_report()]).
#'
#' By default each design is diagnosed with a short `diagnose_design()` run
#' (`sims = 2`). A design that loads but does not run is a failure. Pass
#' `sims = NULL` only for a load-and-params scan while editing a single file;
#' `refresh_library()` and the test suite always run the design.
#'
#' Failures are collected per design and (by default) written to
#' `tools/audit_report.csv`, `tools/audit_report.md`, and
#' `tools/audit_report.txt` (FAIL/SKIP first, then OK) under the package root
#' via [write_audit_report()]. Soft notes (undocumented params, coverage gaps)
#' do not mark a design as failed.
#'
#' @param designs Character vector of ids/aliases, or `NULL` for all.
#' @param sims Number of simulations for a short `diagnose_design()` (default
#'   2). If `NULL`, skip the run check (load and params only).
#' @param write_report If `TRUE` (default), write CSV + markdown under `tools/`.
#' @param report_dir Directory for reports; default `tools/` under package root.
#' @return An object of class `research_designs_audit`.
#' @export
audit_designs <- function(
  designs = NULL,
  sims = 2,
  write_report = TRUE,
  report_dir = NULL
) {
  idx <- make_index()
  if (!is.null(designs)) {
    keys <- vapply(designs, normalize_design_key, character(1))
    keep <- idx$id %in% keys | idx$alias %in% keys | idx$file %in% paste0(keys, ".R")
    idx <- idx[keep, , drop = FALSE]
  }
  if (!nrow(idx)) {
    stop("No designs to audit.", call. = FALSE)
  }

  results <- lapply(seq_len(nrow(idx)), function(i) {
    id <- idx$id[[i]]
    row <- list(
      id = id,
      alias = idx$alias[[i]],
      file = idx$file[[i]],
      packages = idx$packages[[i]],
      ok = FALSE,
      skipped = FALSE,
      issue = NA_character_,
      error = NA_character_,
      load_ok = FALSE,
      params_ok = FALSE,
      yaml_ok = FALSE,
      sims_ok = NA,
      params = character(0),
      missing_docs = character(0),
      extra_docs = character(0),
      coverage_gaps = character(0),
      coverage_gaps_atomic = character(0),
      checklist = contributor_checklist()
    )

    # Parked designs (functional: false) - do not fail audit
    if ("functional" %in% names(idx) && !isTRUE(idx$functional[[i]])) {
      row$ok <- TRUE
      row$skipped <- TRUE
      row$issue <- "disabled"
      row$error <- "functional: false (parked; skipped by audit)"
      return(row)
    }

    parsed <- tryCatch(resolve_design(id), error = function(e) e)
    if (inherits(parsed, "error")) {
      row$error <- conditionMessage(parsed)
      row$issue <- classify_audit_issue(row$error)
      return(row)
    }

    design <- tryCatch(eval_design(parsed), error = function(e) e)
    if (inherits(design, "error")) {
      row$error <- conditionMessage(design)
      row$issue <- classify_audit_issue(row$error)
      return(row)
    }
    row$load_ok <- TRUE

    v <- tryCatch(
      validate_params_against_design(parsed$meta, design, code = parsed$code),
      error = function(e) e
    )
    if (inherits(v, "error")) {
      row$error <- conditionMessage(v)
      row$issue <- classify_audit_issue(row$error)
      # still try coverage from parsed code only
      gaps <- tryCatch(param_coverage_gaps(parsed), error = function(e) NULL)
      if (!is.null(gaps) && nrow(gaps) && !("error" %in% names(gaps) && !is.na(gaps$error[[1]]))) {
        row$coverage_gaps <- gaps$name
        row$coverage_gaps_atomic <- gaps$name[isTRUE(gaps$atomic) | gaps$atomic %in% TRUE]
      }
      return(row)
    }
    row$params_ok <- TRUE
    row$params <- v$in_design
    row$missing_docs <- v$missing_docs
    row$extra_docs <- v$extra_docs

    gaps <- tryCatch(param_coverage_gaps(parsed), error = function(e) NULL)
    if (!is.null(gaps) && nrow(gaps) && !("error" %in% names(gaps) && !is.na(gaps$error[[1]]))) {
      row$coverage_gaps <- gaps$name
      row$coverage_gaps_atomic <- gaps$name[isTRUE(gaps$atomic) | gaps$atomic %in% TRUE]
    }

    if (length(v$extra_docs)) {
      row$yaml_ok <- FALSE
      row$error <- paste0("YAML params not in design: ", paste(v$extra_docs, collapse = ", "))
      row$issue <- "yaml_extra_params"
      return(row)
    }
    row$yaml_ok <- TRUE

    if (!is.null(sims)) {
      sim_res <- tryCatch(
        DeclareDesign::diagnose_design(design, sims = as.integer(sims)),
        error = function(e) e
      )
      if (inherits(sim_res, "error")) {
        row$sims_ok <- FALSE
        row$error <- conditionMessage(sim_res)
        row$issue <- "diagnose_failed"
        return(row)
      }
      row$sims_ok <- TRUE
    }

    row$ok <- TRUE
    row
  })

  out <- structure(
    list(
      results = results,
      n = length(results),
      n_ok = sum(vapply(results, function(r) isTRUE(r$ok) && !isTRUE(r$skipped), logical(1))),
      n_skipped = sum(vapply(results, function(r) isTRUE(r$skipped), logical(1))),
      n_fail = sum(vapply(results, function(r) !isTRUE(r$ok), logical(1))),
      checklist = contributor_checklist(),
      report_paths = character(0)
    ),
    class = "research_designs_audit"
  )

  if (isTRUE(write_report)) {
    paths <- tryCatch(
      write_audit_report(out, dir = report_dir),
      error = function(e) {
        warning("Could not write audit report: ", conditionMessage(e), call. = FALSE)
        character(0)
      }
    )
    out$report_paths <- paths
  }
  out
}
#' Order audit results: FAIL, then SKIP, then OK-with-notes, then clean OK
#' @noRd
order_audit_results <- function(results) {
  rank <- vapply(results, function(r) {
    if (!isTRUE(r$ok)) return(1L)
    if (isTRUE(r$skipped)) return(2L)
    has_notes <- length(r$missing_docs) || length(r$coverage_gaps_atomic) || length(r$extra_docs)
    if (has_notes) return(3L)
    4L
  }, integer(1))
  results[order(rank, vapply(results, function(r) r$id %||% "", character(1)))]
}

#' One-line status string for an audit result
#' @noRd
format_audit_result_line <- function(r) {
  status <- if (isTRUE(r$skipped)) {
    "SKIP"
  } else if (isTRUE(r$ok)) {
    "OK"
  } else {
    "FAIL"
  }
  line <- paste0(" - ", r$id, ": ", status)
  if (!isTRUE(r$ok) || isTRUE(r$skipped)) {
    if (!is.null(r$issue) && length(r$issue) && !is.na(r$issue) && nzchar(r$issue)) {
      line <- paste0(line, " [", r$issue, "]")
    }
    if (!is.null(r$error) && length(r$error) && !is.na(r$error) && nzchar(r$error)) {
      line <- paste0(line, " - ", r$error)
    }
  }
  extras <- character(0)
  if (length(r$extra_docs)) {
    extras <- c(extras, paste0("     YAML tip for non-param: ", paste(r$extra_docs, collapse = ", ")))
  }
  if (length(r$missing_docs)) {
    extras <- c(extras, paste0("     no YAML tip (ok): ", paste(r$missing_docs, collapse = ", ")))
  }
  # Only atomic gaps are actionable; design steps/helpers are filtered upstream
  if (length(r$coverage_gaps_atomic)) {
    extras <- c(
      extras,
      paste0(
        "     assigned before design but not redesignable: ",
        paste(r$coverage_gaps_atomic, collapse = ", ")
      )
    )
  }
  if (length(extras)) paste(c(line, extras), collapse = "\n") else line
}

#' Write audit results to CSV, markdown, and plain text under `tools/`
#'
#' @param x A `research_designs_audit` object from [audit_designs()].
#' @param dir Output directory. Default: `tools/` under the package root.
#' @return Character vector of paths written (invisibly).
#' @export
write_audit_report <- function(x, dir = NULL) {
  if (!inherits(x, "research_designs_audit")) {
    stop("x must be a research_designs_audit object from audit_designs().", call. = FALSE)
  }
  if (is.null(dir)) {
    root <- tryCatch(find_package_root(), error = function(e) getwd())
    dir <- file.path(root, "tools")
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  collapse <- function(v) {
    if (is.null(v) || length(v) == 0L) return("")
    paste(as.character(v), collapse = ", ")
  }

  ordered <- order_audit_results(x$results)

  rows <- lapply(ordered, function(r) {
    data.frame(
      id = r$id %||% "",
      alias = if (is.null(r$alias) || length(r$alias) == 0L || is.na(r$alias)) "" else as.character(r$alias)[[1]],
      file = r$file %||% "",
      ok = isTRUE(r$ok),
      skipped = isTRUE(r$skipped),
      issue = if (is.null(r$issue) || length(r$issue) == 0L || is.na(r$issue)) "" else as.character(r$issue)[[1]],
      error = if (is.null(r$error) || length(r$error) == 0L || is.na(r$error)) "" else as.character(r$error)[[1]],
      load_ok = isTRUE(r$load_ok),
      params_ok = isTRUE(r$params_ok),
      yaml_ok = isTRUE(r$yaml_ok),
      packages = r$packages %||% "",
      params = collapse(r$params),
      extra_yaml = collapse(r$extra_docs),
      missing_docs = collapse(r$missing_docs),
      coverage_gaps_atomic = collapse(r$coverage_gaps_atomic),
      coverage_gaps = collapse(r$coverage_gaps),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  csv_path <- file.path(dir, "audit_report.csv")
  utils::write.csv(df, csv_path, row.names = FALSE)

  md_path <- file.path(dir, "audit_report.md")
  lines <- c(
    "# ResearchDesigns audit report",
    "",
    paste0(
      "Summary: **", x$n_ok %||% sum(vapply(x$results, function(r) isTRUE(r$ok) && !isTRUE(r$skipped), logical(1))),
      "/", x$n, "** designs OK",
      if (!is.null(x$n_skipped) && x$n_skipped > 0) paste0(", **", x$n_skipped, "** parked (`functional: false`)") else "",
      if (!is.null(x$n_fail) && x$n_fail > 0) paste0(", **", x$n_fail, "** failed") else "",
      "."
    ),
    "",
    "Issue types: `missing_packages`, `yaml_extra_params`, `param_discovery`, `load_error`, `missing_object`, `diagnose_failed`, `disabled`, `other`.",
    "",
    "Soft notes (do not fail the audit):",
    "- `no YAML tip`: redesignable param has no tip string in YAML (optional).",
    "- `assigned before design but not redesignable`: top-level `name <- ...` is used by the design but `redesign()` cannot change it (often a fixed vector/data object).",
    "Design steps (`declare_*` pieces) are not parameters. Functions assigned before `design <-` are R-only parameters.",
    "Parked designs (`functional: false`) are listed under Disabled and do not count as failures.",
    "Plain-text listing (`audit_report.txt`) puts FAIL/SKIP first, then OK.",
    ""
  )

  disabled <- Filter(function(r) isTRUE(r$skipped), ordered)
  if (length(disabled)) {
    lines <- c(lines, "## Disabled (`functional: false`)", "")
    for (r in disabled) {
      lines <- c(lines, paste0("- **", r$id, "** - ", r$error %||% "parked"))
    }
    lines <- c(lines, "")
  }

  failed <- Filter(function(r) !isTRUE(r$ok), ordered)
  if (!length(failed)) {
    lines <- c(lines, "## Failures", "", "_None._", "")
  } else {
    lines <- c(lines, "## Failures", "")
    by_issue <- split(failed, vapply(failed, function(r) {
      iss <- r$issue %||% "other"
      if (is.null(iss) || length(iss) == 0L || is.na(iss) || !nzchar(iss)) "other" else iss
    }, character(1)))
    for (iss in sort(names(by_issue))) {
      lines <- c(lines, paste0("### ", iss), "")
      for (r in by_issue[[iss]]) {
        lines <- c(
          lines,
          paste0("- **", r$id, "**", if (!is.null(r$alias) && length(r$alias) && !is.na(r$alias)) paste0(" (`", r$alias, "`)") else ""),
          paste0("  - ", r$error %||% "(no message)"),
          if (length(r$extra_docs)) paste0("  - YAML tip for non-param: ", paste(r$extra_docs, collapse = ", ")) else NULL,
          if (length(r$coverage_gaps_atomic)) {
            paste0("  - assigned but not redesignable: ", paste(r$coverage_gaps_atomic, collapse = ", "))
          } else {
            NULL
          },
          ""
        )
      }
    }
  }

  notes <- Filter(function(r) {
    isTRUE(r$ok) && !isTRUE(r$skipped) &&
      (length(r$missing_docs) || length(r$coverage_gaps_atomic))
  }, ordered)
  if (length(notes)) {
    lines <- c(lines, "## Soft notes (OK designs)", "")
    for (r in notes) {
      bits <- character(0)
      if (length(r$missing_docs)) {
        bits <- c(bits, paste0("no YAML tip: ", paste(r$missing_docs, collapse = ", ")))
      }
      if (length(r$coverage_gaps_atomic)) {
        bits <- c(bits, paste0("assigned but not redesignable: ", paste(r$coverage_gaps_atomic, collapse = ", ")))
      }
      lines <- c(lines, paste0("- **", r$id, "**: ", paste(bits, collapse = "; ")))
    }
    lines <- c(lines, "")
  }

  lines <- c(
    lines,
    "## Full table",
    "",
    "See `audit_report.csv` / `audit_report.txt` in this folder (problems first).",
    ""
  )
  writeLines(lines, md_path)

  # Plain text: problems at top, OK designs later
  txt_path <- file.path(dir, "audit_report.txt")
  header <- paste0(
    "ResearchDesigns audit: ",
    x$n_ok %||% sum(vapply(x$results, function(r) isTRUE(r$ok) && !isTRUE(r$skipped), logical(1))),
    "/", x$n, " ok",
    if (!is.null(x$n_skipped) && x$n_skipped > 0) paste0(", ", x$n_skipped, " parked") else "",
    if (!is.null(x$n_fail) && x$n_fail > 0) paste0(", ", x$n_fail, " failed") else "",
    "\n(Order: FAIL, SKIP/parked, OK-with-notes, OK)\n"
  )
  body <- vapply(ordered, format_audit_result_line, character(1))
  writeLines(c(header, body), txt_path)

  legacy <- file.path(dir, "audit_failures.txt")
  if (file.exists(legacy)) unlink(legacy)

  paths <- c(csv = csv_path, md = md_path, txt = txt_path)
  message("Audit report written:\n  ", paste(paths, collapse = "\n  "))
  invisible(paths)
}

#' @export
print.research_designs_audit <- function(x, ...) {
  cat(
    "ResearchDesigns audit: ", x$n_ok, "/", x$n, " ok",
    if (!is.null(x$n_skipped) && x$n_skipped > 0) paste0(", ", x$n_skipped, " parked") else "",
    if (!is.null(x$n_fail) && x$n_fail > 0) paste0(", ", x$n_fail, " failed") else "",
    "\n",
    sep = ""
  )
  if (length(x$report_paths)) {
    cat("Report: ", paste(x$report_paths, collapse = ", "), "\n", sep = "")
  }
  for (r in order_audit_results(x$results)) {
    cat(format_audit_result_line(r), "\n", sep = "")
  }
  invisible(x)
}


#' Bake compact diagnosis previews into inst/previews
#'
#' Each design is baked independently. Failures are collected and returned;
#' they do not abort the rest of the bake.
#'
#' @param designs Ids/aliases, or `NULL` for shiny-included designs.
#' @param sims Number of simulations (package default is 100).
#' @return Invisibly, a list with `paths` (character) and `failures`
#'   (data frame with `id` and `error`).
#' @export
bake_previews <- function(designs = NULL, sims = 100) {
  paths_info <- package_write_paths()
  out_dir <- paths_info$previews
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  idx <- make_index()
  if (is.null(designs)) {
    idx <- idx[idx$include_in_shiny, , drop = FALSE]
  } else {
    keys <- vapply(designs, normalize_design_key, character(1))
    keep <- idx$id %in% keys | idx$alias %in% keys
    idx <- idx[keep, , drop = FALSE]
  }
  empty_fail <- data.frame(id = character(0), error = character(0), stringsAsFactors = FALSE)
  if (!nrow(idx)) {
    warning("No designs selected for previews.", call. = FALSE)
    return(invisible(list(paths = character(0), failures = empty_fail)))
  }

  paths <- character(0)
  fail_ids <- character(0)
  fail_errs <- character(0)

  for (i in seq_len(nrow(idx))) {
    id <- idx$id[[i]]
    message("Baking preview: ", id, " (", i, "/", nrow(idx), ")")
    # Return the written path from tryCatch. Do not use `<<-` here:
    # tryCatch evaluates in this frame, so `<<-` would skip local `paths`
    # and refresh_library() would report 0 writes after a successful bake.
    result <- tryCatch({
      design <- make_design(id)
      diagnosis <- DeclareDesign::diagnose_design(design, sims = as.integer(sims))
      summary <- tryCatch(
        DeclareDesign::get_diagnosands(diagnosis),
        error = function(e) NULL
      )
      tidy <- tryCatch({
        if (requireNamespace("generics", quietly = TRUE)) {
          generics::tidy(diagnosis)
        } else {
          summary
        }
      }, error = function(e) summary)
      path <- file.path(out_dir, paste0(id, ".rds"))
      saveRDS(
        list(
          id = id,
          sims = as.integer(sims),
          summary = summary,
          tidy = tidy,
          diagnosis = diagnosis
        ),
        path
      )
      path
    }, error = function(e) e)

    if (inherits(result, "error")) {
      fail_ids <- c(fail_ids, id)
      fail_errs <- c(fail_errs, conditionMessage(result))
      message("  FAILED: ", conditionMessage(result))
    } else {
      paths <- c(paths, result)
    }
  }

  failures <- data.frame(id = fail_ids, error = fail_errs, stringsAsFactors = FALSE)
  if (nrow(failures)) {
    warning(
      "bake_previews(): ", nrow(failures), "/", nrow(idx),
      " failed: ", paste(failures$id, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(list(paths = paths, failures = failures))
}

#' Write index artifact under inst/library_index
#' @noRd
write_index_artifact <- function(index = make_index(use_cache = FALSE)) {
  paths_info <- package_write_paths()
  out_dir <- paths_info$index
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(out_dir, "designs_index.rds")
  csv_path <- file.path(out_dir, "designs_index.csv")
  files <- tryCatch(design_files(), error = function(e) character(0))
  mtimes <- if (length(files)) {
    stats::setNames(as.numeric(file.info(files)$mtime), basename(files))
  } else {
    numeric(0)
  }
  to_write <- index
  if (nrow(to_write)) {
    to_write$file_mtime <- as.numeric(unname(mtimes[as.character(to_write$file)]))
  } else {
    to_write$file_mtime <- numeric(0)
  }
  attr(to_write, "source_mtimes") <- mtimes

  atomic_replace <- function(tmp, dest) {
    if (file.exists(dest)) unlink(dest)
    ok <- isTRUE(file.rename(tmp, dest))
    if (!ok) {
      file.copy(tmp, dest, overwrite = TRUE)
      unlink(tmp)
    }
    dest
  }

  # CSV first: git artifact, Dropbox-safe, and what list_designs() reads.
  csv_df <- to_write
  csv_df$file_mtime <- NULL
  attr(csv_df, "source_mtimes") <- NULL
  csv_tmp <- paste0(csv_path, ".tmp")
  utils::write.csv(csv_df, csv_tmp, row.names = FALSE)
  atomic_replace(csv_tmp, csv_path)

  rds_tmp <- paste0(path, ".tmp")
  tryCatch({
    saveRDS(to_write, rds_tmp)
    atomic_replace(rds_tmp, path)
  }, error = function(e) {
    if (file.exists(rds_tmp)) unlink(rds_tmp)
    warning("Could not write designs_index.rds: ", conditionMessage(e), call. = FALSE)
  })
  csv_path
}

#' Refresh the library (maintainer one-stop)
#'
#' Runs the contributor-facing checks and refreshes baked artifacts:
#' index, audits (including a short diagnosis of every design), and
#' diagnosis previews (`sims = 100` by default).
#' Audit and preview failures are reported at the end; they do not abort
#' the refresh.
#'
#' @param sims Preview simulations (default 100).
#' @param designs Optional subset; default all for audit, shiny-on for previews.
#' @return A list with `index`, `audit`, `previews`, `ok_ids`, `preview_failures`,
#'   and `report`.
#' @export
refresh_library <- function(sims = 100, designs = NULL) {
  paths_info <- package_write_paths()
  message("ResearchDesigns refresh_library()")
  message("Package root: ", paths_info$root)
  message("Designs dir:  ", designs_dir())
  message("Checklist:")
  for (item in contributor_checklist()) message("  [ ] ", item)

  index <- make_index(use_cache = FALSE)
  write_index_artifact(index)
  message("Index: ", nrow(index), " design(s)")
  if (nrow(index)) {
    message("  ", paste(index$id, collapse = ", "))
  }

  audit <- audit_designs(designs = designs, write_report = TRUE)
  index <- overlay_audit_params(index, audit)
  write_index_artifact(index)
  print(audit)

  ok_ids <- vapply(audit$results, function(r) {
    if (isTRUE(r$ok) && !isTRUE(r$skipped)) r$id else NA_character_
  }, character(1))
  ok_ids <- ok_ids[!is.na(ok_ids)]

  audit_failed <- vapply(
    Filter(function(r) !isTRUE(r$ok), audit$results),
    function(r) r$id,
    character(1)
  )

  empty_fail <- data.frame(id = character(0), error = character(0), stringsAsFactors = FALSE)

  bake_ids <- if (!length(ok_ids)) {
    character(0)
  } else if (is.null(designs)) {
    # default bake: shiny-included intersect audit OK
    idx_ok <- index[index$id %in% ok_ids & index$include_in_shiny, , drop = FALSE]
    idx_ok$id
  } else {
    intersect(ok_ids, {
      keys <- vapply(designs, normalize_design_key, character(1))
      index$id[index$id %in% keys | index$alias %in% keys]
    })
  }

  bake <- if (length(bake_ids)) {
    bake_previews(designs = bake_ids, sims = sims)
  } else {
    list(paths = character(0), failures = empty_fail)
  }
  preview_paths <- bake$paths %||% character(0)
  preview_failures <- bake$failures %||% empty_fail

  lines <- c(
    "ResearchDesigns refresh_library() summary",
    paste0("Package root: ", paths_info$root),
    paste0("Index designs: ", nrow(index)),
    paste0(
      "Audit: ", audit$n_ok, " ok, ",
      audit$n_skipped %||% 0, " parked, ",
      audit$n_fail %||% 0, " failed"
    ),
    paste0("Previews written: ", length(preview_paths), " of ", length(bake_ids), " targets"),
    ""
  )
  if (length(audit_failed)) {
    lines <- c(lines, "Audit failures:", paste0("  - ", audit_failed), "")
  }
  if (nrow(preview_failures)) {
    lines <- c(
      lines,
      "Preview bake failures:",
      paste0("  - ", preview_failures$id, ": ", preview_failures$error),
      ""
    )
  }
  if (!length(audit_failed) && !nrow(preview_failures)) {
    lines <- c(lines, "No failures.", "")
  }
  if (length(audit$report_paths)) {
    lines <- c(lines, "Audit report:", paste0("  ", audit$report_paths), "")
  }

  report_dir <- file.path(paths_info$root, "tools")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  report_path <- file.path(report_dir, "refresh_report.txt")
  writeLines(lines, report_path)

  message("\n===== refresh_library summary =====")
  for (ln in lines) message(ln)
  message("Wrote ", report_path)

  if (length(audit_failed) || nrow(preview_failures)) {
    warning(
      "refresh_library() finished with issues. See ", report_path,
      call. = FALSE
    )
  }

  invisible(list(
    index = index,
    audit = audit,
    previews = preview_paths,
    ok_ids = ok_ids,
    preview_failures = preview_failures,
    report = report_path
  ))
}

#' Copy directory contents (Dropbox-friendly retries)
#' @noRd
copy_dir_contents <- function(from, to, tries = 6L, sleep = 1) {
  from <- normalizePath(from, winslash = "/", mustWork = TRUE)
  to <- normalizePath(to, winslash = "/", mustWork = FALSE)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)

  last_err <- NULL
  for (i in seq_len(tries)) {
    ok <- tryCatch({
      old <- list.files(to, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      if (length(old)) unlink(old, recursive = TRUE, force = TRUE)
      items <- list.files(from, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      if (!length(items)) stop("Build directory is empty: ", from, call. = FALSE)
      copied <- vapply(
        items,
        function(it) isTRUE(file.copy(it, to, recursive = TRUE, overwrite = TRUE)),
        logical(1)
      )
      if (!all(copied)) stop("Some files failed to copy into ", to, call. = FALSE)
      TRUE
    }, error = function(e) {
      last_err <<- e
      FALSE
    })
    if (isTRUE(ok)) return(invisible(to))
    Sys.sleep(sleep)
  }
  stop(
    "Could not refresh ", to, " after ", tries, " tries",
    if (!is.null(last_err)) paste0(": ", conditionMessage(last_err)) else ".",
    "\nIf the path is under Dropbox, pause sync briefly and retry.",
    call. = FALSE
  )
}

#' Build the pkgdown site (Dropbox-safe)
#'
#' On Windows Dropbox folders, `pkgdown::build_site()` / `build_article()` often
#' fail in `xml2::write_html()` with "Invalid argument" / "Error closing file"
#' when writing directly into `docs/`. This helper builds into a local temp
#' directory, then copies the result into `docs/`.
#'
#' @param pkg Package root. Default: `find_package_root()` via
#'   `options(ResearchDesigns.root=...)` or the current working directory.
#' @param ... Passed to [pkgdown::build_site()] (for example `devel = TRUE`).
#' @return Invisibly, the path to `docs/`.
#' @export
#' @examples
#' \dontrun{
#' options(ResearchDesigns.root = "C:/path/to/ResearchDesigns")
#' build_docs()
#' }
build_docs <- function(pkg = NULL, ...) {
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    stop('Install pkgdown first: install.packages("pkgdown")', call. = FALSE)
  }
  if (is.null(pkg)) {
    pkg <- tryCatch(find_package_root(), error = function(e) getwd())
  }
  pkg <- normalizePath(pkg, winslash = "/", mustWork = TRUE)
  dest <- file.path(pkg, "docs")

  tmp_root <- tempfile("ResearchDesigns-docs-")
  dir.create(tmp_root, recursive = TRUE)
  on.exit(unlink(tmp_root, recursive = TRUE, force = TRUE), add = TRUE)

  # Keep knit/pandoc intermediates off the synced drive too
  old_tmpdir <- Sys.getenv("TMPDIR", unset = "")
  old_tmp <- Sys.getenv("TMP", unset = "")
  old_temp <- Sys.getenv("TEMP", unset = "")
  Sys.setenv(TMPDIR = tmp_root, TMP = tmp_root, TEMP = tmp_root)
  on.exit({
    if (nzchar(old_tmpdir)) Sys.setenv(TMPDIR = old_tmpdir) else Sys.unsetenv("TMPDIR")
    if (nzchar(old_tmp)) Sys.setenv(TMP = old_tmp) else Sys.unsetenv("TMP")
    if (nzchar(old_temp)) Sys.setenv(TEMP = old_temp) else Sys.unsetenv("TEMP")
  }, add = TRUE)

  message("Building pkgdown site under ", tmp_root)
  pkgdown::build_site(
    pkg = pkg,
    override = list(destination = tmp_root),
    ...
  )

  message("Copying into ", dest)
  copy_dir_contents(tmp_root, dest)
  writeLines(character(0), file.path(dest, ".nojekyll"))
  message("Done. Commit docs/ and push for GitHub Pages.")
  invisible(dest)
}
