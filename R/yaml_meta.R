# Default metadata when a design file has little or no YAML.
# The design object remains the source of truth for parameters.

#' @noRd
default_meta <- function(id) {
  list(
    id = id,
    alias = NA_character_,
    object = "design",
    label = humanize_id(id),
    description = NA_character_,
    category = "Other",
    keywords = character(0),
    packages = character(0),
    diagnosands = character(0),
    include_in_shiny = TRUE,
    functional = TRUE,
    book_link = NA_character_,
    params = list(),
    coupled = list()
  )
}

#' Coerce YAML truthy / falsey scalars
#' @noRd
yaml_is_true <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) {
    return(isTRUE(default))
  }
  if (is.logical(x)) return(isTRUE(x[[1]]))
  identical(tolower(trimws(as.character(x)[[1]])), "true")
}

#' Split optional YAML frontmatter from an R design file
#'
#' Files may begin with a YAML block delimited by `---` lines. Everything
#' after the closing `---` is R code. Files with no frontmatter are valid.
#'
#' Results are cached by path, modification time, and size, since every
#' lookup reads the whole library.
#'
#' @param path Path to a `.R` design file.
#' @return List with `meta` (list) and `code` (character scalar).
#' @noRd
parse_design_file <- function(path) {
  info <- file.info(path, extra_cols = FALSE)
  stamp <- paste(info$mtime, info$size)
  hit <- design_file_cache[[path]]
  if (!is.null(hit) && identical(hit$stamp, stamp)) return(hit$parsed)
  parsed <- read_design_file(path)
  design_file_cache[[path]] <- list(stamp = stamp, parsed = parsed)
  parsed
}

design_file_cache <- new.env(parent = emptyenv())

#' Parse several design files, skipping any that fail
#'
#' One malformed file must not break every lookup, so each failure is a
#' warning naming the file, and the rest are returned.
#' @return The parsed files, named by path.
#' @noRd
parse_design_files <- function(files) {
  parsed <- lapply(files, function(f) {
    tryCatch(parse_design_file(f), error = function(e) {
      warning("Skipping design file ", basename(f), ": ", conditionMessage(e), call. = FALSE)
      NULL
    })
  })
  names(parsed) <- files
  Filter(Negate(is.null), parsed)
}

#' @noRd
read_design_file <- function(path) {
  if (!file.exists(path)) {
    stop("Design file not found: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines) == 0L) {
    stop("Design file is empty: ", path, call. = FALSE)
  }

  id_guess <- sub("\\.R$", "", basename(path), ignore.case = TRUE)
  meta <- default_meta(id_guess)
  code_lines <- lines

  if (grepl("^---\\s*$", lines[[1]])) {
    end <- which(grepl("^---\\s*$", lines))[-1]
    if (length(end) == 0L) {
      stop("YAML frontmatter opened but not closed in: ", path, call. = FALSE)
    }
    yaml_text <- paste(lines[seq.int(2L, end[[1]] - 1L)], collapse = "\n")
    code_lines <- if (end[[1]] < length(lines)) {
      lines[seq.int(end[[1]] + 1L, length(lines))]
    } else {
      character(0)
    }
    parsed <- tryCatch(
      yaml_load_design_meta(yaml_text),
      error = function(e) {
        stop(
          "Invalid YAML in ", path, ": ", conditionMessage(e),
          "\nTip: always quote parameter names, e.g. \"N\": \"Sample size\" (YAML treats bare N/n/Y/y as booleans).",
          call. = FALSE
        )
      }
    )
    if (!is.null(parsed) && length(parsed)) {
      meta <- merge_meta(meta, parsed, id_guess = id_guess)
    }
  }

  if (is.null(meta$id) || !nzchar(as.character(meta$id)[[1]])) {
    meta$id <- id_guess
  }
  meta$id <- as.character(meta$id)[[1]]

  list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    meta = meta,
    code = paste(code_lines, collapse = "\n")
  )
}

#' Merge user YAML onto defaults
#' @noRd
merge_meta <- function(defaults, parsed, id_guess) {
  out <- defaults
  if (!is.null(parsed$id)) out$id <- as.character(parsed$id)[[1]]
  if (!is.null(parsed$alias)) out$alias <- as.character(parsed$alias)[[1]]
  if (!is.null(parsed$object)) out$object <- as.character(parsed$object)[[1]]
  if (!is.null(parsed$label)) out$label <- as.character(parsed$label)[[1]]
  if (!is.null(parsed$description)) {
    out$description <- as.character(parsed$description)[[1]]
  }
  if (!is.null(parsed$category)) out$category <- as.character(parsed$category)[[1]]
  if (!is.null(parsed$keywords)) out$keywords <- as_chr(parsed$keywords)
  if (!is.null(parsed$packages)) out$packages <- as_chr(parsed$packages)
  if (!is.null(parsed$diagnosands)) out$diagnosands <- normalize_diagnosands(parsed$diagnosands)
  # functional: false parks a design (unavailable deps, WIP). Also accept `function:`.
  if (!is.null(parsed$functional)) {
    out$functional <- yaml_is_true(parsed$functional, default = TRUE)
  } else if (!is.null(parsed[["function"]])) {
    out$functional <- yaml_is_true(parsed[["function"]], default = TRUE)
  }
  if (!is.null(parsed$include_in_shiny)) {
    out$include_in_shiny <- yaml_is_true(parsed$include_in_shiny, default = TRUE)
  }
  # Disabled designs stay out of Shiny even if include_in_shiny was true
  if (!isTRUE(out$functional)) {
    out$include_in_shiny <- FALSE
  }
  if (!is.null(parsed$book_link)) {
    out$book_link <- as.character(parsed$book_link)[[1]]
  }
  if (!is.null(parsed$params) && is.list(parsed$params)) {
    out$params <- normalize_params_map(parsed$params)
  }
  if (!is.null(parsed$coupled)) {
    out$coupled <- normalize_coupled(parsed$coupled)
  }
  if (is.null(out$label) || !nzchar(as.character(out$label)[[1]])) {
    out$label <- humanize_id(out$id %||% id_guess)
  }
  out
}

#' Coerce YAML `coupled:` to a named list of character vectors
#'
#' ```yaml
#' coupled:
#'   m_arms: [outcome_means, outcome_sds, conditions]
#' ```
#' means changing `m_arms` requires those dependents to have compatible
#' length (typically `length == m_arms`).
#' @noRd
normalize_coupled <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  if (!is.list(x)) return(list())
  nms <- names(x)
  if (is.null(nms)) return(list())
  out <- list()
  for (i in seq_along(x)) {
    nm <- nms[[i]]
    if (is.null(nm) || !nzchar(nm) || is.na(nm)) next
    deps <- as_chr(x[[i]])
    deps <- trimws(deps)
    deps <- deps[!is.na(deps) & nzchar(deps)]
    if (length(deps)) out[[nm]] <- unname(deps)
  }
  out
}

#' English list: a; a and b; a, b, and c
#' @noRd
oxford_and <- function(x) {
  x <- as.character(x %||% character(0))
  x <- x[!is.na(x) & nzchar(x)]
  n <- length(x)
  if (n == 0L) return("")
  if (n == 1L) return(x)
  if (n == 2L) return(paste(x, collapse = " and "))
  paste0(paste(x[-n], collapse = ", "), ", and ", x[[n]])
}

#' User-facing note for one coupled driver
#' @noRd
format_coupled_note <- function(driver, dependents) {
  sprintf(
    "Changing %s requires matching-length %s.",
    as.character(driver)[[1]],
    oxford_and(dependents)
  )
}

#' Notes for every coupled driver on a metadata list
#' @noRd
coupled_notes <- function(meta) {
  coupled <- meta$coupled %||% list()
  if (!length(coupled)) return(character(0))
  nms <- names(coupled)
  if (is.null(nms)) return(character(0))
  nms <- nms[!is.na(nms) & nzchar(nms)]
  if (!length(nms)) return(character(0))
  vapply(nms, function(drv) {
    format_coupled_note(drv, coupled[[drv]])
  }, character(1), USE.NAMES = FALSE)
}

#' Coupled notes for a design id, parsed meta, or `design_info()` list
#' @noRd
coupled_help_text <- function(design) {
  if (is.list(design) && !is.null(design$coupled)) {
    return(coupled_notes(design))
  }
  coupled_notes(resolve_design(design)$meta)
}

#' Unwrap `prepare_redesign_dots()` list wrapping
#' @noRd
unwrap_redesign_value <- function(val) {
  if (is.list(val) && !is.data.frame(val) && !is.function(val) && length(val) == 1L) {
    return(val[[1]])
  }
  val
}

#' Expected length of coupled dependents given a driver value
#'
#' A length-1 numeric driver (e.g. `m_arms = 4`) is the expected length.
#' @noRd
coupled_expected_length <- function(driver_val) {
  val <- unwrap_redesign_value(driver_val)
  if (is.numeric(val) && length(val) == 1L && is.finite(val[[1]])) {
    return(as.integer(val[[1]]))
  }
  NA_integer_
}

#' Length of a coupled dependent value
#' @noRd
coupled_dep_length <- function(val) {
  val <- unwrap_redesign_value(val)
  if (is.list(val) && !is.data.frame(val) && !is.function(val)) {
    lens <- vapply(val, function(x) {
      if (is.atomic(x)) length(x) else NA_integer_
    }, integer(1))
    if (anyNA(lens)) return(NA_integer_)
    u <- unique(lens)
    if (length(u) == 1L) return(u)
    return(NA_integer_)
  }
  if (is.atomic(val)) return(length(val))
  NA_integer_
}

#' Named list of current parameter values from a design object
#' @noRd
param_value_map <- function(design, code = NULL) {
  if (is.null(design) || !inherits(design, "design")) return(list())
  params <- tryCatch(
    discover_design_params(design, code = code),
    error = function(e) NULL
  )
  if (is.null(params) || !nrow(params)) return(list())
  stats::setNames(
    lapply(seq_len(nrow(params)), function(i) params$value[[i]]),
    params$name
  )
}

#' Coupled notes that currently mismatch (dots and/or design values)
#' @noRd
coupled_issues <- function(meta, dots = list(), design = NULL, code = NULL) {
  coupled <- meta$coupled %||% list()
  if (!length(coupled)) return(character(0))
  current <- param_value_map(design, code = code)
  notes <- character(0)
  for (drv in names(coupled)) {
    if (is.null(drv) || !nzchar(drv)) next
    deps <- coupled[[drv]]
    drv_val <- if (drv %in% names(dots)) {
      unwrap_redesign_value(dots[[drv]])
    } else {
      current[[drv]]
    }
    if (is.null(drv_val)) next
    n_expect <- coupled_expected_length(drv_val)
    if (is.na(n_expect)) {
      if (drv %in% names(dots)) {
        notes <- c(notes, format_coupled_note(drv, deps))
      }
      next
    }
    ok <- TRUE
    for (dep in deps) {
      dep_val <- if (dep %in% names(dots)) {
        unwrap_redesign_value(dots[[dep]])
      } else {
        current[[dep]]
      }
      if (is.null(dep_val)) next
      n_dep <- coupled_dep_length(dep_val)
      if (is.na(n_dep) || n_dep != n_expect) {
        ok <- FALSE
        break
      }
    }
    if (!ok) notes <- c(notes, format_coupled_note(drv, deps))
  }
  unique(notes)
}

#' `message()` coupled-parameter notes; return the notes shown
#' @noRd
message_coupled_if_needed <- function(meta, dots = list(), design = NULL, code = NULL) {
  notes <- coupled_issues(meta, dots = dots, design = design, code = code)
  for (note in notes) message(note)
  invisible(notes)
}

#' Load design YAML with safer booleans for param keys like N/n
#' @noRd
yaml_load_design_meta <- function(yaml_text) {
  # YAML 1.1 treats N/n/Y/y as booleans; keep them as strings when possible.
  handlers <- list(
    "bool#yes" = function(x) x,
    "bool#no" = function(x) x
  )
  yaml::yaml.load(yaml_text, handlers = handlers)
}

#' Coerce YAML diagnosands to a character vector
#'
#' Accepts a YAML list (`[bias, power]`), a scalar (`bias`), or a
#' comma/semicolon-separated string (`"rmse, bias"`). Tokens starting with
#' `-` (e.g. `-bias`) are kept as exclusions for the display list.
#' @noRd
normalize_diagnosands <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) return(character(0))
  ch <- as_chr(x)
  parts <- unlist(strsplit(ch, "[,;]+"), use.names = FALSE)
  parts <- trimws(as.character(parts))
  parts[nzchar(parts)]
}

#' Split diagnosand tokens into preferred and excluded names
#'
#' Positive tokens (e.g. `rmse`) are preferred defaults. Tokens that start
#' with `-` (e.g. `-bias`) exclude that diagnosand from the display list.
#' @param tokens Character vector from YAML `diagnosands:`.
#' @return List with `prefer` and `exclude` character vectors.
#' @noRd
split_diagnosand_tokens <- function(tokens) {
  tokens <- as.character(tokens %||% character(0))
  tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens)]
  is_excl <- startsWith(tokens, "-")
  prefer <- tokens[!is_excl]
  exclude <- sub("^-", "", tokens[is_excl])
  exclude <- exclude[nzchar(exclude)]
  list(prefer = prefer, exclude = unique(exclude))
}

#' Coerce YAML params to a named list of tip strings
#'
#' Preferred form (always quote keys; YAML treats bare N/n/Y/y as booleans):
#' ```yaml
#' params:
#'   "N": "Number of units"
#'   "b": "Treatment effect size"
#' ```
#' Nested `{ tip: "..." }` is also accepted for compatibility.
#' @noRd
normalize_params_map <- function(params) {
  if (is.null(params) || !length(params)) return(list())
  out <- list()
  nms <- names(params)
  if (is.null(nms)) nms <- rep("", length(params))
  # Recover if keys were coerced to logical (FALSE/TRUE from N/n/Y/y)
  if (is.logical(nms)) {
    nms <- ifelse(is.na(nms), "", ifelse(nms, "Y", "N"))
  }
  nms <- as.character(nms)
  for (i in seq_along(params)) {
    nm <- nms[[i]]
    if (!nzchar(nm)) next
    entry <- params[[i]]
    if (is.null(entry)) {
      out[[nm]] <- NA_character_
    } else if (is.character(entry) || is.numeric(entry) || is.logical(entry)) {
      out[[nm]] <- as.character(entry)[[1]]
    } else if (is.list(entry)) {
      tip <- entry$tip %||% entry$tips %||% entry$description %||% entry[[1]]
      out[[nm]] <- if (is.null(tip)) NA_character_ else as.character(tip)[[1]]
    } else {
      out[[nm]] <- as.character(entry)[[1]]
    }
  }
  out
}

#' List design file paths in the package library
#' @noRd
design_files <- function(dir = designs_dir()) {
  if (!dir.exists(dir)) return(character(0))
  sort(list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE))
}
