#' Empty parameter table used by discovery helpers
#' @noRd
empty_params_df <- function() {
  data.frame(
    name = character(0),
    value_str = character(0),
    value = I(list()),
    step = integer(0),
    kind = character(0),
    declared = logical(0),
    shiny = logical(0),
    stringsAsFactors = FALSE
  )
}

#' Discover modifiable parameters from a design object
#'
#' Uses DeclareDesignZero's object finder. When `code` is supplied, only names
#' that also appear as top-level assignments before `design <-` are kept — so
#' literal step arguments like `se_type = "stata"` are not treated as knobs.
#' Functions assigned before `design <-` are knobs (`kind = "function"`).
#' Pre-design assignments the finder missed (or an error in the finder) are
#' still exposed.
#'
#' @param design A DeclareDesignZero `design` object.
#' @param code Optional design file code (without YAML) used to restrict to
#'   author-assigned knobs.
#' @return Data frame with `name`, `value_str`, `value`, `step`, `kind`, `shiny`.
#' @noRd
discover_design_params <- function(design, code = NULL) {
  objs <- tryCatch(
    DeclareDesignZero:::find_all_objects(design),
    error = function(e) NULL
  )
  params <- filter_modifiable_params(objs)
  if (!is.null(code) && nzchar(trimws(code))) {
    pre <- extract_pre_design_objects(code)
    # Author knobs: top-level assignments before design <-, including functions.
    # Literals like declare_model(N = 1000) and declare_* pieces are not knobs.
    knobs <- if (nrow(pre)) {
      unique(pre$name[!pre$type %in% "design_piece"])
    } else {
      character(0)
    }
    # A `declare_parameters()` step is the design stating its own knobs, so it
    # outranks the source-text scan: a ported design has no pre-design
    # assignment left for `extract_pre_design_objects()` to find.
    params <- params[params$name %in% knobs | params$declared, , drop = FALSE]
    missing <- setdiff(knobs, params$name)
    if (length(missing)) {
      extra <- params_from_pre_assignments(pre, missing)
      if (nrow(extra)) {
        params <- rbind(params, extra)
      }
    }
    if (nrow(params) && nrow(pre)) {
      ord <- match(params$name, pre$name)
      params <- params[order(is.na(ord), ord), , drop = FALSE]
    }
    rownames(params) <- NULL
  }
  params
}

#' Longest atomic vector still treated as a Shiny text-box parameter
#' @noRd
shiny_vector_max <- function() 20L

#' Classify a redesignable value: scalar, vector, data, or function
#'
#' `scalar` — length-1 atomic, no dim. Shiny treats comma-separated input as a
#' sweep (`redesign(N = c(50, 100))`).
#' `vector` — short atomic vector, no dim. Shiny edits the whole vector as one
#' value; wrap in `list()` for `redesign()` so it is not expanded as a sweep.
#' `data` — data frame, matrix/array, or a longer atomic vector. Package
#' parameter (`make_design(..., data = ...)`); not a Shiny control.
#' `function` — a function (for example an outcome `Y`). R-only; not a Shiny
#' control (`make_design(..., Y = ...)`).
#'
#' @param val A parameter value.
#' @return `"scalar"`, `"vector"`, `"data"`, or `"function"`.
#' @noRd
classify_param_kind <- function(val) {
  if (is.function(val)) return("function")
  if (is.null(val)) return("scalar")
  if (is.data.frame(val)) return("data")
  if (is.atomic(val) && !is.null(dim(val))) return("data")
  if (is.atomic(val)) {
    n <- length(val)
    if (n <= 1L) return("scalar")
    if (n <= shiny_vector_max()) return("vector")
    return("data")
  }
  "data"
}

#' Whether a kind is offered as a Shiny text box
#' @noRd
is_shiny_param_kind <- function(kind) {
  kind %in% c("scalar", "vector")
}

#' Wrap vector replacements so redesign() does not treat them as a sweep
#'
#' DeclareDesign expands an atomic vector of scalars into one design per value.
#' A parameter whose default is already a vector must be passed as
#' `list(c(...))` to replace the vector, or `list(v1, v2)` to sweep vectors.
#'
#' @param params Data frame from discover_design_params().
#' @param dots Named list of user arguments.
#' @return `dots` with vector-kind atomics wrapped in `list()`.
#' @noRd
prepare_redesign_dots <- function(params, dots) {
  if (!length(dots) || is.null(params) || !nrow(params)) return(dots)
  kinds <- if ("kind" %in% names(params)) params$kind else {
    vapply(params$value, classify_param_kind, character(1))
  }
  for (nm in names(dots)) {
    if (is.null(nm) || !nzchar(nm)) next
    i <- match(nm, params$name)
    if (is.na(i)) next
    if (!identical(kinds[[i]], "vector")) next
    val <- dots[[nm]]
    if (!is.list(val)) dots[[nm]] <- list(val)
  }
  dots
}

#' Whether a value can be a redesign target
#' @noRd
is_modifiable_value <- function(val) {
  if (is.null(val)) return(TRUE)
  if (is.function(val)) return(TRUE)
  if (is.data.frame(val)) return(TRUE)
  is.atomic(val)
}

#' Drop non-redesignable objects; dedupe by name
#'
#' Keeps atomic values (including matrices and short/long vectors), data
#' frames, and functions. Other lists are dropped.
#' @noRd
filter_modifiable_params <- function(objs) {
  empty <- empty_params_df()
  if (is.null(objs) || !nrow(objs)) return(empty)

  keep <- logical(nrow(objs))
  values <- vector("list", nrow(objs))

  for (i in seq_len(nrow(objs))) {
    name <- objs$name[[i]]
    # find_all_objects can emit "" / character(0) (anonymous formulas); get("") errors
    name <- tryCatch({
      if (is.null(name) || length(name) < 1L) {
        ""
      } else {
        as.character(name)[[1L]]
      }
    }, error = function(e) "")
    if (is.na(name) || !nzchar(name)) {
      keep[[i]] <- FALSE
      next
    }
    env <- objs$env[[i]]
    val <- NULL
    if (!is.null(env)) {
      val <- tryCatch(
        get(name, envir = env, inherits = FALSE),
        error = function(e) NULL
      )
      if (!is.null(val) && (is.symbol(val) || is.name(val))) {
        val <- tryCatch(eval(val, envir = env), error = function(e) NULL)
      }
    }
    if (is.null(val) && nzchar(objs$value_str[[i]] %||% "")) {
      val <- tryCatch(
        eval(parse(text = objs$value_str[[i]]), envir = baseenv()),
        error = function(e) NULL
      )
    }
    values[[i]] <- val
    keep[[i]] <- is_modifiable_value(val)
  }

  out <- objs[keep, , drop = FALSE]
  values <- values[keep]
  if (nrow(out) && "name" %in% names(out)) {
    # A declared parameter appears once for the `declare_parameters()` step and
    # once for the step that reads it, and only the reading row carries an
    # environment the value can be read out of. Taking the first row blindly
    # gives a NULL value, which classifies as "scalar": a vector parameter then
    # loses its `list()` wrapper and `redesign()` sweeps it instead of setting
    # it. Keep, per name, the first row whose value is recoverable.
    keep_i <- vapply(unique(out$name), function(nm) {
      idx <- which(out$name == nm)
      with_val <- idx[!vapply(values[idx], is.null, logical(1))]
      if (length(with_val)) with_val[[1L]] else idx[[1L]]
    }, integer(1))
    keep_i <- sort(keep_i)
    out <- out[keep_i, , drop = FALSE]
    values <- values[keep_i]
  }

  kinds <- vapply(values, classify_param_kind, character(1))
  data.frame(
    name = out$name,
    value_str = out$value_str,
    value = I(values),
    step = out$step,
    kind = kinds,
    declared = if ("declared" %in% names(out)) as.logical(out$declared) else FALSE,
    shiny = is_shiny_param_kind(kinds),
    stringsAsFactors = FALSE
  )
}

#' Compare YAML-documented params to design params
#' @noRd
validate_params_against_design <- function(meta, design, code = NULL) {
  in_design <- discover_design_params(design, code = code)$name
  documented <- names(meta$params %||% list())
  if (is.null(documented)) documented <- character(0)

  list(
    # Superfluous YAML names are the hard problem; missing tips are soft
    ok = length(setdiff(documented, in_design)) == 0L,
    in_design = in_design,
    documented = documented,
    missing_docs = setdiff(in_design, documented),
    extra_docs = setdiff(documented, in_design)
  )
}

#' Parse an RHS fragment, treating an empty one as incomplete
#'
#' `parse(text = "")` succeeds and returns a zero-length expression, so a
#' fragment that is only whitespace has to read as "not finished yet" or the
#' caller stops accumulating continuation lines the moment it sees a `<-` at
#' the end of a line.
#' @noRd
parse_rhs <- function(rhs) {
  expr <- tryCatch(parse(text = rhs), error = function(e) NULL)
  if (is.null(expr) || !length(expr)) return(NULL)
  expr
}

#' Top-level assignments appearing before `design <-` in a design file
#'
#' @param code Character scalar (R code without YAML).
#' @return Data frame with `name`, `rhs`, `type`, `atomic`.
#' @noRd
extract_pre_design_objects <- function(code) {
  empty <- data.frame(
    name = character(0),
    rhs = character(0),
    type = character(0),
    atomic = logical(0),
    stringsAsFactors = FALSE
  )
  if (!nzchar(trimws(code %||% ""))) return(empty)

  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  des_i <- which(grepl("^\\s*design\\s*<-", lines))
  if (!length(des_i)) return(empty)
  head_lines <- lines[seq_len(des_i[[1]] - 1L)]

  names_out <- character(0)
  rhs_out <- character(0)
  type_out <- character(0)
  atomic_out <- logical(0)

  i <- 1L
  n <- length(head_lines)
  while (i <= n) {
    ln <- head_lines[[i]]
    i <- i + 1L
    if (grepl("^\\s*#", ln) || !nzchar(trimws(ln))) next
    # `<-` or a top-level `=`; `==` is a comparison, not an assignment
    if (!grepl("^\\s*[A-Za-z.][A-Za-z0-9._]*\\s*(<-|=(?!=))\\s*", ln, perl = TRUE)) next
    if (grepl("^\\s*(if|for|while)\\b", ln)) next
    nm <- sub("^\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*(<-|=).*$", "\\1", ln)
    rhs <- sub("^\\s*[A-Za-z.][A-Za-z0-9._]*\\s*(<-|=)\\s*", "", ln)

    # Accumulate continuation lines until the RHS parses (multi-line declare_* etc.).
    # An RHS of "" parses to a zero-length expression, so a `name <-` that ends
    # the line has to keep reading or every multi-line assignment reads as empty.
    expr <- parse_rhs(rhs)
    while (is.null(expr) && i <= n) {
      rhs <- paste(rhs, head_lines[[i]], sep = "\n")
      i <- i + 1L
      expr <- parse_rhs(rhs)
    }
    rhs <- trimws(rhs)

    if (grepl("\\bfunction\\s*\\(", rhs)) {
      names_out <- c(names_out, nm)
      rhs_out <- c(rhs_out, rhs)
      type_out <- c(type_out, "function")
      atomic_out <- c(atomic_out, FALSE)
      next
    }

    # Prefer structural cues before eval (eval of declare_* needs the DD stack)
    # Note: do not use \\b after "declare_" — "_" is a word char, so declare_model
    # would not match. as_tibble() builds a data frame (a data knob), not a step.
    if (grepl("(declare_|make_model\\(|add_level\\(|fabricate\\()", rhs)) {
      names_out <- c(names_out, nm)
      rhs_out <- c(rhs_out, rhs)
      type_out <- c(type_out, "design_piece")
      atomic_out <- c(atomic_out, FALSE)
      next
    }

    val <- tryCatch(eval(parse(text = rhs), envir = baseenv()), error = function(e) NULL)
    typ <- if (is.null(val)) {
      "other"
    } else if (is.function(val)) {
      "function"
    } else if (is.atomic(val)) {
      paste0(typeof(val), if (length(val) != 1L) paste0("[", length(val), "]") else "")
    } else {
      paste(class(val), collapse = "/")
    }
    is_atom <- !is.null(val) && is.atomic(val) && !is.function(val)
    names_out <- c(names_out, nm)
    rhs_out <- c(rhs_out, rhs)
    type_out <- c(type_out, typ)
    atomic_out <- c(atomic_out, is_atom)
  }

  if (!length(names_out)) return(empty)
  # last assignment wins for duplicates
  keep <- !duplicated(names_out, fromLast = TRUE)
  data.frame(
    name = names_out[keep],
    rhs = rhs_out[keep],
    type = type_out[keep],
    atomic = atomic_out[keep],
    stringsAsFactors = FALSE
  )
}

#' Display string for a discovered parameter value
#' @noRd
describe_param_value_str <- function(val, rhs = "") {
  if (is.function(val)) return("function")
  if (is.null(val)) return(trimws(rhs %||% ""))
  if (is.atomic(val) && is.null(dim(val)) && length(val) <= 5L) {
    return(paste(deparse(val), collapse = ""))
  }
  if (is.data.frame(val)) {
    return(sprintf("<data.frame[%s x %s]>", nrow(val), ncol(val)))
  }
  paste0("<", class(val)[[1L]], ">")
}

#' Parameter rows from pre-design assignments the object finder missed
#'
#' Evaluates top-level assignments in order so lazy vectors (e.g. `rep(0, m_arms)`)
#' can resolve. Used when `find_all_objects()` errors or skips closures.
#' @noRd
params_from_pre_assignments <- function(pre, names) {
  empty <- empty_params_df()
  if (!length(names) || is.null(pre) || !nrow(pre)) return(empty)
  names <- unique(as.character(names))
  names <- names[!is.na(names) & nzchar(names)]
  if (!length(names)) return(empty)

  env <- new.env(parent = globalenv())
  for (i in seq_len(nrow(pre))) {
    if (identical(pre$type[[i]], "design_piece")) next
    nm <- pre$name[[i]]
    if (is.null(nm) || !nzchar(nm)) next
    expr <- tryCatch(parse(text = pre$rhs[[i]]), error = function(e) NULL)
    if (is.null(expr) || !length(expr)) next
    tryCatch(
      assign(nm, eval(expr, envir = env), envir = env),
      error = function(e) NULL
    )
  }

  keep_names <- character(0)
  values <- list()
  value_str <- character(0)
  for (nm in names) {
    if (!exists(nm, envir = env, inherits = FALSE)) next
    val <- get(nm, envir = env, inherits = FALSE)
    if (!is_modifiable_value(val)) next
    keep_names <- c(keep_names, nm)
    values <- c(values, list(val))
    rhs <- pre$rhs[match(nm, pre$name)]
    value_str <- c(value_str, describe_param_value_str(val, rhs))
  }
  if (!length(keep_names)) return(empty)
  kinds <- vapply(values, classify_param_kind, character(1))
  data.frame(
    name = keep_names,
    value_str = value_str,
    value = I(values),
    step = rep(NA_integer_, length(keep_names)),
    kind = kinds,
    shiny = is_shiny_param_kind(kinds),
    stringsAsFactors = FALSE
  )
}

#' Whether a symbol name appears used in design code (word boundary)
#' @noRd
symbol_used_in_code <- function(name, code) {
  if (!nzchar(name) || !nzchar(code)) return(FALSE)
  grepl(paste0("\\b", gsub("\\.", "\\\\.", name), "\\b"), code, perl = TRUE)
}

#' Objects declared before design, used by it, but missing from design params
#'
#' Runs the design, reads `discover_design_params()`, and compares to top-level
#' assignments before `design <-`. Flags names that are used in the design body
#' (or seen by DeclareDesignZero's object finder) but not in the redesignable
#' parameter list.
#'
#' Design steps (MIDA pieces built with `declare_*`, etc.) are not parameters
#' and are omitted unless `include_steps = TRUE`. Functions assigned before
#' `design <-` are R-only parameters.
#'
#' @param design Design id/alias, or a parsed design list from `parse_design_file()`.
#' @param include_steps If `TRUE`, also report design pieces.
#'   Default `FALSE` (audit-friendly).
#' @return Data frame of gaps (possibly empty) with columns
#'   `id`, `name`, `type`, `atomic`, `used_in_code`, `in_finder`, `in_params`.
#' @export
param_coverage_gaps <- function(design, include_steps = FALSE) {
  if (is.list(design) && !is.null(design$code) && !is.null(design$meta)) {
    parsed <- design
  } else {
    parsed <- resolve_design(design)
  }
  id <- parsed$meta$id %||% NA_character_
  code <- parsed$code %||% ""
  # design body only (after design <-)
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  des_i <- which(grepl("^\\s*design\\s*<-", lines))
  body <- if (length(des_i)) {
    paste(lines[seq.int(des_i[[1]], length(lines))], collapse = "\n")
  } else {
    code
  }

  pre <- extract_pre_design_objects(code)
  empty <- data.frame(
    id = character(0),
    name = character(0),
    type = character(0),
    atomic = logical(0),
    used_in_code = logical(0),
    in_finder = logical(0),
    in_params = logical(0),
    stringsAsFactors = FALSE
  )
  if (!nrow(pre)) return(empty)

  dobj <- tryCatch(eval_design(parsed), error = function(e) e)
  if (inherits(dobj, "error")) {
    return(data.frame(
      id = id,
      name = NA_character_,
      type = "load_error",
      atomic = NA,
      used_in_code = NA,
      in_finder = NA,
      in_params = NA,
      error = conditionMessage(dobj),
      stringsAsFactors = FALSE
    ))
  }

  finder_names <- character(0)
  objs <- tryCatch(DeclareDesignZero:::find_all_objects(dobj), error = function(e) NULL)
  if (!is.null(objs) && nrow(objs) && "name" %in% names(objs)) {
    finder_names <- unique(as.character(objs$name))
  }
  param_names <- tryCatch(
    discover_design_params(dobj, code = code)$name,
    error = function(e) character(0)
  )

  used_in_code <- vapply(pre$name, symbol_used_in_code, body, FUN.VALUE = logical(1))
  in_finder <- pre$name %in% finder_names
  in_params <- pre$name %in% param_names
  used <- used_in_code | in_finder

  gaps <- pre[used & !in_params, , drop = FALSE]
  if (!nrow(gaps)) return(empty)

  # Design steps are not redesignable params — don't treat as coverage gaps.
  # Functions assigned before design <- are parameters (kind "function").
  if (!isTRUE(include_steps)) {
    gaps <- gaps[is.na(gaps$type) | gaps$type != "design_piece", , drop = FALSE]
    if (!nrow(gaps)) return(empty)
  }

  # Realign logical vectors to remaining gap rows by name
  gap_names <- gaps$name
  data.frame(
    id = id,
    name = gaps$name,
    type = gaps$type,
    atomic = gaps$atomic,
    used_in_code = used_in_code[match(gap_names, pre$name)],
    in_finder = in_finder[match(gap_names, pre$name)],
    in_params = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Report declared-before-design objects missing from design parameters
#'
#' For each library design: evaluate the file, read redesignable parameters from
#' the design object, and list top-level objects that are used by the design but
#' do not appear in that parameter list.
#'
#' @param designs Ids/aliases, or `NULL` for all.
#' @param atomic_only If `TRUE`, only report atomic gaps (likely should be
#'   redesignable). If `FALSE`, also report other non-step gaps (e.g. data frames).
#' @param include_steps If `TRUE`, also report design pieces.
#' @return A data frame (class `research_designs_param_coverage`) of gaps.
#' @export
param_coverage_report <- function(designs = NULL, atomic_only = FALSE, include_steps = FALSE) {
  idx <- make_index()
  if (!is.null(designs)) {
    keys <- vapply(designs, normalize_design_key, character(1))
    keep <- idx$id %in% keys | idx$alias %in% keys
    idx <- idx[keep, , drop = FALSE]
  }
  if (!nrow(idx)) {
    stop("No designs to check.", call. = FALSE)
  }

  parts <- lapply(seq_len(nrow(idx)), function(i) {
    id <- idx$id[[i]]
    if ("functional" %in% names(idx) && !isTRUE(idx$functional[[i]])) {
      return(data.frame(
        id = character(0),
        name = character(0),
        type = character(0),
        atomic = logical(0),
        used_in_code = logical(0),
        in_finder = logical(0),
        in_params = logical(0),
        stringsAsFactors = FALSE
      ))
    }
    tryCatch(
      param_coverage_gaps(id, include_steps = include_steps),
      error = function(e) {
        data.frame(
          id = id,
          name = NA_character_,
          type = "check_error",
          atomic = NA,
          used_in_code = NA,
          in_finder = NA,
          in_params = NA,
          error = conditionMessage(e),
          stringsAsFactors = FALSE
        )
      }
    )
  })

  # Drop empty frames; bind remaining (pad missing cols with NA)
  parts <- Filter(function(df) is.data.frame(df) && nrow(df) > 0L, parts)
  if (!length(parts)) {
    out <- data.frame(
      id = character(0),
      name = character(0),
      type = character(0),
      atomic = logical(0),
      used_in_code = logical(0),
      in_finder = logical(0),
      in_params = logical(0),
      stringsAsFactors = FALSE
    )
  } else {
    all_names <- unique(unlist(lapply(parts, names), use.names = FALSE))
    parts <- lapply(parts, function(df) {
      for (nm in setdiff(all_names, names(df))) {
        if (nm %in% c("atomic", "used_in_code", "in_finder", "in_params")) {
          df[[nm]] <- rep(NA, nrow(df))
        } else {
          df[[nm]] <- rep(NA_character_, nrow(df))
        }
      }
      df[all_names]
    })
    out <- do.call(rbind, parts)
  }
  if (is.null(out) || !nrow(out)) {
    out <- data.frame(
      id = character(0),
      name = character(0),
      type = character(0),
      atomic = logical(0),
      used_in_code = logical(0),
      in_finder = logical(0),
      in_params = logical(0),
      stringsAsFactors = FALSE
    )
  }
  if (isTRUE(atomic_only) && nrow(out) && "atomic" %in% names(out)) {
    out <- out[isTRUE(out$atomic) | (!is.na(out$type) & out$type %in% c("load_error", "check_error")), , drop = FALSE]
  }
  rownames(out) <- NULL
  structure(out, class = c("research_designs_param_coverage", "data.frame"), atomic_only = atomic_only, include_steps = include_steps)
}

#' @export
print.research_designs_param_coverage <- function(x, ...) {
  atomic_only <- isTRUE(attr(x, "atomic_only"))
  cat(
    "Param coverage gaps",
    if (atomic_only) " (atomic only)" else "",
    ": ",
    nrow(x),
    " row(s)\n",
    sep = ""
  )
  if (!nrow(x)) {
    cat("All pre-design objects used by designs appear in the parameter list.\n")
    return(invisible(x))
  }
  errs <- x[!is.na(x$type) & x$type %in% c("load_error", "check_error"), , drop = FALSE]
  gaps <- x[is.na(x$type) | !x$type %in% c("load_error", "check_error"), , drop = FALSE]
  if (nrow(errs)) {
    cat("\nLoad / check errors:\n")
    for (i in seq_len(nrow(errs))) {
      msg <- if ("error" %in% names(errs)) errs$error[[i]] else ""
      cat(" - ", errs$id[[i]], ": ", msg, "\n", sep = "")
    }
  }
  if (nrow(gaps)) {
    cat("\nDeclared before design, used, but not in design parameters:\n")
    show <- gaps[, intersect(c("id", "name", "type", "atomic", "used_in_code", "in_finder"), names(gaps)), drop = FALSE]
    print(show, row.names = FALSE)
  }
  invisible(x)
}

#' Drop a trailing semicolon hint from a vector display string
#' @noRd
strip_trailing_semicolons <- function(s) {
  trimws(sub(";+\\s*$", "", trimws(s %||% "")))
}

#' Display string for a Shiny parameter box
#'
#' Vector defaults include a trailing `;` as a sweep hint (`0, 0, 0;`).
#' Scalars do not.
#' @noRd
format_shiny_param_default <- function(value, kind, value_str = "") {
  vs <- trimws(value_str %||% "")
  if (identical(kind, "vector")) {
    val <- value
    if (is.null(val) && nzchar(vs)) {
      val <- tryCatch(eval(parse(text = vs), envir = baseenv()), error = function(e) NULL)
    }
    body <- ""
    if (is.atomic(val) && length(val)) {
      body <- paste(as.character(val), collapse = ", ")
    } else if (nzchar(vs)) {
      body <- vs
    }
    if (!nzchar(body)) return("")
    if (!grepl(";\\s*$", body)) body <- paste0(body, ";")
    return(body)
  }
  if (!nzchar(vs)) return("")
  parsed <- tryCatch(eval(parse(text = vs), envir = baseenv()), error = function(e) NULL)
  if (is.numeric(parsed) && length(parsed) >= 1L) {
    v <- parsed[[1L]]
    if (abs(v - round(v)) < 1e-6) return(as.character(as.integer(round(v))))
    return(as.character(round(v, 4)))
  }
  if (is.atomic(parsed) && length(parsed) >= 1L) return(as.character(parsed[[1L]]))
  vs
}

#' Parse a Shiny text box for one parameter
#'
#' Scalar: comma-separated numbers are a sweep. Vector: commas replace the
#' vector; semicolons separate alternative vectors for a sweep. A trailing
#' semicolon on a vector (the display hint) is not an empty second vector.
#'
#' @return A list with `skip`, `value`, `sweep`, and optional `error`.
#' @noRd
parse_shiny_param_raw <- function(raw, kind, default_display = "") {
  raw <- trimws(raw %||% "")
  if (!nzchar(raw)) return(list(skip = TRUE))
  default_display <- trimws(default_display %||% "")
  same <- if (identical(kind, "vector")) {
    identical(strip_trailing_semicolons(raw), strip_trailing_semicolons(default_display))
  } else {
    identical(raw, default_display)
  }
  if (same) return(list(skip = TRUE))

  eval_piece <- function(str) {
    str <- trimws(str)
    if (!nzchar(str)) return(NULL)
    expr <- str
    if (!grepl("^c\\s*\\(", str) && !grepl("^seq\\s*\\(", str) &&
        grepl("[,;\\s]", str)) {
      parts <- strsplit(str, "[,\\s]+")[[1]]
      parts <- trimws(parts)
      parts <- parts[nzchar(parts)]
      if (length(parts) >= 2L) expr <- paste0("c(", paste(parts, collapse = ", "), ")")
    }
    tryCatch(
      eval(parse(text = expr), envir = baseenv()),
      error = function(e) e
    )
  }

  if (identical(kind, "vector")) {
    chunks <- strsplit(raw, ";", fixed = TRUE)[[1]]
    chunks <- trimws(chunks)
    # Trailing `;` is a display hint, not an empty sweep vector.
    chunks <- chunks[nzchar(chunks)]
    vecs <- lapply(chunks, eval_piece)
    bad <- vapply(vecs, inherits, "error", FUN.VALUE = logical(1))
    if (any(bad)) {
      return(list(skip = FALSE, error = conditionMessage(vecs[bad][[1]])))
    }
    if (length(vecs) == 1L) {
      return(list(skip = FALSE, value = vecs[[1]], sweep = FALSE))
    }
    return(list(skip = FALSE, value = vecs, sweep = TRUE))
  }

  val <- eval_piece(raw)
  if (inherits(val, "error")) {
    return(list(skip = FALSE, error = conditionMessage(val)))
  }
  list(skip = FALSE, value = val, sweep = length(val) > 1L)
}

#' Escape text interpolated into Shiny help HTML
#' @noRd
html_escape <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

#' Example string for Shiny vector-sweep help
#' @noRd
vector_sweep_example <- function(name, default) {
  v <- tryCatch(as.numeric(unlist(default, use.names = FALSE)), error = function(e) numeric(0))
  v <- v[is.finite(v)]
  if (!length(v)) {
    return(paste0(name, ": 0, 0.1, 0.2; 0.1, 0.2, 0.3"))
  }
  first <- paste(v, collapse = ", ")
  alt <- paste(round(seq_along(v) * 0.1, 4), collapse = ", ")
  paste0(name, ": ", first, "; ", alt)
}

#' HTML `<ul>` of redesign-pane help bullets
#'
#' Always includes the scalar range-sweep bullet. Later bullets appear only
#' when the design has vector parameters, YAML `coupled:`, or function/data knobs.
#' @param id Design id.
#' @param args Optional `get_args()` table; looked up from `id` when `NULL`.
#' @return Character scalar of HTML.
#' @noRd
redesign_kind_help <- function(id, args = NULL) {
  id <- as.character(id %||% "")[[1]]
  if (is.null(args)) {
    args <- tryCatch(get_args(id), error = function(e) NULL)
  }
  kinds <- if (!is.null(args) && NROW(args) && "kind" %in% names(args)) {
    as.character(args$kind)
  } else {
    character(0)
  }
  items <- paste0(
    "You can change parameter values below. A comma-separated range like ",
    "<code>0, 10, 20</code> (on at most two parameters) can be used to ",
    "redesign over a range."
  )

  if (any(kinds == "vector")) {
    vec_idx <- which(kinds == "vector")
    i <- vec_idx[[1]]
    nms <- if ("name" %in% names(args)) as.character(args$name) else character(0)
    if ("outcome_means" %in% nms[vec_idx]) {
      i <- which(nms == "outcome_means" & kinds == "vector")[[1]]
    }
    def <- if ("default" %in% names(args)) args$default[[i]] else NULL
    nm <- if (length(nms)) nms[[i]] else "outcome_means"
    ex <- vector_sweep_example(nm, def)
    items <- c(
      items,
      paste0(
        "On a vector parameter, commas replace the whole vector; ",
        "use a semicolon to sweep alternative vectors (e.g. <code>",
        html_escape(ex),
        "</code>)."
      )
    )
  }

  coupled <- tryCatch(coupled_help_text(id), error = function(e) character(0))
  if (length(coupled)) {
    items <- c(items, paste0("Note: ", html_escape(coupled)))
  }

  if (length(kinds) && any(kinds %in% c("data", "function"))) {
    nms <- as.character(args$name[kinds %in% c("data", "function")])
    nms <- nms[!is.na(nms) & nzchar(nms)]
    if (length(nms)) {
      items <- c(
        items,
        paste0(
          "Functions, data frames, and matrices are not edited here — pass them in R with ",
          "<code>make_design(\"", html_escape(id), "\", ",
          html_escape(paste(sprintf("%s = ...", nms), collapse = ", ")),
          ")</code>."
        )
      )
    }
  }

  paste0("<ul>", paste0("<li>", items, "</li>", collapse = ""), "</ul>")
}

#' Build get_args() table: design values + optional YAML tips
#' @noRd
build_args_table <- function(meta, design, code = NULL) {
  params <- discover_design_params(design, code = code)
  tips_map <- normalize_params_map(meta$params %||% list())

  tip_for <- function(name) {
    tip <- tips_map[[name]]
    if (is.null(tip) || (length(tip) == 1L && is.na(tip))) return(NA_character_)
    as.character(tip)[[1]]
  }

  n <- nrow(params)
  if (n == 0L) {
    return(data.frame(
      name = character(0),
      default = I(list()),
      value_str = character(0),
      tip = character(0),
      kind = character(0),
      shiny = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  defaults <- vector("list", n)
  tips <- character(n)
  for (i in seq_len(n)) {
    nm <- params$name[[i]]
    if (!is.null(params$value[[i]])) {
      defaults[[i]] <- params$value[[i]]
    } else {
      defaults[[i]] <- params$value_str[[i]]
    }
    tips[[i]] <- tip_for(nm)
  }

  kinds <- if ("kind" %in% names(params)) {
    as.character(params$kind)
  } else {
    vapply(defaults, classify_param_kind, character(1))
  }

  data.frame(
    name = params$name,
    default = I(defaults),
    value_str = params$value_str,
    tip = tips,
    kind = kinds,
    shiny = is_shiny_param_kind(kinds),
    stringsAsFactors = FALSE
  )
}
