#' Resolve a design id or book alias to a parsed file
#' @noRd
resolve_design <- function(design) {
  key <- normalize_design_key(design)
  files <- design_files()
  if (!length(files)) {
    stop("No designs found in ", designs_dir(), call. = FALSE)
  }

  parsed_all <- lapply(files, parse_design_file)
  ids <- vapply(parsed_all, function(x) x$meta$id, character(1))
  aliases <- vapply(parsed_all, function(x) {
    a <- x$meta$alias
    if (is.null(a) || (length(a) == 1L && is.na(a))) "" else as.character(a)[[1]]
  }, character(1))

  hit <- which(ids == key | aliases == key | basename_id(files) == key)
  if (!length(hit)) {
    stop(
      "Unknown design '", key, "'. ",
      "See list_designs() for ids and aliases.",
      call. = FALSE
    )
  }
  if (length(hit) > 1L) {
    stop(
      "Design key '", key, "' matches multiple files: ",
      paste(basename(files[hit]), collapse = ", "),
      call. = FALSE
    )
  }
  parsed_all[[hit[[1]]]]
}

#' @noRd
basename_id <- function(paths) {
  sub("\\.[Rr]$", "", basename(paths))
}

#' @noRd
normalize_design_key <- function(design) {
  if (is.null(design)) stop("design id/alias is required", call. = FALSE)
  if (is.symbol(design) || is.name(design)) {
    return(as.character(design))
  }
  if (is.character(design) && length(design) >= 1L && nzchar(design[[1]])) {
    return(as.character(design[[1]]))
  }
  # Allow declaration_2.1 style names passed unquoted via substitute in wrappers
  stop("design must be a character id/alias (e.g. \"two_arm_simple\" or \"2.1\")", call. = FALSE)
}

#' Source a parsed design file and return the design object
#'
#' Evaluates the file at its written defaults. Parameter changes go through
#' `redesign()` in [make_design()], not by skipping assignments here.
#' @noRd
eval_design <- function(parsed) {
  pkgs <- unique(c(core_packages(), parsed$meta$packages %||% character(0)))
  missing <- ensure_packages_attached(pkgs)
  if (length(missing)) {
    stop(
      "Design '", parsed$meta$id, "' needs packages not installed: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  env <- new.env(parent = globalenv())
  # Design files may use `%||%`; R < 4.4 has it only if rlang is attached.
  env$`%||%` <- `%||%`
  expr <- parse(text = parsed$code, keep.source = TRUE)
  eval(expr, envir = env)

  obj_name <- parsed$meta$object %||% "design"
  if (!exists(obj_name, envir = env, inherits = FALSE)) {
    # Fallbacks: id, then sole design-looking object
    if (exists(parsed$meta$id, envir = env, inherits = FALSE)) {
      obj_name <- parsed$meta$id
    } else {
      stop(
        "Object '", parsed$meta$object, "' not found after sourcing ",
        basename(parsed$path), ". Set `object:` in YAML or name it `design`.",
        call. = FALSE
      )
    }
  }

  design <- get(obj_name, envir = env, inherits = FALSE)
  attr(design, "research_designs_id") <- parsed$meta$id
  attr(design, "research_designs_alias") <- parsed$meta$alias
  attr(design, "research_designs_path") <- parsed$path
  design
}

#' Design ids that match exported DesignLibrary designer names
#' @noRd
designlibrary_core_ids <- function() {
  c(
    "two_arm_flexible",
    "two_arm_attrition",
    "pretest_posttest",
    "randomized_response",
    "mediation_analysis",
    "multiarm_trial",
    "two_by_two",
    "block_cluster_two_arm"
  )
}

#' Pedagogical starter sequence for list_designs() print and row order.
#' Ids that are not yet in the library are skipped by grouping.
#' @noRd
starter_design_ids <- function() {
  c(
    "two_arm_simple",         # Simple two-arm trial
    "two_arm_flexible",       # Flexible two-arm trial (library)
    "three_arm",              # Three-arm trial
    "two_by_two",             # 2x2 factorial (library)
    "two_arm_block_cluster",  # Two arm trial with blocks and clusters
    "two_arm_with_blocks",    # Two-arm trial with blocks
    "two_arm_attrition",      # Two-arm trial with attrition
    "factorial_2x2x2",        # 2x2x2 factorial
    "multiarm_trial",         # Multi-arm trial
    "pretest_posttest",       # Pretest-posttest design
    "randomized_response",    # Randomized response
    "mediation_analysis"      # Mediation analysis
  )
}

#' Group index: 1 starter sequence, 2 other templates, 3 RDSS, 4 other.
#' Starter ids stay in group 1 even if YAML category is rdss.
#' @noRd
design_list_group <- function(id, category) {
  id <- as.character(id)
  catg <- tolower(trimws(as.character(category %||% "")))
  starter <- id %in% starter_design_ids()
  template <- catg %in% c("template", "templates")
  rdss <- catg == "rdss"
  ifelse(starter, 1L, ifelse(template, 2L, ifelse(rdss, 3L, 4L)))
}

#' Map YAML category to a Shiny library-browser tab key.
#' `template` / `templates` -> templates; `rdss` -> rdss; blank / `Other` -> other.
#' Any other YAML name is kept (lowercased) so later categories get their own tab.
#' @noRd
library_tab_key <- function(category) {
  catg <- tolower(trimws(as.character(category %||% "")))
  catg[is.na(catg) | !nzchar(catg)] <- "other"
  catg[catg %in% c("template", "templates")] <- "templates"
  catg
}

#' Library-browser tab keys: templates, then RDSS, then remaining first-seen keys.
#' @param always Keys that always appear (even if the current index has none).
#' @noRd
library_tab_keys <- function(category, always = c("templates", "rdss")) {
  keys <- library_tab_key(category)
  seen <- unique(c(as.character(always %||% character(0)), keys))
  seen <- seen[nzchar(seen)]
  preferred <- c("templates", "rdss")
  c(intersect(preferred, seen), setdiff(seen, preferred))
}

#' Human labels for library-browser tabs.
#' @noRd
library_tab_label <- function(key) {
  key <- as.character(key)
  known <- c(templates = "Templates", rdss = "RDSS designs", other = "Other")
  out <- unname(known[key])
  miss <- is.na(out)
  if (any(miss)) {
    raw <- key[miss]
    out[miss] <- paste0(toupper(substr(raw, 1L, 1L)), substring(raw, 2L))
  }
  out
}

#' Rows matching a library search query (global; not scoped to the visible tab).
#' @noRd
library_search_rows <- function(df, q) {
  n <- if (is.null(df)) 0L else nrow(df)
  if (!n) return(logical(0))
  q <- trimws(as.character(q %||% "")[[1]])
  if (!nzchar(q)) return(rep(TRUE, n))
  hay <- tolower(paste(
    df$id, df$alias %||% "", df$label %||% "", df$params %||% "",
    df$packages %||% "", df$keywords %||% "", df$category %||% "",
    sep = " "
  ))
  grepl(tolower(q), hay, fixed = TRUE)
}

#' Filter the Shiny library table: search is global, then the selected tab.
#' If the query matches only other tabs, `tab` is switched to the first match.
#' Row order is preserved (starter sequence, then other groups).
#' @return list(rows, tab, match_tabs, n_match)
#' @noRd
filter_library_browser <- function(df, tab = "templates", q = "") {
  df <- as.data.frame(df)
  hit <- library_search_rows(df, q)
  matched <- df[hit, , drop = FALSE]
  keys_all <- library_tab_keys(df$category)
  if (!length(keys_all)) keys_all <- "templates"
  tab <- trimws(as.character(tab %||% "")[[1]])
  if (!nzchar(tab)) tab <- keys_all[[1]]
  match_tabs <- if (nrow(matched)) library_tab_keys(matched$category, always = NULL) else {
    character(0)
  }
  q <- trimws(as.character(q %||% "")[[1]])
  if (nzchar(q) && length(match_tabs) && !tab %in% match_tabs) {
    tab <- match_tabs[[1]]
  }
  if (!tab %in% keys_all) tab <- keys_all[[1]]
  tab_hit <- library_tab_key(matched$category) == tab
  rows <- matched[tab_hit, , drop = FALSE]
  rownames(rows) <- NULL
  match_counts <- if (length(match_tabs)) {
    stats::setNames(
      vapply(match_tabs, function(k) {
        sum(library_tab_key(matched$category) == k)
      }, integer(1)),
      match_tabs
    )
  } else {
    integer(0)
  }
  list(
    rows = rows,
    tab = tab,
    match_tabs = match_tabs,
    match_counts = match_counts,
    n_match = nrow(matched)
  )
}

#' List designs in the library
#'
#' Returns a data frame of design metadata. Printing is a grouped list of
#' `id (label)` lines, starting with a short getting-started sequence, then
#' other templates, RDSS, and remaining designs. Default listing is
#' metadata-only (baked library index plus a live-file overlay). The `params`
#' column is a comma-separated name string from that index (YAML `params:`
#' keys plus pre-design assignment names); designs are not evaluated. Use
#' [design_info()], [get_args()], or
#' `as.data.frame(list_designs(discover_params = TRUE))` for redesignable
#' parameters from a loaded design. Row order matches print order.
#'
#' @param shiny_only If `TRUE`, only designs with `include_in_shiny: true`
#'   (the default when the field is omitted).
#' @param discover_params If `TRUE`, load each design once to list
#'   redesignable parameters (overrides index names). Default `FALSE` uses
#'   baked index names and does not evaluate designs.
#' @param list_all If `FALSE` (default), printing shows the full getting-started
#'   sequence and up to 10 designs in each remaining group. If `TRUE`, printing
#'   lists every design.
#' @return A data frame with class `research_designs_list`.
#' @export
list_designs <- function(shiny_only = FALSE, discover_params = FALSE, list_all = FALSE) {
  idx <- make_index()
  empty <- data.frame(
    id = character(0),
    alias = character(0),
    params = character(0),
    packages = character(0),
    label = character(0),
    category = character(0),
    keywords = character(0),
    include_in_shiny = logical(0),
    functional = logical(0),
    file = character(0),
    stringsAsFactors = FALSE
  )
  if (!nrow(idx)) {
    return(structure(
      empty,
      class = c("research_designs_list", "data.frame"),
      list_all = isTRUE(list_all)
    ))
  }

  params <- if ("params" %in% names(idx)) {
    as.character(idx$params)
  } else {
    rep("", nrow(idx))
  }
  params[is.na(params)] <- ""
  if (isTRUE(discover_params)) {
    dir <- designs_dir()
    for (i in seq_len(nrow(idx))) {
      if (!isTRUE(idx$functional[[i]])) next
      params[[i]] <- tryCatch({
        parsed <- parse_design_file(file.path(dir, idx$file[[i]]))
        design <- eval_design(parsed)
        pnames <- discover_design_params(design, code = parsed$code)$name
        if (!length(pnames)) "" else paste(pnames, collapse = ", ")
      }, error = function(e) NA_character_)
    }
  }

  out <- data.frame(
    id = idx$id,
    alias = idx$alias,
    params = params,
    packages = idx$packages,
    label = idx$label,
    category = idx$category,
    keywords = idx$keywords,
    include_in_shiny = idx$include_in_shiny,
    functional = idx$functional,
    file = idx$file,
    stringsAsFactors = FALSE
  )
  if (isTRUE(shiny_only)) {
    out <- out[out$include_in_shiny, , drop = FALSE]
    rownames(out) <- NULL
  }
  if (nrow(out)) {
    out <- arrange_design_list(out)
  }
  structure(
    out,
    class = c("research_designs_list", "data.frame"),
    list_all = isTRUE(list_all)
  )
}

#' Pedagogical row order for list_designs()
#' @noRd
arrange_design_list <- function(out) {
  grp <- design_list_group(out$id, out$category)
  starter <- starter_design_ids()
  starter_rank <- match(out$id, starter)
  starter_rank[is.na(starter_rank)] <- length(starter) + 1L
  o <- order(grp, starter_rank, as.character(out$id))
  out <- out[o, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @export
print.research_designs_list <- function(x, ..., list_all = NULL, n = 10L) {
  n_all <- nrow(x)
  cat(
    "ResearchDesigns library: ", n_all, " design",
    if (n_all == 1L) "" else "s",
    "\n",
    sep = ""
  )
  if (n_all > 0L) {
    if (is.null(list_all)) {
      list_all <- isTRUE(attr(x, "list_all"))
    }
    n_show <- if (isTRUE(list_all)) n_all else max(1L, as.integer(n)[1])
    grp <- design_list_group(x$id, x$category)
    headings <- c(
      "* Getting started",
      "* Other design templates",
      "* Other RDSS designs",
      "* Other designs"
    )
    truncated <- FALSE
    for (g in 1:4) {
      rows <- x[grp == g, , drop = FALSE]
      if (!nrow(rows)) next
      cat("\n", headings[[g]], "\n", sep = "")
      show <- rows
      extra <- 0L
      # Always list the starter sequence in full; cap other groups at n.
      cap_group <- g != 1L && nrow(rows) > n_show
      if (cap_group) {
        show <- rows[seq_len(n_show), , drop = FALSE]
        extra <- nrow(rows) - n_show
        truncated <- TRUE
      }
      for (i in seq_len(nrow(show))) {
        lab <- as.character(show$label[[i]] %||% "")
        cat("  ", show$id[[i]], " (", lab, ")\n", sep = "")
      }
      if (extra > 0L) {
        cat("  ... and ", extra, " more\n", sep = "")
      }
    }
    if (truncated) {
      cat("\nprint(list_designs(), list_all = TRUE) lists every design.\n")
    }
  }
  cat(
    "\nSee design_info(\"id\") or get_args(\"id\") for details;\n",
    "as.data.frame(list_designs(discover_params = TRUE)) for parameters.\n",
    sep = ""
  )
  invisible(x)
}

#' Design metadata (YAML + defaults)
#'
#' Printing uses short English prose. The underlying list is unchanged for
#' programmatic use (`info$id`, `info$params`, `info$diagnosands`, `info$coupled`,
#' etc.).
#'
#' @param design Design id or book alias.
#' @return A named list of metadata with class `research_designs_info`.
#' @export
design_info <- function(design) {
  if (length(design) > 1L) design <- design[[1L]]
  meta <- resolve_design(design)$meta
  structure(meta, class = c("research_designs_info", "list"))
}

#' Preferred diagnosands declared in a design's YAML
#'
#' Reads optional `diagnosands:` from the design frontmatter (for example
#' `diagnosands: [rmse, bias]` or `diagnosands: rmse, -bias, power`).
#' Positive names are preferred defaults for diagnosis and redesign display.
#' Names prefixed with `-` (e.g. `-bias`) are exclusions and are omitted here;
#' see [excluded_diagnosands()].
#'
#' @param design Design id or book alias.
#' @return Character vector (possibly empty).
#' @export
#' @examples
#' \dontrun{
#' preferred_diagnosands("two_arm_simple")
#' }
preferred_diagnosands <- function(design) {
  if (length(design) > 1L) design <- design[[1L]]
  meta <- resolve_design(design)$meta
  split_diagnosand_tokens(meta$diagnosands)$prefer
}

#' Diagnosands excluded from display by YAML
#'
#' Tokens like `-bias` in `diagnosands:` remove that name from the Shiny
#' diagnosand list for the design.
#'
#' @param design Design id or book alias.
#' @return Character vector (possibly empty).
#' @export
excluded_diagnosands <- function(design) {
  if (length(design) > 1L) design <- design[[1L]]
  meta <- resolve_design(design)$meta
  split_diagnosand_tokens(meta$diagnosands)$exclude
}

#' @export
print.research_designs_info <- function(x, ...) {
  id <- as.character(x$id %||% "")[[1]]
  alias <- x$alias
  has_alias <- !is.null(alias) && length(alias) && !is.na(alias) && nzchar(as.character(alias)[[1]])
  label <- as.character(x$label %||% humanize_id(id))[[1]]

  if (has_alias) {
    cat(sprintf('%s (alias "%s") declares a %s.\n', id, as.character(alias)[[1]], label))
  } else {
    cat(sprintf("%s declares a %s.\n", id, label))
  }

  desc <- x$description
  if (!is.null(desc) && length(desc) && !is.na(desc) && nzchar(trimws(as.character(desc)[[1]]))) {
    cat("\nThe design description is:\n")
    wrapped <- strwrap(trimws(as.character(desc)[[1]]), width = 72, prefix = "  ", initial = "  ")
    cat(paste(wrapped, collapse = "\n"), "\n", sep = "")
  }

  args <- tryCatch(get_args(id), error = function(e) NULL)
  cat("\nThe modifiable parameters are:\n")
  if (is.null(args) || !nrow(args)) {
    cat("  (none found)\n")
  } else {
    for (i in seq_len(nrow(args))) {
      tip <- args$tip[[i]]
      val <- args$value_str[[i]] %||% ""
      kind <- if ("kind" %in% names(args)) args$kind[[i]] else "scalar"
      line <- sprintf("  %s = %s", args$name[[i]], val)
      if (identical(kind, "data") || identical(kind, "function")) {
        line <- paste0(line, " [R only: make_design(..., ", args$name[[i]], " = ...)]")
      } else if (identical(kind, "vector")) {
        line <- paste0(line, " [vector]")
      }
      if (!is.null(tip) && length(tip) && !is.na(tip) && nzchar(tip)) {
        line <- paste0(line, " — ", tip)
      }
      cat(line, "\n", sep = "")
    }
  }

  cat_bits <- character(0)
  catg <- x$category %||% "Other"
  if (!is.null(catg) && nzchar(as.character(catg)[[1]])) {
    cat_bits <- c(cat_bits, paste0("category ", as.character(catg)[[1]]))
  }
  kw <- x$keywords %||% character(0)
  if (length(kw) && any(nzchar(kw))) {
    cat_bits <- c(cat_bits, paste0("keywords ", paste(kw[nzchar(kw)], collapse = ", ")))
  }
  if (length(cat_bits)) {
    cat("\nIt is filed under ", paste(cat_bits, collapse = "; "), ".\n", sep = "")
  }

  pkgs <- x$packages %||% character(0)
  pkgs <- pkgs[nzchar(as.character(pkgs))]
  if (length(pkgs)) {
    cat("\nExtra packages required:\n  ", paste(pkgs, collapse = ", "), "\n", sep = "")
  }

  dgs <- x$diagnosands %||% character(0)
  dgs <- dgs[nzchar(as.character(dgs))]
  if (length(dgs)) {
    parts <- split_diagnosand_tokens(dgs)
    if (length(parts$prefer)) {
      cat("\nPreferred diagnosands:\n  ", paste(parts$prefer, collapse = ", "), "\n", sep = "")
    }
    if (length(parts$exclude)) {
      cat("\nExcluded diagnosands:\n  ", paste(parts$exclude, collapse = ", "), "\n", sep = "")
    }
  }

  notes <- coupled_notes(x)
  if (length(notes)) {
    cat("\nCoupled parameters:\n")
    for (note in notes) {
      cat("  ", note, "\n", sep = "")
    }
  }

  link <- x$book_link
  if (!is.null(link) && length(link) && !is.na(link) && nzchar(as.character(link)[[1]])) {
    cat("\nBook reference:\n  ", as.character(link)[[1]], "\n", sep = "")
  }

  cat(
    "\nSee make_design(\"", id, "\"), get_args(\"", id, "\"), ",
    "or get_code(\"", id, "\") for more.\n",
    sep = ""
  )
  invisible(x)
}

#' Build a design, optionally with redesigned parameters
#'
#' Loads the declared design at its defaults. Named `...` values are applied
#' with [DeclareDesign::redesign()]. The design object is the source of
#' truth for which names can be changed; see [get_args()]. Vector parameters
#' are wrapped so `redesign()` replaces the whole vector instead of sweeping.
#' YAML `coupled:` drivers (for example `m_arms`) emit a `message()` when
#' dependents do not match in length; `redesign()` still runs.
#'
#' In RStudio, Positron, and other tools that complete from formals, typing
#' `make_design("` and pressing Tab lists installed design ids (and aliases).
#' See [list_designs()] for the same catalogue in the console.
#'
#' @param design Design id or book alias. Defaults to `"two_arm_simple"` (or the
#'   first installed design). Tab-completion offers the full library list.
#' @param ... Named parameter values passed to `redesign()`.
#' @return A design object (or a list of designs if a parameter is a vector
#'   and `redesign()` expands).
#' @export
#' @examples
#' \dontrun{
#' make_design()
#' make_design("two_arm_simple", b = 0.5)
#' make_design("2.1", b = 0.5)  # book alias
#' }
make_design <- function(design = "two_arm_simple", ...) {
  # When formals are the full library vector (set in .onLoad for IDE completion),
  # a bare make_design() call receives that vector — use the first id.
  if (length(design) > 1L) design <- design[[1L]]
  parsed <- resolve_design(design)
  d <- eval_design(parsed)
  dots <- list(...)
  if (!length(dots)) return(d)
  params <- discover_design_params(d, code = parsed$code)
  unknown <- setdiff(names(dots), params$name)
  unknown <- unknown[nzchar(unknown %||% "")]
  if (length(unknown)) {
    stop(
      "Unknown parameter(s) for '", normalize_design_key(design), "': ",
      paste(unknown, collapse = ", "),
      ". Use get_args(\"", normalize_design_key(design), "\").",
      call. = FALSE
    )
  }
  dots <- prepare_redesign_dots(params, dots)
  shown <- message_coupled_if_needed(parsed$meta, dots, design = d, code = parsed$code)
  # `.design` since DeclareDesign 2.0: an undotted `design` partially
  # matched, so a design with a parameter named `d` (mediation_analysis) had
  # its design object replaced by the parameter's value.
  d <- do.call(DeclareDesign::redesign, c(list(.design = d), dots))
  if (inherits(d, "design")) {
    extra <- setdiff(
      coupled_issues(parsed$meta, list(), design = d, code = parsed$code),
      shown
    )
    for (note in extra) message(note)
  }
  d
}

#' Editable parameters for a design
#'
#' Reads parameters from the design object. Optional YAML `params:` entries
#' only add tips (and never invent new parameter names).
#'
#' The `kind` column is `"scalar"`, `"vector"`, `"data"`, or `"function"`.
#' `shiny` is `TRUE` for scalar and short-vector parameters that the browser
#' can edit as text. Data frames, matrices, long vectors, and functions stay
#' redesignable in R (`make_design(..., data = ...)`, `make_design(..., Y = ...)`)
#' but are not Shiny controls.
#'
#' @param design Design id or book alias.
#' @return A data frame with `name`, `default`, `value_str`, `tip`, `kind`,
#'   and `shiny`.
#' @export
get_args <- function(design) {
  if (length(design) > 1L) design <- design[[1L]]
  parsed <- resolve_design(design)
  d <- eval_design(parsed)
  build_args_table(parsed$meta, d, code = parsed$code)
}

#' Prefix design source with `library()` for YAML `packages:`
#'
#' Only extra packages from the design YAML are included. Core stack
#' packages are omitted unless they appear in `packages:`.
#' @noRd
prefix_yaml_libraries <- function(code, packages) {
  pkgs <- unique(as_chr(packages))
  pkgs <- trimws(pkgs)
  pkgs <- pkgs[!is.na(pkgs) & nzchar(pkgs)]
  if (!length(pkgs)) return(code)
  libs <- paste0("library(", pkgs, ")", collapse = "\n")
  paste0(libs, "\n\n", trim_code_snippet(code))
}

#' Code for a design: simple `make_design()` call and/or full source
#'
#' Returns a list with `$simple` (a one-line `make_design()` call) and `$full`
#' (the design file source, prefixed with `library()` calls for YAML
#' `packages:` when any are listed). Printing uses [cat()] on `$full` so the
#' console output is copy-paste ready. Use `style = "simple"` to print the
#' one-liner instead. Programmatic access is unchanged: `get_code(id)$simple`,
#' `get_code(id)$full`.
#'
#' @param design Design id or book alias.
#' @param style `"simple"`, `"full"`, or `"both"`. Controls which snippet
#'   [print()] and [as.character()] show. `$simple` and `$full` are always
#'   both present.
#' @param ... Optional parameter values included in the simple snippet.
#' @return A list with `simple` and `full`, class `research_designs_code`.
#' @export
#' @examples
#' \dontrun{
#' get_code("two_arm_simple")
#' get_code("two_arm_simple")$simple
#' }
get_code <- function(design, style = c("both", "simple", "full"), ...) {
  style <- match.arg(style)
  if (length(design) > 1L) design <- design[[1L]]
  key <- normalize_design_key(design)
  parsed <- resolve_design(design)
  dots <- list(...)

  simple <- paste0("make_design(\"", key, "\"")
  if (length(dots)) {
    args <- vapply(names(dots), function(nm) {
      paste0(nm, " = ", deparse1(dots[[nm]]))
    }, character(1))
    simple <- paste0(simple, ", ", paste(args, collapse = ", "))
  }
  simple <- paste0(simple, ")")

  structure(
    list(
      simple = simple,
      full = prefix_yaml_libraries(parsed$code, parsed$meta$packages)
    ),
    class = c("research_designs_code", "list"),
    style = style
  )
}

#' @noRd
code_snippet <- function(x, style = NULL) {
  style <- style %||% attr(x, "style") %||% "full"
  raw <- if (identical(style, "simple")) x$simple else x$full
  paste(raw, collapse = "\n")
}

#' Strip surrounding blank lines so console output is copy-paste ready
#' @noRd
trim_code_snippet <- function(code) {
  code <- sub("^\\s*\n", "", code, perl = TRUE)
  sub("\n\\s*$", "", code, perl = TRUE)
}

#' @export
format.research_designs_code <- function(x, ..., style = NULL) {
  trim_code_snippet(code_snippet(x, style = style))
}

#' @export
as.character.research_designs_code <- function(x, ..., style = NULL) {
  format(x, ..., style = style)
}

#' @export
print.research_designs_code <- function(x, ..., style = NULL) {
  cat(format(x, ..., style = style), "\n", sep = "")
  invisible(x)
}
