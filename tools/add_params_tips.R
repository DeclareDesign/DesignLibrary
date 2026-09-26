# Add / refresh YAML params: tips for pre-design assignments (and design-discovered params)
# Rscript tools/add_params_tips.R

root <- "C:/WZB Dropbox/Macartan Humphreys/5_github/ResearchDesigns"
dir <- file.path(root, "inst/designs")
options(DesignLibrary.root = root)

suppressPackageStartupMessages({
  if (!requireNamespace("yaml", quietly = TRUE)) stop("yaml required")
})

# Load package internals if available
pkg_ok <- requireNamespace("DesignLibrary", quietly = TRUE)
if (pkg_ok) {
  suppressPackageStartupMessages(library(DesignLibrary))
}

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

yaml_escape <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x
}

# Curated tip guesses by parameter name (case-sensitive first, then lower)
TIP_EXACT <- c(
  N = "Number of units (sample or population size)",
  n = "Sample size (drawn from N when both appear)",
  n_villages = "Number of villages sampled",
  citizens_per_village = "Citizens sampled per village",
  b = "Treatment effect (outcome scale)",
  beta = "Regression / treatment coefficient",
  tau = "Average treatment effect",
  ate = "Average treatment effect",
  effect = "Treatment effect size",
  effect_size = "Treatment effect size",
  prob = "Assignment probability to treatment",
  p = "Probability (assignment or success)",
  rho = "Correlation parameter",
  R2 = "R-squared / explained variance",
  sd = "Standard deviation of the error term",
  sigma = "Error standard deviation",
  mu = "Mean of the outcome or covariate",
  mean = "Mean parameter",
  alpha = "Intercept or Type I error rate (context-dependent)",
  gamma = "Secondary effect or interaction coefficient",
  delta = "Effect difference or shift",
  lambda = "Rate or penalty parameter",
  theta = "Structural parameter",
  phi = "Auxiliary structural parameter",
  kappa = "Auxiliary design parameter",
  clusters = "Number of clusters",
  n_clusters = "Number of clusters",
  cluster_size = "Units per cluster",
  blocks = "Number of blocks",
  n_blocks = "Number of blocks",
  block_size = "Units per block",
  periods = "Number of time periods",
  T = "Number of time periods",
  cutoff = "RDD cutoff",
  bandwidth = "RDD bandwidth",
  compliance = "Compliance rate",
  takeup = "Take-up / compliance rate",
  attrition = "Attrition rate",
  response_rate = "Survey response rate",
  sims = "Number of simulations (if used in the design)",
  strategies = "Process-tracing / inquiry strategies",
  causal_model = "Causal model object used by the estimator"
)

TIP_PREFIX <- list(
  n_ = "Count / sample-size parameter",
  N_ = "Size parameter",
  p_ = "Probability parameter",
  prob_ = "Probability parameter",
  sd_ = "Standard deviation",
  mean_ = "Mean parameter",
  effect_ = "Effect-size parameter",
  ate_ = "Average treatment effect",
  rho_ = "Correlation parameter"
)

guess_tip <- function(name, value = NULL) {
  if (name %in% names(TIP_EXACT)) return(unname(TIP_EXACT[[name]]))
  low <- tolower(name)
  if (low %in% tolower(names(TIP_EXACT))) {
    i <- match(low, tolower(names(TIP_EXACT)))
    return(unname(TIP_EXACT[[i]]))
  }
  for (pref in names(TIP_PREFIX)) {
    if (startsWith(name, pref) || startsWith(low, tolower(pref))) {
      return(TIP_PREFIX[[pref]])
    }
  }
  # type-informed fallback
  type_bit <- ""
  if (!is.null(value)) {
    if (is.logical(value) && length(value) == 1L) type_bit <- " (logical)"
    else if (is.integer(value) && length(value) == 1L) type_bit <- " (integer)"
    else if (is.numeric(value) && length(value) == 1L) type_bit <- " (numeric)"
    else if (is.character(value) && length(value) == 1L) type_bit <- " (character)"
    else if (is.numeric(value) && length(value) > 1L) type_bit <- " (numeric vector)"
    else if (is.character(value) && length(value) > 1L) type_bit <- " (character vector)"
  }
  paste0("Design parameter `", name, "`", type_bit)
}

# Parse simple top-level assignments before design <-
extract_pre_design_assigns <- function(code) {
  # strip YAML if present (caller should pass code only)
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  # find design <- start
  des_i <- which(grepl("^\\s*design\\s*<-", lines))
  if (!length(des_i)) {
    des_i <- which(grepl("^\\s*declaration_[A-Za-z0-9._]+\\s*<-", lines))
  }
  if (!length(des_i)) return(list())
  head_lines <- lines[seq_len(des_i[[1]] - 1L)]
  # drop comments-only and blank
  assigns <- list()
  for (ln in head_lines) {
    if (grepl("^\\s*#", ln) || !nzchar(trimws(ln))) next
    # name <- value  (simple; skip function defs and control flow)
    if (grepl("^\\s*[A-Za-z.][A-Za-z0-9._]*\\s*<-\\s*", ln) &&
        !grepl("^\\s*(if|for|while|function)\\b", ln) &&
        !grepl("\\bfunction\\s*\\(", ln)) {
      nm <- sub("^\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*<-.*$", "\\1", ln)
      rhs <- sub("^\\s*[A-Za-z.][A-Za-z0-9._]*\\s*<-\\s*", "", ln)
      # skip if RHS looks like a declare_* composition starter without simple value
      val <- tryCatch(eval(parse(text = rhs), envir = baseenv()), error = function(e) NULL)
      # keep atomic / simple lists / short character; skip large models
      if (is.null(val)) {
        # still record name with NULL value for tip guess from name alone,
        # but skip declare_* / heavy objects by RHS text
        if (grepl("declare_|make_model|add_level|function\\s*\\(", rhs)) next
        assigns[[nm]] <- NULL
      } else if (is.atomic(val) && !is.function(val)) {
        assigns[[nm]] <- val
      } else if (is.list(val) && length(val) <= 20L) {
        assigns[[nm]] <- val
      } else {
        next
      }
    }
  }
  assigns
}

split_yaml_code <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (!length(lines) || !grepl("^---\\s*$", lines[[1]])) {
    return(list(yaml_lines = NULL, code = paste(lines, collapse = "\n"), lines = lines))
  }
  end <- which(grepl("^---\\s*$", lines))[-1]
  if (!length(end)) stop("Unclosed YAML in ", path)
  yaml_lines <- lines[seq.int(2L, end[[1]] - 1L)]
  code_lines <- if (end[[1]] < length(lines)) lines[seq.int(end[[1]] + 1L, length(lines))] else character(0)
  list(yaml_lines = yaml_lines, code = paste(code_lines, collapse = "\n"), lines = lines, yaml_end = end[[1]])
}

parse_existing_params <- function(yaml_lines) {
  if (is.null(yaml_lines)) return(list())
  txt <- paste(yaml_lines, collapse = "\n")
  parsed <- tryCatch(yaml::yaml.load(txt), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$params)) return(list())
  out <- list()
  nms <- names(parsed$params)
  if (is.null(nms)) return(list())
  # recover N/n boolean keys
  if (is.logical(nms)) {
    nms <- ifelse(is.na(nms), "", ifelse(nms, "Y", "N"))
  }
  for (i in seq_along(parsed$params)) {
    nm <- as.character(nms[[i]])
    if (!nzchar(nm)) next
    entry <- parsed$params[[i]]
    tip <- if (is.list(entry)) entry$tip %||% entry[[1]] else entry
    out[[nm]] <- as.character(tip)[[1]]
  }
  out
}

# Remove existing params: block from yaml lines (top-level key)
strip_params_block <- function(yaml_lines) {
  if (is.null(yaml_lines) || !length(yaml_lines)) return(character(0))
  out <- character(0)
  i <- 1L
  n <- length(yaml_lines)
  while (i <= n) {
    if (grepl("^params\\s*:", yaml_lines[[i]])) {
      i <- i + 1L
      while (i <= n && grepl("^\\s", yaml_lines[[i]]) && !grepl("^[A-Za-z]", yaml_lines[[i]])) {
        i <- i + 1L
      }
      # also skip blank lines inside? already handled by indent rule
      next
    }
    out <- c(out, yaml_lines[[i]])
    i <- i + 1L
  }
  out
}

format_params_yaml <- function(params) {
  if (!length(params)) return(character(0))
  # stable order: N, n, then alpha order
  nms <- names(params)
  pref <- intersect(c("N", "n"), nms)
  rest <- sort(setdiff(nms, pref))
  nms <- c(pref, rest)
  lines <- "params:"
  for (nm in nms) {
    tip <- yaml_escape(params[[nm]])
    lines <- c(lines, sprintf('  "%s": "%s"', nm, tip))
  }
  lines
}

insert_params <- function(yaml_lines, params_lines) {
  if (!length(params_lines)) return(yaml_lines)
  # insert before include_in_shiny / book_link / diagnosands if present, else at end
  anchors <- c("^diagnosands\\s*:", "^book_link\\s*:", "^include_in_shiny\\s*:")
  pos <- NA_integer_
  for (a in anchors) {
    hit <- which(grepl(a, yaml_lines))
    if (length(hit)) {
      pos <- hit[[1]]
      break
    }
  }
  if (is.na(pos)) {
    c(yaml_lines, params_lines)
  } else {
    c(yaml_lines[seq_len(pos - 1L)], params_lines, yaml_lines[seq.int(pos, length(yaml_lines))])
  }
}

files <- sort(list.files(dir, pattern = "\\.R$", full.names = TRUE))
report <- data.frame(
  file = character(),
  n_params = integer(),
  names = character(),
  source = character(),
  stringsAsFactors = FALSE
)

for (f in files) {
  parts <- split_yaml_code(f)
  existing <- parse_existing_params(parts$yaml_lines)
  pre <- extract_pre_design_assigns(parts$code)

  # Prefer design-discovered params when package + design load
  discovered <- character(0)
  disc_vals <- list()
  id <- sub("\\.R$", "", basename(f))
  if (pkg_ok) {
    d <- tryCatch(make_design(id), error = function(e) NULL)
    if (!is.null(d)) {
      args <- tryCatch(get_args(id), error = function(e) NULL)
      if (!is.null(args) && nrow(args)) {
        discovered <- as.character(args$name)
        for (i in seq_len(nrow(args))) {
          disc_vals[[args$name[[i]]]] <- args$value[[i]]
        }
      }
    }
  }

  # Candidate names: discovered if any, else pre-design assigns
  if (length(discovered)) {
    nms <- discovered
    src <- "discovered"
  } else if (length(pre)) {
    nms <- names(pre)
    src <- "pre_design"
  } else {
    nms <- character(0)
    src <- "none"
  }

  # Always include simple pre-design atomics even if discovery failed partially
  if (length(pre) && src == "discovered") {
    nms <- unique(c(nms, names(pre)))
  }

  params <- list()
  for (nm in nms) {
    if (nm %in% names(existing) && nzchar(existing[[nm]] %||% "")) {
      params[[nm]] <- existing[[nm]]
      next
    }
    val <- if (nm %in% names(disc_vals)) disc_vals[[nm]] else if (nm %in% names(pre)) pre[[nm]] else NULL
    params[[nm]] <- guess_tip(nm, val)
  }

  # Preserve existing tips for names not in candidates? Only if still valid — skip extras
  # (audit rejects extra YAML params)

  yaml_body <- strip_params_block(parts$yaml_lines %||% character(0))
  # If no YAML at all, create a minimal header
  if (is.null(parts$yaml_lines)) {
    yaml_body <- c(
      paste0("id: ", id),
      paste0("label: ", gsub("_", " ", id)),
      "category: rdss",
      "include_in_shiny: true"
    )
  }

  new_yaml <- insert_params(yaml_body, format_params_yaml(params))
  out_lines <- c("---", new_yaml, "---", "", strsplit(parts$code, "\n", fixed = TRUE)[[1]])
  # trim trailing empties excess
  while (length(out_lines) > 1L && !nzchar(out_lines[[length(out_lines)]]) && !nzchar(out_lines[[length(out_lines) - 1L]])) {
    out_lines <- out_lines[-length(out_lines)]
  }
  writeLines(out_lines, f, useBytes = TRUE)

  report <- rbind(
    report,
    data.frame(
      file = basename(f),
      n_params = length(params),
      names = paste(names(params), collapse = ", "),
      source = src,
      stringsAsFactors = FALSE
    )
  )
}

out_csv <- file.path(root, "tools/params_tips_report.csv")
write.csv(report, out_csv, row.names = FALSE)
message("Updated ", nrow(report), " files. Params total: ", sum(report$n_params))
message("Zero-param files: ", sum(report$n_params == 0L))
message("Report: ", out_csv)
print(utils::head(report[order(-report$n_params), ], 15))
cat("\nZero-param:\n")
print(report$file[report$n_params == 0L])
