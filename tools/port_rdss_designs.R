# Port rdss_wizard declarations into DesignLibrary/inst/designs/
# Run from DesignLibrary root:
#   Rscript tools/port_rdss_designs.R

wizard_dir <- "C:/WZB Dropbox/Macartan Humphreys/5_github/rdss_wizard"
decl_dir <- file.path(wizard_dir, "assets/declarations")
meta_path <- file.path(wizard_dir, "assets/designs_metadata.csv")
out_dir <- "C:/WZB Dropbox/Macartan Humphreys/5_github/ResearchDesigns/inst/designs"

meta <- read.csv(meta_path, stringsAsFactors = FALSE)
meta$decl_file <- basename(meta$declaration_file)
meta$alias <- sub("^declaration_", "", sub("\\.R$", "", meta$decl_file, ignore.case = TRUE))

# Keep hand-crafted ports (do not overwrite)
keep_ids <- c(
  "two_arm_trial",
  "two_arm_with_blocks",
  "two_arm_trial_rdss",
  "two_arm_with_blocks_rdss",
  "pate_with_sampling",
  "logit_probit_ols"
)
keep_aliases <- c("2.1", "2.2", "4.1", "11.5")

# Prefer descriptive ids for short / awkward labels
id_overrides <- c(
  "2.1" = "two_arm_trial_rdss",
  "2.2" = "two_arm_with_blocks_rdss",
  "4.1" = "pate_with_sampling",
  "5.1" = "example_declaration",
  "7.1" = "population_estimands",
  "9.1" = "italian_village",
  "9.2" = "italian_village_continued",
  "9.3" = "italian_village_bayes",
  "9.4" = "italian_village_vary_mean",
  "9.5" = "bootstrapped",
  "9.6" = "restoring_parallelism",
  "9.7" = "randomization_inference",
  "10.1" = "declaration_using_declare",
  "10.2" = "design_10_2",
  "10.3" = "two_outcome_models",  # split below
  "10.4" = "design_10_4",
  "10a" = "design_10a",
  "11.1" = "baseline_over_N",
  "11.2" = "uncertainty_over_effect_size",
  "11.3" = "bare_bones_two_arm",
  "11.4" = "conditional_expectation",
  "11.5" = "logit_probit_ols",
  "12.1a" = "village_campaign",  # combined from a–d
  "13.1" = "linear_regression",
  "13.2" = "two_arm_randomized_experiment",
  "15.1" = "simple_random_sampling",
  "15.2" = "survey_nonresponse",
  "15.3" = "cluster_random_sampling",
  "15.3a" = "cluster_sampling_part_a",
  "15.3b" = "cluster_sampling_part_b",
  "15.4" = "multilevel",
  "15.5" = "multilevel_answer_strategies",
  "15.6" = "latent_variables",
  "16.1a" = "process_tracing_setup",
  "16.1b" = "process_tracing",
  "16.2" = "matching",
  "16.3" = "diff_in_diff",
  "16.4" = "instrumental_variables",
  "16.5" = "regression_discontinuity",
  "16.6" = "regression_discontinuity_fuzzy",
  "17.1" = "audit_experiment",
  "17.2" = "audit_intervention",
  "17.3" = "list_experiment",
  "17.4" = "list_or_direct_questions",
  "17.5" = "conjoint",
  "17.6" = "trust_game",
  "17.6_a" = "trust_game_variant",
  "18.1" = "two_arm_trial_library",
  "18.2" = "covariate_adjustment",
  "18.3" = "lin_estimator",
  "18.4" = "block_randomized_trial",
  "18.5" = "blocked_and_clustered",
  "18.6" = "subgroup_effects",
  "18.7" = "factorial_2x2",
  "18.8" = "encouragement",
  "18.9a" = "encouragement_vs_placebo",
  "18.9b" = "encouragement_design_b",
  "18.9c" = "encouragement_design_c",
  "18.10" = "stepped_wedge",
  "18.11" = "single_period_two_arm",
  "18.12" = "randomized_saturation",
  "18.13" = "network_experiment",
  "19.1" = "random_forests",
  "19.2" = "structural_estimation",
  "19.3" = "meta_analysis",
  "19.4" = "multi_site_studies",
  "23.1a" = "reanalysis_part_a",
  "23.1b" = "reanalysis_part_b",
  "23.1c" = "reanalysis_part_c",
  "23.1d" = "reanalysis"
)

slugify <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[''`]", "", x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x <- gsub("_+", "_", x)
  if (!nzchar(x)) x <- "design"
  if (grepl("^[0-9]", x)) x <- paste0("design_", x)
  x
}

yaml_quote <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  paste0('"', x, '"')
}

yaml_block <- function(id, alias, label, description, category, keywords,
                       packages, diagnosands, book_link, include_in_shiny) {
  lines <- c("---", paste0("id: ", id))
  if (!is.null(alias) && !is.na(alias) && nzchar(alias)) {
    lines <- c(lines, paste0("alias: ", yaml_quote(alias)))
  }
  lines <- c(lines, paste0("label: ", label))
  lines <- c(lines, paste0("category: ", category))
  if (length(keywords)) {
    lines <- c(lines, paste0("keywords: [", paste(keywords, collapse = ", "), "]"))
  }
  desc <- trimws(description %||% "")
  if (nzchar(desc)) {
    # fold as YAML folded scalar
    lines <- c(lines, "description: >")
    wrapped <- strwrap(desc, width = 72)
    lines <- c(lines, paste0("  ", wrapped))
  }
  if (length(packages)) {
    lines <- c(lines, paste0("packages: [", paste(packages, collapse = ", "), "]"))
  }
  if (length(diagnosands)) {
    lines <- c(lines, paste0("diagnosands: [", paste(diagnosands, collapse = ", "), "]"))
  }
  if (!is.null(book_link) && !is.na(book_link) && nzchar(book_link)) {
    lines <- c(lines, paste0("book_link: ", book_link))
  }
  shiny <- isTRUE(include_in_shiny) || identical(tolower(as.character(include_in_shiny)), "true")
  lines <- c(lines, paste0("include_in_shiny: ", if (shiny) "true" else "false"))
  lines <- c(lines, "---", "")
  paste(lines, collapse = "\n")
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a

extract_packages <- function(code) {
  libs <- character(0)
  m <- gregexpr("(?:library|require)\\(([^)]+)\\)", code, perl = TRUE)
  hits <- regmatches(code, m)[[1]]
  if (length(hits)) {
    pkgs <- gsub("(?:library|require)\\(|\\)|\"|'|\\s", "", hits)
    pkgs <- gsub(",.*$", "", pkgs)
    libs <- unique(pkgs[nzchar(pkgs)])
  }
  # strip library/require lines
  code2 <- gsub("(?m)^\\s*(library|require)\\([^)]*\\)\\s*(#.*)?$", "", code, perl = TRUE)
  list(packages = libs, code = code2)
}

# Read all declaration sources keyed by alias (from filename)
src_files <- list.files(decl_dir, pattern = "\\.R$", full.names = TRUE)
src_by_alias <- list()
for (f in src_files) {
  al <- sub("^declaration_", "", sub("\\.R$", "", basename(f), ignore.case = TRUE))
  src_by_alias[[al]] <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# Combine 12.1a–d into one design source under alias 12.1a
combine_12.1 <- function() {
  parts <- paste(
    src_by_alias[["12.1a"]],
    src_by_alias[["12.1b"]],
    src_by_alias[["12.1c"]],
    src_by_alias[["12.1d"]],
    sep = "\n\n"
  )
  paste0(
    parts,
    "\n\ndesign <- model_12.1 + inquiry_12.1 + data_strategy_12.1 + answer_strategy_12.1\n"
  )
}

# Expand declaration_X references by inlining parent source (best-effort)
inline_deps <- function(alias, code, depth = 0L) {
  if (depth > 8L) return(code)
  refs <- unique(regmatches(code, gregexpr("declaration_[A-Za-z0-9._]+", code, perl = TRUE))[[1]])
  refs <- setdiff(refs, paste0("declaration_", alias))
  if (!length(refs)) return(code)
  prepend <- character(0)
  for (ref in refs) {
    parent_alias <- sub("^declaration_", "", ref)
    if (!parent_alias %in% names(src_by_alias)) next
    parent_code <- src_by_alias[[parent_alias]]
    parent_code <- inline_deps(parent_alias, parent_code, depth + 1L)
    # rename parent's main assignment to the declaration_* name if needed
    parent_code <- gsub(
      paste0("declaration_", gsub("\\.", "\\\\.", parent_alias), "\\s*<-"),
      paste0("declaration_", parent_alias, " <-"),
      parent_code
    )
    prepend <- c(prepend, parent_code, "")
  }
  paste(c(prepend, code), collapse = "\n")
}

transform_code <- function(alias, code) {
  pkg <- extract_packages(code)
  code <- pkg$code
  code <- inline_deps(alias, code)

  # Special: 12.1 combined already assigns design
  if (alias == "12.1a") {
    return(list(code = trimws(code), packages = pkg$packages, object = "design", ok = TRUE))
  }

  # 10.3 has two designs — handled separately
  if (alias == "10.3") {
    return(list(code = code, packages = pkg$packages, object = "special", ok = TRUE))
  }

  # Rename main declaration_* <- to design <-
  main <- paste0("declaration_", alias)
  if (grepl(paste0(main, "\\s*<-"), code)) {
    code <- sub(paste0(main, "\\s*<-"), "design <-", code)
  } else if (grepl("\\bdesign\\s*<-", code)) {
    # already design
  } else {
    # try any declaration_* assignment as main
    m <- regexpr("declaration_[A-Za-z0-9._]+\\s*<-", code, perl = TRUE)
    if (m > 0) {
      code <- sub("declaration_[A-Za-z0-9._]+\\s*<-", "design <-", code, perl = TRUE)
    } else if (!grepl("\\bdesign\\s*<-", code)) {
      # no design object — mark incomplete
      return(list(code = code, packages = pkg$packages, object = NA_character_, ok = FALSE))
    }
  }

  # Collapse leftover model + inquiry + ... style already composed into design
  list(code = trimws(code), packages = unique(pkg$packages), object = "design", ok = TRUE)
}

keywords_from_group <- function(group, label) {
  g <- tolower(group %||% "")
  lab <- tolower(label %||% "")
  out <- character(0)
  if (grepl("experimental", g)) out <- c(out, "experiment")
  if (grepl("observational", g)) out <- c(out, "observational")
  if (grepl("descriptive", g)) out <- c(out, "descriptive")
  if (grepl("causal", g)) out <- c(out, "causal")
  if (grepl("complex", g)) out <- c(out, "complex")
  if (grepl("block", lab)) out <- c(out, "blocking")
  if (grepl("sample|survey", lab)) out <- c(out, "sampling")
  if (grepl("cluster", lab)) out <- c(out, "cluster")
  unique(out)
}

fill_description <- function(row) {
  desc <- trimws(row$description %||% "")
  if (nzchar(desc)) return(desc)
  lab <- trimws(row$label %||% row$alias)
  paste0("Research design from RDSS (", row$alias, "): ", lab, ".")
}

lookup_id <- function(alias, label) {
  if (alias %in% names(id_overrides)) return(unname(id_overrides[[alias]]))
  slugify(label)
}

fill_label <- function(row) {
  lab <- trimws(row$label %||% "")
  if (nzchar(lab) && !grepl("^[0-9]", lab) && nchar(lab) > 3) {
    lab <- gsub("!+$", "", lab)
    lab <- trimws(lab)
    return(lab)
  }
  id <- lookup_id(row$alias, lab)
  gsub("_", " ", id)
}

# Build one meta row per unique decl file (fix stepped-wedge alias)
meta_u <- meta[!duplicated(meta$decl_file), , drop = FALSE]

# Ensure 18.10 row exists with correct alias (CSV id wrongly 18.1)
meta_u$alias[meta_u$decl_file == "declaration_18.10.R"] <- "18.10"

# Skip non-designs / pieces / redesign wrappers / design lists
skip_aliases <- c(
  "10a", "12.1a", "12.1b", "12.1c", "12.1d",
  "15.3a", "15.3b", "16.1a", "17.6_a",
  "18.9a", "18.9b", "18.9c",
  "19.3", "23.1a", "23.1b", "23.1c", "23.1d",
  "9.4", "9.6"
)
# village_campaign is maintained separately as the combined 12.1 design

# Already ported aliases
skip_write_aliases <- keep_aliases

make_unique_ids <- function(aliases, labels) {
  ids <- character(length(aliases))
  used <- keep_ids
  for (i in seq_along(aliases)) {
    al <- aliases[[i]]
    id <- lookup_id(al, labels[[i]])
    base <- id
    k <- 2L
    while (id %in% used) {
      id <- paste0(base, "_", k)
      k <- k + 1L
    }
    used <- c(used, id)
    ids[[i]] <- id
  }
  ids
}

aliases <- meta_u$alias
labels <- vapply(seq_len(nrow(meta_u)), function(i) fill_label(meta_u[i, , drop = FALSE]), "")
ids <- make_unique_ids(aliases, labels)

# Fix pate / existing category: update only non-kept files below
# Also update pate_with_sampling category to rdss
pate_path <- file.path(out_dir, "pate_with_sampling.R")
if (file.exists(pate_path)) {
  txt <- paste(readLines(pate_path, warn = FALSE), collapse = "\n")
  if (grepl("category: template", txt)) {
    txt <- sub("category: template", "category: rdss", txt)
    writeLines(txt, pate_path, useBytes = TRUE)
    message("Updated pate_with_sampling category -> rdss")
  }
}

report <- data.frame(
  alias = character(),
  id = character(),
  status = character(),
  note = character(),
  stringsAsFactors = FALSE
)

write_design <- function(path, yaml, code) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(paste0(yaml, code, "\n"), path, useBytes = TRUE)
}

# Special combined 12.1
src_by_alias[["12.1a"]] <- combine_12.1()

for (i in seq_len(nrow(meta_u))) {
  row <- meta_u[i, , drop = FALSE]
  alias <- row$alias
  id <- ids[[i]]
  label <- labels[[i]]

  if (alias %in% skip_aliases) {
    report <- rbind(report, data.frame(alias = alias, id = id, status = "skipped", note = "absorbed into village_campaign", stringsAsFactors = FALSE))
    next
  }
  if (alias %in% skip_write_aliases) {
    report <- rbind(report, data.frame(alias = alias, id = lookup_id(alias, label), status = "kept", note = "existing hand port", stringsAsFactors = FALSE))
    next
  }
  if (!alias %in% names(src_by_alias)) {
    report <- rbind(report, data.frame(alias = alias, id = id, status = "missing", note = "source file missing", stringsAsFactors = FALSE))
    next
  }

  tr <- transform_code(alias, src_by_alias[[alias]])
  packages <- tr$packages
  diagnosands <- character(0)
  if (identical(alias, "16.1b") || grepl("process tracing", tolower(label))) {
    diagnosands <- c("rmse")
  }

  # category: rdss for book-numbered designs
  category <- "rdss"

  # Special 10.3: two designs
  if (alias == "10.3") {
    code <- tr$code
    code_a <- sub("declaration_10\\.3a\\s*<-", "design <-", code)
    code_a <- gsub("declaration_10\\.3b\\s*<-.*", "", code_a)
    code_b <- sub("declaration_10\\.3b\\s*<-", "design <-", code)
    # keep both M1/M2/IDA and only one design assignment for b
    # simpler: evaluate-style split
    code_a <- paste(readLines(file.path(decl_dir, "declaration_10.3.R"), warn = FALSE), collapse = "\n")
    code_a <- sub("declaration_10\\.3a\\s*<-", "design <-", code_a)
    code_a <- sub("(?m)^declaration_10\\.3b\\s*<-.*$", "", code_a, perl = TRUE)
    code_b <- paste(readLines(file.path(decl_dir, "declaration_10.3.R"), warn = FALSE), collapse = "\n")
    code_b <- sub("declaration_10\\.3b\\s*<-", "design <-", code_b)
    code_b <- sub("(?m)^declaration_10\\.3a\\s*<-.*$", "", code_b, perl = TRUE)

    for (pair in list(
      list(id = "two_outcome_model_a", alias = "10.3a", code = code_a, lab = "Two-outcome model A"),
      list(id = "two_outcome_model_b", alias = "10.3b", code = code_b, lab = "Two-outcome model B")
    )) {
      y <- yaml_block(
        id = pair$id, alias = pair$alias, label = pair$lab,
        description = fill_description(row), category = category,
        keywords = keywords_from_group(row$group, pair$lab),
        packages = packages, diagnosands = diagnosands,
        book_link = row$book_link, include_in_shiny = row$include_in_shiny
      )
      write_design(file.path(out_dir, paste0(pair$id, ".R")), y, trimws(pair$code))
      report <- rbind(report, data.frame(alias = pair$alias, id = pair$id, status = "written", note = "split from 10.3", stringsAsFactors = FALSE))
    }
    next
  }

  if (!isTRUE(tr$ok)) {
    # still write incomplete with include_in_shiny false and note
    y <- yaml_block(
      id = id, alias = alias, label = label,
      description = paste(fill_description(row), "(Incomplete port: no design object found in source.)"),
      category = category,
      keywords = keywords_from_group(row$group, label),
      packages = packages, diagnosands = diagnosands,
      book_link = row$book_link, include_in_shiny = FALSE
    )
    write_design(file.path(out_dir, paste0(id, ".R")), y, tr$code)
    report <- rbind(report, data.frame(alias = alias, id = id, status = "incomplete", note = "no design object", stringsAsFactors = FALSE))
    next
  }

  shiny_flag <- row$include_in_shiny
  y <- yaml_block(
    id = id, alias = alias, label = label,
    description = fill_description(row), category = category,
    keywords = keywords_from_group(row$group, label),
    packages = packages, diagnosands = diagnosands,
    book_link = row$book_link, include_in_shiny = shiny_flag
  )
  write_design(file.path(out_dir, paste0(id, ".R")), y, tr$code)
  report <- rbind(report, data.frame(alias = alias, id = id, status = "written", note = "", stringsAsFactors = FALSE))
}

# Templates without aliases already exist; leave them.

out_report <- file.path("C:/WZB Dropbox/Macartan Humphreys/5_github/ResearchDesigns/tools/port_report.csv")
write.csv(report, out_report, row.names = FALSE)
message("Wrote ", sum(report$status == "written"), " designs; kept ", sum(report$status == "kept"),
        "; incomplete ", sum(report$status == "incomplete"),
        "; skipped ", sum(report$status == "skipped"))
message("Report: ", out_report)
print(table(report$status))
