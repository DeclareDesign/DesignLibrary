#!/usr/bin/env Rscript
# Check every design in its OWN R process, and check that estimates are numbers.
#
# audit_designs() cannot do either. It loops over designs in one session, and
# ensure_packages_attached() calls library(), so a design that forgets its
# packages: line is covered by whatever ran before it in alphabetical order.
# Six designs were silently relying on that. It also treats a design as OK when
# diagnose_design() returns without erroring, and DeclareDesign 2.0 records a
# failed estimator draw rather than aborting, so an estimator that never
# produces a number reads as healthy.
#
# Usage:
#   Rscript tools/check_designs_isolated.R            # every design
#   Rscript tools/check_designs_isolated.R <id> ...   # named designs
#
# One line per design. Exit status is 1 if any design errored or returned a
# non-finite estimate.

`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = TRUE)
# run from the package root, or from tools/
root <- getwd()
if (!dir.exists(file.path(root, "inst", "designs")) &&
    dir.exists(file.path(root, "..", "inst", "designs"))) {
  root <- normalizePath(file.path(root, ".."))
}
if (!dir.exists(file.path(root, "inst", "designs"))) {
  stop("run this from the DesignLibrary package root", call. = FALSE)
}

# The worker runs in a fresh process, so nothing another design attached leaks in.
worker <- function(id, root) {
  script <- c(
    sprintf('suppressMessages(pkgload::load_all(%s, quiet = TRUE))', deparse(root)),
    sprintf('id <- %s', deparse(id)),
    'p <- parse_design_file(file.path("inst/designs", paste0(id, ".R")))',
    'if (!isTRUE(p$meta$functional)) { cat("PARKED\\n"); quit(status = 0) }',
    'out <- tryCatch({',
    '  d <- eval_design(p)',
    '  es <- draw_estimates(d)',
    '  if (is.null(es) || !nrow(es)) "NO ESTIMATE ROWS"',
    '  else {',
    '    bad <- sum(!is.finite(es$estimate))',
    '    if (bad) sprintf("NON-FINITE %d/%d estimates", bad, nrow(es))',
    '    else sprintf("ok %d estimates", nrow(es))',
    '  }',
    '}, error = function(e) paste("ERROR", gsub("\\n", " ", conditionMessage(e))))',
    'cat(out, "\\n", sep = "")'
  )
  f <- tempfile(fileext = ".R")
  writeLines(script, f)
  on.exit(unlink(f))
  res <- suppressWarnings(system2("Rscript", f, stdout = TRUE, stderr = FALSE))
  if (!length(res)) "ERROR worker produced no output" else trimws(tail(res, 1))
}

ids <- if (length(args)) args else
  sub("\\.R$", "", basename(list.files(file.path(root, "inst", "designs"), pattern = "\\.R$")))

setwd(root)
bad <- character(0)
for (id in ids) {
  res <- worker(id, root)
  cat(sprintf("%-32s %s\n", id, res))
  if (grepl("^(ERROR|NON-FINITE|NO ESTIMATE)", res)) bad <- c(bad, id)
}

cat("\n", length(ids) - length(bad), "/", length(ids), " clean\n", sep = "")
if (length(bad)) {
  cat("problems: ", paste(bad, collapse = ", "), "\n", sep = "")
  quit(status = 1)
}
