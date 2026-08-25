# Check ported designers against DeclareDesign 2.0 and the frozen baseline.
#
#   Rscript tools/check_port.R two_arm_attrition_designer [more names...]
#   Rscript tools/check_port.R            # every designer
#
# For each designer: it builds with defaults and its formals match the
# baseline; run_design() returns estimates that are not all NA; a 30-sim
# diagnosis has exactly the baseline's (inquiry, estimator) rows; the `code`
# attribute parses and evaluates to a design that runs; and every argument in
# `shiny_arguments` changes a seeded run when redesign() changes it, which
# is the check a slider interface cannot survive quietly.
suppressPackageStartupMessages({
  library(DeclareDesign); library(fabricatr); library(randomizr); library(estimatr)
  devtools::load_all(".", quiet = TRUE)
})
stopifnot(packageVersion("DeclareDesign") >= "2.0.0")
baseline <- readRDS("tests/baseline/designers_baseline.rds")
args <- commandArgs(TRUE)
names_all <- grep("_designer$", ls("package:DesignLibrary"), value = TRUE)
names_all <- names_all[!grepl("^simple_", names_all)]
todo <- if (length(args)) args else names_all

same_rows <- function(diag, base) {
  a <- unique(paste(diag$inquiry, diag$estimator, sep = " | "))
  b <- unique(paste(base$inquiry, base$estimator, sep = " | "))
  setequal(a, b)
}

check_one <- function(nm) {
  designer <- get(nm, envir = asNamespace("DesignLibrary"))
  out <- list(designer = nm)
  d <- designer()
  out$builds <- inherits(d, "design")
  base1 <- baseline$tier1[[nm]]
  out$formals_match <- setequal(names(formals(designer)), names(base1$formals))
  r <- run_design(d)
  # Process tracing returns posterior_H rather than estimate; any numeric
  # column the estimators filled counts.
  num <- names(r)[vapply(r, is.numeric, logical(1))]
  num <- setdiff(num, c("estimand", "sim_ID"))
  out$estimates_ok <- nrow(r) > 0 && length(num) > 0 &&
    any(vapply(num, function(nm) !all(is.na(r[[nm]])), logical(1)))
  set.seed(1)
  diag <- diagnose_design(d, sims = 30, bootstrap_sims = 0)$diagnosands_df
  out$rows_match_baseline <- same_rows(diag, baseline$tier2[[nm]])
  if (!out$rows_match_baseline) {
    out$rows_diff <- paste0(
      "missing: ", paste(setdiff(unique(paste(baseline$tier2[[nm]]$inquiry, baseline$tier2[[nm]]$estimator, sep = " | ")),
                                 unique(paste(diag$inquiry, diag$estimator, sep = " | "))), collapse = "; "),
      " extra: ", paste(setdiff(unique(paste(diag$inquiry, diag$estimator, sep = " | ")),
                               unique(paste(baseline$tier2[[nm]]$inquiry, baseline$tier2[[nm]]$estimator, sep = " | "))), collapse = "; "))
  }
  code <- attr(d, "code")
  out$code_runs <- tryCatch({
    env <- new.env(parent = globalenv())
    eval(parse(text = code), envir = env)
    objs <- Filter(function(x) inherits(x, "design"), mget(ls(env), envir = env))
    length(objs) >= 1 && nrow(draw_estimates(objs[[length(objs)]])) > 0
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
  shiny <- attr(designer, "shiny_arguments")
  dead <- character(0); notfound <- character(0)
  # Defaults may read other formals (mean_A0B1 = outcome_means[2]), so they
  # are evaluated as R would, as promises in one environment.
  fenv <- new.env()
  for (f in names(formals(designer))) {
    # do.call() inlines the default's expression into the call, so the promise
    # holds `outcome_means[2]` rather than `formals(designer)[[f]]`.
    do.call(delayedAssign, list(f, formals(designer)[[f]],
                                eval.env = fenv, assign.env = fenv))
  }
  for (p in names(shiny)) {
    default <- tryCatch(get(p, envir = fenv), error = function(e) NULL)
    alt <- shiny[[p]]
    alt <- alt[!vapply(alt, function(v) isTRUE(all.equal(v, default)), logical(1))]
    if (!length(alt)) next
    w <- NULL
    d2 <- withCallingHandlers(
      do.call(redesign, c(list(d), setNames(list(alt[[1]]), p))),
      warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
    if (any(grepl("not found in the design", w))) { notfound <- c(notfound, p); next }
    set.seed(3); a <- run_design(d); set.seed(3); b <- run_design(d2)
    if (isTRUE(all.equal(a, b))) dead <- c(dead, p)
  }
  out$params_checked <- length(shiny)
  out$dead <- paste(dead, collapse = ",")
  out$not_found <- paste(notfound, collapse = ",")
  out
}

for (nm in todo) {
  res <- tryCatch(check_one(nm), error = function(e) list(designer = nm, error = conditionMessage(e)))
  if (!is.null(res$error)) { cat(sprintf("%-36s ERROR: %s\n", nm, gsub("\\s+", " ", res$error))); next }
  cat(sprintf("%-36s builds %s formals %s estimates %s rows %s code %s params %d dead [%s] not_found [%s]\n",
              nm, res$builds, res$formals_match, res$estimates_ok, res$rows_match_baseline,
              if (isTRUE(res$code_runs)) "TRUE" else res$code_runs, res$params_checked, res$dead, res$not_found))
  if (!is.null(res$rows_diff)) cat("    ", res$rows_diff, "\n")
}
