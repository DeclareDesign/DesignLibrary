test_that("param coverage: declared+used objects appear in design params (atomic)", {
  skip_if_not_installed("DeclareDesign")
  skip_on_cran()

  if (dir.exists("../../inst/designs")) {
    options(DesignLibrary.root = normalizePath("../..", winslash = "/"))
  }

  rep <- param_coverage_report(atomic_only = TRUE)
  # Persist full report (including non-atomic) for inspection when this fails
  full <- param_coverage_report(atomic_only = FALSE)
  out <- file.path(tempdir(), "param_coverage_report.csv")
  utils::write.csv(full, out, row.names = FALSE)

  bad <- rep[!is.na(rep$type) & !rep$type %in% c("load_error", "check_error"), , drop = FALSE]
  if (nrow(bad)) {
    msg <- paste0(
      "Atomic objects declared before design, used by design, but not in design parameters:\n",
      paste(sprintf("  %s: %s (%s)", bad$id, bad$name, bad$type), collapse = "\n"),
      "\nFull report: ", out
    )
    fail(msg)
  } else {
    succeed()
  }
})
