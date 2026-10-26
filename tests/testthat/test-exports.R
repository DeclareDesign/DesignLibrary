test_that("previews ship for the core designs and read back", {
  expect_true(has_preview("two_arm_simple"))
  preview <- get_preview("two_arm_simple")
  expect_equal(preview$id, "two_arm_simple")
  expect_true(all(c("sims", "summary", "diagnosis") %in% names(preview)))
  expect_null(get_preview("no_such_design"))
  expect_null(get_preview("../no_such_design"))
  expect_false(has_preview("no_such_design"))
})

test_that("design_profile() is the printed design_info()", {
  profile <- design_profile("two_arm_simple")
  expect_type(profile, "character")
  expect_length(profile, 1)
  expect_match(profile, "^two_arm_simple declares")
})

test_that("excluded_diagnosands() is empty when the YAML excludes none", {
  expect_identical(excluded_diagnosands("two_arm_simple"), character(0))
  expect_error(excluded_diagnosands("no_such_design"), "Unknown design")
})

test_that("make_index() lists every design file once", {
  idx <- make_index()
  expect_s3_class(idx, "data.frame")
  expect_false(anyDuplicated(idx$id) > 0)
  expect_true("two_arm_simple" %in% idx$id)
  expect_type(idx$alias, "character")
})

test_that("param_coverage_gaps() finds nothing missing in two_arm_simple", {
  gaps <- param_coverage_gaps("two_arm_simple")
  expect_s3_class(gaps, "data.frame")
  expect_equal(nrow(gaps), 0)
})

test_that("audit_designs() and write_audit_report() run one design into a tempdir", {
  audit <- audit_designs(designs = "two_arm_simple", sims = 1)
  expect_s3_class(audit, "design_library_audit")
  expect_equal(audit$n_ok, 1L)
  expect_equal(audit$n_fail, 0L)
  dir <- withr::local_tempdir()
  paths <- suppressMessages(write_audit_report(audit, dir = dir))
  expect_true(all(file.exists(paths)))
  expect_true(all(startsWith(normalizePath(paths), normalizePath(dir))))
})

test_that("maintainer writers refuse an installed package", {
  installed <- system.file(package = "DesignLibrary")
  skip_if_not(file.exists(file.path(installed, "DESCRIPTION")), "loaded from source")
  withr::local_options(DesignLibrary.root = installed)
  expect_error(refresh_library(sims = 1), "source tree")
  expect_error(bake_previews(designs = "two_arm_simple", sims = 1), "source tree")
  skip_if_not_installed("pkgdown")
  expect_error(build_docs(), "source tree")
})
