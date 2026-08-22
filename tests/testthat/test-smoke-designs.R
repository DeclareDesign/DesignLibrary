test_that("each design loads with make_design and runs simulate_design once", {
  skip_if_not_installed("DeclareDesign")

  idx <- list_designs(discover_params = FALSE)
  expect_true(nrow(idx) >= 1L)
  expect_true("packages" %in% names(idx))

  for (i in seq_len(nrow(idx))) {
    id <- idx$id[[i]]
    if ("functional" %in% names(idx) && !isTRUE(idx$functional[[i]])) next
    pkg_str <- idx$packages[[i]]
    if (is.null(pkg_str) || is.na(pkg_str)) pkg_str <- ""
    pkgs <- trimws(strsplit(pkg_str, ",")[[1]])
    pkgs <- pkgs[nzchar(pkgs)]
    missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
    if (length(missing)) {
      # Do not skip() the whole test — just leave this design for a dedicated test
      next
    }

    design <- make_design(id)
    expect_true(
      inherits(design, "design"),
      info = paste0(id, ": make_design() did not return a design")
    )

    sim <- DeclareDesign::simulate_design(design, sims = 1)
    expect_true(
      is.data.frame(sim) || inherits(sim, "simulations_df") || length(sim) > 0L,
      info = paste0(id, ": simulate_design(..., sims = 1) failed or returned empty")
    )
  }
})

test_that("list_designs reports YAML packages for dependency example", {
  idx <- list_designs(discover_params = FALSE)
  row <- idx[idx$id == "logit_probit_ols", , drop = FALSE]
  skip_if(nrow(row) == 0L, "logit_probit_ols not in library")
  expect_match(row$packages[[1]], "margins")
})

test_that("logit_probit_ols runs when its packages are installed", {
  skip_if_not_installed("DeclareDesign")
  skip_if_not_installed("margins")
  skip_if_not_installed("broom")

  design <- make_design("logit_probit_ols")
  expect_true(inherits(design, "design"))
  sim <- DeclareDesign::simulate_design(design, sims = 1)
  expect_true(is.data.frame(sim) || inherits(sim, "simulations_df") || length(sim) > 0L)
})
