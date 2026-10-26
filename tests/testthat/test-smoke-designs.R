# One test per design. On CRAN, only designs whose packages are all in
# Suggests run; the rest (Stan fits, packages CRAN machines may have but we
# do not declare) are skipped there.
smoke_idx <- list_designs(discover_params = FALSE)
suggested <- trimws(sub("\\s*\\(.*", "", strsplit(
  utils::packageDescription("DesignLibrary")$Suggests, ","
)[[1]]))

for (i in seq_len(nrow(smoke_idx))) {
  local({
    id <- smoke_idx$id[[i]]
    functional <- !("functional" %in% names(smoke_idx)) || isTRUE(smoke_idx$functional[[i]])
    pkg_str <- smoke_idx$packages[[i]]
    if (is.null(pkg_str) || is.na(pkg_str)) pkg_str <- ""
    pkgs <- trimws(strsplit(pkg_str, ",")[[1]])
    pkgs <- pkgs[nzchar(pkgs)]

    test_that(paste0(id, ": make_design() builds and simulate_design() runs once"), {
      skip_if_not(functional, paste0(id, " is marked functional: false"))
      if (!all(pkgs %in% suggested)) skip_on_cran()
      for (p in pkgs) skip_if_not_installed(p)

      design <- make_design(id)
      expect_s3_class(design, "design")

      sim <- DeclareDesign::simulate_design(design, sims = 1)
      expect_gte(nrow(sim), 1L)
      if ("estimate" %in% names(sim)) {
        expect_false(all(is.na(sim$estimate)), info = id)
      }
    })
  })
}

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
