# Redesigning a parameter to the value it already holds must not change the
# data; a difference means the parameter reported is not the one the design
# reads, or that redesign() splits a value into a sweep. Redesigning it to a
# different value must change the data, the estimands, or the estimates; no
# change means the parameter is dead.
test_that("each parameter is live, and redesigning it to its own value is a no-op", {
  skip_on_cran()
  skip_if_not_installed("DeclareDesign", "2.0.0")

  draw <- function(d) {
    set.seed(343)
    DeclareDesign::draw_data(d)
  }
  run <- function(d) {
    set.seed(343)
    DeclareDesign::run_design(d)
  }
  perturb <- function(x) {
    if (is.logical(x)) return(!x)
    if (!is.numeric(x)) return(NULL)
    # Probability vectors stay probability vectors
    if (length(x) > 1L && all(x >= 0) && abs(sum(x) - 1) < 1e-8) {
      w <- x + seq_along(x)
      return(w / sum(w))
    }
    if (all(x >= 2 & x == round(x))) return(x + 2)
    if (all(x > 0 & x < 1)) return(x / 2)
    x + 0.5
  }
  redesign_one <- function(design, nm, val) {
    do.call(DeclareDesign::redesign, c(list(design), stats::setNames(list(list(val)), nm)))
  }

  # Live, but without effect at the defaults: the weights average cell effects
  # that are all zero, and the correlation scales a clue probability of zero.
  inert_at_defaults <- c(
    "two_by_two$weight_A", "two_by_two$weight_B",
    "process_tracing_bayes$cor_E1E2_not_H"
  )

  ids <- list_designs(list_all = TRUE)$id
  n_checked <- 0L
  not_perturbed <- character(0)
  for (id in ids) {
    pkgs <- DesignLibrary:::resolve_design(id)$meta$packages
    if (!all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE))) next
    design <- make_design(id)
    base <- draw(design)

    args <- get_args(id)
    args <- args[args$kind %in% c("scalar", "vector"), , drop = FALSE]
    for (i in seq_len(nrow(args))) {
      nm <- args$name[[i]]
      val <- args$default[[i]]
      label <- paste0(id, "$", nm)

      same <- tryCatch(draw(redesign_one(design, nm, val)), error = function(e) e, warning = function(w) w)
      expect_false(inherits(same, "condition"), label = paste0(label, ": ", if (inherits(same, "condition")) conditionMessage(same)))
      if (!inherits(same, "condition")) expect_equal(same, base, label = label)

      new_val <- perturb(val)
      changed <- if (!is.null(new_val)) tryCatch({
        moved <- redesign_one(design, nm, new_val)
        !identical(draw(moved), base) || !identical(run(moved), run(design))
      }, error = function(e) NA, warning = function(w) NA)
      if (label %in% inert_at_defaults) next
      if (is.null(new_val) || is.na(changed)) {
        not_perturbed <- c(not_perturbed, label)
        next
      }
      expect_true(changed, label = paste0(label, " changes nothing when set to ", deparse(new_val)))
      n_checked <- n_checked + 1L
    }
  }
  expect_gt(n_checked, 100L)
  # Character parameters, and values the perturbation makes invalid, are not
  # checked for liveness; list them so a growing number is visible.
  if (length(not_perturbed)) {
    message("Not perturbed (", length(not_perturbed), "): ", paste(not_perturbed, collapse = ", "))
  }
})
