# Redesigning a parameter to the value it already holds must not change the
# data. A difference means the parameter reported is not the one the design
# reads (a phantom parameter), or that redesign() splits a value into a sweep.
test_that("redesigning every parameter to its current value leaves draws unchanged", {
  skip_on_cran()
  skip_if_not_installed("DeclareDesign", "2.0.0")

  draw <- function(d) {
    set.seed(343)
    DeclareDesign::draw_data(d)
  }

  ids <- list_designs(list_all = TRUE)$id
  n_checked <- 0L
  for (id in ids) {
    design <- tryCatch(make_design(id), error = function(e) NULL)
    if (is.null(design)) next
    base <- tryCatch(draw(design), error = function(e) NULL)
    if (is.null(base)) next

    objs <- DeclareDesign::design_parameters(design)
    if (is.null(objs) || nrow(objs) == 0L) next
    params <- filter_modifiable_params(objs)

    for (nm in params$name) {
      val <- DeclareDesign:::current_param_value(design, nm)
      if (is.null(val)) next
      after <- tryCatch(
        draw(do.call(DeclareDesign::redesign, c(list(design), setNames(list(list(val)), nm)))),
        error = function(e) e,
        warning = function(w) w
      )
      expect_false(
        inherits(after, "condition"),
        label = paste0(id, "$", nm, ": ", if (inherits(after, "condition")) conditionMessage(after))
      )
      if (!inherits(after, "condition")) {
        expect_equal(after, base, label = paste0(id, "$", nm))
      }
      n_checked <- n_checked + 1L
    }
  }
  expect_gt(n_checked, 100L)
})
