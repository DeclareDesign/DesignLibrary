# `cor_E1E2_H` is consumed in the body; sweep it by calling the designer per value.
test_designer_contract("process_tracing_designer", redesign_exempt = c("cor_E1E2_H"))

test_that("process_tracing_designer refuses invalid arguments", {
  expect_error(process_tracing_designer(N = -1), "N must be a positive integer.", fixed = TRUE)
  expect_error(process_tracing_designer(prob_X = 100), "prob_X must be in [0,1].", fixed = TRUE)
  expect_error(process_tracing_designer(process_proportions = 1:5), "process_proportions must be of length 4.", fixed = TRUE)
  expect_error(process_tracing_designer(process_proportions = 1:4), "process_proportions must be a 3-simplex.", fixed = TRUE)
  expect_error(process_tracing_designer(prior_H = 100), "prior_H must be in [0,1].", fixed = TRUE)
  expect_error(process_tracing_designer(p_E1_H = 100), "p_E1_H must be in [0,1].", fixed = TRUE)
  expect_error(process_tracing_designer(p_E1_not_H = 100), "p_E1_not_H must be in [0,1].", fixed = TRUE)
  expect_error(process_tracing_designer(p_E2_H = 100), "p_E2_H must be in [0,1].", fixed = TRUE)
  expect_error(process_tracing_designer(p_E2_not_H = 100), "p_E2_not_H must be in [0,1].", fixed = TRUE)
  expect_error(process_tracing_designer(cor_E1E2_H = 100), "cor_E1E2_H must be in [-1,1].", fixed = TRUE)
  expect_error(process_tracing_designer(cor_E1E2_not_H = 100), "cor_E1E2_not_H must be in [-1,1].", fixed = TRUE)
  expect_error(process_tracing_designer(p_E1_not_H = 0.2, p_E2_not_H = 0.5, cor_E1E2_not_H = 1), "Correlation coefficient not compatible with probabilities", fixed = TRUE)
  expect_error(process_tracing_designer(p_E1_H = 0.2, p_E2_H = 0.5, cor_E1E2_H = 1), "Correlation coefficient not compatible with probabilities", fixed = TRUE)
  expect_error(process_tracing_designer(label_E1 = LETTERS[1:10]), "label_E1 must be a character of length 1.", fixed = TRUE)
  expect_error(process_tracing_designer(label_E2 = LETTERS[1:10]), "label_E2 must be a character of length 1.", fixed = TRUE)
})

test_that("process_tracing_designer reports the default diagnosands", {
  # It used to attach its own diagnosands through set_diagnosands(), which
  # replaces the default set and does not survive redesign().
  design <- process_tracing_designer()
  expect_null(attr(design, "diagnosands"))
  dx <- diagnose_design(design, sims = 3, bootstrap_sims = FALSE)$diagnosands_df
  expect_in(c("mean_estimand", "mean_estimate", "bias", "rmse"), names(dx))
})

test_that("process_tracing_designer still diagnoses after redesign", {
  # Its estimators once reported `posterior_H`, so when redesign() dropped the
  # attached diagnosands every default came back NA.
  design <- redesign(process_tracing_designer(), N = 120)
  dx <- diagnose_design(design, sims = 3, bootstrap_sims = FALSE)$diagnosands_df
  expect_false(anyNA(dx$mean_estimate))
  expect_false(anyNA(dx$bias))
})
