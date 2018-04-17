## ---- include=FALSE, eval=TRUE-------------------------------------------
options("keep.source"=TRUE,
        knitr.duplicate.label = "allow" )
library(knitr)
library(ggplot2)
library(DesignLibrary)

## ---- code = designer_default_args_text(pretest_posttest_designer)-------
N <- 100
ate <- 0.25
rho <- 0.25
attrition_rate <- 0

## ---- code = pretest_posttest_designer(code = TRUE)----------------------
# M: Model
pop <- declare_population(
  N = N,
  u_t1 = rnorm(N, 0, sqrt(1 - rho)),
  u_t2 = rnorm(N, 0, sqrt(1 - rho)),
  u_i = rnorm(N, 0, sqrt(rho))
)
pos_t1 <- declare_potential_outcomes(Y_t1 ~ u_i + u_t1)
pos_t2 <- declare_potential_outcomes(Y_t2 ~ ate * Z + u_i + u_t2)
report <- declare_assignment(m = round(N * (1 - attrition_rate)),
                             assignment_variable = R)
# I: Inquiry
estimand <- declare_estimand(ATE = mean(Y_t2_Z_1 - Y_t2_Z_0))
# D: Data Strategy
assignment <-
  declare_assignment(m = round(N / 2), assignment_variable = Z)
# A: Answer Strategy
pretest_lhs <- declare_estimator((Y_t2 - Y_t1) ~ Z,
                                 model = lm_robust,
                                 estimand = estimand,
                                 subset = R == 1,
                                 label = "Change score"
)
pretest_rhs <- declare_estimator(
  Y_t2 ~ Z + Y_t1,
  model = lm_robust,
  estimand = estimand,
  subset = R == 1,
  label = "Condition on pretest"
)
posttest_only <- declare_estimator(
  Y_t2 ~ Z,
  model = lm_robust,
  estimand = estimand,
  label = "Posttest only"
)
# Design
pretest_posttest_design <- declare_design(
  pop,
  pos_t1,
  pos_t2,
  estimand,
  assignment,
  report,
  declare_reveal(Y_t1),
  declare_reveal(Y_t2),
  pretest_lhs,
  pretest_rhs,
  posttest_only
)

## ----pretest_posttest_diagnosis,echo = FALSE-----------------------------
 diagnosis <- get_or_run_diagnosis(pretest_posttest_design, sims = 1000, bootstrap = FALSE)
 knitr::kable(reshape_diagnosis(diagnosis, digits = 2))

## ----pretest_posttest_shiny_diagnosis,include = FALSE--------------------
 get_or_run_shiny_diagnosis(pretest_posttest_designer, 1000, bootstrap = FALSE)

## ----eval = FALSE--------------------------------------------------------
#  diagnosis <- diagnose_design(pretest_posttest_design, sims = 1000, bootstrap = FALSE)

