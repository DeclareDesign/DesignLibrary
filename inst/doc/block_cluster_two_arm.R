## ---- include=FALSE, eval=TRUE-------------------------------------------
options("keep.source"=TRUE,
        knitr.duplicate.label = "allow" )
library(knitr)
library(ggplot2)
library(DesignLibrary)

## ---- code = designer_default_args_text(block_cluster_two_arm_designer)----
N_blocks <- 10
N_clusters_in_block <- 2
N_i_in_cluster <- 1
sd_block <- 0.2
sd_cluster <- 0.2
sd_i <- sqrt(max(0, 1 - sd_block^2 - sd_cluster^2))
prob <- 0.5
control_mean <- 0
ate <- 0
treatment_mean <- control_mean + ate

## ---- code = block_cluster_two_arm_designer(code = TRUE)-----------------
# M: Model
pop <- declare_population(
  blocks   = add_level(
    N = N_blocks,
    u_b = rnorm(N) * sd_block),
  clusters = add_level(
    N = N_clusters_in_block,
    u_c = rnorm(N) * sd_cluster,
    cluster_size = N_i_in_cluster
  ),
  i = add_level(
    N   = N_i_in_cluster,
    u_i = rnorm(N) * sd_i,
    Z0  = control_mean + u_i + u_b + u_c,
    Z1  = treatment_mean  + u_i + u_b + u_c
  )
)

pos <- declare_potential_outcomes(Y ~ (1 - Z) * Z0 + Z * Z1)

# I: Inquiry
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

# D: Data
assignment <- declare_assignment(prob = prob,
                                 blocks = blocks,
                                 clusters = clusters)

# A: Analysis
est <- declare_estimator(
  Y ~ Z,
  estimand = estimand,
  model = difference_in_means,
  blocks = blocks,
  clusters = clusters
)

# Design
block_cluster_two_arm_design <-  pop / pos / estimand / assignment / 
  declare_reveal() / est

## ----block_cluster_two_arm_diagnosis,echo = FALSE------------------------
 diagnosis <- get_or_run_diagnosis(block_cluster_two_arm_design, sims = 1000, bootstrap = FALSE)
 knitr::kable(diagnosis$diagnosands,digits = 2)

## ----block_cluster_two_arm_shiny_diagnosis,include = FALSE---------------
 get_or_run_shiny_diagnosis(block_cluster_two_arm_designer, 1000, bootstrap = FALSE)

## ----eval = FALSE--------------------------------------------------------
#  diagnosis <- diagnose_design(block_cluster_two_arm, sims = 1000, bootstrap = FALSE)

