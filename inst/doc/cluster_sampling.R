## ---- include=FALSE, eval=TRUE-------------------------------------------
options("keep.source"=TRUE,
        knitr.duplicate.label = "allow" )
library(knitr)
library(ggplot2)
library(DesignLibrary)

## ---- code = designer_default_args_text(cluster_sampling_designer)-------
n_clusters <- 100
n_subjects_per_cluster <- 10
icc <- 0.2
N_clusters <- 1000
N_subjects_per_cluster <- 50

## ---- code = cluster_sampling_designer(code = TRUE)----------------------
# M: Model
population <-
  declare_population(
    cluster = add_level(N = N_clusters),
    subject = add_level(N = N_subjects_per_cluster,
                        latent_ideology = draw_normal_icc(mean = 0, N = N, clusters = cluster, ICC = icc),
                        Y = draw_ordered(x = latent_ideology, breaks = qnorm(seq(0, 1, length.out = 8)))
    )
  )
fixed_pop <- population()

# I: Inquiry
estimand <- declare_estimand(mean(Y), label = "Ybar")

# D: Data Strategy
stage_1_sampling <- declare_sampling(clusters = cluster, n = n_clusters)
stage_2_sampling <- declare_sampling(strata = cluster, n = n_subjects_per_cluster)

# A: Answer Strategy
no_clustering <- declare_estimator(Y ~ 1,
                                   model = lm_robust,
                                   estimand = estimand, 
                                   label = "Standard Errors Not Clustered")
clustered_ses <- declare_estimator(Y ~ 1,
                                   model = lm_robust,
                                   clusters = cluster,
                                   estimand = estimand,
                                   label = "Clustered Standard Errors")


# Design
cluster_sampling_design <- declare_design(fixed_pop, estimand, stage_1_sampling, stage_2_sampling, no_clustering, clustered_ses)

## ----cluster_sampling_diagnosis,echo = FALSE-----------------------------
 diagnosis <- get_or_run_diagnosis(cluster_sampling_design, sims = 1000, bootstrap = FALSE)
 knitr::kable(reshape_diagnosis(diagnosis, digits = 2))

## ----cluster_sampling_shiny_diagnosis,include = FALSE--------------------
 get_or_run_shiny_diagnosis(cluster_sampling_designer, 1000, bootstrap = FALSE)

## ----eval = FALSE--------------------------------------------------------
#  diagnosis <- diagnose_design(cluster_sampling_design, sims = 1000, bootstrap = FALSE)

