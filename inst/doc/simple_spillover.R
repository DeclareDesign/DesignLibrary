## ---- include=FALSE, eval=TRUE-------------------------------------------
options("keep.source"=TRUE,
        knitr.duplicate.label = "allow" )
library(knitr)
library(ggplot2)
library(DesignLibrary)

## ---- code = designer_default_args_text(simple_spillover_designer)-------
n_groups <- 40
group_size <- 3
sd <- 1

## ---- code = simple_spillover_designer(code = TRUE)----------------------

      # Model ----------------------------------------------------------------------
      pop <- declare_population(N = n_groups*group_size, 
                                G = rep(1:n_groups, each = group_size))
      dgp <- function(i, Z, G) (sum(Z[G == G[i]])>0) + rnorm(1)*sd
      
      # Inquiry --------------------------------------------------------------------
      estimand <- declare_estimand(ATE = mean(
        sapply(1:length(G), function(i) {
          Z0 = rep(0, length(G))
          Z1 = (1:length(G))==i
          dgp(i,Z1,G) - dgp(i, Z0, G)})
      ))
      # Data
      assignt <- declare_assignment()
      
      reveal <- declare_reveal(handler=fabricate,
                               Y = sapply(1:N, function(i) dgp(i, Z, G)))
      
      # Answer Strategy -------------------------------------------------------------
      estimator <- declare_estimator(Y ~ Z, estimand = estimand, model = lm_robust)
      
      # Design ----------------------------------------------------------------------
      simple_spillover_design <- pop / assignt / reveal/ estimand / estimator
      

## ----simple_spillover_diagnosis,echo = FALSE-----------------------------
 diagnosis <- get_or_run_diagnosis(simple_spillover_design, sims = 1000, bootstrap = FALSE)
 knitr::kable(reshape_diagnosis(diagnosis, digits = 2))

## ----eval = FALSE--------------------------------------------------------
#  diagnosis <- diagnose_design(simple_spillover_design, sims = 1000, bootstrap = FALSE)

