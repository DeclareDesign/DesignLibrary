---
output: rmarkdown::html_vignette
vignette: >
 %\VignetteIndexEntry{Design Index}
 %\VignetteEngine{knitr::rmarkdown}
 %\VignetteEncoding{UTF-8}
---

## Designers
` audit_experiment_designer ` 
 
 Create an audit experiment design .  Description here 
 
 ` block_cluster_two_arm_designer ` 
 
 Create a two arm design with blocks and clusters .  This designer builds a design with blocks and clusters. Normal shocks can be specified at the  individual, cluster, and block levels. If individual level shocks are not specified and cluster and block  level variances sum to less than 1, then individual level shocks are set such that total variance in outcomes equals 1.  Treatment effects can be specified either by providing  control_mean  and  treatment_mean  or by specifying an  ate . 
 
 ` cluster_sampling_designer ` 
 
 Create a design for cluster random sampling .  Description here 
 
 ` crossover_designer ` 
 
 Create a crossover design .  Description here 
 
 ` mediation_analysis_designer ` 
 
 Create a design for mediation analysis .  Description here 
 
 ` pretest_posttest_designer ` 
 
 Create a pretest-posttest design .  Description here 
 
 ` regression_discontinuity_designer ` 
 
 Create a regression discontinuity design .  Description: here. 
 
 ` simple_spillover_designer ` 
 
 Create a simlpe design with spillovers .  This designer builds a design with  n_groups  groups each containing  group_size  individuals.  An individual is affected if anyone in her group is treated. The default estimand is the average difference  across subjects between no one treated and only that subject treated. 
 
 ` simple_two_arm_designer ` 
 
 Create a simple two arm design .  Create a simple two arm design 
 

