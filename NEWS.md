# DesignLibrary 0.1.1

* Added designer for process-tracing designs (`process_tracing_designer()`)
* Added designer for simple instrumental variables designs (`simple_iv_designer()`)
* Added new arguments to `regression_discontinuity_designer()` to allow for setting variance and shape of potential outcomes functions
* Added blocking to `cluster_sampling_designer()`
* Added more flexible ways to specify `N` in `block_cluster_two_arm_designer()`
* Cleaned up various inconsistencies in documentation
* Cleaned up dependencies in DESCRIPTION
* Cleaner method for handling global variables in DesignLibrary.R
* Used `importFrom` to fix notes and errors in `check_results_DesignLibrary.html`

# DesignLibrary 0.1.0

* First CRAN version
