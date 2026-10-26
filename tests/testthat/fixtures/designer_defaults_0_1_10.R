# Designer defaults in DesignLibrary 0.1.10 that evaluate to constants,
# parsed from the CRAN source tarball. Defaults that were expressions in
# other arguments (such as `treatment_mean = control_mean + ate`) are left out.
designer_defaults_0_1_10 <- list(binary_iv_designer = list(N = 100, type_probs = c(0.333333333333333, 
0.333333333333333, 0.333333333333333, 0), assignment_probs = c(0.5, 
0.5, 0.5, 0.5), a_Y = 1, b_Y = 0, d_Y = 0, outcome_sd = 1, args_to_fix = NULL), 
    block_cluster_two_arm_designer = list(N = NULL, N_blocks = 1, 
        sd = 1, rho = 1, assignment_probs = 0.5, control_mean = 0, 
        ate = 0, verbose = TRUE, args_to_fix = NULL), cluster_sampling_designer = list(
        N_blocks = 1, N_clusters_in_block = 1000, N_i_in_cluster = 50, 
        n_clusters_in_block = 100, n_i_in_cluster = 10, icc = 0.2, 
        args_to_fix = NULL), factorial_designer = list(N = 256, 
        k = 3, sd = 1, outcome_name = "Y", treatment_names = NULL, 
        args_to_fix = NULL), mediation_analysis_designer = list(
        N = 200, a = 1, b = 0.4, c = 0, d = 0.5, rho = 0, args_to_fix = NULL), 
    multi_arm_designer = list(N = 30, m_arms = 3, sd_i = 1, args_to_fix = NULL), 
    pretest_posttest_designer = list(N = 100, ate = 0.25, sd_1 = 1, 
        sd_2 = 1, rho = 0.5, attrition_rate = 0.1, args_to_fix = NULL), 
    process_tracing_designer = list(N = 100, prob_X = 0.5, process_proportions = c(0.25, 
    0.25, 0.25, 0.25), prior_H = 0.5, p_E1_H = 0.8, p_E1_not_H = 0.2, 
        p_E2_H = 0.3, p_E2_not_H = 0, cor_E1E2_H = 0, cor_E1E2_not_H = 0, 
        label_E1 = "Straw in the Wind", label_E2 = "Smoking Gun", 
        args_to_fix = NULL), randomized_response_designer = list(
        N = 1000, prob_forced_yes = 0.6, prevalence_rate = 0.1, 
        withholding_rate = 0.5, args_to_fix = NULL), regression_discontinuity_designer = list(
        N = 1000, tau = 0.15, outcome_sd = 0.1, cutoff = 0.5, 
        bandwidth = 0.5, control_coefs = c(0.5, 0.5), treatment_coefs = c(-5, 
        1), poly_reg_order = 4, args_to_fix = NULL), spillover_designer = list(
        N_groups = 80, N_i_group = 3, sd_i = 0.2, gamma = 2, 
        args_to_fix = NULL), two_arm_attrition_designer = list(
        N = 100, a_R = 0, b_R = 1, a_Y = 0, b_Y = 1, rho = 0, 
        args_to_fix = NULL), two_arm_covariate_designer = list(
        N = 100, prob = 0.5, control_mean = 0, sd = 1, ate = 1, 
        h = 0, rho_WY = 0, rho_WZ = 0, args_to_fix = NULL), two_arm_designer = list(
        N = 100, assignment_prob = 0.5, control_mean = 0, control_sd = 1, 
        ate = 1, rho = 1, args_to_fix = NULL), two_by_two_designer = list(
        N = 100, prob_A = 0.5, prob_B = 0.5, weight_A = 0.5, 
        weight_B = 0.5, outcome_means = c(0, 0, 0, 0), sd_i = 1, 
        outcome_sds = c(0, 0, 0, 0), args_to_fix = NULL))
