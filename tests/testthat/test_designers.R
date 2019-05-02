
context(desc = "Testing that designers in the library work as they should")

functions <- ls("package:DesignLibrary")
designers <- functions[grepl("_designer\\b",functions)]
designers <- designers[!grepl("simple",designers)]

for(designer in designers){
  
  the_designer <- get(x = designer)
  has_shiny <- !is.null(attributes(the_designer)$shiny_arguments)
  has_definition <- !is.null(attributes(the_designer)$definitions)
  
  designer_args <- formals(the_designer)
  designer_attr <- attributes(the_designer)
  one_design <- the_designer()
  design_attr <- attributes(one_design)
  
  testthat::test_that(
    desc = paste0("designs returned by ", designer," should have a code attribute"),
    code = {
      expect_true("code" %in% names(design_attr)) 
    }
  )
  
  testthat::test_that(
    desc = paste0(designer," returns a DD-type design."),
    code = {
      expect_true("design" %in% class(one_design))
    })
  
  
  testthat::test_that(
    desc = paste0(designer,"'s default design runs."),
    code = {
      expect_is( DeclareDesign::diagnose_design(one_design, sims = 10, bootstrap_sims = FALSE)$diagnosands_df, "data.frame" )
    })
  
  testthat::test_that(
    desc = paste0(designer, " should return designs that have code as a character string in attributes"),
    code = {
      expect_true(class(design_attr$code) == "character")
    })
  
  testthat::test_that(
    desc = paste0(designer, "'s default design code runs without errors"),
    code = {
      expect_error(eval(parse(text = get_design_code(one_design))), NA)
    })
  
  testthat::test_that(
    desc = paste0("Code inside ",designer, " runs and creates an appropriately named design object."),
    code = {
      eval(parse(text = design_attr$code))
      expect_true(exists(x = gsub("_designer\\b","_design",designer)))
    })
  
  
  if(has_shiny){
    
    shiny_arguments <- designer_attr$shiny_arguments
    shiny_tips <- designer_attr$definitions$names
    
    testthat::test_that(
      desc = paste0("Any shiny_arguments in the attributes of ",designer," should all be in the its formals."),
      code = {
        expect_true(
          all(names(shiny_arguments) %in% names(designer_args))
        ) 
      }
    )
    testthat::test_that(
      desc = paste0("All shiny_arguments in the attributes of ",designer," have a tip."),
      code = {
        expect_true(all(names(shiny_arguments) %in% shiny_tips))
      }
    )
  }
  
  if(has_definition){
    test_that(
      desc = paste0("Definitions attribute of ", designer," has the same row number as its formals."),
      code = {
        definitions <- designer_attr$definitions
        expect_equal(nrow(definitions), length(designer_args)
        )
      }
    )
    
    test_that(
      desc = paste0("All 'names' in definitions attribute of ", designer," are contained in its formals."),
      code = {
        definitions <- designer_attr$definitions
        expect_true(all(definitions$names %in% names(designer_args)))
      }
    )
    test_that(
      desc = paste0("All 'class' values in  definitions attribute of ", designer," exist."),
      code = {
        classes <- designer_attr$definitions[["class"]]
        expect_true(all(classes %in% c("character", "numeric", "integer", "logical")))
      }
    )
    
    test_that(
      desc = paste0("Definitions attribute of ", designer, " contains all columns."),
      code= {
        expect_length(setdiff(names(designer_attr$definitions), c("names", "tips", "class", "vector", "min", 
                                                                  "max", "inspector_min", "inspector_step")), 0)
      }
    )
    
    test_that(
      # Testing that `diagnose_design(expand_design())` works when calling every 
      # possible design parameter by checking if estimator column names overlap with
      # designer terms
      desc = paste0("No column names in estimator data.frame conflict with parameters in ", designer),
      code = {
        params <- designer_attr$definitions$names
        estimator <- draw_estimates(the_designer())
        expect_length(intersect(params, names(estimator)), 0)
      }
    )
  }
}


# Individual tests for coverage -------------------------------------------

test_that(desc = "block_cluster_two_arm_designer errors when it should",
          code = {
            expect_error(block_cluster_two_arm_designer(N_blocks = -2))
            expect_error(block_cluster_two_arm_designer(sd_block = -1))
            expect_error(block_cluster_two_arm_designer(sd_cluster = -1))
            expect_error(block_cluster_two_arm_designer(sd_i_0 = -1))
            expect_error(block_cluster_two_arm_designer(sd_i_1 = -1))
            expect_error(block_cluster_two_arm_designer(assignment_probs = 10))
            expect_error(block_cluster_two_arm_designer(assignment_probs = 2:10 / (sum(2:10))))
            expect_error(block_cluster_two_arm_designer(rho = 10))
            expect_error(block_cluster_two_arm_designer(N = 1, N_i_in_cluster = 10))
            expect_error(block_cluster_two_arm_designer(
              N_blocks = 2,
              N_clusters_in_block = 2,
              N_i_in_cluster = c(4,6)))
            expect_error(block_cluster_two_arm_designer(
              N_blocks = 2,
              N_clusters_in_block = c(2,2),
              N_i_in_cluster = c(4,6)))
            expect_error(block_cluster_two_arm_designer(
              N_blocks = 4,
              N_clusters_in_block = c(2,2)))
          })

test_that(desc = "two_by_two_designer errors when it should",
          code = {
            expect_error(two_by_two_designer(weight_A = 10))
            expect_error(two_by_two_designer(weight_B = 10))
            expect_error(two_by_two_designer(outcome_sds = -1))
            expect_error(two_by_two_designer(prob_A = -1))
            expect_error(two_by_two_designer(prob_A = 3))
            expect_error(two_by_two_designer(prob_B = -1))
            expect_error(two_by_two_designer(prob_B = 3))
          })

test_that(desc = "two_arm_designer errors when it should",
          code = {
            expect_error(two_arm_designer(control_sd = -1))
            expect_error(two_arm_designer(assignment_prob = 10))
            expect_error(two_arm_designer(rho = 10))
            expect_warning(two_arm_designer(ate = 1, control_mean = 1, treatment_mean = 1))
          })

test_that(desc = "mediation_analysis_designer errors when it should",
          code = {
            expect_error(mediation_analysis_designer(rho = 10))
          })

test_that(desc = "spillover_designer errors when it should",
          code = {
            expect_error(spillover_designer(sd_i = -10))
            expect_error(spillover_designer(N_i_group = -10))
          })

test_that(desc = "regression_discontinuity_designer errors when it should",
          code = {
            expect_error(regression_discontinuity_designer(cutoff = -10))
            expect_error(regression_discontinuity_designer(poly_reg_order = -10))
            expect_error(regression_discontinuity_designer(poly_reg_order = -.10))
            expect_error(regression_discontinuity_designer(poly_reg_order = "hello"))
            expect_error(regression_discontinuity_designer(control_coefs = NULL))
            expect_error(regression_discontinuity_designer(treatment_coefs = NULL))
            expect_error(regression_discontinuity_designer(outcome_sd = -1))
          })

test_that(desc = "randomized_response_designer errors when it should",
          code = {
            expect_error(randomized_response_designer(prob_forced_yes = -10))
            expect_error(randomized_response_designer(prevalence_rate = -10))
            expect_error(randomized_response_designer(withholding_rate = -10))
          })

test_that(desc = "crossover_designer errors when it should",
          code = {
            expect_error(crossover_designer(rho = 10))
            expect_error(crossover_designer(N = -10))
          })

test_that(desc = "two_arm_attrition_designer errors when it should",
          code = {
            expect_error(two_arm_attrition_designer(rho = 10))
          })

test_that(desc = "pretest_posttest_designer errors when it should",
          code = {
            expect_error(pretest_posttest_designer(rho = 10))
            expect_error(pretest_posttest_designer(attrition_rate = 10))
            expect_error(pretest_posttest_designer(sd_1 = -1))
          })

test_that(desc = "cluster_sampling_designer errors when it should",
          code = {
            expect_error(cluster_sampling_designer(n_clusters_in_block = 10, 
                                                   N_clusters_in_block = 1))
            expect_error(cluster_sampling_designer(n_i_in_cluster = 30, N_i_in_cluster = 10))
            expect_error(cluster_sampling_designer(icc = 2))
          })


test_that(desc = "multi_arm_designer errors when it should",
          code = {
            expect_error(multi_arm_designer(outcome_means = rep(1,2), m_arms = 10))
            expect_error(multi_arm_designer(m_arms = .5,outcome_means = 2))
            expect_error(multi_arm_designer(outcome_sds = c(-10,-10),outcome_means = c(2,2), m_arms = 2))
            expect_error(multi_arm_designer(sd_i = -1))
          })

test_that(desc = "two_arm_covariate_designer errors when it should",
          code = {
            expect_error(two_arm_covariate_designer(sd = -1))
            expect_error(two_arm_covariate_designer(prob = 10))
            expect_error(two_arm_covariate_designer(rho_WY = 10))
            expect_error(two_arm_covariate_designer(rho_WZ = 10))
          })

test_that(desc = "factorial_designer errors when it should",
          code = {
            expect_error(factorial_designer(outcome_name = c("Y ")))
            expect_error(factorial_designer(outcome_means = 1, k = 2))
            expect_error(factorial_designer(outcome_sds = 1, k = 2))
            expect_error(factorial_designer(treatment_names = "A", k = 2))
            expect_error(factorial_designer(assignment_probs = .5, k = 2))
            expect_error(factorial_designer(assignment_probs = .5, k = 1))
            expect_error(factorial_designer(k = .5))
            expect_error(factorial_designer(outcome_sds = c(-1,-1,-1,-1), k = 2))
            expect_error(factorial_designer(assignment_probs = c(-.5,.5), k = 2))
          })

test_that(desc = "process_tracing_designer errors when it should",
          code = {
            expect_error(process_tracing_designer(N = -1))
            expect_error(process_tracing_designer(prob_X = 100))
            expect_error(process_tracing_designer(process_proportions = 1:5))
            expect_error(process_tracing_designer(process_proportions = 1:4))
            expect_error(process_tracing_designer(prior_H = 100))
            expect_error(process_tracing_designer(p_E1_H = 100))
            expect_error(process_tracing_designer(p_E1_not_H = 100))
            expect_error(process_tracing_designer(p_E2_H = 100))
            expect_error(process_tracing_designer(p_E2_not_H = 100))
            expect_error(process_tracing_designer(cor_E1E2_H = 100))
            expect_error(process_tracing_designer(cor_E1E2_not_H = 100))
            expect_error(process_tracing_designer(p_E1_not_H = .2, p_E2_not_H = .5,
                                                  cor_E1E2_not_H = 1))
            expect_error(process_tracing_designer(p_E1_H = .2, p_E2_H = .5,
                                                  cor_E1E2_H = 1))
            expect_error(process_tracing_designer(label_E1 = LETTERS[1:10]))
            expect_error(process_tracing_designer(label_E2 = LETTERS[1:10]))
          })


test_that(desc = "binary_iv_designer errors when it should",
          code = {
            expect_error(binary_iv_designer(assignment_probs = -20))
            expect_error(binary_iv_designer(assignment_probs = 20))
            expect_error(binary_iv_designer(outcome_sd = -20))
            expect_error(binary_iv_designer(a = -20))
            expect_error(binary_iv_designer(b = -20))
            expect_error(binary_iv_designer(d = -20))
          })


# Test `args_to_fix` argument works ---------------------------------------------

test_that(desc = "args_to_fix argument works",
          code = {
            expect_error(factorial_designer(args_to_fix = names(formals(factorial_designer))), NA)
            expect_error(multi_arm_designer(args_to_fix = names(formals(multi_arm_designer))), NA)
          })


test_that(desc = "block_cluster designer handles reports ICC with verbose = TRUE",
          code = {
            expect_output(d <- block_cluster_two_arm_designer(sd = 2))
            expect_silent(d <- block_cluster_two_arm_designer(sd = 2, verbose = FALSE))
            expect_output(d <- block_cluster_two_arm_designer(sd = 1, sd_block = 2, verbose = TRUE))
            expect_silent(d <- block_cluster_two_arm_designer(sd = 1, sd_block = 2, verbose = FALSE))
          })


# Aliases -----------------------------------------------------------------

test_that(desc = "aliases throw a warning",
          code = {
            expect_warning(simple_factorial_designer())
            expect_warning(simple_iv_designer())
            expect_warning(simple_spillover_designer())
            expect_warning(simple_two_arm_designer(N = 10))
          })
