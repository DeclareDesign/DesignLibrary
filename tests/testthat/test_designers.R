
context(desc = "Testing that designers in the library work as they should")


functions <- ls("package:DesignLibrary")
designers <- functions[grepl("_designer\\b",functions)]

for(designer in designers){
  
  the_designer <- get(x = designer)
  has_shiny <- !is.null(attributes(the_designer)$shiny_arguments)
  
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
    shiny_tips <- designer_attr$tips
    
    testthat::test_that(
      desc = paste0("Any shiny_arguments in the attributes of ",designer," should all be in the its formals."),
      code = {
        expect_true(
          all(names(shiny_arguments) %in% names(designer_args))
        ) 
      }
    )
    testthat::test_that(
      desc = paste0("Any shiny_arguments in the attributes of ",designer," have associated tips."),
      code = {
        expect_length(setdiff(names(shiny_arguments),names(shiny_tips)),0)
        expect_length(setdiff(names(shiny_tips),names(shiny_arguments)),0)
      }
    )
  }
}


# Individual tests for coverage -------------------------------------------

test_that(desc = "block_cluster_two_arm_designer errors when it should",
          code = {
            expect_error(block_cluster_two_arm_designer(sd_block = -1))
            expect_error(block_cluster_two_arm_designer(sd_cluster = -1))
            expect_error(block_cluster_two_arm_designer(sd_i_0 = -1))
            expect_error(block_cluster_two_arm_designer(sd_i_1 = -1))
            expect_error(block_cluster_two_arm_designer(prob = 10))
                               })

test_that(desc = "simple_factorial_designer errors when it should",
          code = {
            expect_error(simple_factorial_designer(w_A = 10))
            expect_error(simple_factorial_designer(w_B = 10))
            expect_error(simple_factorial_designer(outcome_sds = -1))
            expect_error(simple_factorial_designer(prob_A = -1))
            expect_error(simple_factorial_designer(prob_A = 3))
            expect_error(simple_factorial_designer(prob_B = -1))
            expect_error(simple_factorial_designer(prob_B = 3))
                               })

test_that(desc = "simple_two_arm_designer errors when it should",
          code = {
            expect_error(simple_two_arm_designer(control_sd = -1))
            expect_error(simple_two_arm_designer(prob = 10))
            expect_error(simple_two_arm_designer(rho = 10))
                               })

test_that(desc = "mediation_analysis_designer errors when it should",
          code = {
            expect_error(mediation_analysis_designer(rho = 10))
          })

test_that(desc = "simple_spillover_designer errors when it should",
          code = {
            expect_error(simple_spillover_designer(sd = -10))
            expect_error(simple_spillover_designer(N_i_group = -10))
          })

test_that(desc = "regression_discontinuity_designer errors when it should",
          code = {
            expect_error(regression_discontinuity_designer(cutoff = -10))
            expect_error(regression_discontinuity_designer(poly_order = -10))
          })

test_that(desc = "randomized_response_designer errors when it should",
          code = {
            expect_error(randomized_response_designer(prob_forced_yes = -10))
            expect_error(randomized_response_designer(prevalence_rate = -10))
            expect_error(randomized_response_designer(withholding_rate = -10))
          })

test_that(desc = "block_cluster_two_arm_designer errors when it should",
          code = {
            expect_error(block_cluster_two_arm_designer(rho = 10))
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
          })

test_that(desc = "cluster_sampling_designer errors when it should",
          code = {
            expect_error(cluster_sampling_designer(n_clusters = 10,N_clusters = 1))
            expect_error(cluster_sampling_designer(n_subjects_per_cluster = 30,N_subjects_per_cluster = 10))
          })


test_that(desc = "multi_arm_designer errors when it should",
          code = {
            expect_error(multi_arm_designer(means = rep(1,2),m_arms = 10))
            expect_error(multi_arm_designer(m_arms = .5,means = 2))
            expect_error(multi_arm_designer(sds = c(-10,-10),means = c(2,2), m_arms = 2))
          })





