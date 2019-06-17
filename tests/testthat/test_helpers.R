context(desc = "Testing that helpers in the library work as they should")

testthat::test_that(
  desc = "functions can be passed to designer and returned by construct_design_code",
  code = {
    
    test_designer <- function(summary_function,N){
      {{{
        summary_function(rnorm(N))
      }}}
      construct_design_code(designer = test_designer,args = match.call.defaults())
    }
    
    args <- test_designer(summary_function = mean,N = 100)
    
    expect_true(any(grepl("mean", args))) 
  }
)

test_that(
  desc = "internal helper `find_triple_bracket` for when source code is missing work",
  code = {
    test_function <- function(){
      {{{
        mean(rnorm(100))
      }}}
    }
    expect_is(DesignLibrary:::find_triple_bracket(f = test_function),"character")
    expect_equal(DesignLibrary:::find_triple_bracket(f = mean),"")
  })

test_that(
  desc = "internal helper `pred` for when source code is missing work",
  code = {
    expect_false(DesignLibrary:::pred(expr = mean(1:2), depth = 1))
    expect_true(DesignLibrary:::pred(expr = rlang::parse_expr("{{{{mean(1)}}}}"), depth = 1))
  })

test_that(desc = "construct_design_code works as it should when source is missing",
          code = {
            expect_equal(DesignLibrary:::construct_design_code(designer = mean, args = c("x"), arguments_as_values = F,exclude_args = NULL),c("",""))
            expect_is(DesignLibrary:::construct_design_code(designer = function(x = 1) {eval(rlang::parse_expr("{{{x}}}"))}, args = c(x = 1), arguments_as_values = TRUE, exclude_args = NULL),"character")
            test_designer <- function() "{{{"
            expect_error(DesignLibrary:::construct_design_code(designer = test_designer,args = "x"))
            test_designer <- function() "}}}"
            expect_error(DesignLibrary:::construct_design_code(designer = test_designer,args = "x"))
          }) 


test_that(desc = "construct_design_code works as it should when using rlang functions and source is missing",
          code = {
            test_function <- function(){
              {{{
                t1 <- rlang::quo(paste0(!!letters[1]))
              }}}
              
              DesignLibrary:::construct_design_code(designer = test_function, 
                                                    args = DesignLibrary:::match.call.defaults(),
                                                    arguments_as_values = FALSE,
                                                    exclude_args = NULL,
                                                    args_to_fix = NULL)
            }
            expect_equal(test_function(), c("", "t1 <- rlang::quo(paste0(!!letters[1]))"))
          }) 

# test_that(desc = "construct_design_code works when args_to_fix is not NULL and exclude_args is not NULL",
# code = expect_is(get_design_code(binary_iv_designer(args_to_fix = "N")), "character")
# )
# 
# test_that(desc = "construct_design_code works when args_to_fix is not NULL and exclude_args is NULL",
# code = expect_is(get_design_code(two_arm_attrition_designer(args_to_fix = "N")), "character")
# )


test_that(desc = "match.call.defaults has all cases tested",
          code = {
            test_fun <- function(...){...}
            expect_error(match.call.defaults(test_fun,expand.dots = T),NA)
          }) 


test_that(desc = "return_args works fine",
          code = {
            expect_error(DesignLibrary:::return_args(c(A = 1, B = 2, C = 3, D = 4, E = 5),fixes = NULL),NA)
            expect_error(DesignLibrary:::return_args(c(A = 1, B = 2, C = 3, D = 4, E = 5),fixes = LETTERS[1:2]),NA)
          })

test_that(desc = "str_within() works fine",
          code = {
            expect_identical(DesignLibrary:::str_within("structure(list(a = 1), .Names = \"N\")"), "list(a = 1)")
            expect_identical(DesignLibrary:::str_within("list(a = 1)"), "list(a = 1)")
            expect_identical(DesignLibrary:::str_within(c("structure(list(a = 1,", "b = 1), .Names = c(\"a\", \"b\"))")), "list(a = 1, b = 1)")
          })
