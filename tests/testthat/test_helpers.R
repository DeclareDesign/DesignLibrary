context(desc = "Testing that helpers in the library work as they should")



testthat::test_that(
  desc = paste0("functions can be passed to designer and returned by construct_design_code"),
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



