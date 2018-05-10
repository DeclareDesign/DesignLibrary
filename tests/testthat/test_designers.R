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
      diagnose_design(one_design,sims = 5,bootstrap = F)
    })
  
  testthat::test_that(
    desc = paste0(designer, " should return designs that have code as a character string in attributes"),
    code = {
      expect_true(class(design_attr$code) == "character")
    })

  testthat::test_that(
    desc = paste0(designer, " uses declare_design declaration somewhere"),
    code = {
      expect_true(any(grepl("declare_design|/",design_attr$code)))
    })
  
  testthat::test_that(
    desc = paste0("Code inside ",designer, " runs and creates an appropriately named design object."),
    code = {
      eval(parse(text = design_attr$code))
      expect_true(exists(x = gsub("_designer\\b","_design",designer)))
    })
  
  
  if(has_shiny){
    
    shiny_arguments <- designer_attr$shiny_arguments
    
    testthat::test_that(
      desc = paste0("Any shiny_arguments in the attributes of ",designer," should all be in the its formals."),
      code = {
        expect_true(
          all(names(shiny_arguments) %in% names(designer_args))
        ) 
      }
    )
    
    # Should create a dependency on ShinyDeclareDesign and put these tests back in:
    # testthat::test_that(
    #   desc = paste0("shiny_arguments in ",designer," evaluate properly."),
    #   code = {
    #     shiny_args <- expand_designer_shiny_args_text(designer = the_designer)
    #     lapply(shiny_args,function(x)eval(parse(text = x)))
    #   }
    # )
    # 
    # testthat::test_that(
    #   desc = paste0("get_shiny_diagnosis works with ",designer," for at least 5 sims."),
    #   code = {
    #     get_shiny_diagnosis(designer = the_designer,sims = 2)
    #   }
    # )
  }
}



