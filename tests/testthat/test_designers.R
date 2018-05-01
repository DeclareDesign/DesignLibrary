context(desc = "Testing that designers in the library work as they should")

functions <- ls("package:DesignLibrary")
designers <- functions[grepl("_designer\\b",functions)]


for(designer in designers){
  
  the_designer <- get(x = designer)
  has_shiny <- !is.null(attributes(the_designer)$shiny_arguments)
  designer_args <- formals(the_designer)
  
  testthat::test_that(
    desc = paste0(designer," should have a logical code argument"),
    code = {
      expect_true("code" %in% names(designer_args)) 
    }
  )
  
  testthat::test_that(
    desc = paste0(designer," returns a DD-type design."),
    code = {
      expect_true("design" %in% class(the_designer()))
    })
  
  testthat::test_that(
    desc = paste0(designer,"'s default design runs."),
    code = {
      test_design <- the_designer()
      diagnose_design(test_design,sims = 5,bootstrap = F)
    })
  
  testthat::test_that(
    desc = paste0(designer, " should return code as a character string when code = TRUE"),
    code = {
      expect_true(class(the_designer(code = TRUE)) == "character")
    })

  testthat::test_that(
    desc = paste0(designer, " uses declare_design declaration somewhere"),
    code = {
      expect_true(any(grepl("declare_design|/",the_designer(code = TRUE))))
    })
  
  testthat::test_that(
    desc = paste0("Code inside ",designer, " runs and creates an appropriately named design object."),
    code = {
      eval(parse(text = designer_default_args_text(the_designer)))
      eval(parse(text = the_designer(code = TRUE)))
      expect_true(exists(x = gsub("_designer\\b","_design",designer)))
    })
  
  testthat::test_that(
    desc = paste0(designer," is able to make vignettes."),
    code = {
      make_vignette(design_or_designer = the_designer,title = "Test Vignette",overwrite = TRUE,front_text = "Blah",end_text = "Blah")
    })
  
  # testthat::test_that(
  #   desc = paste0("contribute_design works with ",designer,"."),
  #   code = {
  #     contribute_design(the_designer,designer,description = "test design")
  #     file.remove("R/the_designer.R")
  #     file.remove("data/the_designer.Rdata")
  #   }
  # )
  
  if(has_shiny){
    
    shiny_arguments <- attributes(the_designer)$shiny_arguments
    
    testthat::test_that(
      desc = paste0("Any shiny_arguments in the attributes of ",designer," should all be in the its formals."),
      code = {
        expect_true(
          all(names(shiny_arguments) %in% names(designer_args))
        ) 
      }
    )
    
    
    testthat::test_that(
      desc = paste0("shiny_arguments in ",designer," evaluate properly."),
      code = {
        shiny_args <- expand_designer_shiny_args_text(designer = the_designer)
        lapply(shiny_args,function(x)eval(parse(text = x)))
      }
    )
    
    testthat::test_that(
      desc = paste0("get_shiny_diagnosis works with ",designer," for at least 5 sims."),
      code = {
        get_shiny_diagnosis(designer = the_designer,sims = 5)
      }
    )
  }
}



