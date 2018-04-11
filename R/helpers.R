#' Get code from a designer
#' @param designer A function that builds designs, and contains the code to be grabbed inside triple braces
#' @return Verbatim code 
#' @export
#'
get_design_code <- function(designer) {
  txt <- as.character(getSrcref(designer))
  
  open <- grep("[{]{3}", txt)
  close <- grep("[}]{3}", txt)
  
  if(length(open) != 1) stop("could not find opening tag in ", substitute(designer))
  if(length(close) != 1) stop("could not find opening tag in ", substitute(designer))
  
  txt <- txt[seq(open + 1, close - 1)]
  
  indentation <- strsplit(txt[1], "")[[1]]
  indentation <- indentation[cumprod(indentation == " ") == 1]
  indentation <- paste0("^", paste(indentation, collapse=""))
  
  sub(indentation, "", txt)
}

#' Run diagnosis and store RDS
#' @param ... Design, number of simulations and bootstraps for DeclareDesign
#' @return Diagnosis
#' @export
#'
get_or_run_diagnosis <- function(design,sims,bootstrap) {
  design_name <- substitute(design)
  file_name <- paste0(design_name,"_diagnosis.RDS")
  if(file.exists(file_name)){ 
    diagnosis <- readRDS(file = file_name)
  } else {
    diagnosis <- DeclareDesign::diagnose_design(design,sims = sims,bootstrap = bootstrap)
    saveRDS(diagnosis, file_name)
  }
  diagnosis
}




