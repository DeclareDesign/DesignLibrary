

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

#' @export
#'
get_shiny_arguments <- function(designer){
  attributes(designer)$shiny_arguments
}

#' @export
#'
get_constants <- function(designer){
  shiny_arguments <- get_shiny_arguments(designer)
  formals(designer)[!names(formals(designer)) %in% names(shiny_arguments)]
}

#' @export
#'
designer_default_args_text <- function(designer) {
  shinys <- get_shiny_arguments(designer)
  shinys <- lapply(shinys,function(x)x[1])
  args <- c(shinys,get_constants(designer))
  args <- args[names(formals(designer))]
  args <- lapply(args,deparse)
  mapply(paste, names(args), "<-", args, USE.NAMES = FALSE)
}

#' @export
#'
expand_designer_shiny_args_text <- function(designer) {
  shinys <- get_shiny_arguments(designer)
  all_shinys <- expand.grid(shinys)
  lapply(1:nrow(all_shinys),function(i){
    shiny_args <- (all_shinys[i,])
    names(shiny_args) <- names(all_shinys)
    args <- c(shiny_args,get_constants(designer))
    args <- args[names(formals(designer))]
    args <- lapply(args,deparse)
    mapply(paste, names(args), "<-", args, USE.NAMES = FALSE)
  })
}

#' @export
#'
get_shiny_diagnosis <- function(designer,sims) {
  shiny_args <- get_shiny_arguments(designer)
  all_designs <- expand_design(template = designer,expand = TRUE,shiny_args)
  diagnosis <- diagnose_design(all_designs,sims = sims,bootstrap = FALSE)
  argument_list <- expand_designer_shiny_args_text(designer = designer)
  return(list(diagnosis = diagnosis, argument_list = argument_list))
}

#' @export
#'
get_or_run_shiny_diagnosis <- function(designer,sims,bootstrap) {
  designer_name <- substitute(designer)
  design_name <- gsub(pattern = "_designer",replacement = "",x = designer_name)
  file_name <- paste0(design_name,"_shiny_diagnosis.RDS")
  parameters <- expand.grid(get_shiny_arguments(designer), stringsAsFactors = FALSE)
  if(file.exists(file_name)){ 
    diagnosis_list <- readRDS(file = file_name)
    diagnosis <- diagnosis_list$diagnosis
  } else {
    diagnosis_list <- DesignLibrary::get_shiny_diagnosis(designer,sims = sims)
    diagnosis <- diagnosis_list$diagnosis
    diagnosis$diagnosands <- cbind(diagnosis$diagnosands,parameters)
    diagnosis_list$diagnosis <- diagnosis
    saveRDS(diagnosis_list, file_name)
  }
  diagnosis
}


