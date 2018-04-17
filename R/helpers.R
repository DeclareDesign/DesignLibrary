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
  args <- args[-which(names(args) == "code")]
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
    args <- args[-which(names(args) == "code")]
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
 
 #' @export
 #'
contribute_design <- function(design,title,description,wd_path = ""){
  design_name <- substitute(design)
  if(!grepl("_design",design_name)) stop("Your design must have the suffix _design.")
  if(!grepl("designs",getwd())) stop("You should contribute designs from within the DesignLibrary working directory.")
  
  roxy_header <- paste0(
    "#' ",title,"\n#' \n",
    "#' ",description,"\n#' \n",
    "#' @docType data \n",
    "#' @keywords datasets \n",
    "#' @name ", design_name, "\n",
    "#' @usage summary(",design_name,")\n",
    "#' @format A design\n\n", 
    "'",design_name,"'"
  )
  
  documentation_path <- paste0(wd_path,"R/",design_name,".R")
  cat(roxy_header,file = documentation_path)
  
  data_path <- paste0(wd_path,"data/",design_name,".RData")
  eval(parse(text = paste0("save(",design_name,",file = data_path,eval.promises = TRUE)")))
  
  message(paste0("Documentation for ",design_name," exported to ",
                 documentation_path,".\nData file for ",
                 design_name, " exported to ",data_path,"."))
}

 

 
#' Clean up DeclareDesign diagnosis object for printing 
#'
#' If diagnosands are bootstrapped, se's are put in parenthese on a second line and rounded to \code{digits}. 
#' Function uses presence of "se(" to identify bootrapped diagnoses; avoid errors by not using "se(" in naming of diagnosands. 
#'
#' @param diagnosis An object from \code{declare_design} 
#' @param digits Number of digits.
#' @param col.names Allows user to provide names of columns for output. If NULL uses names from diagnosis object, if "default" uses names of default diagnosands.
#' @param n_text_fields Number of initial text fields in diagnosis that do not have associated standard errors.  
#' @return A formatted text table with bootstrapped standard errors in parentheses.
#' @export
#'
#' @examples
#' diagnosis <- diagnose_design(simple_two_arm_designer(), sims = 3)
#' reshape_diagnosis(diagnosis)
#' reshape_diagnosis(diagnosis, col.names = 1:11)
#' reshape_diagnosis(diagnosis, col.names = "default")
#' diagnosis <- diagnose_design(simple_two_arm_designer(), sims = 3, bootstrap = 0)
#' reshape_diagnosis(diagnosis, col.names = "default")


reshape_diagnosis <- function(diagnosis, 
                              digits = 2, 
                              col.names = NULL, 
                              default.names = c("Estimator", "Coef Name", "Estimand", "Bias", "RMSE", "Coverage", "Power", "Mean(Estimate)", "sd(Estimate)", "Type S", "Mean Estimand")
                              ) { 
  
  # Housekeeping
  diagnosis     <- diagnosis[[2]]
  
  if(sum(grepl("se\\(", names(diagnosis))) == 0) { 
    out <- diagnosis
    if(is.null(col.names)) col.names <- colnames(out)
    
  } else {
    n_text_fields <- min(which(grepl("se\\(", names(diagnosis)))) - 2
    D             <- as.matrix(diagnosis[,(n_text_fields+1):ncol(diagnosis)])
    rows          <- nrow(D)
    
    cols       <- ncol(D)/2
    out.width  <- cols+n_text_fields
    
    # Reformatting
    out <- matrix(NA, 2*rows, out.width)
    out[2*(1:rows)-1, (n_text_fields + 1):ncol(out)] <- round(D[,2*(1:cols)-1], digits)
    out[2*(1:rows),   (n_text_fields + 1):ncol(out)] <- paste0("(", round(D[,2*(1:cols)], digits), ")")
    
    out[2*(1:rows)-1, 1:n_text_fields] <- as.matrix(diagnosis)[, 1:n_text_fields]
    out[2*(1:rows), 1:n_text_fields] <- " "
    if(is.null(col.names))        col.names <- colnames(diagnosis[,c(1:n_text_fields, n_text_fields+2*(1:cols)-1)])
    
    }
  
  # Column Names  
  if(col.names[1] == "default") col.names <- default.names  
  colnames(out) <- col.names
  
  return(out)
}  







 
 
