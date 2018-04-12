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
  args <- args[-which(names(args) == "code")]
  args <- lapply(args,deparse)
  mapply(paste, names(args), "<-", args, USE.NAMES = FALSE)
}

#' @export
#'
 get_shiny_diagnosis <- function(designer,sims) {
   shiny_args <- get_shiny_arguments(designer)
   all_designs <- fill_out(template = designer,expand = TRUE,shiny_args)
   diagnose_design(all_designs,sims = sims,bootstrap = FALSE)
}

 #' @export
 #'
 get_or_run_shiny_diagnosis <- function(designer,sims,bootstrap) {
   designer_name <- substitute(designer)
   design_name <- gsub(pattern = "_designer",replacement = "",x = designer_name)
   file_name <- paste0(design_name,"_shiny_diagnosis.RDS")
   parameters <- expand.grid(get_shiny_arguments(designer), stringsAsFactors = FALSE)
   if(file.exists(file_name)){ 
     diagnosis <- readRDS(file = file_name)
   } else {
     diagnosis <- DesignLibrary::get_shiny_diagnosis(designer,sims = sims)
     diagnosis$diagnosands <- cbind(diagnosis$diagnosands,parameters)
     saveRDS(diagnosis, file_name)
   }
   diagnosis
 }
 
 #' @export
 #'
contribute_design <- function(design,title,description){
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
  
  documentation_path <- paste0("R/",design_name,".R")
  cat(roxy_header,file = documentation_path)
  
  data_path <- paste0("data/",design_name,".rda")
  eval(parse(text = paste0("save(",design_name,",file = data_path)")))
  
  message(paste0("Documentation for ",design_name," exported to ",
                 documentation_path,".\n Data file for ",
                 design_name, " exported to ",data_path,"."))
}



  


 
 
 
 
 
 
 
 
 
 
 
 
 
 
