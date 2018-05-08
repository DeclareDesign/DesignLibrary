#' Get code from a designer
#' @param designer A function that builds designs, and contains the code to be grabbed inside triple braces
#' @return Verbatim code 
#' @export
#'
get_design_code <- function(design){
  cat(attr(design, "code"), sep = "\n")
}

#' @export
construct_design_code <- function(designer, args){
  # get the code for the design 
  txt <- as.character(getSrcref(designer))
  
  open <- grep("[{]{3}", txt)
  close <- grep("[}]{3}", txt)
  
  if(length(open) != 1) stop("could not find opening tag in ", substitute(designer))
  if(length(close) != 1) stop("could not find opening tag in ", substitute(designer))
  
  txt <- txt[seq(open + 1, close - 1)]
  
  indentation <- strsplit(txt[1], "")[[1]]
  indentation <- indentation[cumprod(indentation == " ") == 1]
  indentation <- paste0("^", paste(indentation, collapse=""))
  
  code <- sub(indentation, "", txt)
  
  # convert args to text
  args_text <- as.character(sapply(names(args[2:length(args)]), function(x) paste0(x, " <- ", deparse(args[[x]]))))
  
  # add arguments and code
  code <- c(args_text, "", code)
  
  code
}

#' @export
match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))
  
  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )
  
  
  match.call(sys.function(sys.parent()), call)
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
                              default.names = c("Estimator", "Coef Name", "Estimand", "Bias", "RMSE",  "Power", "Coverage", "Mean(Estimate)", "sd(Estimate)", "Mean(se)", "Type S", "Mean(Estimand)")
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







#' Function Based on getDocData from package cardoonTools
#' @param create_html logical. Create html 
#' @return create_overview does not return an object.
#' @export
#'
#'
create_overview <- function(create_html = FALSE){
  our_package = "DesignLibrary"
  rdb_path <- file.path(system.file("help", package= our_package),our_package)
  designer_list <-  ls(paste0("package:",our_package)) 
  is_designer <- endsWith(designer_list, "_designer")
  designer_list <- designer_list[is_designer]
  
  overview <- sapply(designer_list, function(designer){
    help_text <- tools:::fetchRdDB(rdb_path, designer)
    classes <- sapply( help_text, function(x) attr(x, "Rd_tag"))
    title <- help_text[[which(grepl("\\\\title", classes))]]
    desc <- help_text[[which(grepl("\\\\description", classes))]]
    desc  <- do.call(paste, desc)
    desc  <- gsub("\n", "", desc)
    c("`",designer,"` \n \n", title,".", desc ,"\n \n")
  })
  overview <- do.call(paste, overview)
  
  if(create_html){
    yaml <- paste0('---
                   output: rmarkdown::html_vignette
                   vignette: >
                   %\\VignetteIndexEntry{Design Index}
                   %\\VignetteEngine{knitr::rmarkdown}
                   %\\VignetteEncoding{UTF-8}
                   ---
                   ')
    file.create("Design_and_Designer_Index.Rmd")
    cat(yaml,
        "## Designers",
        overview,
        sep = "\n",
        file = "Design_and_Designer_Index.Rmd")
  }
  knitr::knit("Design_and_Designer_Index.Rmd")
  
  cat(overview,  sep = "\n")
}


 
