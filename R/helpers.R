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





