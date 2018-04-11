

get_design_code <- function(template) {
  txt <- as.character(getSrcref(template))
  
  open <- grep("[{]{3}", txt)
  close <- grep("[}]{3}", txt)
  
  if(length(open) != 1) stop("could not find opening tag in ", substitute(template))
  if(length(close) != 1) stop("could not find opening tag in ", substitute(template))
  
  txt <- txt[seq(open + 1, close - 1)]
  
  
  indentation <- strsplit(txt[1], "")[[1]]
  indentation <- indentation[cumprod(indentation == " ") == 1]
  indentation <- paste0("^", paste(indentation, collapse=""))
  
  
  sub(indentation, "", txt)
}
