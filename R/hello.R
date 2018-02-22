#' @export
#' @importFrom utils getSrcref
template_text <- function(template) {
  txt <- as.character(getSrcref(template))

  open <- grep("[{]{3}", txt)
  close <- grep("[}]{3}", txt)

  txt <- txt[seq(open + 1, close - 1)]


  indentation <- strsplit(txt[1], "")[[1]]
  indentation <- indentation[cumprod(indentation == " ") == 1]
  indentation <- paste0("^", paste(indentation, collapse=""))


  sub(indentation, "", txt)
}

#' @export
template_default_args_text <- function(template) {
  args <- lapply(formals(template), eval)
  args <- lapply(args, head, n=1)
  mapply(paste, names(args), "<-", args, USE.NAMES = FALSE)
}

#' @export
diagnose_design <- function(...) {
  fname <- paste0(knitr::opts_current$get("label"), ".RDS")
  if(file.exists(fname)) return(readRDS(file = fname))
  diagn <- DeclareDesign::diagnose_design(...)
  saveRDS(diagn, fname)
  diagn

}
