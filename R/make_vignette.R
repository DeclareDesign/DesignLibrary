make_header <- function(title = "", shiny, file_name = "" ){
  x <- paste0('---
title: " ', title, ' " 
output: rmarkdown::html_vignette
bibliography: bib.bib
vignette: >
 %\\VignetteIndexEntry{',title,'}
 %\\VignetteEngine{knitr::rmarkdown}
 %\\VignetteEncoding{UTF-8}
---
              
```{r, include=FALSE, eval=TRUE}
options("keep.source"=TRUE,
        knitr.duplicate.label = "allow" )
library(knitr)
library(ggplot2)
library(DesignLibrary)
```
              
<div class="btn-group">
  <a class="btn btn-primary" href="http://declaredesign.org/articles/',file_name,'.R">
     <i class="fa fa-code" title = "Download code for design" fa-2x></i> Download code
  </a>
  <a class="btn btn-primary" href="http://declaredesign.org/articles/',file_name,'_designer.RDS">
     <i class="fa fa-clipboard" title = "Download template for design" fa-2x></i> Download template
  </a>', shiny,' 
</div>
            
  
')}

make_design_chunks <- function(file_name, sims, bootstrap) {
  paste0(' 
              
```{r, ', file_name, '_diagnosis}
diagnosis <- get_or_run_diagnosis(', file_name, '_design, sims = ', sims, ', bootstrap = ', bootstrap, ')
```
         
')
}

make_designer_chunks <- function(file_name, sims, bootstrap, has_shiny) {
  chunk2 <- ""
  chunk1 <- paste0('
```{r, code = designer_default_args_text(', file_name, '_designer)}
```

  
```{r, code = ',file_name,'_designer(code = TRUE)}
```
  

```{r ',file_name,'_diagnosis,echo = FALSE}
 diagnosis <- get_or_run_diagnosis(',file_name,'_design, sims = ',sims,', bootstrap = ',bootstrap,')
 knitr::kable(reshape_diagnosis(diagnosis, digits = 2))
```
  
  
')
  ## has_shiny
  if(has_shiny){
   chunk2 <- paste0('
```{r ',file_name,'_shiny_diagnosis,include = FALSE}
 get_or_run_shiny_diagnosis(',file_name,'_designer, ',sims,', bootstrap = ',bootstrap,')
```
                     
')  
  }
  
  chunk3 <- paste0('
```{r,eval = FALSE}
diagnosis <- diagnose_design(',file_name,'_design, sims = ',sims,', bootstrap = ',bootstrap,')
```
')
  
  paste0(chunk1, chunk2, chunk3)
}

make_text <- function(text, text_path){
  
  if(!is.null(text_path)){ 
    rmarkdown::render(text_path, quiet = TRUE, output_file = "DD_temp_rmd.Rmd")
    file.remove("DD_temp_rmd.Rmd")
    p <- readLines(text_path)
    text <- paste0(p[grep("```", p)[1]:length(p)], sep = "\n")
  }
  text
}

#' Create a simple two arm design
#' @param design_or_designer A design or a designer
#' @param vignette_title A character string 
#' @param front_text A character string 
#' @param end_text A character string 
#' @param front_text_path Path to .Rmd; If different from NULL, it overwrites front_text
#' @param end_text_path   Path to .Rmd; If different from NULL, it overwrites end_text
#' @param sims number of simulations for DeclareDesign
#' @param bootstraps  number of bootstraps for DeclareDesign 
#' @return This function creates a .Rmd 
#' @export 
#'

make_vignette <- function(design_or_designer = NULL, title = NULL, front_text = "", end_text = "", front_text_path = NULL, end_text_path = NULL , sims = 1000, bootstrap = FALSE){
  options(error = NULL)
  object_name <- deparse(substitute(design_or_designer))
  shiny = ""
  class_a <- class(design_or_designer)
  has_shiny <- FALSE
  
  if(any(class_a  == "design") ) {
  if(!endsWith(object_name, "_design"))
    stop("Design's name must end with suffix '_design'")
    make_chunks <- make_design_chunks
  }
  else if(is.function(design_or_designer)) 
  {
    class_a <- class(design_or_designer()) 
    if(!any(class_a  == "design" ))
      stop("Argument 'design_or_designer' must either be an object of class 'design' or a designer that returns an object of class 'design'")
    else if(!endsWith(object_name, "_designer")) 
      stop("Designer's name must end with suffix '_designer'")
    else 
    {
      has_shiny <- !is.null(get_shiny_arguments(design_or_designer))
      object_name <- sub("_designer", replacement = "", object_name) 
      make_chunks <- make_designer_chunks
    }
  }
  else  
    stop("Argument 'design_or_designer' must either be an object of class 'design' or a designer that returns an object of class 'design'")
  
  #title_ <- object_name
  file_name <- tolower(object_name)

  if(is.null(title)) title <-  gsub("_", replacement = " ", object_name) 
  
  if(has_shiny) shiny <- 
    paste0('
<!--
  <a class="btn btn-primary" href="http://shiny.declaredesign.org/inspector/?topic=',file_name,'">
     <i class="fa fa-area-chart" title = "Go to the design inspector" fa-2x></i> Inspect design
   </a>
-->'
    
  )
 
  front_text <- make_text(front_text, front_text_path)
  end_text   <- make_text(end_text, end_text_path)
  
  if(file.exists(paste0(file_name, ".Rmd")))
    stop("Vignette already exists")
  file.create(paste0(file_name, ".Rmd"))
  cat(make_header(title, shiny, file_name ),
      front_text,
      make_chunks(file_name, sims, bootstrap, has_shiny),
      end_text, sep ="\n", file = paste0(file_name, ".Rmd"))
  
}

