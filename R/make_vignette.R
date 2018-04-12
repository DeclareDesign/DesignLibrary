make_header <- function(title = "", has_shiny, file_name = "" ){
  x <- paste0('
---
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
library(designs)
```
              
<div class="btn-group">
  <a class="btn btn-primary" href="http://declaredesign.org/articles/',file_name,'.R">
     <i class="fa fa-code" title = "Download code for design" fa-2x></i> Download code
        </a>
    <a class="btn btn-primary" href="http://declaredesign.org/articles/',file_name,'_designer.RDS">
     <i class="fa fa-clipboard" title = "Download template for design" fa-2x></i> Download template
    </a>', has_shiny,' 
  </div>
            
  
')}

make_design_chunks <- function(file_name, n_sims, n_bootstrap) {
  paste0(' 
              
```{r, ', file_name, '_diagnosis}
diagnosis <- get_or_run_diagnosis(', file_name, ', sims = ', n_sims, ', bootstrap = ', n_bootstrap, ')
```
         
')
}


make_designer_chunks <- function(file_name, n_sims, n_bootstrap) {
  
  chunk2 <- ""
  chunk1 <- paste0('
```{r, code = designer_default_args_text(', file_name, '_designer)}
```

  
```{r, code = ',file_name,'_designer(code = TRUE)}
```
  

```{r ',file_name,'_diagnosis,echo = FALSE}
 diagnosis <- get_or_run_diagnosis(',file_name,', sims = ',n_sims,', bootstrap = ',n_bootstrap,')
 knitr::kable(diagnosis$diagnosands,digits = 2)
```
  
  
')
  if(is.null(get_shiny_arguments(paste0(file_name,'_designer')))){
   chunk2 <- paste0('
```{r ',file_name,'_shiny_diagnosis,include = FALSE}
 get_or_run_shiny_diagnosis(',file_name,'_designer, ',n_sims,', bootstrap = ',n_bootstrap,')
```
                     
')  
  }
  
  chunk3 <- paste0('
```{r,eval = FALSE}
diagnosis <- diagnose_design(',file_name,', sims = ',n_sims,', bootstrap = ',n_bootstrap,')
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
#' @param front_text A character string 
#' @param end_text A character string 
#' @param front_text_path Path to .Rmd; If different from NULL, it overwrites front_text
#' @param end_text_path   Path to .Rmd; If different from NULL, it overwrites end_text
#' @param n_sims number of simulations for DeclareDesign
#' @param n_bootstraps  number of bootstraps for DeclareDesign 
#' @return A .Rmd and A RDS
#' @export
#'
#' @examples

make_vignette <- function(design_or_designer = NULL, front_text = "", end_text = "", front_text_path = NULL, end_text_path = NULL , n_sims = 1000, n_bootstrap = FALSE, has_shiny = FALSE){
  
  object_name <- deparse(substitute(design_or_designer))
  
  if(class(design_or_designer) == "design") 
    make_chunks <- make_design_chunks
  else {
    object_name <- sub("_designer", replacement = "", object_name) 
    make_chunks <- make_designer_chunks
  }
  
  title_ <- object_name
  file_name <- tolower(object_name)
  title <-  sub("_", replacement = " ", object_name) 
  
  if(has_shiny) shiny <- 
    paste0('
<!--
  <a class="btn btn-primary" href="http://shiny.declaredesign.org/inspector/?topic=',file_name,'">
     <i class="fa fa-area-chart" title = "Go to the design inspector" fa-2x></i> Inspect design
   </a>
-->'
    
  )
  else 
    shiny <- ""
  
  front_text <- make_text(front_text, front_text_path)
  
  end_text   <- make_text(end_text, end_text_path)

  file.create(paste0(file_name, ".Rmd"))
  
  cat(make_header(title = file_name, has_shiny ),
      front_text,
      make_chunks(file_name, n_sims, n_bootstrap),
      end_text, sep ="\n", file = paste0(file_name, ".Rmd"))
  
}



simple_two_arm_design <- simple_two_arm_designer

## examples 
make_vignette(design_or_designer = simple_two_arm_design, front_text_path = "rmd1.Rmd", end_text_path = "rmd1.Rmd")

