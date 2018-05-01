make_header <- function(title = "", shiny, file_name = "" ){
  x <- paste0('---
title: " ', title, ' " 
output:
  rmarkdown::html_vignette:
              css: !expr system.file("css", "vignette.css", package = "DesignLibrary")
              bibliography: bib.bib
vignette: |
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
  &nbsp;
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
```{r,eval = FALSE}
diagnosis <- diagnose_design(',file_name,'_design, sims = ',sims,', bootstrap = ',bootstrap,')
```
```{r ',file_name,'_diagnosis,echo = FALSE}
 diagnosis <- get_or_run_diagnosis(',file_name,'_design, sims = ',sims,', bootstrap = ',bootstrap,')
 knitr::kable(reshape_diagnosis(diagnosis, digits = 2), digits = 2)
```
')
  ## has_shiny
  if(has_shiny){
   chunk2 <- paste0('
```{r ',file_name,'_shiny_diagnosis,include = FALSE}
 get_or_run_shiny_diagnosis(',file_name,'_designer(), ',sims,', bootstrap = ',bootstrap,')
```
                     
')  
  }

  paste0(chunk1, chunk2)
}

make_text <- function(text, text_path){
  
  if(!is.null(text_path))
  { 
    text <- readLines(text_path)
    if(endsWith(text_path, ".Rmd"))
    {
     rmarkdown::render(text_path, quiet = TRUE, output_file = "DD_temp_rmd.Rmd")
     file.remove("DD_temp_rmd.Rmd")
     text <- paste0(p[grep("```", text)[1]:length(text)], sep = "\n")
     }
  }
  text
}

#' Create a vignetten based on a design or a designer.
#' @param design_or_designer A design or a designer.
#' @param title A character string for the vignette title.  If NULL then a default based on the name of design_or_designer.
#' @param front_text A character string.
#' @param end_text A character string.
#' @param front_text_path Path to .Rmd; If different from NULL, it overwrites front_text.
#' @param end_text_path   Path to .Rmd; If different from NULL, it overwrites end_text.
#' @param overwrite If TRUE overwrites .Rmd
#' @param sims Number of simulations for DeclareDesign.
#' @param bootstraps  Number of bootstraps for DeclareDesign. 
#' @param output_folder If NULL, vignette is saved in working directory.
#' @return This function creates a .Rmd 
#' @importFrom devtools is.package as.package
#' @export
make_vignette <- function(design_or_designer,
                          title = NULL,
                          front_text = NULL,
                          end_text = NULL,
                          front_text_path = NULL,
                          end_text_path = NULL,
                          overwrite = FALSE, 
                          sims = 1000,
                          bootstrap = FALSE,
                          output_folder = NULL) {
  
  options(error = NULL)
  our_package = "DesignLibrary"
  object_name <- deparse(substitute(design_or_designer))
  shiny = ""
  class_object <- class(design_or_designer)
  has_shiny <- FALSE
  if(is.null(output_folder)) {
    output_folder <- ""
    pkg <- "."
    if(is.package(as.package(pkg))) {
      pkg <- as.package(pkg)
      if(pkg$package ==  our_package)
        output_folder <- paste0(as.package(pkg)$path, "\\vignettes\\")
    } 
  }  
  
  if (is.null(title))
    title <-  gsub("_", replacement = " ", object_name)

  if(is.null(end_text)) 
    end_text <- ""
  
  if (any(class_object  == "design")) {
    if (!endsWith(object_name, "_design"))
      stop("Design's name must end with suffix '_design'")
    make_chunks <- make_design_chunks
    object_name <- sub("_design", replacement = "", object_name)
    if(is.null(front_text)) 
      front_text <- ""
  }
  else if (is.function(design_or_designer))
  {
    class_object <- class(design_or_designer())
    if (!any(class_object  == "design"))
      stop(
        "Argument 'design_or_designer' must either be an object of class 'design' 
         or a designer that returns an object of class 'design'"
      )
    else if (!endsWith(object_name, "_designer"))
      stop("Designer's name must end with suffix '_designer'")
    else
    {
      if(is.null(front_text)){
        rdb_path <- file.path(system.file("help", package= our_package),our_package)
        help_text <- tools:::fetchRdDB(rdb_path, object_name)
        classes <- sapply( help_text, function(x) attr(x, "Rd_tag"))
        desc <- help_text[[which(grepl("\\\\description", classes))]]
        desc  <- do.call(paste, desc)
        desc  <- gsub("\n", "", desc)
        front_text <- desc
      }
      has_shiny <- !is.null(get_shiny_arguments(design_or_designer))
      object_name <- sub("_designer", replacement = "", object_name)
      make_chunks <- make_designer_chunks
    }
  }
  else
    stop(
      "Argument 'design_or_designer' must either be an object of class 'design'
      or a designer that returns an object of class 'design'"
    )

  file_name  <- tolower(object_name)
  front_text <- make_text(front_text, front_text_path)
  end_text   <- make_text(end_text, end_text_path)

  if (has_shiny)
    shiny <- paste0(' 
<!--
   <a class="btn btn-primary" href="http://shiny.declaredesign.org/inspector/?topic=',file_name,'">
    <i class="fa fa-area-chart" title = "Go to the design inspector" fa-2x></i> Inspect design
  </a>
-->')
  
  if (file.exists(paste0(output_folder ,file_name, ".Rmd")) & !overwrite)
    stop("Vignette already exists")
  
  file.create(paste0(output_folder, file_name, ".Rmd"))
  cat(make_header(title, shiny, file_name),
      front_text,
      make_chunks(file_name, sims, bootstrap, has_shiny),
      end_text,
      sep = "\n",
      file = paste0(output_folder, file_name, ".Rmd"))
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

