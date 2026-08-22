#' ResearchDesigns: a library of declared designs
#'
#' Lightweight library of research designs declared with DeclareDesign.
#' Designs live as self-contained R files under `inst/designs/`. Editable
#' parameters are read from the design object; optional YAML metadata adds
#' labels, categories, book aliases, `diagnosands:` display defaults, and
#' `params:` tip strings.
#'
#' @section Workflow:
#' ```r
#' list_designs()
#' make_design("two_arm_trial", b = 0.5)
#' two_arm_designer(N = 40, ate = 0.2)
#' get_args("two_arm_trial")
#' get_code("two_arm_trial")
#' run_shiny()
#' ```
#'
#' @section Shiny deploy:
#' ```r
#' remotes::install_github("macartan/ResearchDesigns")
#' install_library_dependencies()
#' copy_library_shiny("/path/to/shiny-app")
#' ```
#'
#' @keywords internal
#' @importFrom yaml yaml.load
#' @importFrom utils write.csv install.packages packageVersion
"_PACKAGE"
