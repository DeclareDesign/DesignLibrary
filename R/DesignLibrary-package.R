#' DesignLibrary: a library of declared designs
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
#' make_design("two_arm_simple", b = 0.5)
#' two_arm_designer(N = 40, ate = 0.2)
#' get_args("two_arm_simple")
#' get_code("two_arm_simple")
#' run_shiny()
#' ```
#'
#' @section Shiny deploy:
#' ```r
#' install.packages("DesignLibrary")
#' install_library_dependencies()
#' copy_library_shiny("/path/to/shiny-app")
#' ```
#'
#' @keywords internal
#' @importFrom yaml yaml.load
#' @importFrom utils write.csv install.packages packageVersion
# DeclareDesign (in Depends) attaches these three, and the design files call
# them by name, so nothing here calls them with `::` and `R CMD check`
# reports the Imports as unused. They stay in Imports for their 2.0 version
# floors; importing one symbol from each says a design cannot run without them.
#' @importFrom fabricatr fabricate
#' @importFrom estimatr lm_robust
#' @importFrom randomizr complete_ra
#' @importFrom DeclareDesign redesign
"_PACKAGE"
