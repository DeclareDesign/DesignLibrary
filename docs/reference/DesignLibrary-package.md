# DesignLibrary: a library of declared designs

Lightweight library of research designs declared with DeclareDesign.
Designs live as self-contained R files under `inst/designs/`. Editable
parameters are read from the design object; optional YAML metadata adds
labels, categories, book aliases, `diagnosands:` display defaults, and
`params:` tip strings.

## Workflow

    list_designs()
    make_design("two_arm_simple", b = 0.5)
    two_arm_designer(N = 40, ate = 0.2)
    get_args("two_arm_simple")
    get_code("two_arm_simple")
    run_shiny()

## Shiny deploy

    remotes::install_github("DeclareDesign/DesignLibrary@RDrewrite")
    install_library_dependencies()
    copy_library_shiny("/path/to/shiny-app")

## See also

Useful links:

- <https://github.com/DeclareDesign/DesignLibrary>

- <https://declaredesign.org/r/designlibrary/>

- Report bugs at <https://github.com/DeclareDesign/DesignLibrary/issues>

## Author

**Maintainer**: Macartan Humphreys <macartan.humphreys@wzb.eu>

Authors:

- Macartan Humphreys <macartan.humphreys@wzb.eu>

- Graeme Blair <graeme.blair@ucla.edu>

- Jasper Cooper <jjc2247@columbia.edu>

- Alexander Coppock <alex.coppock@northwestern.edu>

- Clara Bicalho <clarabmcorreia@gmail.com>

- Neal Fultz <nfultz@gmail.com>

- Lily Medina <lilymiru@gmail.com>
