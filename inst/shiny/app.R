# DesignLibrary Shiny browser
# Deploy: remotes::install_github(...); install_library_dependencies(); copy_library_shiny(dest)
# Flow: Library (searchable table) -> Design / Diagnosis / Redesign

library(shiny)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

.app_dir <- if (nzchar(Sys.getenv("SHINY_APP_DIR", ""))) {
  Sys.getenv("SHINY_APP_DIR")
} else {
  getwd()
}
for (cfg in c("deploy-options.R", "local.R")) {
  path <- file.path(.app_dir, cfg)
  if (file.exists(path)) source(path, local = FALSE)
}

PKGDOWN_URL <- "https://declaredesign.org/r/designlibrary/"
GITHUB_URL <- "https://github.com/DeclareDesign/DesignLibrary"
DECLAREDESIGN_URL <- "https://declaredesign.org/"
BOOK_URL <- "https://book.declaredesign.org/"

has_bslib <- requireNamespace("bslib", quietly = TRUE)
has_dt <- requireNamespace("DT", quietly = TRUE)
has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

# Colors from Research Design in the Social Sciences cover + DeclareDesign logo/site
# Cover: magenta title #C41E7A, navy body text #142F5E; logo blue #2860F6; site #2780e3
DD_NAVY <- "#142F5E"
DD_MAGENTA <- "#C41E7A"
DD_BLUE <- "#2860F6"
DD_PRIMARY <- "#2780e3"
DD_LINK <- "#1d48a8"
DD_SOFT <- "#d4e6f9"
DD_SOFT2 <- "#eaf2fc"
DD_SOFT_MAGENTA <- "#f9e8f1"
DD_BODY <- "#343a40"
DD_MUTED <- "#6c757d"
DD_BORDER <- "#dee2e6"
DD_CODE_BG <- "#f4f7fb"

app_css <- sprintf("
:root {
  --dd-navy: %s;
  --dd-magenta: %s;
  --dd-blue: %s;
  --dd-primary: %s;
  --dd-link: %s;
  --dd-soft: %s;
  --dd-soft2: %s;
  --dd-soft-magenta: %s;
  --dd-body: %s;
  --dd-muted: %s;
  --dd-border: %s;
  --dd-code-bg: %s;
}
body { color: var(--dd-body); background: #f8f9fa; }
.navbar, .navbar.navbar-default, .navbar.bg-primary {
  background-color: #fff !important;
  border-bottom: 1px solid var(--dd-border);
  box-shadow: 0 1px 0 rgba(20, 47, 94, 0.04);
}
.navbar .navbar-brand, .navbar .nav-link, .navbar .navbar-nav > li > a {
  color: var(--dd-navy) !important;
}
.navbar .nav-link:hover, .navbar .navbar-nav > li > a:hover {
  color: var(--dd-magenta) !important;
}
.navbar .nav-link.active, .navbar .active > .nav-link,
.navbar .navbar-nav > .active > a {
  color: var(--dd-magenta) !important;
  font-weight: 650;
  box-shadow: inset 0 -2px 0 var(--dd-magenta);
}
.rd-brand {
  display: inline-flex; align-items: center; gap: 0.55rem;
  font-weight: 700; color: var(--dd-navy) !important;
  letter-spacing: -0.01em;
}
.rd-brand img { display: block; height: 1.65rem; width: auto; }
.rd-wrap { width: 90%%; max-width: 1600px; margin: 0 auto; padding: 1rem 1.25rem 2rem; }
.rd-wrap-library { width: 94%%; max-width: none; }
.rd-hero h1 { font-size: 1.75rem; margin-bottom: 0.25rem; color: var(--dd-navy); }
.rd-hero p { color: var(--dd-muted); margin-bottom: 1rem; }
.rd-card {
  background: #fff; border: 1px solid var(--dd-border); border-radius: 10px;
  padding: 1rem 1.15rem; margin-bottom: 1rem;
  box-shadow: 0 1px 2px rgba(20, 47, 94, 0.04);
}
.rd-muted { color: var(--dd-muted); font-size: 0.92rem; }
.rd-code {
  background: var(--dd-code-bg); border: 1px solid var(--dd-border);
  border-radius: 8px; padding: 0.85rem 1rem; margin: 0 0 1rem;
  font-size: 0.85rem; white-space: pre-wrap; overflow-x: auto;
  color: var(--dd-navy); font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
}
.rd-code-oneline {
  white-space: pre; overflow-x: auto;
}
.rd-profile {
  background: var(--dd-soft2); border-left: 4px solid var(--dd-magenta);
  border-radius: 0 8px 8px 0;
  padding: 0.85rem 1rem; white-space: pre-wrap;
  font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
  font-size: 0.84rem; line-height: 1.45; color: var(--dd-body);
}
.rd-chip {
  display: inline-block; background: var(--dd-soft-magenta); color: var(--dd-navy);
  border-radius: 999px; padding: 0.15rem 0.55rem; margin: 0.1rem 0.2rem 0.1rem 0;
  font-size: 0.78rem; font-weight: 600;
}
details.rd-details {
  border: 1px solid var(--dd-border); border-radius: 10px;
  padding: 0.65rem 0.9rem; background: #fff;
}
details.rd-details > summary {
  cursor: pointer; font-weight: 600; list-style: none; color: var(--dd-navy);
}
details.rd-details > summary::-webkit-details-marker { display: none; }
details.rd-details > summary::before {
  content: '\\25B8  ';
  display: inline-block;
  margin-right: 0.15rem;
  color: var(--dd-magenta);
}
details.rd-details[open] > summary::before { content: '\\25BE  '; }
details.rd-details[open] > summary { margin-bottom: 0.65rem; }
.rd-selected-banner {
  display: flex; flex-wrap: wrap; gap: 0.75rem; align-items: center;
  justify-content: space-between; margin-bottom: 1rem;
}
.rd-selected-banner h3 { color: var(--dd-navy); }
.rd-param-grid {
  display: grid;
  grid-template-columns: minmax(6rem, 14%%) minmax(1.25rem, 1.75rem) 1fr;
  gap: 0.15rem 0.45rem;
  align-items: center;
  margin: 0.35rem 0 0.5rem;
}
.rd-param-grid .form-group,
.rd-param-grid .shiny-input-container {
  margin-bottom: 0 !important;
  padding-bottom: 0 !important;
}
.rd-param-grid .form-control {
  padding-top: 0.25rem;
  padding-bottom: 0.25rem;
  min-height: calc(1.4em + 0.5rem);
}
.rd-param-name { font-weight: 650; color: var(--dd-navy); font-size: 0.92rem; }
.rd-tip {
  display: inline-flex; align-items: center; justify-content: center;
  width: 1.05rem; height: 1.05rem; border-radius: 999px;
  background: var(--dd-soft-magenta); color: var(--dd-magenta); font-size: 0.68rem; font-weight: 700;
  cursor: help; user-select: none;
}
.rd-tip-empty { visibility: hidden; }
.rd-help-box {
  background: var(--dd-soft2); border: 1px solid var(--dd-border); border-radius: 8px;
  padding: 0.45rem 0.7rem; font-size: 0.86rem; color: var(--dd-body);
  margin-bottom: 0.45rem;
}
.rd-help-box ul { margin: 0.15rem 0 0.15rem 1.15rem; padding: 0; }
.rd-help-box li + li { margin-top: 0.2rem; }
.rd-help-box code { background: #fff; padding: 0.05rem 0.3rem; border-radius: 4px; color: var(--dd-navy); }
.rd-error { color: #ff0039; font-size: 0.9rem; margin: 0.35rem 0; }
.rd-design-link {
  color: var(--dd-blue); font-weight: 600; text-decoration: none;
  border-bottom: 1px solid transparent;
}
.rd-design-link:hover { color: var(--dd-magenta); border-bottom-color: var(--dd-magenta); }
.rd-label-cell { white-space: nowrap; }
.rd-permalink {
  display: inline-flex; align-items: center; justify-content: center;
  margin-left: 0.35rem; vertical-align: middle;
  color: var(--dd-muted); text-decoration: none;
  border-radius: 4px; padding: 0.1rem;
  border-bottom: none !important; font-weight: 400;
}
.rd-permalink svg { width: 0.95rem; height: 0.95rem; display: block; }
.rd-permalink:hover { color: var(--dd-magenta); background: var(--dd-soft); }
.rd-share-url {
  font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
  font-size: 0.78rem; color: var(--dd-muted); word-break: break-all;
}
.rd-share-row {
  display: inline-flex; align-items: center; gap: 0.35rem;
  margin-top: 0.35rem; font-size: 0.85rem; color: var(--dd-muted);
}
.rd-steps { counter-reset: rd-step; list-style: none; padding-left: 0; margin: 0.5rem 0 1rem; }
.rd-steps > li {
  position: relative; padding: 0.65rem 0.75rem 0.65rem 3rem;
  margin-bottom: 0.55rem; background: var(--dd-soft2); border: 1px solid var(--dd-border);
  border-radius: 10px;
}
.rd-steps > li::before {
  counter-increment: rd-step; content: counter(rd-step);
  position: absolute; left: 0.7rem; top: 0.7rem;
  width: 1.55rem; height: 1.55rem; border-radius: 999px;
  background: var(--dd-magenta); color: #fff; font-size: 0.82rem; font-weight: 700;
  display: flex; align-items: center; justify-content: center;
}
.rd-steps > li strong { color: var(--dd-navy); }
.rd-yaml-table { width: 100%%; border-collapse: collapse; font-size: 0.9rem; }
.rd-yaml-table th, .rd-yaml-table td {
  text-align: left; vertical-align: top; padding: 0.45rem 0.55rem;
  border-bottom: 1px solid var(--dd-border);
}
.rd-yaml-table th { color: var(--dd-navy); font-weight: 650; background: var(--dd-soft-magenta); }
.rd-yaml-table code { background: var(--dd-soft); padding: 0.05rem 0.3rem; border-radius: 4px; }
.btn-primary {
  background-color: var(--dd-magenta) !important;
  border-color: var(--dd-magenta) !important;
}
.btn-primary:hover {
  background-color: var(--dd-navy) !important;
  border-color: var(--dd-navy) !important;
}
.btn-outline-secondary {
  color: var(--dd-navy) !important;
  border-color: var(--dd-border) !important;
}
.btn-outline-secondary:hover {
  background: var(--dd-soft-magenta) !important;
  color: var(--dd-magenta) !important;
}
a { color: var(--dd-blue); }
a:hover { color: var(--dd-magenta); }
.rd-cover {
  float: right; width: 140px; max-width: 32%%; margin: 0 0 1rem 1rem;
  border: 1px solid var(--dd-border); border-radius: 4px;
  box-shadow: 0 2px 8px rgba(20, 47, 94, 0.12);
}
#library_table table.dataTable tbody tr { cursor: pointer; }
#library_table table.dataTable tbody tr:hover { background-color: var(--dd-soft-magenta) !important; }
#library_table table.dataTable thead th {
  color: var(--dd-navy); border-bottom-color: var(--dd-magenta) !important;
}
", DD_NAVY, DD_MAGENTA, DD_BLUE, DD_PRIMARY, DD_LINK, DD_SOFT, DD_SOFT2, DD_SOFT_MAGENTA, DD_BODY, DD_MUTED, DD_BORDER, DD_CODE_BG)
app_css <- paste0(
  app_css,
  "
.rd-lib-tabs { margin: 0.15rem 0 0.45rem; }
.rd-lib-tabs .tab-content { display: none; }
.rd-lib-tabs .nav-tabs { border-bottom-color: var(--dd-border); }
.rd-lib-tabs .nav-link, .rd-lib-tabs .nav-tabs > li > a {
  color: var(--dd-navy); padding: 0.35rem 0.8rem; font-weight: 600;
}
.rd-lib-tabs .nav-link.active, .rd-lib-tabs .nav-tabs > .active > a {
  color: var(--dd-magenta) !important; font-weight: 650;
  border-bottom-color: var(--dd-magenta);
}
.rd-lib-search { margin-bottom: 0.35rem; }
.rd-lib-hint { margin: 0 0 0.55rem; }
"
)

brand_title <- function() {
  tags$span(
    class = "rd-brand",
    tags$img(src = "dd-logo.svg", alt = "DeclareDesign", height = "28"),
    "DesignLibrary"
  )
}

app_head <- tags$head(
  tags$link(rel = "icon", type = "image/svg+xml", href = "dd-logo.svg"),
  tags$link(rel = "apple-touch-icon", href = "dd-icon.png"),
  tags$meta(name = "theme-color", content = DD_MAGENTA),
  tags$title("DesignLibrary")
)

theme_obj <- if (has_bslib) {
  bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = DD_MAGENTA,
    secondary = DD_NAVY,
    success = "#3fb618",
    info = DD_PRIMARY,
    warning = "#f0ad4e",
    danger = "#ff0039",
    "body-color" = DD_BODY,
    "link-color" = DD_BLUE,
    "navbar-bg" = "#ffffff",
    "navbar-light-color" = DD_NAVY,
    "navbar-light-active-color" = DD_MAGENTA,
    "navbar-light-hover-color" = DD_MAGENTA,
    "font-size-base" = "0.98rem"
  )
} else {
  NULL
}

library_category_tabs <- function(categories) {
  keys <- DesignLibrary:::library_tab_keys(categories)
  if (!length(keys)) keys <- c("templates", "rdss")
  panels <- lapply(keys, function(k) {
    tabPanel(title = DesignLibrary:::library_tab_label(k), value = k)
  })
  div(
    class = "rd-lib-tabs",
    do.call(
      tabsetPanel,
      c(list(id = "lib_category", selected = keys[[1]]), panels)
    )
  )
}

library_panel <- function() {
  idx <- tryCatch({
    out <- DesignLibrary::list_designs(shiny_only = TRUE)
    if (!nrow(out)) DesignLibrary::list_designs() else out
  }, error = function(e) NULL)
  cats <- if (!is.null(idx) && nrow(idx)) as.character(idx$category) else character(0)
  div(
    class = "rd-card",
    div(
      class = "rd-lib-search",
      textInput(
        "lib_search",
        NULL,
        placeholder = "Search all categories: id, alias, label, params, keywords…",
        width = "100%"
      )
    ),
    library_category_tabs(cats),
    uiOutput("lib_search_hint"),
    if (has_dt) {
      DT::DTOutput("library_table")
    } else {
      tagList(
        helpText("Install DT for a searchable table; showing a basic table instead."),
        tableOutput("library_table_basic")
      )
    }
  )
}

design_panel <- function() {
  tagList(
    uiOutput("selected_header"),
    div(
      class = "rd-card",
      tags$details(
        class = "rd-details",
        tags$summary("Design profile"),
        verbatimTextOutput("design_profile_text")
      )
    ),
    div(
      class = "rd-card",
      h4("One-line"),
      uiOutput("simple_code_ui"),
      h4("Full declaration"),
      uiOutput("full_code_ui"),
      h4("Editable parameters"),
      tableOutput("args_table"),
      br(),
      actionButton("run_once", "Run this design once", class = "btn-secondary btn-sm"),
      verbatimTextOutput("run_once_out")
    )
  )
}

diagnosis_panel <- function() {
  tagList(
    uiOutput("selected_header_diag"),
    div(
      class = "rd-card",
      p(class = "rd-muted", "Baked preview from the package when available; otherwise run a live diagnosis."),
      uiOutput("preview_status"),
      numericInput("diag_sims", "Simulations", value = 100, min = 1, step = 10, width = "140px"),
      actionButton("run_diagnosis", "Run diagnosis", class = "btn-primary"),
      br(), br(),
      h4("Diagnosands"),
      uiOutput("diag_diagnosand_ui"),
      tableOutput("diagnosis_table"),
      if (has_ggplot2) plotOutput("diagnosis_plot", height = "320px") else NULL
    )
  )
}

redesign_panel <- function() {
  tagList(
    uiOutput("redesign_top"),
    uiOutput("mod_error"),
    # Keep plot/table outside renderUI so ggplot binds reliably
    conditionalPanel(
      condition = "output.mod_show_results == true",
      div(
        class = "rd-card",
        fluidRow(
          column(5, uiOutput("mod_diagnosand_ui")),
          column(7, uiOutput("mod_plot_controls"))
        ),
        if (has_ggplot2) plotOutput("mod_plot", height = "380px") else NULL,
        tags$details(
          class = "rd-details",
          style = "margin-top: 0.75rem;",
          tags$summary("Diagnosand table"),
          tableOutput("mod_table")
        )
      )
    )
  )
}

about_panel <- function() {
  div(
    class = "rd-card",
    tags$a(
      href = BOOK_URL, target = "_blank",
      tags$img(
        src = "rdss-cover.png",
        class = "rd-cover",
        alt = "Research Design in the Social Sciences"
      )
    ),
    h3("About DesignLibrary"),
    p(
      tags$img(src = "dd-logo.svg", alt = "DeclareDesign", height = "36", style = "vertical-align: middle; margin-right: 0.5rem;"),
      "A library of declared designs in the DeclareDesign ecosystem. Designs are self-contained R files; editable parameters come from the design object."
    ),
    p(
      class = "rd-muted",
      "Built around declaration, diagnosis, and redesign — see ",
      tags$a(href = BOOK_URL, target = "_blank", "Research Design in the Social Sciences"),
      "."
    ),
    tags$ul(
      tags$li(tags$a(href = DECLAREDESIGN_URL, target = "_blank", "DeclareDesign")),
      tags$li(tags$a(href = BOOK_URL, target = "_blank", "Research Design in the Social Sciences (book)")),
      tags$li(tags$a(href = PKGDOWN_URL, target = "_blank", "Package documentation (pkgdown)")),
      tags$li(tags$a(href = paste0(PKGDOWN_URL, "articles/contributing.html"), target = "_blank", "Contributing guide")),
      tags$li(tags$a(href = GITHUB_URL, target = "_blank", "GitHub repository"))
    ),
    p(class = "rd-muted", style = "clear: both;",
      paste0(
        "Package version ",
        tryCatch(as.character(utils::packageVersion("DesignLibrary")), error = function(e) "?"),
        "."
      )
    )
  )
}

contribute_yaml_tips <- function() {
  data.frame(
    Field = c(
      "id", "alias", "label", "description", "category", "keywords",
      "packages", "diagnosands", "params", "book_link", "include_in_shiny", "functional", "object"
    ),
    Tip = c(
      "Substantive snake_case name; should match the filename stem (e.g. two_arm_trial).",
      "Optional book reference string (e.g. \"2.1\"). Quoted if it looks numeric.",
      "Short human-readable title shown in the library.",
      "One or two sentences on what the design does. Use YAML > for a folded block.",
      "Grouping label. Use rdss for book designs; template for teaching starters; otherwise a short group name.",
      "List of search terms, e.g. [experiment, blocking].",
      "Extra R packages the design needs beyond DeclareDesign, e.g. [margins, broom].",
      "Preferred display diagnosands, e.g. [rmse, bias]. Prefix with - to hide one (rmse, -bias, power).",
      "Map parameter names to tip strings. Always quote keys: \"N\": \"Sample size\". Only top-level assignments before design <- count (N <- 1000), not literals inside declare_*(N = 1000). No design-step names.",
      "URL to a book section or external docs.",
      "true (default) or false. Set false for incomplete or heavy designs.",
      "true (default) or false. Set false to park a design (e.g. unavailable packages); skips audit/smoke/install and forces include_in_shiny false.",
      "Name of the design object in the file. Omit if the object is named design."
    ),
    stringsAsFactors = FALSE
  )
}

contribute_panel <- function() {
  tips <- contribute_yaml_tips()
  tip_rows <- lapply(seq_len(nrow(tips)), function(i) {
    tags$tr(
      tags$td(tags$code(tips$Field[[i]])),
      tags$td(tips$Tip[[i]])
    )
  })

  tagList(
    div(
      class = "rd-card",
      h3("Contribute a design"),
      p(
        "You contribute ", tags$strong("one self-contained R file"),
        " in ", tags$code("inst/designs/"),
        ". That file holds the declared design and an optional YAML header. ",
        "Nothing else is required for the library, API, or this browser to pick it up."
      ),
      p(
        class = "rd-muted",
        "Full write-up: ",
        tags$a(
          href = paste0(PKGDOWN_URL, "articles/contributing.html"),
          target = "_blank",
          "Contributing a design (vignette)"
        ),
        "."
      )
    ),
    div(
      class = "rd-card",
      h4("Workflow"),
      tags$ol(
        class = "rd-steps",
        tags$li(
          tags$strong("Fork"),
          " the repository on GitHub and clone your fork locally."
        ),
        tags$li(
          tags$strong("Add your file"),
          " under ", tags$code("inst/designs/"),
          ". Name it after the design id (e.g. ", tags$code("my_design.R"),
          "). Declare an object named ", tags$code("design"),
          " (or set ", tags$code("object:"), " in YAML). Keep the file self-contained—no ",
          tags$code("source()"), " of other designs."
        ),
        tags$li(
          tags$strong("Refresh"),
          " so index, audit, and preview artifacts are built for your design:",
          tags$pre(
            class = "rd-code",
            "options(DesignLibrary.root = \"/path/to/your/DesignLibrary\")\nrefresh_library()"
          )
        ),
        tags$li(
          tags$strong("Check"),
          " locally. At minimum:",
          tags$pre(
            class = "rd-code",
            "list_designs()\nmake_design(\"my_design\")\naudit_designs()\nrun_shiny()"
          ),
          "Confirm your design appears in the Library, opens cleanly, and diagnosis/redesign behave as you expect."
        ),
        tags$li(
          tags$strong("Pull request"),
          " from your fork to ",
          tags$a(href = GITHUB_URL, target = "_blank", "macartan/DesignLibrary"),
          ". Keep the PR focused on the new (or updated) design file and any packages it needs."
        )
      )
    ),
    div(
      class = "rd-card",
      h4("Minimal file"),
      p("YAML is optional. This is enough:"),
      tags$pre(
        class = "rd-code",
        paste(
          "b <- 0.2",
          "design <-",
          "  declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ b * Z + U)) +",
          "  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +",
          "  declare_assignment(Z = complete_ra(N)) +",
          "  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +",
          "  declare_estimator(Y ~ Z, inquiry = \"ATE\")",
          sep = "\n"
        )
      ),
      p(
        class = "rd-muted",
        "Parameters are discovered from the design object. Tip strings and library metadata live in optional YAML."
      )
    ),
    div(
      class = "rd-card",
      h4("YAML fields"),
      p("Optional frontmatter between ", tags$code("---"), " lines at the top of the file:"),
      tags$table(
        class = "rd-yaml-table",
        tags$thead(tags$tr(tags$th("Field"), tags$th("Tip"))),
        tags$tbody(tip_rows)
      ),
      tags$details(
        class = "rd-details",
        style = "margin-top: 0.85rem;",
        tags$summary("Example YAML header"),
        tags$pre(
          class = "rd-code",
          paste(
            "---",
            "id: two_arm_trial",
            "label: Simple two-arm trial",
            "category: template",
            "keywords: [experiment, two-arm]",
            "description: >",
            "  Simple two-arm trial with complete random assignment.",
            "params:",
            "  \"N\": \"Sample size\"",
            "  \"b\": \"Treatment effect\"",
            "diagnosands: [bias, power]",
            "include_in_shiny: true",
            "functional: true",
            "---",
            sep = "\n"
          )
        )
      )
    ),
    div(
      class = "rd-card",
      h4("Checklist"),
      tags$ul(
        lapply(DesignLibrary::contributor_checklist(), function(item) tags$li(item))
      ),
      p(
        class = "rd-muted",
        "In R: ", tags$code("contributor_checklist()"), ", ",
        tags$code("audit_designs()"), ", ",
        tags$code("refresh_library()"), "."
      )
    )
  )
}

ui_body <- if (has_bslib) {
  bslib::page_navbar(
    title = brand_title(),
    id = "main_nav",
    theme = theme_obj,
    header = tagList(app_head, tags$style(HTML(app_css))),
    bslib::nav_panel("Library", div(class = "rd-wrap rd-wrap-library", library_panel())),
    bslib::nav_panel("Design", div(class = "rd-wrap", design_panel())),
    bslib::nav_panel("Diagnosis", div(class = "rd-wrap", diagnosis_panel())),
    bslib::nav_panel("Redesign", div(class = "rd-wrap", redesign_panel())),
    bslib::nav_spacer(),
    bslib::nav_panel("Contribute", div(class = "rd-wrap", contribute_panel())),
    bslib::nav_panel("About", div(class = "rd-wrap", about_panel()))
  )
} else {
  fluidPage(
    app_head,
    tags$style(HTML(app_css)),
    titlePanel(brand_title()),
    tabsetPanel(
      id = "main_nav",
      tabPanel("Library", div(class = "rd-wrap rd-wrap-library", library_panel())),
      tabPanel("Design", div(class = "rd-wrap", design_panel())),
      tabPanel("Diagnosis", div(class = "rd-wrap", diagnosis_panel())),
      tabPanel("Redesign", div(class = "rd-wrap", redesign_panel())),
      tabPanel("Contribute", div(class = "rd-wrap", contribute_panel())),
      tabPanel("About", div(class = "rd-wrap", about_panel()))
    )
  )
}

ui <- ui_body

server <- function(input, output, session) {
  idx_all <- DesignLibrary::list_designs(shiny_only = TRUE)
  if (!nrow(idx_all)) idx_all <- DesignLibrary::list_designs()

  selected_id <- reactiveVal(NA_character_)
  live_diag <- reactiveVal(NULL)
  mod_diag <- reactiveVal(NULL)
  mod_status <- reactiveVal("")
  run_once_txt <- reactiveVal("")
  mod_last_ranges <- reactiveVal(character(0))

  # Comma/space-separated values -> c(...) for eval; leave single values alone
  normalize_value_expression <- function(str) {
    str <- trimws(str %||% "")
    if (!nzchar(str)) return(str)
    if (grepl("^c\\s*\\(", str) || grepl("^seq\\s*\\(", str)) return(str)
    parts <- strsplit(str, "[,;\\s]+")[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    if (length(parts) < 2L) return(str)
    parses_ok <- vapply(parts, function(p) {
      tryCatch({
        eval(parse(text = p), envir = baseenv())
        TRUE
      }, error = function(e) FALSE)
    }, logical(1L))
    if (all(parses_ok)) paste0("c(", paste(parts, collapse = ", "), ")") else str
  }

  format_arg_default <- function(args_row) {
    kind <- if ("kind" %in% names(args_row)) args_row$kind[[1]] else "scalar"
    val <- if ("default" %in% names(args_row)) args_row$default[[1]] else NULL
    DesignLibrary:::format_shiny_param_default(
      val, kind, args_row$value_str[[1]] %||% ""
    )
  }

  html_escape <- function(x) {
    x <- as.character(x %||% "")
    x[is.na(x)] <- ""
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x
  }

  tip_title <- function(tip, kind = "scalar") {
    tip <- trimws(tip %||% "")
    ex <- if (identical(kind, "vector")) {
      "Vector: 0.1, 0.2, 0.3. Sweep: 0,0,0,1; 0,0.5,0.5,1"
    } else {
      "Range example: 0, 10, 20"
    }
    if (nzchar(tip) && !is.na(tip)) paste0(tip, "\n", ex) else ex
  }

  redesign_kind_help <- function(id) {
    HTML(DesignLibrary:::redesign_kind_help(id))
  }

  collect_mod_dots <- function(id) {
    args <- DesignLibrary::get_args(id)
    if (!nrow(args)) {
      return(list(dots = list(), exprs = list(), lengths = integer(0), range_params = character(0)))
    }
    if ("shiny" %in% names(args)) {
      args <- args[isTRUE(args$shiny) | args$shiny %in% TRUE, , drop = FALSE]
    }
    dots <- list()
    exprs <- list()
    lengths <- integer(0)
    for (i in seq_len(nrow(args))) {
      nm <- args$name[[i]]
      kind <- if ("kind" %in% names(args)) args$kind[[i]] else "scalar"
      raw <- trimws(input[[paste0("mod_val_", nm)]] %||% "")
      if (!nzchar(raw)) next
      def <- format_arg_default(args[i, , drop = FALSE])
      parsed <- DesignLibrary:::parse_shiny_param_raw(raw, kind, def)
      if (isTRUE(parsed$skip)) next
      if (!is.null(parsed$error)) {
        return(list(error = paste0("Could not parse ", nm, ": ", parsed$error)))
      }
      dots[[nm]] <- parsed$value
      if (isTRUE(parsed$sweep) && identical(kind, "vector")) {
        exprs[[nm]] <- paste0("list(", paste(vapply(parsed$value, function(v) {
          paste0("c(", paste(as.character(v), collapse = ", "), ")")
        }, character(1)), collapse = ", "), ")")
        lengths[[nm]] <- length(parsed$value)
      } else if (isTRUE(parsed$sweep)) {
        exprs[[nm]] <- paste0("c(", paste(as.character(parsed$value), collapse = ", "), ")")
        lengths[[nm]] <- length(parsed$value)
      } else {
        val <- parsed$value
        exprs[[nm]] <- if (length(val) > 1L) {
          paste0("c(", paste(as.character(val), collapse = ", "), ")")
        } else {
          raw
        }
        lengths[[nm]] <- if (identical(kind, "vector")) 1L else length(val)
      }
    }
    range_params <- names(lengths)[lengths > 1L]
    list(dots = dots, exprs = exprs, lengths = lengths, range_params = range_params)
  }

  # Tabs: templates first, then RDSS, then any later YAML categories.
  # Search is global; if the current tab has no hits, switch to the first
  # matching tab (filter_library_browser).
  library_browser_state <- reactive({
    DesignLibrary:::filter_library_browser(
      as.data.frame(idx_all),
      tab = input$lib_category %||% "templates",
      q = input$lib_search %||% ""
    )
  })

  observeEvent(input$lib_search, {
    res <- library_browser_state()
    cur <- trimws(as.character(input$lib_category %||% ""))
    if (nzchar(res$tab) && !identical(cur, res$tab)) {
      updateTabsetPanel(session, "lib_category", selected = res$tab)
    }
  }, ignoreInit = TRUE)

  output$lib_search_hint <- renderUI({
    q <- trimws(input$lib_search %||% "")
    if (!nzchar(q)) return(NULL)
    res <- library_browser_state()
    if (!res$n_match) {
      return(div(class = "rd-muted rd-lib-hint", "No matching designs."))
    }
    others <- setdiff(names(res$match_counts), res$tab)
    if (!length(others)) {
      return(div(
        class = "rd-muted rd-lib-hint",
        sprintf(
          "%d match%s in %s (searched all categories).",
          res$n_match,
          if (res$n_match == 1L) "" else "es",
          DesignLibrary:::library_tab_label(res$tab)
        )
      ))
    }
    bits <- vapply(others, function(k) {
      sprintf(
        "%d in %s",
        as.integer(res$match_counts[[k]]),
        DesignLibrary:::library_tab_label(k)
      )
    }, character(1))
    div(
      class = "rd-muted rd-lib-hint",
      sprintf(
        "%d match%s across all categories. Showing %s; also %s.",
        res$n_match,
        if (res$n_match == 1L) "" else "es",
        DesignLibrary:::library_tab_label(res$tab),
        paste(bits, collapse = ", ")
      )
    )
  })

  filtered_idx <- reactive({
    # Keep list_designs() row order (starter sequence within templates).
    library_browser_state()$rows
  })

  # Inline SVG link icon for deep links / share
  permalink_svg <- paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" fill="currentColor" aria-hidden="true">',
    '<path d="M6.354 5.5H4a3 3 0 0 0 0 6h3a3 3 0 0 0 2.83-4H9c-.086 0-.17.01-.25.031A2 2 0 0 1 7 10.5H4a2 2 0 1 1 0-4h1.535c.218-.376.495-.714.82-1z"/>',
    '<path d="M9 5.5a3 3 0 0 0-2.83 4h1.098A2 2 0 0 1 9 6.5h3a2 2 0 1 1 0 4h-1.535a4.02 4.02 0 0 1-.82 1H12a3 3 0 1 0 0-6H9z"/>',
    "</svg>"
  )

  library_display <- reactive({
    df <- filtered_idx()
    ids <- as.character(df$id)
    labels <- as.character(df$label)
    bad_lab <- is.na(labels) | !nzchar(labels)
    labels[bad_lab] <- ids[bad_lab]
    label_html <- vapply(seq_along(ids), function(i) {
      id <- ids[[i]]
      href <- paste0("?design=", utils::URLencode(id, reserved = TRUE))
      sprintf(
        paste0(
          '<span class="rd-label-cell">%s',
          '<a class="rd-permalink" href="%s" title="Link to this design" ',
          'aria-label="Link to design %s">%s</a></span>'
        ),
        html_escape(labels[[i]]),
        href,
        html_escape(id),
        permalink_svg
      )
    }, character(1))
    params_disp <- as.character(df$params)
    params_disp[is.na(df$params)] <- ""
    data.frame(
      label = if (has_dt) label_html else labels,
      params = params_disp,
      packages = as.character(df$packages %||% ""),
      stringsAsFactors = FALSE,
      .id_key = ids,
      check.names = FALSE
    )
  })

  resolve_design_key <- function(key) {
    key <- trimws(as.character(key %||% "")[[1]])
    if (!nzchar(key)) return(NA_character_)
    df <- as.data.frame(idx_all)
    if (key %in% df$id) return(key)
    als <- as.character(df$alias %||% "")
    hit <- which(!is.na(als) & nzchar(als) & als == key)
    if (length(hit)) return(as.character(df$id[[hit[[1]]]]))
    tryCatch({
      info <- DesignLibrary::design_info(key)
      as.character(info$id[[1]])
    }, error = function(e) NA_character_)
  }

  design_query <- function(id) {
    paste0("?design=", utils::URLencode(as.character(id), reserved = TRUE))
  }

  design_share_url <- function(id) {
    proto <- session$clientData$url_protocol %||% "http:"
    host <- session$clientData$url_hostname %||% "127.0.0.1"
    path <- session$clientData$url_pathname %||% "/"
    port <- session$clientData$url_port %||% ""
    port_bit <- if (nzchar(as.character(port)) && !port %in% c("80", "443", "")) {
      paste0(":", port)
    } else {
      ""
    }
    paste0(proto, "//", host, port_bit, path, design_query(id))
  }

  go_to_design <- function(id, update_url = TRUE) {
    if (is.null(id) || is.na(id) || !nzchar(id)) return()
    id <- resolve_design_key(id)
    if (is.na(id) || !nzchar(id)) return()
    selected_id(id)
    live_diag(NULL)
    mod_diag(NULL)
    mod_last_ranges(character(0))
    mod_status("")
    run_once_txt("")
    updateSelectInput(session, "diag_diagnosands", selected = character(0))
    updateSelectInput(session, "mod_diagnosands", selected = character(0))
    if (isTRUE(update_url)) {
      shiny::updateQueryString(design_query(id), mode = "replace", session = session)
    }
    if (has_bslib) {
      bslib::nav_select("main_nav", selected = "Design", session = session)
    } else {
      updateTabsetPanel(session, "main_nav", selected = "Design")
    }
  }

  # Deep link: ?design=<id-or-alias> opens that design on load / URL change
  observe({
    search <- session$clientData$url_search
    q <- shiny::parseQueryString(search %||% "")
    key <- q$design %||% q$id
    if (is.null(key) || !nzchar(as.character(key)[[1]])) return()
    id <- resolve_design_key(key)
    if (is.na(id)) return()
    cur <- selected_id()
    if (is.na(cur) || !identical(cur, id)) {
      go_to_design(id, update_url = FALSE)
    }
  })

  if (has_dt) {
    output$library_table <- DT::renderDT({
      disp <- library_display()
      show <- disp[, c("label", "params", "packages"), drop = FALSE]
      DT::datatable(
        show,
        selection = "single",
        rownames = FALSE,
        escape = FALSE,
        class = "display nowrap",
        options = list(
          pageLength = 100,
          lengthChange = FALSE,
          autoWidth = FALSE,
          scrollX = TRUE,
          # Search is global (all tabs); lib_category is the visible tab
          searching = FALSE,
          order = list(),  # keep list_designs() row order from filtered_idx()
          ordering = TRUE,
          # Must match the 3 columns in `show` (id and alias columns are hidden)
          columnDefs = list(
            list(width = "46%", targets = 0),  # label (+ permalink)
            list(width = "32%", targets = 1),  # params
            list(width = "22%", targets = 2)   # packages
          ),
          # Hide Previous/Next when everything fits on one page
          drawCallback = DT::JS(
            "function(settings) {",
            "  var api = this.api();",
            "  var pages = api.page.info().pages;",
            "  var $pag = $(api.table().container()).find('.dataTables_paginate');",
            "  if (pages <= 1) { $pag.hide(); } else { $pag.show(); }",
            "}"
          )
        )
      )
    })
  } else {
    output$library_table_basic <- renderTable({
      library_display()[, c("label", "params", "packages"), drop = FALSE]
    })
  }

  observeEvent(input$library_table_rows_selected, {
    rows <- input$library_table_rows_selected
    df <- library_display()
    if (length(rows) && rows[[1]] <= nrow(df)) {
      go_to_design(df$.id_key[[rows[[1]]]])
    }
  }, ignoreNULL = TRUE)

  selected_header_ui <- function() {
    id <- selected_id()
    if (is.na(id) || !nzchar(id)) {
      return(div(class = "rd-card", p("Open a design from the Library tab.")))
    }
    info <- DesignLibrary::design_info(id)
    share <- tryCatch(design_share_url(id), error = function(e) design_query(id))
    div(
      class = "rd-selected-banner",
      div(
        h3(style = "margin: 0;", info$label %||% id),
        p(
          class = "rd-muted", style = "margin: 0.2rem 0 0;",
          id,
          if (!is.null(info$alias) && !is.na(info$alias)) paste0(' · alias "', info$alias, '"') else NULL,
          " · ", info$category %||% "Other"
        ),
        tags$a(
          href = design_query(id),
          class = "rd-permalink",
          style = "margin-top: 0.35rem;",
          title = share,
          `aria-label` = paste("Shareable link to", id),
          HTML(permalink_svg)
        )
      ),
      actionButton("back_library", "Back to library", class = "btn-outline-secondary btn-sm")
    )
  }

  output$selected_header <- renderUI(selected_header_ui())
  output$selected_header_diag <- renderUI(selected_header_ui())

  go_to_library <- function() {
    shiny::updateQueryString("", mode = "replace", session = session)
    if (has_bslib) {
      bslib::nav_select("main_nav", selected = "Library", session = session)
    } else {
      updateTabsetPanel(session, "main_nav", selected = "Library")
    }
  }

  observeEvent(input$back_library, go_to_library())
  observeEvent(input$goto_library_from_mod, go_to_library())

  output$redesign_top <- renderUI({
    id <- selected_id()
    if (is.na(id) || !nzchar(id)) {
      return(div(
        class = "rd-card",
        p(
          "First open a design from the ",
          actionLink("goto_library_from_mod", "Library"),
          ", then come back here to change parameters and re-diagnose."
        )
      ))
    }
    tagList(
      selected_header_ui(),
      div(
        class = "rd-card",
        div(
          class = "rd-help-box",
          redesign_kind_help(id)
        ),
        uiOutput("mod_param_grid"),
        fluidRow(
          column(3, numericInput("mod_sims", "Simulations", value = 50, min = 1, step = 10, width = "100%")),
          column(
            9,
            div(
              style = "margin-top: 1.7rem; display: flex; gap: 0.5rem; flex-wrap: wrap;",
              actionButton("run_redesign", "Run redesign and diagnosis", class = "btn-primary"),
              actionButton("reset_mods", "Reset", class = "btn-outline-secondary btn-sm")
            )
          )
        )
      )
    )
  })

  output$mod_show_results <- reactive(!is.null(mod_diag()))
  outputOptions(output, "mod_show_results", suspendWhenHidden = FALSE)

  output$design_profile_text <- renderText({
    id <- selected_id()
    req(!is.na(id), nzchar(id))
    DesignLibrary::design_profile(id)
  })

  output$simple_code_ui <- renderUI({
    id <- selected_id()
    req(!is.na(id), nzchar(id))
    code <- DesignLibrary::get_code(id, style = "simple")
    tags$pre(class = "rd-code rd-code-oneline", code$simple)
  })

  output$full_code_ui <- renderUI({
    id <- selected_id()
    req(!is.na(id), nzchar(id))
    code <- DesignLibrary::get_code(id, style = "full")
    tags$pre(class = "rd-code", code$full)
  })

  output$args_table <- renderTable({
    id <- selected_id()
    req(!is.na(id), nzchar(id))
    args <- DesignLibrary::get_args(id)
    show <- data.frame(
      name = args$name,
      default = args$value_str,
      kind = if ("kind" %in% names(args)) args$kind else "",
      tip = args$tip,
      stringsAsFactors = FALSE
    )
    show
  })

  observeEvent(input$run_once, {
    id <- selected_id()
    req(!is.na(id))
    out <- tryCatch({
      d <- DesignLibrary::make_design(id)
      run <- DeclareDesign::run_design(d)
      paste(utils::capture.output(print(run)), collapse = "\n")
    }, error = function(e) paste("Error:", conditionMessage(e)))
    run_once_txt(out)
  })

  output$run_once_out <- renderText(run_once_txt())

  # ---- Diagnosis ----
  current_diagnosis <- reactive({
    id <- selected_id()
    req(!is.na(id), nzchar(id))
    live <- live_diag()
    if (!is.null(live)) return(live)
    DesignLibrary::get_preview(id)
  })

  output$preview_status <- renderUI({
    id <- selected_id()
    if (is.na(id) || !nzchar(id)) return(NULL)
    if (!is.null(live_diag())) {
      return(p(tags$strong("Showing: "), "live diagnosis from this session."))
    }
    prev <- DesignLibrary::get_preview(id)
    if (is.null(prev)) {
      p(class = "rd-muted", "No baked preview yet. Click “Run diagnosis”, or ask a maintainer to run refresh_library().")
    } else {
      p(tags$strong("Showing: "), sprintf("baked preview (%s sims).", prev$sims %||% "?"))
    }
  })

  observeEvent(input$run_diagnosis, {
    id <- selected_id()
    req(!is.na(id))
    sims <- as.integer(input$diag_sims %||% 100)
    withProgress(message = "Diagnosing…", value = 0.3, {
      res <- tryCatch({
        d <- DesignLibrary::make_design(id)
        diagnosis <- DeclareDesign::diagnose_design(d, sims = sims)
        summary <- tryCatch(DeclareDesign::get_diagnosands(diagnosis), error = function(e) NULL)
        tidy <- tryCatch(generics::tidy(diagnosis), error = function(e) NULL)
        list(id = id, sims = sims, summary = summary, tidy = tidy, diagnosis = diagnosis, live = TRUE)
      }, error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(conditionMessage(res), type = "error")
      } else {
        live_diag(res)
        showNotification("Diagnosis complete.", type = "message")
      }
    })
  })

  diagnosand_df <- function(obj) {
    if (is.null(obj)) return(NULL)
    if (!is.null(obj$tidy) && is.data.frame(obj$tidy) && nrow(obj$tidy) &&
        "diagnosand" %in% names(obj$tidy)) {
      return(obj$tidy)
    }
    if (!is.null(obj$diagnosis)) {
      tidy <- tryCatch(generics::tidy(obj$diagnosis), error = function(e) NULL)
      if (is.null(tidy)) {
        tidy <- tryCatch(DeclareDesign::tidy.diagnosis(obj$diagnosis), error = function(e) NULL)
      }
      if (!is.null(tidy) && is.data.frame(tidy) && nrow(tidy)) return(tidy)
    }
    if (!is.null(obj$summary) && is.data.frame(obj$summary) && nrow(obj$summary) &&
        "diagnosand" %in% names(obj$summary)) {
      return(obj$summary)
    }
    if (!is.null(obj$summary) && is.data.frame(obj$summary) && nrow(obj$summary)) {
      return(obj$summary)
    }
    NULL
  }

  default_diagnosands <- function(all_names, design_id = NULL) {
    all_names <- available_diagnosands(all_names, design_id)
    prefer <- character(0)
    if (!is.null(design_id) && length(design_id) && !is.na(design_id) && nzchar(as.character(design_id)[[1]])) {
      prefer <- tryCatch(
        DesignLibrary::preferred_diagnosands(as.character(design_id)[[1]]),
        error = function(e) character(0)
      )
    }
    if (!length(prefer)) prefer <- c("bias", "power")
    hit <- character(0)
    for (p in prefer) {
      m <- all_names[tolower(all_names) == tolower(p)]
      if (length(m)) hit <- c(hit, m[[1]])
    }
    hit <- unique(hit)
    if (length(hit)) return(hit)
    utils::head(all_names, 2L)
  }

  available_diagnosands <- function(all_names, design_id = NULL) {
    all_names <- unique(as.character(all_names))
    exclude <- character(0)
    if (!is.null(design_id) && length(design_id) && !is.na(design_id) && nzchar(as.character(design_id)[[1]])) {
      exclude <- tryCatch(
        DesignLibrary::excluded_diagnosands(as.character(design_id)[[1]]),
        error = function(e) character(0)
      )
    }
    if (length(exclude)) {
      all_names <- all_names[!tolower(all_names) %in% tolower(exclude)]
    }
    all_names
  }

  filter_diagnosands <- function(df, selected) {
    if (is.null(df) || !nrow(df) || !"diagnosand" %in% names(df)) return(df)
    if (is.null(selected) || !length(selected)) return(df)
    df[as.character(df$diagnosand) %in% selected, , drop = FALSE]
  }

  y_value_col <- function(df) {
    if ("estimate" %in% names(df)) return("estimate")
    if ("mean" %in% names(df)) return("mean")
    if ("estimand" %in% names(df) && is.numeric(df$estimand)) return("estimand")
    NULL
  }

  # DeclareDesign tidy() gives estimate + se(<diagnosand>), not conf.low/high
  add_diagnosand_ci <- function(df, z = 1.96) {
    if (is.null(df) || !nrow(df)) return(df)
    if (!"estimate" %in% names(df) && "mean" %in% names(df)) {
      df$estimate <- df$mean
    }
    if (!all(c("diagnosand", "estimate") %in% names(df))) return(df)
    if (all(c("conf.low", "conf.high") %in% names(df))) {
      # Still fill any missing CI from se(...) when possible
      need <- is.na(df$conf.low) | is.na(df$conf.high)
      if (!any(need)) return(df)
    } else {
      df$conf.low <- NA_real_
      df$conf.high <- NA_real_
      need <- rep(TRUE, nrow(df))
    }

    se <- vapply(seq_len(nrow(df)), function(i) {
      if (!isTRUE(need[[i]])) return(NA_real_)
      d <- as.character(df$diagnosand[[i]])
      col <- paste0("se(", d, ")")
      if (col %in% names(df)) {
        v <- suppressWarnings(as.numeric(df[[col]][[i]]))
        if (length(v) && is.finite(v)) return(v)
      }
      for (alt in c("std.error", "se", "std_error")) {
        if (alt %in% names(df)) {
          v <- suppressWarnings(as.numeric(df[[alt]][[i]]))
          if (length(v) && is.finite(v)) return(v)
        }
      }
      NA_real_
    }, numeric(1))

    fill <- need & is.finite(se) & is.finite(df$estimate)
    df$conf.low[fill] <- df$estimate[fill] - z * se[fill]
    df$conf.high[fill] <- df$estimate[fill] + z * se[fill]
    df
  }

  # Forest-style panels (original wizard diagnosis plot) when no parameter ranges
  plot_diagnosand_forest <- function(df) {
    df <- add_diagnosand_ci(df)
    if (!"estimate" %in% names(df) && "mean" %in% names(df)) {
      df$estimate <- df$mean
    }
    if (!"estimate" %in% names(df) || !"diagnosand" %in% names(df)) return(NULL)

    inquiry_col <- if ("inquiry" %in% names(df)) "inquiry" else NULL
    est_col <- if ("estimator" %in% names(df)) {
      "estimator"
    } else if ("estimator_label" %in% names(df)) {
      "estimator_label"
    } else {
      NULL
    }
    term_col <- if ("term" %in% names(df)) "term" else NULL
    n_est <- if (!is.null(est_col)) length(unique(df[[est_col]])) else 0L
    n_term <- if (!is.null(term_col)) length(unique(df[[term_col]])) else 0L

    if (is.null(est_col)) {
      df$.y <- "estimate"
    } else if (n_est > 1L && n_term > 1L && !is.null(term_col)) {
      df$.y <- paste(df[[est_col]], "-", df[[term_col]])
    } else if (n_term > 1L && !is.null(term_col)) {
      df$.y <- as.character(df[[term_col]])
    } else {
      df$.y <- as.character(df[[est_col]])
    }

    has_ci <- all(c("conf.low", "conf.high") %in% names(df)) &&
      any(is.finite(df$conf.low) & is.finite(df$conf.high))
    df$diagnosand <- factor(df$diagnosand, levels = unique(as.character(df$diagnosand)))

    use_color <- !is.null(inquiry_col) && length(unique(df[[inquiry_col]])) > 1L
    # Also dodge when several series share a y-level (e.g. color by inquiry)
    n_per_y <- as.integer(table(paste(df$diagnosand, df$.y, sep = "\r")))
    need_dodge <- use_color || any(n_per_y > 1L)
    pd <- if (need_dodge) ggplot2::position_dodge(width = 0.5) else "identity"

    if (use_color) {
      df$.inquiry <- df[[inquiry_col]]
      p <- ggplot2::ggplot(df, ggplot2::aes(x = estimate, y = .y, color = .inquiry, group = .inquiry))
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = estimate, y = .y))
    }
    if (has_ci) {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(xmin = conf.low, xmax = conf.high),
        orientation = "y",
        width = 0.25,
        position = pd,
        na.rm = TRUE
      )
    }
    p <- p +
      ggplot2::geom_point(size = 2.4, position = pd, na.rm = TRUE) +
      ggplot2::facet_wrap(~diagnosand, scales = "free_x", ncol = 2) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::labs(x = NULL, y = NULL, color = NULL)

    n_y <- length(unique(df$.y))
    if (n_y <= 1L) {
      p <- p + ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
    }
    p
  }

  # Dodge width in x-data units when x is numeric; otherwise a fraction of a category
  dodge_width_for_x <- function(x) {
    x <- x[is.finite(as.numeric(x))]
    if (!length(x)) return(0.4)
    if (is.numeric(x) || is.integer(x)) {
      u <- sort(unique(as.numeric(x)))
      if (length(u) >= 2L) return(0.25 * min(diff(u)))
      return(0.1)
    }
    0.4
  }

  output$diag_diagnosand_ui <- renderUI({
    df <- diagnosand_df(current_diagnosis())
    if (is.null(df) || !"diagnosand" %in% names(df)) return(NULL)
    all_d <- available_diagnosands(unique(as.character(df$diagnosand)), selected_id())
    if (!length(all_d)) return(NULL)
    sel <- input$diag_diagnosands
    if (is.null(sel) || !length(intersect(sel, all_d))) {
      sel <- default_diagnosands(all_d, selected_id())
    }
    selectInput(
      "diag_diagnosands",
      NULL,
      choices = all_d,
      selected = sel,
      multiple = TRUE,
      width = "100%"
    )
  })

  output$diagnosis_table <- renderTable({
    df <- diagnosand_df(current_diagnosis())
    if (is.null(df) || !nrow(df) || !"diagnosand" %in% names(df)) return(df)
    all_d <- available_diagnosands(unique(as.character(df$diagnosand)), selected_id())
    sel <- input$diag_diagnosands
    if (is.null(sel) || !length(sel)) sel <- default_diagnosands(all_d, selected_id())
    filter_diagnosands(df, sel)
  })

  if (has_ggplot2) {
    output$diagnosis_plot <- renderPlot({
      df <- diagnosand_df(current_diagnosis())
      if (!is.null(df) && nrow(df) && "diagnosand" %in% names(df)) {
        all_d <- available_diagnosands(unique(as.character(df$diagnosand)), selected_id())
        sel <- input$diag_diagnosands
        if (is.null(sel) || !length(sel)) sel <- default_diagnosands(all_d, selected_id())
        df <- filter_diagnosands(df, sel)
      }
      if (is.null(df) || !nrow(df)) {
        plot.new()
        text(0.5, 0.5, "No diagnosis to plot yet.")
        return(invisible())
      }
      p <- plot_diagnosand_forest(df)
      if (is.null(p)) {
        plot.new()
        text(0.5, 0.5, "Diagnosis loaded; see table for values.")
        return(invisible())
      }
      p
    })
  }

  # ---- Redesign ----
  output$mod_param_grid <- renderUI({
    id <- selected_id()
    if (is.na(id) || !nzchar(id)) return(NULL)
    args <- DesignLibrary::get_args(id)
    data_args <- if ("kind" %in% names(args) && nrow(args)) {
      args[args$kind %in% c("data", "function", "list"), , drop = FALSE]
    } else {
      args[0, , drop = FALSE]
    }
    shiny_args <- if ("shiny" %in% names(args) && nrow(args)) {
      args[isTRUE(args$shiny) | args$shiny %in% TRUE, , drop = FALSE]
    } else {
      args
    }
    note <- NULL
    if (nrow(data_args)) {
      note <- p(
        class = "rd-muted",
        "Also takes in R: ",
        tags$code(
          paste0(
            "make_design(\"", id, "\", ",
            paste(sprintf("%s = ...", data_args$name), collapse = ", "),
            ")"
          )
        )
      )
    }
    if (!nrow(shiny_args)) {
      return(tagList(note, helpText("No modifiable parameters in the browser.")))
    }
    rows <- lapply(seq_len(nrow(shiny_args)), function(i) {
      nm <- shiny_args$name[[i]]
      kind <- if ("kind" %in% names(shiny_args)) shiny_args$kind[[i]] else "scalar"
      tip <- tip_title(shiny_args$tip[[i]], kind)
      tagList(
        tags$div(class = "rd-param-name", nm),
        tags$span(class = "rd-tip", title = tip, "i"),
        textInput(
          inputId = paste0("mod_val_", nm),
          label = NULL,
          value = format_arg_default(shiny_args[i, , drop = FALSE]),
          width = "100%",
          placeholder = if (identical(kind, "vector")) {
            "e.g. 0.1, 0.2, 0.3;"
          } else {
            "e.g. 100 or 0, 10, 20"
          }
        )
      )
    })
    tagList(note, div(class = "rd-param-grid", rows))
  })

  current_mod_state <- reactive({
    id <- selected_id()
    req(!is.na(id), nzchar(id))
    args <- DesignLibrary::get_args(id)
    lapply(args$name, function(nm) input[[paste0("mod_val_", nm)]])
    collect_mod_dots(id)
  })

  output$mod_error <- renderUI({
    msg <- mod_status()
    if (!nzchar(msg %||% "")) return(NULL)
    div(class = "rd-error", msg)
  })

  output$mod_diagnosand_ui <- renderUI({
    df <- diagnosand_df(mod_diag())
    if (is.null(df) || !"diagnosand" %in% names(df)) return(NULL)
    all_d <- available_diagnosands(unique(as.character(df$diagnosand)), selected_id())
    if (!length(all_d)) return(NULL)
    sel <- input$mod_diagnosands
    if (is.null(sel) || !length(intersect(sel, all_d))) {
      sel <- default_diagnosands(all_d, selected_id())
    }
    selectInput(
      "mod_diagnosands",
      "Diagnosands",
      choices = all_d,
      selected = sel,
      multiple = TRUE,
      width = "100%"
    )
  })

  output$mod_plot_controls <- renderUI({
    st <- tryCatch(current_mod_state(), error = function(e) NULL)
    rp <- mod_last_ranges()
    if (!length(rp)) rp <- st$range_params %||% character(0)
    if (length(rp) != 2L) return(NULL)
    fluidRow(
      column(6, selectInput("mod_plot_x", "X-axis", choices = rp, selected = input$mod_plot_x %||% rp[[1]], width = "100%")),
      column(6, selectInput("mod_plot_group", "Group / color", choices = rp, selected = input$mod_plot_group %||% rp[[2]], width = "100%"))
    )
  })

  observeEvent(input$reset_mods, {
    id <- selected_id()
    req(!is.na(id))
    args <- DesignLibrary::get_args(id)
    if ("shiny" %in% names(args)) {
      args <- args[isTRUE(args$shiny) | args$shiny %in% TRUE, , drop = FALSE]
    }
    for (i in seq_len(nrow(args))) {
      updateTextInput(
        session,
        paste0("mod_val_", args$name[[i]]),
        value = format_arg_default(args[i, , drop = FALSE])
      )
    }
    mod_diag(NULL)
    mod_last_ranges(character(0))
    mod_status("")
  })

  observeEvent(input$run_redesign, {
    id <- selected_id()
    req(!is.na(id))
    sims <- as.integer(input$mod_sims %||% 50)
    if (is.na(sims) || sims < 1L) sims <- 50L
    st <- collect_mod_dots(id)
    if (!is.null(st$error)) {
      mod_status(st$error)
      return()
    }
    if (length(st$range_params) > 2L) {
      mod_status(paste0(
        "At most two parameters may be given a range. You have ranges for: ",
        paste(st$range_params, collapse = ", "), "."
      ))
      return()
    }
    mod_status("")
    withProgress(message = "Redesign + diagnosis…", value = 0.2, {
      res <- tryCatch({
        design <- do.call(DesignLibrary::make_design, c(list(design = id), st$dots))
        diagnosis <- DeclareDesign::diagnose_design(design, sims = sims)
        tidy <- tryCatch(generics::tidy(diagnosis), error = function(e) NULL)
        if (is.null(tidy)) {
          tidy <- tryCatch(DeclareDesign::tidy.diagnosis(diagnosis), error = function(e) NULL)
        }
        summary <- tryCatch(DeclareDesign::get_diagnosands(diagnosis), error = function(e) NULL)
        list(
          id = id,
          sims = sims,
          tidy = tidy,
          summary = summary,
          diagnosis = diagnosis,
          range_params = st$range_params,
          plot_x = input$mod_plot_x %||% (st$range_params[1] %||% NA_character_),
          plot_group = input$mod_plot_group %||% (st$range_params[2] %||% NA_character_)
        )
      }, error = function(e) e)

      if (!inherits(res, "error")) {
        mod_diag(res)
        mod_last_ranges(st$range_params)
        mod_status("")
      } else {
        mod_status(paste("Error:", conditionMessage(res)))
      }
    })
  })

  filtered_mod_df <- reactive({
    df <- diagnosand_df(mod_diag())
    if (is.null(df) || !nrow(df)) return(NULL)
    if ("diagnosand" %in% names(df)) {
      all_d <- available_diagnosands(unique(as.character(df$diagnosand)), selected_id())
      sel <- input$mod_diagnosands
      if (is.null(sel) || !length(sel)) sel <- default_diagnosands(all_d, selected_id())
      sel <- intersect(sel, all_d)
      if (length(sel)) df <- df[as.character(df$diagnosand) %in% sel, , drop = FALSE]
    }
    df
  })

  output$mod_table <- renderTable({
    filtered_mod_df()
  })

  if (has_ggplot2) {
    output$mod_plot <- renderPlot({
      obj <- mod_diag()
      df <- filtered_mod_df()
      if (is.null(df) || !nrow(df)) {
        plot.new()
        text(0.5, 0.5, "Run redesign and diagnosis to plot.")
        return(invisible())
      }
      if (!"diagnosand" %in% names(df)) {
        plot.new()
        text(0.5, 0.5, "Diagnosis loaded; open the table below.")
        return(invisible())
      }

      ranges <- obj$range_params %||% mod_last_ranges()
      ranges <- ranges[ranges %in% names(df)]

      # No ranges: forest / CI panels like the original diagnosis plot
      if (length(ranges) == 0L) {
        p <- plot_diagnosand_forest(df)
        if (is.null(p)) {
          plot.new()
          text(0.5, 0.5, "Diagnosis loaded; open the table below.")
          return(invisible())
        }
        return(p)
      }

      y_col <- y_value_col(df)
      if (is.null(y_col)) {
        plot.new()
        text(0.5, 0.5, "Diagnosis loaded; open the table below.")
        return(invisible())
      }
      df <- add_diagnosand_ci(df)
      df$.y <- df[[y_col]]
      df$diagnosand <- factor(df$diagnosand)
      has_ci <- all(c("conf.low", "conf.high") %in% names(df)) &&
        any(is.finite(df$conf.low) & is.finite(df$conf.high))

      if (length(ranges) == 1L) {
        xp <- ranges[[1]]
        df$.x <- df[[xp]]
        # Color by estimator when several share an x-value
        est_col <- if ("estimator" %in% names(df)) "estimator" else NULL
        use_color <- !is.null(est_col) && length(unique(df[[est_col]])) > 1L
        if (use_color) {
          df$.series <- factor(df[[est_col]])
          p <- ggplot2::ggplot(df, ggplot2::aes(x = .x, y = .y, color = .series, group = .series))
        } else {
          p <- ggplot2::ggplot(df, ggplot2::aes(x = .x, y = .y, group = 1))
        }
        pd <- if (use_color) {
          ggplot2::position_dodge(width = dodge_width_for_x(df$.x))
        } else {
          "identity"
        }
        if (has_ci) {
          p <- p + ggplot2::geom_errorbar(
            ggplot2::aes(ymin = conf.low, ymax = conf.high),
            width = 0,
            linewidth = 0.5,
            position = pd,
            na.rm = TRUE
          )
        }
        p +
          ggplot2::geom_line(linewidth = 0.8, position = pd) +
          ggplot2::geom_point(size = 2.2, position = pd, na.rm = TRUE) +
          ggplot2::facet_wrap(~diagnosand, scales = "free_y") +
          ggplot2::theme_bw(base_size = 12) +
          ggplot2::labs(x = xp, y = y_col, color = if (use_color) "estimator" else NULL)
      } else {
        xp <- input$mod_plot_x %||% obj$plot_x %||% ranges[[1]]
        gp <- input$mod_plot_group %||% obj$plot_group %||% ranges[[2]]
        if (!xp %in% names(df)) xp <- ranges[[1]]
        if (!gp %in% names(df)) gp <- setdiff(ranges, xp)[1]
        if (identical(xp, gp)) gp <- setdiff(ranges, xp)[1]
        df$.x <- df[[xp]]
        df$.g <- factor(df[[gp]])
        pd <- ggplot2::position_dodge(width = dodge_width_for_x(df$.x))
        p <- ggplot2::ggplot(df, ggplot2::aes(x = .x, y = .y, color = .g, group = .g))
        if (has_ci) {
          p <- p + ggplot2::geom_errorbar(
            ggplot2::aes(ymin = conf.low, ymax = conf.high),
            width = 0,
            linewidth = 0.5,
            position = pd,
            na.rm = TRUE
          )
        }
        p +
          ggplot2::geom_line(linewidth = 0.8, position = pd) +
          ggplot2::geom_point(size = 2.2, position = pd, na.rm = TRUE) +
          ggplot2::facet_wrap(~diagnosand, scales = "free_y") +
          ggplot2::theme_bw(base_size = 12) +
          ggplot2::labs(x = xp, y = y_col, color = gp)
      }
    })
  }
}

shinyApp(ui, server)
