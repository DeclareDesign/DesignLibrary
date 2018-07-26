context(desc = "Testing that overview table is correctly formatted")

# Design overview table
overview <- read.csv(system.file("extdata", "overview.csv", package = "DesignLibrary"))

# Get designers
functions <- ls("package:DesignLibrary")
designers <- functions[grepl("_designer\\b",functions)]

# Get design vignettes
vignettes <- list.files("vignettes")
exclude_vignettes <- vignettes[which(grepl("how_to_write|bib.bib",vignettes,TRUE))]
vignettes <- vignettes[-which(vignettes %in% exclude_vignettes)]
vignettes <- gsub(".Rmd","",vignettes)

testthat::test_that(
  desc = paste0("No designers in the table should be absent from the library"),
  code = {
    expect_true(all(overview$designer %in% designers))
  }
)

testthat::test_that(
  desc = paste0("All designers in the library should be documented in the table"),
  code = {
    expect_true(all(designers %in% overview$designer))
  }
)

testthat::test_that(
  desc = paste0("No vignettes in the table should be absent from the library"),
  code = {
    expect_true(all(vignettes %in% overview$vignette))
  }
)

testthat::test_that(
  desc = paste0("All vignettes in the library should be documented in the table"),
  code = {
    expect_true(all(vignettes %in% overview$vignette))
  }
)



