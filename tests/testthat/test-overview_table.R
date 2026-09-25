# The overview table in inst/extdata lists every designer and the vignette
# that describes it. The vignettes are excluded from the package build, so the
# vignette half can only run from a source checkout.

overview <- read.csv(system.file("extdata", "overview.csv", package = "DesignLibrary"))
exports <- getNamespaceExports("DesignLibrary")
designers <- exports[grepl("_designer$", exports) & !grepl("^simple_", exports)]

test_that("every designer in the table is in the library, and the reverse", {
  table_designers <- overview$designer[overview$designer != ""]
  expect_setequal(table_designers, designers)
})

test_that("every vignette in the table exists, and every design vignette is in the table", {
  vignette_dir <- test_path("..", "..", "vignettes")
  skip_if_not(dir.exists(vignette_dir), "vignettes are not part of the built package")
  vignettes <- tools::file_path_sans_ext(list.files(vignette_dir, pattern = "\\.Rmd$"))
  vignettes <- vignettes[!grepl("^How_to_Write", vignettes)]
  table_vignettes <- overview$vignette[overview$vignette != ""]
  expect_setequal(tolower(table_vignettes), tolower(vignettes))
})
