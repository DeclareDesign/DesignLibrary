setwd("C:/WZB Dropbox/Macartan Humphreys/5_github/ResearchDesigns")
options(DesignLibrary.root = normalizePath(".", winslash = "/"))
devtools::load_all(".", quiet = TRUE)

# Classify pre-design objects without diagnosing
parsed <- resolve_design("village_campaign")
pre <- DesignLibrary:::extract_pre_design_objects(parsed$code)
print(pre[, c("name", "type", "atomic")])

# Unit-test the filter logic on the typed table
step_types <- c("design_piece", "function")
kept <- pre[!pre$type %in% step_types, , drop = FALSE]
cat("kept after excluding steps:\n")
print(kept[, c("name", "type", "atomic")])
