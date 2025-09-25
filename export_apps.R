# Delete existing exports
unlink("docs", recursive = TRUE)

# Re-export with clean slate
library(shinylive)

shinylive::export(
  appdir = "beta-distribution",
  destdir = "docs/beta-distribution"
)

shinylive::export(
  appdir = "conjugate-beta-model", 
  destdir = "docs/conjugate-beta-model"
)
