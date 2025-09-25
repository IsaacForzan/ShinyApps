# Export script for shinylive apps
library(shinylive)

project_dir <- "C:/Users/ASUS VivoBook/Documents/Profesional/shiny-apps"
setwd(project_dir)

# Create docs directory for GitHub Pages
if (!dir.exists("docs")) {
  dir.create("docs")
}

# Export beta-distribution app
shinylive::export(
  appdir = "beta-distribution",
  destdir = "docs/beta-distribution"
)

# Export conjugate-beta-model app
shinylive::export(
  appdir = "conjugate-beta-model", 
  destdir = "docs/conjugate-beta-model"
)
