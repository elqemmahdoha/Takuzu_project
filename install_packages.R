packages_needed <- c(
  "shiny",
  "bslib",
  "shinyWidgets"
)

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(packages_needed, install_if_missing))

# Installer le package takuzuu créé par notre binome
if (file.exists("takuzuu_0.0.0.9000.tar.gz")) {
  install.packages("takuzuu_0.0.0.9000.tar.gz", repos = NULL, type = "source")
  cat("📦 Le package 'takuzuu' a été installé depuis le fichier local.\n")
} else {
  cat("⚠️ Le fichier takuzuu_0.0.0.9000.tar.gz est introuvable. Veuillez le fournir.\n")
}

cat("✅ Tous les packages nécessaires sont installés !\n")
