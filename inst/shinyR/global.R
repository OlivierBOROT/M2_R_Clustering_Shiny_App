# Increase upload size limit to 500 MB
options(shiny.maxRequestSize = 500 * 1024^2)

# Path of the translations file inside the package
translations_file <- system.file(
  "shinyR/texts/dictionnary.csv",
  package = "M2RClust"
)

# Themes shared by UI/server so server can toggle between them
themes <- list(
  light = bslib::bs_theme(version = 4, bootswatch = "flatly"),
  dark  = bslib::bs_theme(version = 4, bootswatch = "darkly")
)

# Load translations
translations_df <- read.csv(translations_file, stringsAsFactors = FALSE)

translation <- lapply(
  X  = split(translations_df[, -1], translations_df$key),
  FUN = as.list
)

# translation est maintenant disponible dans ui.R et server.R