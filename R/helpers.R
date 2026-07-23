setup_visualization_font <- function(font_path = file.path("assets", "FiraCode.ttf")) {
  font_family <- "sans"

  if (!file.exists(font_path)) {
    warning("Custom font not found at ", font_path, "; using default font.")
    return(font_family)
  }

  tryCatch(
    {
      sysfonts::font_add("FiraCode", regular = font_path)
      showtext::showtext_auto()
      "FiraCode"
    },
    error = function(error) {
      warning("Failed to load custom font, using default: ", error$message)
      font_family
    }
  )
}
