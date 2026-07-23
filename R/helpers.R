setup_visualization_font <- function(font_path = file.path("assets", "DankMono-Regular.otf")) {
  font_family <- "sans"

  if (!file.exists(font_path)) {
    warning("Custom font not found at ", font_path, "; using default font.")
    return(font_family)
  }

  tryCatch(
    {
      font_dir <- dirname(font_path)
      base_name <- tools::file_path_sans_ext(basename(font_path))

      # If the requested font is DankMono-Regular, load the full family.
      if (base_name == "DankMono-Regular") {
        regular <- font_path
        bold <- file.path(font_dir, "DankMono-Bold.otf")
        italic <- file.path(font_dir, "DankMono-Italic.otf")

        sysfonts::font_add(
          "DankMono",
          regular = regular,
          bold = if (file.exists(bold)) bold else regular,
          italic = if (file.exists(italic)) italic else regular
        )
      } else {
        sysfonts::font_add("DankMono", regular = font_path)
      }

      showtext::showtext_auto()
      "DankMono"
    },
    error = function(error) {
      warning("Failed to load custom font, using default: ", error$message)
      font_family
    }
  )
}
