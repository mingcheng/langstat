load_local_env <- function(path = ".env") {
  if (!file.exists(path)) {
    return(invisible(FALSE))
  }

  previous <- Sys.getenv()
  loaded <- FALSE
  on.exit({
    if (!loaded) {
      Sys.unsetenv(setdiff(names(Sys.getenv()), names(previous)))
    }
    # Exported environment variables take precedence over local configuration.
    invisible(do.call(Sys.setenv, as.list(previous)))
  }, add = TRUE)

  loaded <- withCallingHandlers(
    readRenviron(path),
    warning = function(w) {
      # readRenviron warnings can contain the offending line, including secrets.
      stop("Failed to load ", path, "; expected KEY=value entries.", call. = FALSE)
    }
  )
  if (!isTRUE(loaded)) {
    stop("Failed to load local environment file: ", path, call. = FALSE)
  }
  invisible(TRUE)
}

setup_visualization_font <- function(font_path = file.path("assets", "DankMono-Regular.otf"),
                                     dpi = 120) {
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

      showtext::showtext_opts(dpi = dpi)
      showtext::showtext_auto()
      "DankMono"
    },
    error = function(error) {
      warning("Failed to load custom font, using default: ", error$message)
      font_family
    }
  )
}

visualization_style <- function(languages) {
  include_other <- "Other" %in% languages
  languages <- setdiff(languages, "Other")
  languages <- sort(unique(as.character(languages)))
  palette <- c(
    "#7DD3FC", "#A5B4FC", "#5EEAD4", "#FCD34D",
    "#FDA4AF", "#C4B5FD", "#86EFAC", "#FDBA74",
    "#67E8F9", "#F0ABFC", "#BEF264", "#CBD5E1"
  )
  if (length(languages) > length(palette)) {
    palette <- grDevices::hcl.colors(length(languages), palette = "Pastel 1")
  }
  colors <- stats::setNames(palette[seq_along(languages)], languages)
  if (include_other) {
    colors["Other"] <- "#CBD5E1"
  }
  list(
    background = "#F8FAFC",
    foreground = "#0F172A",
    muted = "#475569",
    colors = colors
  )
}

prepare_chart_data <- function(repos, max_languages = Inf) {
  total_score <- sum(repos$final_score)
  if (any(!is.finite(repos$final_score)) || any(repos$final_score < 0) ||
      !is.finite(total_score) || total_score <= 0) {
    stop("Charts require non-negative scores with a positive finite total.")
  }
  if (length(max_languages) != 1L || is.na(max_languages) ||
      max_languages < 1 || max_languages != floor(max_languages)) {
    stop("max_languages must be a positive integer or Inf.")
  }
  repos |>
    dplyr::arrange(dplyr::desc(final_score)) |>
    dplyr::mutate(
      language = dplyr::if_else(
        final_score / .env$total_score >= 0.01 &
          dplyr::row_number() <= .env$max_languages,
        as.character(language), "Other"
      )
    ) |>
    dplyr::group_by(language) |>
    dplyr::summarise(
      final_score = sum(final_score),
      repo_count = sum(repo_count),
      .groups = "drop"
    ) |>
    dplyr::filter(final_score > 0) |>
    dplyr::arrange(language == "Other", dplyr::desc(final_score)) |>
    dplyr::mutate(
      percentage = round(final_score / .env$total_score * 100, 1),
      label = paste0(language, " (", percentage, "%)")
    )
}
