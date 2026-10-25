load_local_env <- function(path = ".env") {
  if (!file.exists(path)) {
    return(invisible(FALSE))
  }

  previous <- Sys.getenv()
  loaded <- FALSE
  on.exit(
    {
      if (!loaded) {
        Sys.unsetenv(setdiff(names(Sys.getenv()), names(previous)))
      }
      # Exported environment variables take precedence over local configuration.
      invisible(do.call(Sys.setenv, as.list(previous)))
    },
    add = TRUE
  )

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

profile_chart_subtitle <- function(username, summary) {
  required <- c(
    "window_start", "window_end", "public_repositories", "scored_repositories",
    "scored_owned_repositories", "scored_contributed_repositories"
  )
  if (!is.list(summary) || !all(required %in% names(summary))) {
    stop("Charts require an activity profile summary with distinct repository counts.")
  }
  sprintf(
    paste0(
      "GitHub: %s | Scored public repos: %d/%d (owned %d, contributed %d)\n",
      "%s to %s UTC | Active-day weighted"
    ),
    username, summary$scored_repositories, summary$public_repositories,
    summary$scored_owned_repositories, summary$scored_contributed_repositories,
    summary$window_start, summary$window_end
  )
}

prepare_chart_data <- function(repos, max_languages = Inf, min_percentage = 1,
                               group_other = TRUE) {
  total_score <- sum(repos$final_score)
  if (any(!is.finite(repos$final_score)) || any(repos$final_score < 0) ||
    !is.finite(total_score) || total_score <= 0) {
    stop("Charts require non-negative scores with a positive finite total.")
  }
  if (!is.numeric(max_languages) || length(max_languages) != 1L || is.na(max_languages) ||
    max_languages < 1 || max_languages != floor(max_languages)) {
    stop("max_languages must be a positive integer or Inf.")
  }
  if (!is.numeric(min_percentage) || length(min_percentage) != 1L ||
    !is.finite(min_percentage) || min_percentage < 0 || min_percentage > 100) {
    stop("min_percentage must be a finite number between 0 and 100.")
  }
  chart_data <- repos |>
    dplyr::arrange(dplyr::desc(final_score))
  visible <- chart_data$final_score / total_score >= min_percentage / 100 &
    seq_len(nrow(chart_data)) <= max_languages & chart_data$final_score > 0
  if (group_other) {
    chart_data$language <- ifelse(visible, as.character(chart_data$language), "Other")
  } else {
    chart_data <- chart_data[visible, , drop = FALSE]
  }
  if (nrow(chart_data) == 0) {
    stop("No languages meet the ", min_percentage, "% chart display threshold.")
  }
  chart_data |>
    dplyr::group_by(language) |>
    dplyr::summarise(
      final_score = sum(final_score),
      .groups = "drop"
    ) |>
    dplyr::filter(final_score > 0) |>
    dplyr::arrange(language == "Other", dplyr::desc(final_score)) |>
    dplyr::mutate(
      exact_percentage = final_score / .env$total_score * 100,
      percentage = round(exact_percentage, 1),
      label = paste0(language, " (", percentage, "%)")
    )
}
